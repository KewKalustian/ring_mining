library(tidyverse)
library(tidytext)
library(magrittr)
library(lsa)
library(tm)

# loading german standard (tm, lsa) and custom stopwords

lsa_stp_wrds <- stopwords_de
    
tm_stp_wrds <- stopwords("german")

spec_stp_wrds  <- readRDS("data/stp_wrds/spec_stp_wrds.RDS")

stp_wrds <- tibble(words = unique(c(lsa_stp_wrds, tm_stp_wrds, spec_stp_wrds)))


operas <- c("rheingold", "walküre", "siegfried", "tog_without_ending", "tog_fin")


operas_path <- paste0("data/ring_txt/", operas, ".txt")

cycle_raw <- lapply(operas_path, read_lines)

####################################
# cleaning text, string processing #
####################################


txt_cleaner <- function(x) {
  
  x %<>% 
    # "\n" replacing line breaks with space
    gsub("\n", " ", .) %>%
    # connecting hyphenated words
    gsub("-$", "",.) %>% 
    # as data.frame 
    data.frame(text = .) %>%
    # tidy text: 1 word per row
    unnest_tokens("words", text) %>% 
    # removing all digits
    mutate(words = str_remove_all(words, "[[:digit:]]")) %>% 
    # extracting all words without those ones in the stp_wrds tibble
    anti_join(stp_wrds, by = "words") %>% 
    # only using words with more than 3 chars
    filter(nchar(words, type = "chars") > 3)
  return(x)
}   

# cleaning
cycle_clean <- lapply(cycle_raw, txt_cleaner)

# joining words per opera/scene to one df
cycle_complete_clean <- cycle_clean[1] %>% 
  data.frame() %>% 
  mutate(opera = "rheingold")


cycle_complete_clean <- cycle_clean[2] %>% 
  data.frame() %>% 
  mutate(opera = "walküre") %>% 
  full_join(cycle_complete_clean,.,by = c("words", "opera"))


cycle_complete_clean <- cycle_clean[3] %>% 
  data.frame() %>% 
  mutate(opera = "siegfried") %>% 
  full_join(cycle_complete_clean,.,by = c("words", "opera"))


cycle_complete_clean <- cycle_clean[4] %>% 
  data.frame() %>% 
  mutate(opera = "tog_without_ending") %>% 
  full_join(cycle_complete_clean,.,by = c("words", "opera"))


cycle_complete_clean <- cycle_clean[5] %>% 
  data.frame() %>% 
  mutate(opera = "tog_fin") %>% 
  full_join(cycle_complete_clean,.,by = c("words", "opera"))


######################
# loading sentiments #
######################


sentis <- read_lines(c("data/sentiws/pos_sentis.txt", 
                       "data/sentiws/neg_sentis.txt")) %>%  
                      lapply(function(x) {
                         # separating words and values
                         res <- strsplit(x , "\t", fixed = T)[[1]]
                         # as data.frame
                         return(data.frame(words = res[1], value = res[2],
                                           stringsAsFactors = F))
                       })

# tidy up
sentis %<>% 
  bind_rows() %>% 
  mutate(words = tolower(gsub("\\|.*", "", words)), 
         value = as.numeric(value)) 
