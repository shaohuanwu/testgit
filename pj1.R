urlfile<-'https://raw.githubusercontent.com/rit-public/HappyDB/master/happydb/data/cleaned_hm.csv'
hm_data <- read_csv(urlfile)
library(NLP)
library(tidytext)
library(tidyverse)
library(tm)
library(DT)

test_data = slice(hm_data,10:30)
corpus = VCorpus(VectorSource(hm_data$cleaned_hm))
corpus

corpus%>%
  tm_map(content_transformer(tolower))%>%
  tm_map(removePunctuation)%>%
  tm_map(removeNumbers)%>%
  tm_map(removeWords, character(0))%>%
  tm_map(stripWhitespace)
corpus

stemmed <- tm_map(corpus, stemDocument) %>%
  tidy() %>%
  select(text)
stemmed

dict <- tidy(corpus) %>%
  select(text) %>%
  unnest_tokens(dictionary, text)
dict

data("stop_words")
stop_words
word <- c("happy","ago","yesterday","lot","today","months","month",
          "happier","happiest","last","week","past")
stop_words <- stop_words %>%
  bind_rows(mutate(tibble(word), lexicon = "updated"))

completed <- stemmed %>%
  mutate(id = row_number()) %>%
  unnest_tokens(stems, text) %>%
  bind_cols(dict) %>%
  anti_join(stop_words, by = c("dictionary" = "word"))
completed

completed <- completed %>%
  group_by(stems) %>%
  count(dictionary) %>%
  mutate(word = dictionary[which.max(n)]) %>%
  ungroup() %>%
  select(stems, word) %>%
  distinct() %>%
  right_join(completed) %>%
  select(-stems)

completed <- completed %>%
  group_by(id) %>%
  summarise(text = str_c(word, collapse = " ")) %>%
  ungroup()

test_data <- test_data %>%
  mutate(id = row_number()) %>%
  inner_join(completed)

datatable(test_data)


