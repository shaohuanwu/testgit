a=c(1,2,3)
b=2
getwd()

install.packages("tidytext")
install.packages("tidyverse")
install.packages("DT")
install.packages("tm")
install.packages("NLP")
library(NLP)
library(tidytext)
library(tidyverse)
library(tm)
library(DT)
library()
urlfile<-'https://raw.githubusercontent.com/rit-public/HappyDB/master/happydb/data/cleaned_hm.csv'
hm_data <- read_csv(urlfile)
corpus <- VCorpus(VectorSource(hm_data$cleaned_hm))%>%
  tm_map(content_transformer(tolower))%>%
  tm_map(removePunctuation)%>%
  tm_map(removeNumbers)%>%
  tm_map(removeWords, character(0))%>%
  tm_map(stripWhitespace)

stemmed <- tm_map(corpus, stemDocument) %>%
  tidy() %>%
  select(text)



