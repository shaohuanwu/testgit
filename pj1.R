install.packages("wordcloud")
install.packages("SnowballC")
install.packages("wordcloud2")
install.packages("jiebaR")
install.packages("jiebaRD")
install.packages("ggplot2")
library(ggplot2)
library(NLP)
library(tidytext)
library(tidyverse)
library(tm)
library(DT)
library(RColorBrewer)
library(wordcloud)
library(SnowballC)
library(wordcloud2)
library(jiebaR)
library(jiebaRD)

urlfile<-'https://raw.githubusercontent.com/rit-public/HappyDB/master/happydb/data/cleaned_hm.csv'
hm_data <- read_csv(urlfile)

urlfile<-'https://raw.githubusercontent.com/rit-public/HappyDB/master/happydb/data/demographic.csv'
demo_data <- read_csv(urlfile)
combined_data <-left_join(hm_data, demo_data, by='wid')

corpus = VCorpus(VectorSource(hm_data$cleaned_hm))
corpus

corpus = corpus %>%
  tm_map(content_transformer(tolower))%>%
  tm_map(removePunctuation)%>%
  tm_map(removeNumbers)%>%
  tm_map(removeWords, character(0))%>%
  tm_map(stripWhitespace)

stemmed <- tm_map(corpus, stemDocument) %>%
  tidy() %>%
  distinct()%>%
  select(text)

data("stop_words")
word <- c("happy","ago","yesterday","lot","today","months","month",
          "happier","happiest","last","week","past","happi","time","day","night",
          "realli","veri","abl","feel")
stop_words <- stop_words %>%
  bind_rows(mutate(tibble(word), lexicon = "updated"))

clean_words <- stemmed %>%
  mutate(id = row_number()) %>%
  distinct()%>%
  unnest_tokens(stems, text) %>%
  anti_join(stop_words, by = c("stems" = "word"))

completed_freq = freq(clean_words$stems)
sorted_index = order(completed_freq$freq, decreasing = T)
completed_freq = completed_freq[sorted_index, ]

wordcloud2(completed_freq, minSize = 5, size = 0.5)

young_data <- combined_data[combined_data$age <= 30, ]
middle_data <- combined_data[30 < combined_data$age & combined_data$age <= 50, ]
old_data <- combined_data[combined_data$age > 50, ]

young_data = select(young_data,cleaned_hm)
middle_data = select(middle_data,cleaned_hm)
old_data = select(old_data,cleaned_hm)

corpus_y = VCorpus(VectorSource(young_data$cleaned_hm))

corpus_y = corpus_y %>%
  tm_map(content_transformer(tolower))%>%
  tm_map(removePunctuation)%>%
  tm_map(removeNumbers)%>%
  tm_map(removeWords, character(0))%>%
  tm_map(stripWhitespace)
stemmed_y <- tm_map(corpus_y, stemDocument) %>%
  tidy() %>%
  distinct()%>%
  select(text)
completed_y <- stemmed_y %>%
  mutate(id = row_number()) %>%
  distinct()%>%
  unnest_tokens(stems, text) %>%
  anti_join(stop_words, by = c("stems" = "word"))
completed_y_freq = freq(completed_y$stems)
sorted_y_index = order(completed_y_freq$freq, decreasing = T)
completed_y_freq = completed_y_freq[sorted_y_index, ]

attach(completed_y_freq)
completed_y_freq$id <- 1
colnames(completed_y_freq) <- c("words", "n", "id")
rownames(completed_y_freq) <- NULL

corpus_m = VCorpus(VectorSource(middle_data$cleaned_hm))
corpus_m = corpus_m %>%
  tm_map(content_transformer(tolower))%>%
  tm_map(removePunctuation)%>%
  tm_map(removeNumbers)%>%
  tm_map(removeWords, character(0))%>%
  tm_map(stripWhitespace)
stemmed_m <- tm_map(corpus_m, stemDocument) %>%
  tidy() %>%
  distinct()%>%
  select(text)
completed_m <- stemmed_m %>%
  mutate(id = row_number()) %>%
  distinct()%>%
  unnest_tokens(stems, text) %>%
  anti_join(stop_words, by = c("stems" = "word"))
completed_m_freq = freq(completed_m$stems)
sorted_m_index = order(completed_m_freq$freq, decreasing = T)
completed_m_freq = completed_m_freq[sorted_m_index, ]

attach(completed_m_freq)
completed_m_freq$id <- 2
colnames(completed_m_freq) <- c("words", "n", "id")
rownames(completed_m_freq) <- NULL

combined_by_row <- rbind(completed_y_freq,completed_m_freq )

corpus_o = VCorpus(VectorSource(old_data$cleaned_hm))
corpus_o = corpus_o %>%
  tm_map(content_transformer(tolower))%>%
  tm_map(removePunctuation)%>%
  tm_map(removeNumbers)%>%
  tm_map(removeWords, character(0))%>%
  tm_map(stripWhitespace)
stemmed_o <- tm_map(corpus_o, stemDocument) %>%
  tidy() %>%
  distinct()%>%
  select(text)
completed_o <- stemmed_o %>%
  mutate(id = row_number()) %>%
  distinct()%>%
  unnest_tokens(stems, text) %>%
  anti_join(stop_words, by = c("stems" = "word"))
completed_o_freq = freq(completed_o$stems)
sorted_o_index = order(completed_o_freq$freq, decreasing = T)
completed_o_freq = completed_o_freq[sorted_o_index, ]

attach(completed_o_freq)
completed_o_freq$id <- 3
colnames(completed_o_freq) <- c("words", "n", "id")
rownames(completed_o_freq) <- NULL

combined_by_row <- rbind(combined_by_row,completed_o_freq )



combined_by_row
combined_by_row %>%
  bind_tf_idf(term = words,document = id,n = n) -> tf_idf_table
tf_idf_table

tf_idf_table %>% 
  group_by(id) %>% 
  top_n(10,tf_idf) %>% 
  ungroup() -> top10

top10cloud = select(top10,words,tf_idf)
wordcloud2(top10cloud)

wordcloud2(top10, color = ifelse(top10[, 3] == 1, 'red', 'skyblue'), 
             shape = 'diamond')

wordcloud2(top10, color = ifelse(top10[, 3] == 2, 'orange', 'skyblue'),
             shape = 'diamond')

 wordcloud2(top10, color = ifelse(top10[, 3] == 3, 'purple', 'skyblue'), 
             shape = 'diamond')


ggplot(top10[1:15,],aes(x = words, y = tf_idf)) +
  geom_bar(stat = "identity", fill = "green")

ggplot(top10[16:25,],aes(x = words, y = tf_idf)) +
  geom_bar(stat = "identity", fill = "blue")

ggplot(top10[26:40,],aes(x = words, y = tf_idf)) +
  geom_bar(stat = "identity", fill = "yellow")

getwd()
