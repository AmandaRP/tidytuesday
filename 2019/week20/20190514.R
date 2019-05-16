library(tidyverse)
library(tidytext)
library(wordcloud)

#Read data:
nobel_winners <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winners.csv")

#Get list of words and their counts
words_df <- nobel_winners %>% 
  select(gender, motivation) %>%
  filter(!is.na(motivation) & !is.na(gender)) %>%
  unnest_tokens(word, motivation) %>%
  anti_join(stop_words) %>%
  group_by(gender, word) %>%
  summarize(cnt = n()) %>%
  arrange(desc(cnt)) %>% 
  ungroup()

#Create document term matrix:
dtm <- words_df %>% cast_dtm(gender, word, cnt) 

#Draw word clouds:
comparison.cloud(t(as.matrix(dtm)), max.words=75)
commonality.cloud(t(as.matrix(dtm)), max.words=40)
