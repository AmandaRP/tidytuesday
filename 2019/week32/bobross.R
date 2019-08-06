
library(circlize)
library(tidyverse)
library(janitor)

#Get the data:
bob_ross <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-06/bob-ross.csv")

#Clean the data:
bob_ross %<>% 
  janitor::clean_names() %>% 
  separate(episode, into = c("season", "episode"), sep = "E") %>% 
  mutate(season = str_extract(season, "[:digit:]+")) %>% 
  mutate_at(vars(season, episode), as.integer)

#Tidy data:
bob_ross %>% 
  gather(element, presence, -season, -episode, -title)  

#Adjacency matrix:
dat <- bob_ross %>% select(-episode, -season, -title)
adj <- crossprod(as.matrix(dat)) 
adj %>% View()

#Looked too noisy:
chordDiagram(adj)

#TODO: get rid of frame data features
#ideas:
#cluster episodes
#heatmap of adj matrix

