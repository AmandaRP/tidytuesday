library(tidyverse)
library(ggpubr)
library(packcircles)

#read data:
bird_counts <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-18/bird_counts.csv")


bird_counts %>% 
  filter(!is.na(how_many_counted_by_hour)) %>%
  group_by(species) %>%
  summarize(med = median(how_many_counted_by_hour)) %>%
  filter(med > 0)

