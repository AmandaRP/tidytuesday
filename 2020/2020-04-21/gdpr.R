#

library(tidyverse)
library(magrittr)


# Get the Data

gdpr_violations <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_violations.tsv')
gdpr_text <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_text.tsv')

gdpr_violations %<>% mutate_at(vars(date),lubridate::mdy) 

View(gdpr_text)
View(gdpr_violations)

gdpr_violations %>% 
  select(picture, name, id) %>%
  group_by(picture, name) %>%
  summarize(cnt = n()) %>%
  arrange(desc(cnt)) %>%
  ggplot(aes(name,cnt)) +
  geom_col()

gdpr_violations %>%
  filter(name == "Spain")

#Idea: "bubble" map with flags showing each country. Size of bubble shows number of violations.

