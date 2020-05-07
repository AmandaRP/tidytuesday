# Get the Data

critic <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/critic.tsv')
user_reviews <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/user_reviews.tsv')
items <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/items.csv')
villagers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/villagers.csv')

# View Data

View(critic)
View(user_reviews)
View(items)
View(villagers)

library(tidyverse)

library(GGally)
ggparcoord(villagers,
           columns = c(5, 7), 
           groupColumn = 4) 

library(fmsb)
villagers %>% 
  group_by(personality) %>%
  summarize(cnt = n()) %>%
  radarchart
  
library(ggradar)
villagers %>% 
  group_by(personality, gender) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(pct = n / sum(n)) %>%
  select(-n) %>%
  pivot_wider(names_from = personality,
              values_from = pct,
              values_fill = list(pct = 0)) %>%
  ggradar(grid.max = 0.16,
          label.gridline.min = FALSE,
          label.gridline.max = FALSE,
          group.point.size = 2,
          legend.text.size = 20,
          group.colours = c("hotpink", "blue"),
          plot.title = "Animal Crossing: Personalities of Villagers by Gender") +
  dark_theme_gray() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank())
  


