library(tidyverse)
library(gganimate)

vb_matches <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-19/vb_matches.csv', guess_max = 76000)
View(vb_matches)
str(vb_matches)
summary(vb_matches)

vb_matches %>% 
  filter(circuit == "FIVB") %>%
  mutate(wcountry = w_p1_country) %>%
  group_by(wcountry, year) %>%
  count() %>%
  ggplot(aes(x = reorder(wcountry, desc(n)), y = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  facet_wrap(~year)  -> myplot

myplot +
  facet_null() +
  aes(group = wcountry) +
  gganimate::transition_time(year)


