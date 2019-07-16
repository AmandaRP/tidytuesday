library(tidyverse)
library(ggimage)

#Get the data
wwc_outcomes <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/wwc_outcomes.csv")
squads <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/squads.csv")
codes <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/codes.csv")

#get alternative country codes
more_codes <- countrycode::codelist_panel %>% 
  group_by(country.name.en) %>% 
  top_n(1, year) %>%
  select(country.name.en, ioc, iso2c, iso3c, genc3c, fips)

#join data with codes
wwc_outcomes_wcodes <- dplyr::left_join(wwc_outcomes, codes, by = "team") %>%
  select(year, team, score, country) %>%
  left_join(more_codes, by = c("team" = "ioc")) %>%
  mutate(year = as.character(year))
#Fix England:
wwc_outcomes_wcodes[wwc_outcomes_wcodes$country == "England", "iso2c"] <- "GB"

#Limit plot to top scoring countries 
top_countries <- wwc_outcomes %>%
  group_by(team) %>%
  summarize(total_score = sum(score)) %>%
  top_n(11, total_score) 

#plot
ggplot(inner_join(wwc_outcomes_wcodes, top_countries), 
       aes(reorder(country, total_score), score, fill = year)) + 
  geom_col(position = position_stack(reverse = TRUE)) + #adjusted position_stack to have years increase from left to right
  ggthemes::scale_fill_tableau(name="Year") +           #nice colors
  coord_flip() +
  labs(
    title = "Womens World Cup Soccer: Total Goals (1991 - 2019)",
    caption = "Data from https://data.world/sportsvizsunday/womens-world-cup-data"
  ) +
  geom_flag(y = -5, aes(image = iso2c)) +
  expand_limits(y = -5) +
  geom_image(aes(x = "France", y = 120, 
                 image = "~/tidytuesday/2019/week28/soccer2.png"),
             size = 0.15) +
  theme_minimal() +
  theme(title = element_text(size=14),
        panel.grid = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.title = element_blank(),
        strip.text.y = element_text(angle = 180)) + 
  guides(fill = guide_legend(reverse = TRUE))  #put 2019 at top of legend

