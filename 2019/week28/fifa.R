
wwc_outcomes <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/wwc_outcomes.csv")
squads <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/squads.csv")
codes <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/codes.csv")

more_codes <- countrycode::codelist_panel %>% 
  group_by(country.name.en) %>% 
  top_n(1, year) %>%
  select(country.name.en, ioc, iso2c, iso3c, genc3c, fips)

wwc_outcomes_wcodes <- dplyr::left_join(wwc_outcomes, codes, by = "team") %>%
  select(year, team, score, country) %>%
  left_join(more_codes, by = c("team" = "ioc")) %>%
  #mutate(score = as.double(score))
  mutate(year = as.character(year))

#TODO: 1) Fix some countries. Figure out which code to use to get flags.
#%>%
#  filter(is.na(fips)) %>%
#  distinct(country)
  

#waffle chart?

library(waffle) #install from github to use geom_waffle
library(ggimage)

f <- system.file("extdata/medals.txt", package="ggimage")
medals <- read.table(f, header=TRUE)

p <- ggplot(medals, aes(Country, count)) + 
  geom_col(aes(fill = medal), width = .8)

p + geom_flag(y = -2, aes(image = code)) +
  coord_flip() + expand_limits(y = -2)  +
  scale_fill_manual(values = c("Gold" = "gold", "Bronze" = "#cd7f32", "Silver" = "#C0C0C0"))

ggplot(franchise, aes(fill=revenue_category, values=revenue)) + 
  geom_waffle(color = "white", size=.3, n_rows = 8, flip = T) +
  facet_wrap(~franchise_start, nrow=1, strip.position = "bottom")

ggplot(inner_join(wwc_outcomes_wcodes, top_countries), 
       aes(fill = year, values = score)) +
  geom_waffle(color = "white", size=1.125, n_rows = 6) +
  facet_wrap(~country.name.en, nrow=1, strip.position = "bottom") +
  #scale_x_discrete() +
  ggthemes::scale_fill_tableau(name=NULL) +
  #coord_equal() +
  #coord_flip() +
  labs(
    title = "Womens World Cup Soccer",
    subtitle = "Data provided by",
    x = "Country",
    y = "Count"
  ) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(panel.grid = element_blank(), axis.ticks.y = element_line()) +
  guides(fill = guide_legend(reverse = TRUE))

top_countries <- wwc_outcomes %>%
  group_by(team) %>%
  summarize(total_score = sum(score)) %>%
  top_n(10, total_score) %>%
  select(team)


ggplot(inner_join(wwc_outcomes_wcodes, top_countries), 
       aes(fill = year, values = score)) + 
  geom_waffle(color = "white", size = .25, n_rows = 4, flip = TRUE) +
  facet_wrap(~country.name.en, nrow = 1, strip.position = "bottom") +
  #scale_x_discrete() + 
  scale_y_continuous(labels = function(x) x * 10, # make this multiplyer the same as n_rows
                     expand = c(0,0)) +
  ggthemes::scale_fill_tableau(name=NULL) +
  #coord_equal() +
  labs(
    title = "Faceted Waffle Bar Chart",
    subtitle = "{dplyr} storms data",
    x = "Year",
    y = "Count"
  ) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(panel.grid = element_blank(), axis.ticks.y = element_line()) +
  guides(fill = guide_legend(reverse = TRUE))
