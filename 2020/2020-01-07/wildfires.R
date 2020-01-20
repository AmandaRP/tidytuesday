# Australian Wildfires TidyTuesday

library(tidyverse)
library(lubridate)
library(magrittr)
library('latex2exp')

temperature <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/temperature.csv')

#Look at city measurements (need to be careful about what obs are used to compute averages)
temperature %>%
  mutate(year = year(date)) %>%
  #filter(year >= 1950, year < 2020) %>%
  replace_na(list(temperature = 0)) %>%
  #select(city_name) %>%
  group_by(city_name, year) %>%
  summarize(avg = mean(temperature), count = n()) %>%
  #distinct()
  ggplot(aes(year,avg, group = city_name, color = city_name)) +
  geom_line()

#Looks like it's best to either:
# 1. use 1950-2018, or
# 2. use 1930-2018 and filter out Brisbane (it was added 1949), or

temp_avgs <- temperature %>% 
  mutate(year = year(date)) %>%
  drop_na() %>%
  group_by(year) %>%
  summarize(avg_temp = mean(temperature, na.rm = TRUE), count = n()) 

# 2019 seems to be incomplete, so we should not use. Visualize:
temp_avgs %>%
  ggplot(aes(year, count)) + 
  geom_point() 

# Filter out years with fewer data points
temp_avgs %<>% filter(year >= 1950, year < 2019)

overall_avg_temp <- temp_avgs %>% 
  summarize(overall_avg_temp = mean(avg_temp)) %>% 
  unlist()

temp_avgs %>% 
  mutate(temp_minus_mean = avg_temp - overall_avg_temp) %>%
  ggplot(aes(year, temp_minus_mean, fill = temp_minus_mean<0)) + 
  geom_col() +
  labs(x = element_blank(), 
       y = element_blank(),
       title = "Australia is getting warmer",
       caption = "Source: Australian Government Bureau of Meteorology") +
  theme_minimal() +
  scale_y_continuous(breaks = c(-1, -0.5, 0.5, 1),
                     labels = c(parse(text = TeX('$-1.0^o$')), 
                                parse(text = TeX('$-0.5^o$')), 
                                parse(text = TeX('$+0.5^o$')), 
                                parse(text = TeX('$+1.0^o$')))) +
  theme(#axis.text.y = element_blank(),
    plot.background = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(linetype = "dashed", color = "grey"),
    panel.grid.minor.x = element_blank(),
    plot.title = element_text(face = "bold"),
    legend.position = "none",
    plot.caption = element_text(color = "darkgrey")) +
  annotate("text", 
           x = 1962, 
           y = 0.7, 
           color = "darkgrey",
           label = "Annual temperature (degrees Celcius)\nabove or below the 1950-2018 average") 

