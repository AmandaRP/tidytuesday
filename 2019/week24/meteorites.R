library(tidyverse)
library(ggdark)
library(gganimate)

#read data:
meteorites <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-11/meteorites.csv")

#Get rid of outlier:
meteorites %<>% filter(long<180 & year <= 2013)

#map:
mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders

ggplot(meteorites) +   
  mapWorld + 
  geom_point(aes(x=long, y=lat) ,color="green", size=1, alpha = .1) +
  dark_mode() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank()) + 
  labs(caption = "Data from he Meteoritical Society\nVisualization by @AmandaPlunkett") +
  labs(title = "Meteorites") 
    
      
# Animation:  
#transition_states(year,
#                    transition_length = 2,
#                    state_length = 1) +
#    ggtitle("Meteorites", subtitle = "Year:  {frame_time}") 
  

#Time series plot:

meteorites %>% 
  filter(year > 1950 & year < 2013) %>%
  group_by(year) %>% 
  count() %>% 
  arrange(desc(n)) %>%
  ggplot(aes(year, n)) +
  geom_line(color = "green") + 
  dark_mode() + 
  labs(title = "Meteorites by Year (1950 - 2012)",
       x = "Count",
       y = "Year")


