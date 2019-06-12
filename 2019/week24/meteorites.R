library(tidyverse)
library(ggdark)
library(gganimate)

#read data:
meteorites <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-11/meteorites.csv")

#Get rid of outliers:
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
  labs(caption = "Data from the Meteoritical Society (and shared by NASA)") +
  labs(title = "Meteorite Crashes on Earth") 
    
      
# Experimentation with animation:  
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
       x = "Year",
       y = "Count") +
  geom_curve(aes(x = 1979 + 5, y = 3000, xend = 1979.5, yend = 3046), curvature = 0.3, arrow = arrow(length=unit(2,"mm")), color = "white") +
  annotate("text", x = 1979 + 7, y = 2950, label = "1979")
  


