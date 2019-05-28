library(tidyverse)
library(ggridges)
library(extrafont)

#read data:
wine_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-28/winemag-data-130k-v2.csv")

#clean up a bit:
wine_ratings <- wine_ratings %>% 
  select(-X1) %>% #X1 is not informative
  distinct()      #de-dupe based on a tip posted on twitter

#Some countries only have a handful of ratings.
# Select only countries that have atleast 1000 ratings:
large_sample_countries <- wine_ratings %>% 
  group_by(country) %>% 
  summarize(med_cntry_pnts = median(points), cnt = n()) %>% 
  arrange(desc(med_cntry_pnts)) %>%  
  filter(cnt >= 1000) #%>% View()

data4plotting <- inner_join(wine_ratings, large_sample_countries)

#Played with ridges, but didn't seem very interesting:
ggplot(data4plotting, aes(x = points, y = country, group = country)) +
  geom_density_ridges() +
  theme_ridges() 

ggplot(data4plotting, aes(x = points, y = taster_twitter_handle, group = taster_twitter_handle)) +
  geom_density_ridges() +
  theme_ridges() 

# Country vs Taster. Do some tasters give higher ratings in general than others?  
data4plotting %>% 
  filter(!is.na(taster_name)) %>%
  group_by(taster_name, country) %>%
  summarize(med_cntry_taster_pnts = median(points), cnt_cntry_taster = n())  %>%
  ungroup() %>%
  mutate(taster_name = fct_reorder(taster_name, med_cntry_taster_pnts)) %>%
  ggplot(aes(x = country, y = taster_name, size = cnt_cntry_taster, color = med_cntry_taster_pnts)) +
  geom_point() +
  scale_colour_gradient(low = "white", high = "dark red") +
  theme_light() +
  labs(y = "Taster", 
       x = "Country of Wine", 
       title = "Wine Taster Rating Profile (by Country)",
       color = "Median Score",
       size = "Number of Reviews",
       caption = "Countries limited to those with atleast 1000 rated wines") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        title = element_text(family = "Lato Semibold"))

  

#The following parallel plot looks cool, but I don't think it's as informative 
# as the bubble plot.
library(ggparallel)  

df <- data4plotting %>% 
    filter(!is.na(taster_name)) %>%
    group_by(taster_name, country) %>%
    summarize(med_cntry_taster_pnts = median(points)) 
  
ggparallel(list("taster_name", "country"), as.data.frame(df))

  