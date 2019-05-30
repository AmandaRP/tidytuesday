library(tidyverse)
library(showtext)

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

#Note: I had to use the showtext package (extrafonts didn't offer very many fonts 
# and newly installed fonts didn't render... may be user error). 
# Required copying and pasting code into terminal window.
# Read more about showtext package here: 
# https://cran.rstudio.com/web/packages/showtext/vignettes/introduction.html
font_add_google("Satisfy", "satisfy")
showtext_auto()


# Country vs Taster. Do some tasters give higher ratings in general than others?  
p <- data4plotting %>% 
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
       x = "Wine Country", 
       title = "Wine Taster Rating Profile (by Country)",
       color = "Median Score",
       size = "Number of Reviews",
       caption = "Countries limited to those with atleast 1000 rated wines") +
  theme(axis.text.x = element_text(angle = 45, hjust=1, size = 20),
        axis.text.y = element_text(size = 20),
        title = element_text(family = "satisfy", size = 36),
        plot.caption = element_text(hjust = 0.5),
        legend.text = element_text(size = 18))

p

ggsave("wine_tasting.png", p, height = 4, width = 7)


### Bonus code:

#The following parallel plot looks cool, but I don't think it's as informative 
# as the bubble plot.
library(ggparallel)  

df <- data4plotting %>% 
    filter(!is.na(taster_name)) %>%
    group_by(taster_name, country) %>%
    summarize(med_cntry_taster_pnts = median(points)) 
  
ggparallel(list("taster_name", "country"), as.data.frame(df))

  