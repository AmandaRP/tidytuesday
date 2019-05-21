library(tidyverse)
library(janitor)
library(inspectdf)
library(cowplot)
library(ggrepel)

#Ideas: 
# 1. explore corrr package. 
# 2. gganimate (for year)
# 3. inspect df

#Read data:
coast_vs_waste <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/coastal-population-vs-mismanaged-plastic.csv") %>% clean_names()
waste_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-plastic-waste-vs-gdp-per-capita.csv")  %>% clean_names()
mismanaged_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-mismanaged-plastic-waste-vs-gdp-per-capita.csv")  %>% clean_names()

#Put data together in one df:
waste <- left_join(waste_vs_gdp, coast_vs_waste, by = c("entity", "code", "year"))
(waste <- left_join(waste, mismanaged_vs_gdp, by = c("entity", "code", "year")))

View(waste)

#Clean names a bit more
(waste <- waste %>%
  select(-total_population_gapminder.y, -total_population_gapminder.x, -gdp_per_capita_ppp_constant_2011_international_rate) %>%
  rename(mismng_pl_waste_tons = mismanaged_plastic_waste_tonnes,
        mismng_pl_waste_per_cap = per_capita_mismanaged_plastic_waste_kilograms_per_person_per_day,
        plastic_waste_per_cap = per_capita_plastic_waste_kilograms_per_person_per_day,
        coast_pop = coastal_population,
        gdp_per_cap = gdp_per_capita_ppp_constant_2011_international_constant_2011_international,
        population = total_population_gapminder))
View(waste)


#Look for correlations:
inspect_cor(waste, show_plot = TRUE) #pop and mismng_pl_waste_tons (.75) *****
inspect_na(waste, show_plot = TRUE)

#Note, we only seem to have 2010 statistics. No gganimate today :(
inspect_na(mismanaged_vs_gdp, show_plot = TRUE)

#Richer nations produce more plastic waste
data4plot <- waste %>% filter(year == 2010) %>% filter(entity != "World")
plt1 <- ggplot(data4plot, aes(x = log(gdp_per_cap), 
             y = log(plastic_waste_per_cap), 
             size = mismng_pl_waste_tons)) +
  geom_smooth(method = "lm") +
  geom_point(color = "#333333") +
  geom_text_repel(aes(label = entity),
    color         = "red",
    size          = 3,
    data          = subset(data4plot, log(plastic_waste_per_cap) > 0 | log(plastic_waste_per_cap) < -4.5 | log(gdp_per_cap) < 6.5 | code %in% c("USA","CHN")),
    nudge_y       = .8,
    segment.size  = 0.2,
    segment.color = "grey50",
    direction     = "x"
  ) +
  labs(title = "Richer nations tend to produce more plastic waste",
       x = "GDP Per Capita (log scale)",
       y = "Plastic Waste Per Capita (log scale)",
       size = "Mismanagement of \nPlastic Waste (tons)")  +
  theme(legend.position = "none",
        plot.title = element_text(family = "Comic Sans MS")) 



#Richer nations manage their plastic waste better
plt2 <- ggplot(data4plot, aes(x = log(gdp_per_cap), 
             y = mismng_pl_waste_per_cap, 
             size = mismng_pl_waste_tons)) +
  geom_smooth() + 
  geom_point(color = "#333333") + 
  geom_text_repel(aes(label = entity), 
    color         = "red",
    size          = 3,
    data          = subset(data4plot, mismng_pl_waste_per_cap > .14 | code %in% c("USA","CHN")),
    nudge_y       = .025,
    nudge_x       = .1,
    segment.size  = 0.2,
    segment.color = "grey50",
    direction     = "x"
  ) + 
  ylim(0, NA) +
  labs(title = "...But countries having mid-range GDP are\nthe worst at management of their waste ",
       x = "GDP Per Capita (log scale)",
       y = "Mismanaged Plastic Waste Per Capita",
       size = "Mismanagement of \nPlastic Waste (tons)") 

l <- get_legend(plt2) 

plot_grid(plt1, 
          plt2 + theme(legend.position = "none"), 
          l,
          nrow = 1)


# Diverging lolly pop

waste_no_world <- waste %>% filter(entity != "World")
x <- waste_no_world$mismng_pl_waste_tons
waste_no_world$mmg_z <- round((x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE), 2) 
waste_no_world$entity <- factor(waste_no_world$entity, levels = unique(waste_no_world$entity)) 
waste %>%
  filter(year == 2010 & !is.na(mmg_z)) %>%
  #head() %>%
  ggplot(aes(x=entity, y=mmg_z, label=mmg_z)) + 
  geom_point(stat='identity', fill="black", size=6)  +
  geom_segment(aes(y = 0, 
                   x = entity, 
                   yend = mmg_z, 
                   xend = entity)) +
  #geom_text(color="white", size=2) +
  #labs(title="Diverging Lollipop Chart", 
  #     subtitle="Normalized mileage from 'mtcars': Lollipop") + 
  #ylim(-2.5, 2.5) +
  coord_flip() +
  theme_bw()
