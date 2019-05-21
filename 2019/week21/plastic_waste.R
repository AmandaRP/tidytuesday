library(tidyverse)
library(janitor)
library(inspectdf)
library(cowplot)
library(ggrepel)


#Read data and clean with janitor:
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


#Look for correlations using the inspectdf package:
inspect_cor(waste, show_plot = TRUE) 
inspect_na(waste, show_plot = TRUE) #Looks like we only have 2010 waste data

#Plots:
data4plot <- waste %>% filter(year == 2010) %>% filter(entity != "World")
plt1 <- ggplot(data4plot, aes(x = log(gdp_per_cap), 
             y = log(plastic_waste_per_cap), 
             size = mismng_pl_waste_tons)) +
  geom_smooth(method = "lm") +
  geom_point(color = "#333333", alpha = 0.7) +
  geom_text_repel(aes(label = entity),
    color         = "red",
    size          = 4,
    data          = subset(data4plot, log(plastic_waste_per_cap) > 0 | log(plastic_waste_per_cap) < -4.5 | (log(gdp_per_cap) < 7 & log(plastic_waste_per_cap) < -4) | code %in% c("USA","CHN")),
    nudge_y       = .8,
    segment.size  = 0.2,
    segment.color = "grey50",
    direction     = "x"
  ) +
  labs(title = "Richer nations tend to produce more plastic waste",
       x = "GDP Per Capita (log scale)",
       y = "Per Capita Plastic Waste in kg/day (log scale)",
       size = "Mismanagement of \nPlastic Waste (tons)")  +
  theme(legend.position = "none") 


#Richer nations manage their plastic waste better
plt2 <- ggplot(data4plot, aes(x = log(gdp_per_cap), 
             y = mismng_pl_waste_per_cap, 
             size = mismng_pl_waste_tons)) +
  geom_smooth(show.legend = FALSE) + 
  geom_point(color = "#333333", alpha = 0.7) + 
  geom_text_repel(aes(label = entity), 
    color         = "red",
    size          = 4,
    data          = subset(data4plot, mismng_pl_waste_per_cap > .14 | code %in% c("USA","CHN")),
    nudge_y       = .025,
    nudge_x       = .1,
    segment.size  = 0.2,
    segment.color = "grey50",
    direction     = "x"
  ) + 
  ylim(0, NA) +
  labs(title = "Many countries having mid-range GDP are\nbad at management of their plastic waste ",
       x = "GDP Per Capita (log scale)",
       y = "Per Capita Mismanaged Plastic Waste in kg/day",
       size = "Mismanaged\nPlastic Waste (tons)") 

#Get the legend from plot 2 so that I can put it in its own plot grid panel:
l <- get_legend(plt2) 

#Final plot
plot_grid(plt1, 
          plt2 + theme(legend.position = "none"), 
          l,
          nrow = 1)

