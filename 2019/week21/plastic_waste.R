library(tidyverse)
library(janitor)
library(inspectdf)

#Ideas: 
# 1. explore corrr package. 
# 2. gganimate (for year)
# 3. inspect df

#Read data:
coast_vs_waste <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/coastal-population-vs-mismanaged-plastic.csv") %>% clean_names()
mismanaged_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-mismanaged-plastic-waste-vs-gdp-per-capita.csv")  %>% clean_names()
waste_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-plastic-waste-vs-gdp-per-capita.csv")  %>% clean_names()

#Clean names a bit more
coast_vs_waste <- coast_vs_waste %>%
  rename(mismng_pl_waste_tons = mismanaged_plastic_waste_tonnes,
        coast_pop = coastal_population,
        population = total_population_gapminder)
View(coast_vs_waste)

waste_vs_gdp <- waste_vs_gdp %>%
  rename(plastic_waste_per_cap = per_capita_plastic_waste_kilograms_per_person_per_day,
         gdp = gdp_per_capita_ppp_constant_2011_international_constant_2011_international,
         population = total_population_gapminder)
View(waste_vs_gdp)

mismanaged_vs_gdp <- mismanaged_vs_gdp %>%
  rename(mismng_pl_waste_per_cap = per_capita_mismanaged_plastic_waste_kilograms_per_person_per_day,
         gdp = gdp_per_capita_ppp_constant_2011_international_rate,
         population = total_population_gapminder)
View(mismanaged_vs_gdp)

#Look for correlations:
inspect_cor(coast_vs_waste, show_plot = TRUE) #pop and mismng_pl_waste_tons (.75) *****
inspect_cor(waste_vs_gdp, show_plot = TRUE) #highest corr is ~.2
inspect_cor(mismanaged_vs_gdp, show_plot = TRUE) # gdp & mismng_pl_waste (.4)

#to plot: pop vs mismg wate (with GDP as circle size)

