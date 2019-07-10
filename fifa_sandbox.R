ggplot(inner_join(wwc_outcomes_wcodes, top_countries), 
       aes(fill = year, values = score)) + 
  geom_waffle(color = "white", size = .25, n_rows = 4, flip = FALSE) +
  facet_wrap(~country.name.en, nrow = 10, strip.position = "left") +
  #scale_x_discrete() + 
  scale_y_continuous(labels = function(x) x * 10, # make this multiplyer the same as n_rows
                     expand = c(0,0)) +
  ggthemes::scale_fill_tableau(name=NULL) +
  #coord_flip() +
  labs(
    title = "Womens World Cup Soccer: Total Goals Scored",
    subtitle = "Data provided by TODO"
  ) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(panel.grid = element_blank(), axis.ticks.y = element_line()) +
  guides(fill = guide_legend(reverse = TRUE))

#TODO:
# rotate country names
#Get rid of labels on tick x and y axis
# add legend title
# add flags https://cran.r-project.org/web/packages/ggimage/vignettes/ggimage.html
# add picture of soccor ball (cowplot)
# refactor ordering highest scoring team first
# add picture of trophy for winning team each year
# what's up with NA?
# do a factor lump? fct_lump

