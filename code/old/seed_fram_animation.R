source('code/read_seedling_data.R')

bgs_df <- seedlings %>% 
  filter(fire == 'Berry-Glade', aspect == 'South', species == 'pico', species == 'pico',  variable == 'height',
         value > 0) 

death_dates <- seedlings %>% 
  filter(fire == 'Berry-Glade', aspect == 'South', species == 'pico', species == 'pico',  variable == 'height',
         value < 0)

library(gapminder)
library(ggplot2)
library(gganimate)


ggplot(bgs_df, aes(x = x_cell, y = y_cell)) +
  geom_point(aes(fill = value), shape = 21) +
  geom_point(data = death_dates, shape = 23, size = 2, fill = 'darkred') +
  scale_fill_viridis_c() +
  facet_grid(aspect~frameID) +
  theme_bw(base_size = 14) +
  transition_states(date)
  