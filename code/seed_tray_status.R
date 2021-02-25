library(tidyverse)
library(cowplot)

this_version <- 'data/seedling_data_complete.csv'

seedlings <- read_csv(this_version) %>% 
  dplyr::select(-starts_with('height_header'),-starts_with('basal_header'), 
                -ends_with('_note'), -`_id`,-`_uuid`,-`_submission_time`,-`_index`) %>% 
  gather(cell, value, -date,-Fire,-Aspect,-Species,-Direction,-frameID,-Notes) %>% 
  separate(cell, into = c('variable','y_cell','x_cell'), sep = '_') %>%
  unite(x_cell, y_cell, col = 'cell', remove = F) %>% 
  modify_at('date', as.POSIXct) %>% 
  modify_at('y_cell', as.integer) %>% 
  mutate(Fire = recode(Fire, 'berry_glade'='Berry-Glade','berry_huck'='Berry-Huck', 'buffalo'='Buffalo', 'maple'='Maple'),
         value = if_else(variable == 'basal', value/100, as.numeric(value))) %>%
  mutate(aspect = Hmisc::capitalize(Aspect)) %>% 
  rename(species = Species,
         fire = Fire,
         rep = Direction) %>% 
  dplyr::select(-Aspect, date, fire, species, rep, frameID, variable, cell, value) %>% 
  modify_at(c('aspect','fire','species','variable'), as_factor) %>% 
  mutate(aspect = fct_relevel(aspect, 'North','Flat','South'),
         species = if_else(species == 'pico', 'PICO', as.character(species)),
         species = if_else(species == 'psme','PSME', as.character(species))) 


status_df <- seedlings %>%
  filter(variable == 'height') %>% 
  mutate(alive_fall_18 = if_else(date > as.POSIXct("2018-10-01") & date < as.POSIXct("2019-01-01") & value > 0, 1, 0),
         alive_spring_19 = if_else(date > as.POSIXct("2019-01-01") & value > 0, 1, 0)) %>% 
  group_by(fire, aspect, species, rep, frameID, y_cell, x_cell) %>% 
  summarise_at(vars(alive_fall_18, alive_spring_19), max, na.rm = T) %>% 
  mutate_at(vars(alive_fall_18, alive_spring_19), as.integer) %>% 
  mutate(name = paste(rep, frameID, sep = '-'))

labels <- status_df %>%  
  filter(alive_fall_18 > 0)

plot_data <- filter(status_df, fire == 'Buffalo', species == 'PICO')
##### REPEAT ---------------- 
north_plot <- 
  ggplot(filter(plot_data, aspect == 'North')) +
  geom_tile(aes(y = fct_rev(x_cell), x = y_cell, fill = alive_fall_18), color = 'grey50', size = 0.75) +
  facet_grid(~name) +
  scale_fill_gradient(low = "transparent", high = "black", guide = F) +
  scale_x_continuous(breaks = seq(1,10), labels = seq(1,10)) +
  #geom_text(data = labels, aes(x = x_cell, y = as.integer(y_cell), label = paste(x_cell, y_cell, ',')))
  theme_bw() +
  theme(axis.title = element_blank(),
        panel.grid = element_blank())

flat_plot <- 
  ggplot(filter(plot_data, aspect == 'Flat')) +
  geom_tile(aes(y = fct_rev(x_cell), x = y_cell, fill = alive_spring_19), color = 'grey50', size = 0.75) +
  facet_grid(~name) +
  scale_fill_gradient(low = "transparent", high = "black", guide = F) +
  scale_x_continuous(breaks = seq(1,10), labels = seq(1,10)) +
  #geom_text(data = labels, aes(x = x_cell, y = as.integer(y_cell), label = paste(x_cell, y_cell, ',')))
  theme_bw() +
  theme(axis.title = element_blank(),
        panel.grid = element_blank())

south_plot <- 
  ggplot(filter(plot_data, aspect == 'South')) +
  geom_tile(aes(y = fct_rev(x_cell), x = y_cell, fill = alive_spring_19), color = 'grey50', size = 0.75) +
  facet_grid(~name) +
  scale_fill_gradient(low = "transparent", high = "black", guide = F) +
  scale_x_continuous(breaks = seq(1,10), labels = seq(1,10)) +
  #geom_text(data = labels, aes(x = x_cell, y = as.integer(y_cell), label = paste(x_cell, y_cell, ',')))
  theme_bw() +
  theme(axis.title = element_blank(),
        panel.grid = element_blank())

frame_grid <- plot_grid(north_plot, flat_plot, south_plot, ncol = 1, labels = c('N','F','S'))
frame_grid
#####

save_plot('frame_status_18_buffalo_psme.png', frame_grid, base_height = 6,
          base_asp = 2)
  
filter(status_df, alive_fall_18 > 0) %>% 
  write_csv('alive_fall18.csv')
  