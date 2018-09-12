library(tidyverse)

this_version <- 'seed_addition_2018_running_update.csv'

data_path <- file.path('/Users/tylerhoecker/Box Sync/PhD/Research_GYE/Data/seedling_data_2018/')

seedlings <- read_csv(paste0(data_path,this_version)) %>% 
  select(-starts_with('height_header'),-starts_with('basal_header'), #-starts_with('basal'),
         -ends_with('_note'), -`_id`,-`_uuid`,-`_submission_time`,-`_index`) %>% 
  gather(cell, value, -date,-Fire,-Aspect,-Species,-Direction,-frameID,-Notes) %>% 
  # mutate(value = as.numeric(value)) %>%
  #        value = if_else(value < 0, NA_integer_, value),
  #        Aspect = as.factor(Aspect)) %>%
  separate(cell, into = c('variable','y_cell','x_cell'), sep = '_') %>%
  unite(x_cell, y_cell, col = 'cell') %>% 
  # mutate(value = if_else(variable == 'basal', value/100, as.numeric(value)),
  #        y_cell = as.integer(y_cell)) %>%
  #spread(variable, value) %>% 
  filter(!is.na(height)) 
  #filter(Species != 'control_pico', Species != 'control', !is.na(value)) 
  

colVals <- c('flat' = '#009E73','north' = '#0072B2','south' = '#E69F00')
legLabs <- c('Flat','North','South')

ggplot(filter(seedlings, Species == 'pmse')) +
  geom_density(aes(x = value, fill = Aspect), alpha = 0.5) +
  # geom_histogram(aes(x = value, fill = Aspect), alpha = 0.5, 
  #                position = 'identity', bins = 30) +
  scale_fill_manual(values = colVals, name = 'Aspect', labels = legLabs) +
  facet_wrap(~variable + Fire, scales = 'free', ncol = 4) +
  theme_bw(base_size = 14) +
  labs(x = 'measurement (mm)', title = 'PSME')

ggplot(filter(seedlings, Species == 'pico')) +
  geom_density(aes(x = value, fill = Aspect), alpha = 0.5) +
  # geom_histogram(aes(x = value, fill = Aspect), alpha = 0.5, 
  #                position = 'identity', bins = 30) +
  scale_fill_manual(values = colVals, name = 'Aspect', labels = legLabs) +
  facet_wrap(~variable + Fire, scales = 'free', ncol = 4) +
  theme_bw(base_size = 14) +
  labs(x = 'measurement (mm)', title = 'PICO')


counts <- seedlings %>% 
  group_by(date, Fire, Aspect, Species, Direction, frameID) %>% 
  unite(x_cell, y_cell, col = 'cell') 

ggplot(counts) +
  geom_bar(aes(x = date, y = count, fill = Aspect), 
           stat = 'identity', position = 'dodge', width = 5) +
  scale_fill_manual(values = colVals, name = 'Aspect', labels = legLabs) +
  facet_wrap(~Fire+Species, ncol = 4) +
  theme_bw()

ggplot(seedlings) +
  geom_point(aes(x = basal, y = height, color = Aspect), alpha = 0.5) +
  # geom_histogram(aes(x = value, fill = Aspect), alpha = 0.5, 
  #                position = 'identity', bins = 30) +
  stat_smooth(aes(x = basal, y = height, color = Aspect), se = F) +
  scale_color_manual(values = colVals, name = 'Aspect', labels = legLabs) +
  facet_wrap(~Fire, scales = 'free') +
  theme_bw(base_size = 14) +
  labs(x = 'measurement (mm)')

