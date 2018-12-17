library(tidyverse)
library(Hmisc)

this_version <- 'data/seedling_data_complete.csv'

seedlings <- read_csv(this_version) %>% 
  select(-starts_with('height_header'),-starts_with('basal_header'), 
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
  select(-Aspect, date, fire, species, rep, frameID, variable, cell, value) %>% 
  modify_at(c('aspect','fire','species','variable'), as_factor)

  

# counts <- seedlings %>%
#   filter(variable == 'height', species != 'control', value > 0) %>%
#   group_by(date, fire, species, aspect) %>%
#   summarize(count = n(),
#             prop = n()/(5*50))
# 
# allMeasures <- seedlings %>%
#   group_by(date, fire, species, aspect) %>%
#   filter(variable == 'height', value < 0 | is.na(value)) %>%
#   summarise(count = 0)
# 
# counts_full <-
#   full_join(counts, allMeasures) %>%
#   group_by(date, fire, species, aspect) %>%
#   filter(species != 'control') %>%
#   summarise(count = sum(count),
#             prop = count/(5*50))
