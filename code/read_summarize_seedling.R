library(tidyverse)

# Import seedling data
this_version <- 'data/seedling_data_complete.csv'

seedlings <- read_csv(this_version) %>% 
  dplyr::select(-starts_with('height_header'),-starts_with('basal_header'), 
                -ends_with('_note'), -`_id`,-`_uuid`,-`_submission_time`,-`_index`) %>% 
  gather(cell, value, -date,-Fire,-Aspect,-Species,-Direction,-frameID,-Notes) %>% 
  separate(cell, into = c('variable','y_cell','x_cell'), sep = '_') %>%
  unite(x_cell, y_cell, col = 'cell', remove = F) %>% 
  modify_at('date', as.POSIXct) %>% 
  modify_at('y_cell', as.integer) %>% 
  mutate(Fire = dplyr::recode(Fire, 'berry_glade'='Berry-Glade','berry_huck'='Berry-Huck', 'buffalo'='Buffalo', 'maple'='Maple'),
         value = if_else(variable == 'basal', value/100, as.numeric(value))) %>%
  mutate(aspect = Hmisc::capitalize(Aspect)) %>% 
  rename(species = Species,
         fire = Fire,
         rep = Direction) %>% 
  dplyr::select(-Aspect, date, fire, species, rep, frameID, variable, cell, value) %>% 
  modify_at(c('aspect','fire','species','variable'), as_factor) %>% 
  mutate(aspect = fct_relevel(aspect, 'South','Flat','North'),
         species = if_else(species == 'pico', 'PICO', as.character(species)),
         species = if_else(species == 'psme','PSME', as.character(species))) 

# Surival and germination figuring-------------------------------------------
# Germinated
germination <- seedlings %>% 
  filter(variable == 'height', species != 'control') %>% 
  mutate(germinated = if_else(is.na(value), 0, 1)) %>% 
  group_by(fire, aspect, species, frameID, cell) %>% 
  summarise(germinated = max(germinated, na.rm = T)) 

# Survived
survival <- seedlings %>%
  filter(variable == 'height', species != 'control') %>% 
  mutate(value = if_else(value == -666, 1, value)) %>% 
  mutate(survived_yr1 = if_else(date > as.POSIXct("2018-10-01") & date < as.POSIXct("2018-11-01") & value > 0, 1, 0),
         survived_yr2 = if_else(date > as.POSIXct("2019-10-01") & date < as.POSIXct("2019-11-01") & value > 0, 1, 0)) %>% 
  group_by(fire, aspect, species, frameID, cell) %>% 
  summarise_at(vars(survived_yr1, survived_yr2), max, na.rm = T) %>% 
  mutate_at(vars(survived_yr1, survived_yr2), ~ ifelse(. == -Inf, NA, .))


# Proportion of germination, survival and their product, establishment for each frame. 
# proportions <- full_join(germination, final) %>% 
#   group_by(fire, aspect, species) %>% # Adjust by frame or site: +/- frameID
#   summarise(Germination = sum(germinated, na.rm = T) / n(),
#             Survival = sum(final, na.rm = T) / sum(germinated, na.rm = T),
#             Establishment = Germination * Survival) %>% 
#   gather(period, value, Germination, Survival, Establishment) %>% 
#   mutate(value = if_else(is.na(value), 0, value)) %>% 
#   # Transform data, then show both ways (all fires and aspects together for clarity)
#   # Using arsine-square-root transform per Ives 2018 sensu Larson and Marx 1981
#   mutate(asinsqrt = asin(sign(value) * sqrt(abs(value))),
#          #logit = log( (value/(1-value)) )
#          logit = car::logit(value, adjust=0)) %>%
#   rename(original = value) %>% 
#   gather(version, value, original, asinsqrt, logit) %>% 
#   mutate(period = fct_relevel(period, 'Germination','Survival','Establishment'))

  # proportions %>% 
#   filter(fire %in% c('Berry-Glade', 'Berry-Huck')) %>%
#   group_by(species, period) %>% 
#   summarise(min = min(value),
#             max = max(value),
#             mean = mean(value),
#             median = median(value),
#             sd = sd(value),
#             n_response = as.integer(sum(value)*50),
#             n_possible = n()*50) %>% 
#   write_csv('establishment_rates_combined.csv')
           




