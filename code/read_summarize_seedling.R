# Import seedling data
source('code/read_seedling_data.R')

# Surival and germination figuring-------------------------------------------
# Germinated
germination <- seedlings %>% 
  filter(variable == 'height', species != 'control') %>% 
  mutate(germinated = if_else(is.na(value), 0, 1)) %>% 
  group_by(fire, aspect, species, frameID, cell) %>% 
  summarise(germinated = max(germinated, na.rm = T)) 

# Survived
final <- seedlings %>%
  filter(variable == 'height', species != 'control') %>% 
  mutate(survived = if_else(date > as.POSIXct("2018-10-01") & value > 0, 1, 0)) %>% 
  group_by(fire, aspect, species, frameID, cell) %>% 
  summarise(final = max(survived, na.rm = T)) 


# Proportion of germination, survival and their product, establishment for each frame. 
proportions <- full_join(germination, final) %>% 
  group_by(fire, aspect, species, frameID) %>% 
  summarise(germination = sum(germinated, na.rm = T) / n(),
            survival = sum(final, na.rm = T) / sum(germinated, na.rm = T),
            establishment = sum(final, na.rm = T) / n()) %>% 
  gather(period, value, germination, survival, establishment) %>% 
  mutate(value = if_else(is.na(value), 0, value)) %>% 
  mutate(period = factor(period, levels = c('germination','survival','establishment'))) %>% 
  # Transform data, then show both ways (all fires and aspects together for clarity)
  # Using arsine-square-root transform per Ives 2018 sensu Larson and Marx 1981
  mutate(asinsqrt = asin(sign(value) * sqrt(abs(value)))) %>%
  rename(original = value) %>% 
  gather(version, value, asinsqrt, original)

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
           




