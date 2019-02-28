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
  summarise(Germination = sum(germinated, na.rm = T) / n(),
            Survival = sum(final, na.rm = T) / sum(germinated, na.rm = T),
            Establishment = sum(final, na.rm = T) / n()) %>% 
  gather(period, value, Germination, Survival, Establishment) %>% 
  mutate(value = if_else(is.na(value), 0, value)) %>% 
  # Transform data, then show both ways (all fires and aspects together for clarity)
  # Using arsine-square-root transform per Ives 2018 sensu Larson and Marx 1981
  mutate(asinsqrt = asin(sign(value) * sqrt(abs(value))),
         #logit = log( (value/(1-value)) )
         logit = car::logit(value, adjust=0)) %>%
  rename(original = value) %>% 
  gather(version, value, original, asinsqrt, logit) %>% 
  mutate(period = fct_relevel(period, 'Germination','Survival','Establishment')) 
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
           




