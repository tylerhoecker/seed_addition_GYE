# Decide on a time period of interest:
start_time <- as.POSIXct(c("2018-06-01 00:00:00"))
end_time <- as.POSIXct(c("2018-09-01 00:00:00"))


# Load and prep soil moisture and temperature predictor variables
# Calculates Q50 and Q75 and minimum for moisture, Q50 and Q75 and maximum for temperature
source('code/soil_data_prep.R') # creates `soil_df` (complete) and `soil_preds` (summarized)

# Load and prep meteorological data (air temperature and VPD)
source('code/atmos_data_prep.R') # creates `atmos_df` (complete) and `atmos_preds` (summarized)

# Run seedling import/summary scripts. Creates `germination`, `final`, `proprotions`, `seed_per`.
source('code/read_summarize_seedling.R')


# Start with a dataframe of seedling data and soil moisture and temperature
seeds_soil_df <- full_join(proportions, soil_preds, by = c('fire','aspect')) %>% 
  filter(version == 'original') %>% 
  rename(proportion = value) %>% 
  # Dealing only with 'establishment'
  filter(period == 'establishment') %>% 
  unite(site, fire, aspect, remove = F) 


# Exploratory Analysis ----------------------------------------------------
colVals <- c('Flat' = '#009E73','North' = '#0072B2','South' = '#E69F00')

seeds_soil_df %>%
  group_by(fire, aspect, species, frameID, period) %>% 
  gather(abiotic, abiotic_value, mois_max:temp_q75) %>% 
  # Plot
  ggplot() +
  geom_point(aes(x = abiotic_value, y = proportion, fill = aspect), shape = 21, size = 2, alpha = 0.6) +
  facet_grid(species~abiotic, scales="free") +
  scale_fill_manual(values = colVals, name = 'aspect') +
  theme_bw(base_size = 10) 

# THE MODELS ---------------------------

library(lme4)
m <- glmer(proportion*50 ~ mois_q75 + (1 | fire),
           family = 'poisson', control = glmerControl(optimizer = "bobyqa"),
           data = as.data.frame(seeds_soil_df))
summary(m)


m <- glm(proportion*50 ~ mois_q75*species*fire,
         family = 'poisson',
         data = as.data.frame(seeds_soil_df))

summary(m)





### EXTRA
covariates <- germ_abiotic_df %>% 
  ungroup() %>% 
  na.omit() %>% 
  as.data.frame()

GGally::ggpairs(covariates)

ggplot(temp, aes(x = aspect, y = value)) +
  geom_jitter(alpha = .1) +
  geom_violin(aes(fill = aspect), alpha = .75) +
  scale_fill_manual(values = colVals, name = 'aspect') +
  facet_wrap(~variable, scales = 'free')

# # Surival and germination figuring-------------------------------------------
# # Germinated
# germination <- seedlings %>% 
#   filter(variable == 'height', species != 'control') %>% 
#   select(site, aspect, species, frameID, cell, value) %>% 
#   mutate(germinated = if_else(is.na(value), 0, 1)) %>% 
#   unite(frameID, cell, col = 'uniqID', remove = FALSE) %>%
#   group_by(site, aspect, species, frameID, uniqID) %>% 
#   summarise(germinated = max(germinated)) %>% 
#   group_by(site, aspect, species, frameID) #%>% 
#   #mutate(problem = if_else(sum(germinated) == 0, 'Yes','No')) %>% 
#   #filter(problem == 'No') 
#   
# 
# # Survived
# survival <- seedlings %>%
#   filter(variable == 'height', species != 'control') %>% 
#   mutate(survived = if_else(date > as.POSIXct("2018-10-01") & value > 0, 1, 0)) %>% 
#   select(site, aspect, species, frameID, cell, survived) %>% 
#   unite(frameID, cell, col = 'uniqID') %>%
#   group_by(site, aspect, species, uniqID) %>% 
#   summarise(survived = max(survived, na.rm = T))  
#   
# 
# establishment_df <- full_join(germination, survival) %>% 
#   ungroup()

