# This code is good as of 7/9/2019 and is based on understanding gain from looking at linear models on transformed responses
library(tidyverse)
library(DescTools)
library(MuMIn)
library(broom)

# Predictor variables ----------------------------------------------------------

# Import soil data, at this point filter for entire season 
start_time <- as.POSIXct(c("2018-06-01 00:00:00"))
end_time <- as.POSIXct(c("2019-10-01 00:00:00"))
source('code/read_soil_data.R')

# Filter to germination and survival periods, and calculate median values 
soil_predictors <- soil_df %>%
  ungroup() %>% 
  #spread(variable, value) %>% 
  mutate(period = if_else(time >= "2018-06-01 00:00:00" & time <= "2018-06-15 00:00:00", 'germ',
                          if_else(time >= "2018-07-08 00:00:00" & time <= "2018-10-01 00:00:00", 'year1', 
                                  if_else(time >= "2019-07-08 00:00:00" & time <= "2019-10-01 00:00:00", 'year2',
                                          'winter')))) %>% 
  # Calculate the median soil moisture 
  group_by(fire, aspect, variable, period) %>% 
  summarise(value = quantile(value, 0.50, na.rm = T)) %>% 
  filter(period != 'winter') %>% 
  unite(var_per, period, variable, sep = '_') %>% 
  spread(var_per, value) 

# Percent cover data
pct_cover <- read_csv('data/pct_cover.csv') %>% 
  group_by(fire, aspect) %>% 
  summarise_all(mean) %>% 
  mutate(veg = forb + gram) %>% 
  dplyr::select(fire, aspect, veg)

# Soil properties/texture
soil_props <- read_csv('data/soil_properties.csv') 

# Site info 
terrain <- read_csv('data/terrain_idx.csv')

# Import response variables (seedling germination, survival)--------------------
source('code/read_summarize_seedling.R')

proportions <- full_join(germination, survival) %>% 
  group_by(fire, aspect, species) %>% 
  summarise(Germination = sum(germinated, na.rm = T) / n(),
            Survival_yr1 = sum(survived_yr1, na.rm = T) / sum(germinated, na.rm = T),
            Survival_yr2 = sum(survived_yr2, na.rm = T) / sum(germinated, na.rm = T),
            #Survival_both = sum(survived_yr2, na.rm = T) / sum(survived_yr1, na.rm = T),
            Establishment_yr1 = sum(survived_yr1, na.rm = T) / n(),
            Establishment_yr2 = sum(survived_yr2, na.rm = T) / n()) 

# Combine response and predictor variables -------------------------------------
complete_df <- proportions %>%
  ungroup() %>% 
  # Add informaiton for weights for binomial regression
  mutate(germ_estab_Weights = as.integer(250),
         surv_Weights = as.integer(Germination * 250)) %>% 
  full_join(soil_predictors) %>% 
  full_join(pct_cover) %>% 
  full_join(soil_props) %>%
  full_join(terrain) %>% 
  modify_at(c('fire','species','texture'), as.factor) %>% 
  dplyr::select(fire, aspect, species, 
                starts_with('germ'), starts_with('Surv'), starts_with('Estab'), 
                starts_with('year1'), starts_with('year2'), 
                N, organic, veg)

# Rescale data
rescaled_df <- complete_df %>%
  mutate_at(vars(germ_mois, germ_temp, year1_mois, year1_temp, year2_mois, year2_temp, N, organic, veg),  ~ scale(.)[,1]) %>% 
  as.data.frame(stringsAsFactors = F)