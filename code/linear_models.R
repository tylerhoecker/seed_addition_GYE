# Predictor variables ----------------------------------------------------------

# Import soil data, at this point filter for entire season 
start_time <- as.POSIXct(c("2018-06-01 00:00:00"))
end_time <- as.POSIXct(c("2018-10-01 00:00:00"))
source('code/read_soil_data.R')

# Filter to germination and survival periods, and calculate median values 
germ_period <- soil_df %>% 
  # Filter to "germination period"
  filter(time >= "2018-06-01 00:00:00" & time <= "2018-06-15 00:00:00") %>% 
  # Calculate the median soil moisture 
  group_by(fire, aspect, variable) %>% 
  summarise(q50 = quantile(value, 0.50, na.rm = T)) %>% 
  gather(quant, value, -c(fire,aspect,variable)) %>% 
  unite(temp, variable, quant) %>% 
  spread(temp, value) %>% 
  mutate(period = 'Germination')

surv_period <- soil_df %>% 
  # Filter to "survival period"
  filter(time >= "2018-07-08 00:00:00" & time <= "2018-10-01 00:00:00") %>% 
  # Calculate the median soil moisture 
  group_by(fire, aspect, variable) %>% 
  summarise(q50 = quantile(value, 0.50, na.rm = T)) %>% 
  gather(quant, value, -c(fire,aspect,variable)) %>% 
  unite(temp, variable, quant) %>% 
  spread(temp, value) %>% 
  mutate(period = 'Survival')

estab_period <- surv_period %>% 
  mutate(period = 'Establishment')

soil_preds <- full_join(surv_period, germ_period) %>% 
  full_join(estab_period)

# Percent cover data
pct_cover <- read_csv('data/pct_cover.csv') %>% 
  group_by(fire, aspect) %>% 
  summarise_all(mean) %>% 
  mutate(veg = forb + gram) %>% 
  dplyr::select(fire, aspect, veg)

# Soil properties/texture
soil_props <- read_csv('data/soil_properties.csv') 


# Import response variables (seedling germination, survival)--------------------
source('code/read_summarize_seedling.R')
seedling_response <- proportions %>% 
  #filter(version == 'asinsqrt') %>% 
  #dplyr::select(-version) %>% 
  group_by(fire, aspect, species, period, version) %>% 
  summarise_if(is.numeric, mean) %>% 
  dplyr::select(-frameID)

# Combine response and predictor variables -------------------------------------
complete_df <-   seedling_response %>% 
  mutate(Weights = 250) %>% 
  full_join(soil_preds) %>% 
  full_join(pct_cover) %>% 
  full_join(soil_props) %>% 
  modify_at(c('species','period','texture'), as.factor) %>% 
  ungroup() %>% 
  mutate(period = fct_relevel(period, 'Germination','Survival','Establishment')) %>% 
  dplyr::select(-fire,-aspect)

rescaled_df <- complete_df %>% 
  ungroup() %>%
  mutate_at(vars(-c(species,period,version,value,Weights,texture)), scale) 



# Linear models ----------------------------------------------------------------
rescaled_df %>%
  filter(version == 'asinsqrt') %>% 
  filter(period == 'Germination') %>% 
  drop_na() %>% 
  group_by(species,period) %>% 
  do(dredge(lm(value ~ mois_q50 + temp_q50 + organic,
                data = ., na.action = 'na.fail'), 
            m.lim = c(0,2), 
            extra = list("R^2", "*" = function(x) {
                s <- summary(x)
                c(Rsq = s$r.squared, adjRsq = s$adj.r.squared, F = s$fstatistic[[1]])
              }),
            rank = 'AIC')) %>% 
  filter(delta <= 2) %>% 
  write_csv(., 'linear_germ_model_sel.csv')


rescaled_df %>%
  filter(version == 'asinsqrt') %>% 
  filter(period %in% c('Survival','Establishment')) %>% 
  group_by(species,period) %>% 
  do(dredge(lm(value ~ mois_q50 + temp_q50 + veg + N,
               data = ., na.action = 'na.fail'), 
            m.lim = c(0,3), 
            extra = list("R^2", "*" = function(x) {
              s <- summary(x)
              c(Rsq = s$r.squared, adjRsq = s$adj.r.squared, F = s$fstatistic[[1]])
            }),
            rank = 'AIC')) %>% 
  filter(delta <= 2) %>% 
  write_csv(., 'linear_surv_estab_model_sel.csv')




