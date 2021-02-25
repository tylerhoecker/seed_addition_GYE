# Import soil data, at this point filter for entire season 
start_time <- as.POSIXct(c("2018-06-01 00:00:00"))
end_time <- as.POSIXct(c("2019-10-01 00:00:00"))
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

# germ_2_temp <- soil_df %>% 
#   filter(time >= "2019-06-01 00:00:00" & time <= "2019-06-15 00:00:00") %>% 
#   group_by(fire, aspect, variable) %>% 
#   summarise(q50 = quantile(value, 0.50, na.rm = T)) %>% 
#   gather(quant, value, -c(fire,aspect,variable)) %>% 
#   unite(temp, variable, quant) %>% 
#   spread(temp, value) %>% 
#   mutate(period = 'Germination')
# 
# test_rels <- full_join(germ_period, germ_2_temp, by = c('fire','aspect'))
# 
# ggplot(test_rels, aes(x = mois_q50.x, y = mois_q50.y)) +
#   geom_point() +
#   geom_smooth(method = 'lm')
# 
# germ_period_filled <- rbind(germ_period, germ_2_temp[c(5,11),]) 
# germ_period_filled[7,] <- germ_2_temp[8,]

surv_period_yr1 <- soil_df %>% 
  # Filter to "survival period"
  filter(time >= "2018-07-08 00:00:00" & time <= "2018-10-01 00:00:00") %>% 
  # Calculate the median soil moisture 
  group_by(fire, aspect, variable) %>% 
  summarise(q50 = quantile(value, 0.50, na.rm = T)) %>% 
  gather(quant, value, -c(fire,aspect,variable)) %>% 
  unite(temp, variable, quant) %>% 
  spread(temp, value) %>% 
  mutate(period = 'Survival_yr1')

surv_period_yr2 <- soil_df %>% 
  # Filter to "survival period"
  filter(time >= "2019-07-08 00:00:00" & time <= "2019-10-01 00:00:00") %>% 
  # Calculate the median soil moisture 
  group_by(fire, aspect, variable) %>% 
  summarise(q50 = quantile(value, 0.50, na.rm = T)) %>% 
  gather(quant, value, -c(fire,aspect,variable)) %>% 
  unite(temp, variable, quant) %>% 
  spread(temp, value) %>% 
  mutate(period = 'Survival_yr2')

# test_rels <- full_join(surv_period_yr1, surv_period_yr2, by = c('fire','aspect'))
# 
# ggplot(test_rels, aes(x = temp_q50.x, y = temp_q50.y)) +
#   geom_point() +
#   geom_smooth(method = 'lm')
# 
#   
# bn_temp <- surv_period_yr1 %>% 
#   filter(fire == 'Buffalo' & aspect == 'North')
# 
# surv_period_yr2 <- surv_period_yr2 %>% 
#   mutate(mois_q50 = if_else(fire == 'Buffalo' & aspect == 'North', bn_temp$mois_q50, mois_q50),
#          temp_q50 = if_else(fire == 'Buffalo' & aspect == 'North', bn_temp$temp_q50, temp_q50))

estab_period_yr1 <- surv_period_yr1 %>% 
  mutate(period = 'Establishment_yr1')

estab_period_yr2 <- surv_period_yr2 %>% 
  mutate(period = 'Establishment_yr2')


soil_preds <- rbind(germ_period, surv_period_yr1, surv_period_yr2, estab_period_yr1, estab_period_yr2)

soil_preds %>% 
  group_by(aspect, period) %>% 
  summarise_at(vars(mois_q50, temp_q50), list(~mean(., na.rm = T))) %>% 
  filter(period != 'Establishment') %>% 
  arrange(period)

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
  group_by(fire, aspect) %>% 
  summarise(Germination = sum(germinated, na.rm = T) / n(),
            Survival_yr1 = sum(survived_yr1, na.rm = T) / sum(germinated, na.rm = T),
            Survival_yr2 = sum(survived_yr2, na.rm = T) / sum(germinated, na.rm = T),
            #Survival_both = sum(survived_yr2, na.rm = T) / sum(survived_yr1, na.rm = T),
            Establishment_yr1 = sum(survived_yr1, na.rm = T) / n(),
            Establishment_yr2 = sum(survived_yr2, na.rm = T) / n()) 

# Combine response and predictor variables -------------------------------------
complete_df <- proportions %>%
  # Add informaiton for weights for binomial regression
  mutate(germ_estab_Weights = as.integer(250),
         surv_Weights = as.integer(Germination * 250)) %>% 
  gather(period, value, -fire, -aspect,  -germ_estab_Weights, -surv_Weights) %>% 
  full_join(soil_preds) %>% 
  full_join(pct_cover) %>% 
  full_join(soil_props) %>%
  full_join(terrain) %>% 
  modify_at(c('species','period','texture'), as.factor) %>% 
  ungroup() %>% 
  mutate(period = fct_relevel(period, 'Germination','Survival_yr1','Survival_yr2','Establishment_yr1','Establishment_yr2'),
         Weights = if_else(period %in% c('Germination','Establishment_yr1','Establishment_yr2'),
                           germ_estab_Weights, # else (survival):
                           surv_Weights),
         site = paste(fire, aspect, sep = '-')) %>% 
  dplyr::select(site, fire, aspect, period, value, Weights, mois_q50, temp_q50, veg, organic, N, Sand, elev, dev_ne, tpi) 

# Plot a 2-parameter model space based on the top model.
colVals <- rev(c('South' = '#E69F00', 'North' = '#0072B2','Flat' = '#009E73'))
#legLabs <- rev(c('South','North','Flat'))
alphaVals <- 0.4
sizeVals <- 1



#plotObj <- 
complete_df %>% 
  filter(period %in% c('Survival_yr2')) %>% 
  ggplot(aes(x = mois_q50, y = temp_q50, size = value, group = site)) +
  geom_point(aes(fill = aspect), shape = 21, color = 'black', stroke = 0.7) +
  #geom_text(aes(label = fire)) +
  #scale_x_continuous(limits = c(0.033,0.12), breaks = seq(0.03,0.12,0.03)) +
  #scale_y_continuous(limits = c(12,18), breaks = seq(12,18,2)) +
  scale_radius("Proportion", range = c(2,8)) + #, 
  facet_grid(~period) +
  theme_bw(base_size = 14) +
  scale_fill_manual('Aspect', values = colVals) +
  labs(x = 'Soil moisture (vwc)', y = bquote('Soil temperature ('*~degree *C*')')) +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = 'bold'))

plotObj
legend <- cowplot::get_legend(plotObj)

plot(legend)