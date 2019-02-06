# Right now this is a working script to test out different approaches for summarizing 
# abiotic data. 
# The percentile/ecdf approach was copied over to `soil_mois_ecdf.R` on 1/15/18,
# for easy calling from `abiotic_modeling.R`


# Decide on a time period of interest:
start_time <- as.POSIXct(c("2018-06-01 00:00:00"))
end_time <- as.POSIXct(c("2018-10-01 00:00:00"))


# Load soil data for sure in either method. Produces `soil_df`
source('code/read_soil_data.R')


# Examine covariance between soil moisture and temperature 
# There is a significant, but weak negative correlation of r = -0.24/ r2 = 0.04
# ------------------------------------------------------------------------------

mois_temp <- soil_df %>% 
  # Just soil moisture and no missing values 
  filter(!is.na(value)) %>% 
  # Just the summer 
  filter(time >= start_time & time <= end_time) %>% 
  # Truncate negative values to 0
  mutate(value = if_else(value < 0, 0, value)) %>% 
  # Average all 4 ports
  group_by(fire, aspect, time, variable) %>%
  summarise(value = mean(value)) %>% 
  spread(variable, value) %>% 
  group_by(fire, aspect) %>% 
  sample_n(100)

mois_temp %>% 
  ggplot() +
  geom_point(aes(x = mois, y = temp)) 

cor.test(mois_temp$mois, mois_temp$temp)

# Two options for a soil moisture index: AUC of log-log model, and percentile (ecdf)
# Log-log exponential model ----------------------------------------------------
soil_sum <- soil_df %>% 
  # Just soil moisture and no missing values 
  filter(!is.na(value), variable == 'mois') %>% 
  # Just the summer 
  filter(time >= start_time & time <= end_time) %>% 
  # Truncate negative values to 0
  mutate(value = if_else(value < 0, 0, value)) %>% 
  # Average all 4 ports
  group_by(fire, aspect, time, variable) %>%
  summarise(value = mean(value)) %>% 
  # Bin (round) values into 3rd decimal place
  mutate(value = round(value, 3)) %>% 
  # Count number of hours in each soil moisture bin
  group_by(fire, aspect, variable, value) %>% 
  summarise(hours = n(),
            date = median(time))

loglog_fn <- function(data){
  exp_model <- lm(log(hours) ~ log(value), data = data)
  #exp_model <- lm(hours ~ poly(value, 2), data = data)
  new_x <- seq(0.01, 0.30, length.out = 1000)
  prediction <- predict(exp_model, list(value = new_x), se.fit = T)
  new_y <- prediction$fit
  output <- data.frame(aspect = rep(data$aspect, length(new_x)),
                       value = new_x,
                       hours = exp(new_y))
  return(output)
}


loglog_df <- soil_sum %>% 
  group_by(fire, aspect) %>% 
  do(loglog_fn(.))

ggplot() +
  geom_point(data = soil_sum, aes(x = value, y = hours, color = as.numeric(date)), alpha = 0.7) + #, 
  #geom_smooth(data = soil_sum, aes(x = value, y = hours), se = F, color = 'black', size = 1.1) +
  geom_line(data = loglog_df, aes(x = value, y = hours), size = 1.1) + #, color = aspect
  scale_color_viridis_c('Median date', breaks = as.numeric(lab_dates), labels = lab_dates) +
  coord_cartesian(ylim = c(0,50)) +
  facet_grid(aspect~fire) +
  theme_bw(base_size = 12)

mois_auc <- loglog_df %>% 
  group_by(fire, aspect) %>% 
  do(auc = DescTools::AUC(.$value, .$hours))

mois_auc %>% 
  unnest(auc) %>% 
  arrange(auc)

auc_test <- DescTools::AUC(x, y)


lab_dates <- pretty(soil_sum$date)




# ECDF approach ----------------------------------------------------------------
soil_sum <- soil_df %>% 
  # Just soil moisture and no missing values 
  filter(!is.na(value)) %>% 
  # Just the summer 
  filter(time >= as.POSIXct(c("2018-06-01 00:00:00")) & time <= as.POSIXct(c("2018-09-01 00:00:00"))) %>% 
  # Truncate negative values to 0 [not sure why there are negative values...]
  mutate(value = if_else(value < 0, 0, value)) %>% 
  # Average all 4 ports
  group_by(fire, aspect, time, variable) %>%
  summarise(value = mean(value)) 


# ecdf_fn <- function(x){
#   Fn <- ecdf(unique(x))
#   pctls <- Fn(x)
#   return(data.frame('value' = x, 'prctl' = pctls))
# }


# Not working with both temp and moisture....
# ecdf_soil <- soil_sum %>% 
#   nest() %>%
#   mutate(Quantiles = map(data, ~ quantile(.$value, c(.5, .75, .90, .99))),
#          Quantiles = map(Quantiles, ~ bind_rows(.) %>% gather())) %>% 
#   unnest(Quantiles)



colVals <- c('Flat' = '#009E73','North' = '#0072B2','South' = '#E69F00')

ggplot(soil_sum) +
  stat_ecdf(aes(x = value, color = aspect, group = interaction(aspect, fire)), size = 1) +
  #geom_rug(data = sample_n(soil_sum, 150), aes(x = value), alpha = 0.3) +
  scale_color_manual(values = colVals, name = 'aspect') +
  facet_grid(fire~variable, scales = 'free') +
  theme_bw(base_size = 12) +
  labs(x = 'sensor value (VWC, deg. C)', y = 'Cumulative probability')

plot_df <- soil_df %>% 
  # no missing values 
  filter(!is.na(value)) %>% 
  # Just the summer 
  filter(time >= start_time & time <= end_time) %>% 
  # Truncate negative values to 0 [not sure why there are negative values...]
  mutate(value = if_else(value < 0, 0, value)) %>% 
  # Average all 4 ports
  group_by(fire, aspect, time, variable) %>%
  summarise(value = mean(value)) %>%  
  spread(variable, value) %>% 
  group_by(fire, aspect)

ggplot(sample_n(plot_df, 500), aes(x = mois, y = temp)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = 'lm') +
  theme_bw(base_size = 14)

ggplot(soil_preds) +
  geom_point(aes(x = mois_q50, y = temp_q50))

# Vapor pressure defecit -------------------------------------------------------
# Load and prep meteorological data (air temperature and VPD)
source('code/atmos_data_prep.R') # creates `atmos_df` (complete) and `atmos_preds` (summarized)

# Correlation
combined <- soil_sum %>% 
  spread(variable, value) %>% 
  full_join(., atmos_df) %>% 
  select(fire:temp, air_temp, vpd) %>% 
  group_by(fire, aspect) %>% 
  sample_n(100) %>% 
  ungroup() %>% 
  select(-fire, -aspect, -time)

library(GGally)
ggpairs(combined, alpha = 0.5)

plot_df <- soil_sum %>% 
  spread(variable, value) %>% 
  full_join(., atmos_df) %>% 
  select(fire:temp, vpd) %>% 
  rename(soil_mois = mois, soil_temp = temp) %>% 
  gather(variable, value, soil_mois:vpd) 


ggplot(plot_df) +
  stat_ecdf(aes(x = value, color = aspect, group = interaction(aspect, fire)), size = 1) +
  #geom_rug(data = sample_n(soil_sum, 150), aes(x = value), alpha = 0.3) +
  scale_color_manual(values = colVals, name = 'aspect') +
  facet_grid(fire~variable, scales = 'free') +
  theme_bw(base_size = 12) +
  labs(x = 'sensor value (VWC, deg. C, kPa)', y = 'Cumulative probability')

