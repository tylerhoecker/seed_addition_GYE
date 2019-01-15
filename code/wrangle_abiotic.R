# Load soil data for sure in either method. Produces `soil_df`
source('code/read_soil_data.R')

# Two options for a soil moisture index: AUC of log-log model, and percentile (ecdf)

# Log-log exponential model ----------------------------------------------------
soil_sum <- soil_df %>% 
  # Just soil moisture and no missing values 
  filter(!is.na(value), variable == 'mois') %>% 
  # Just the summer 
  filter(time >= as.POSIXct(c("2018-06-01 00:00:00")) & time <= as.POSIXct(c("2018-09-01 00:00:00"))) %>% 
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

lab_dates <- pretty(soil_sum$date)

ggplot() +
  geom_point(data = soil_sum, aes(x = value, y = hours, color = as.numeric(date)), alpha = 0.7) + #, 
  #geom_smooth(data = soil_sum, aes(x = value, y = hours), se = F, color = 'black', size = 1.1) +
  geom_line(data = loglog_df, aes(x = value, y = hours), size = 1.1) + #, color = aspect
  scale_color_viridis_c('Median date', breaks = as.numeric(lab_dates), labels = lab_dates) +
  coord_cartesian(ylim = c(0,50)) +
  facet_grid(aspect~fire) +
  theme_bw(base_size = 12)


# ECDF approach ----------------------------------------------------------------
soil_sum <- soil_df %>% 
  # Just soil moisture and no missing values 
  filter(!is.na(value), variable == 'mois') %>% 
  select(-variable) %>% 
  # Just the summer 
  filter(time >= as.POSIXct(c("2018-06-01 00:00:00")) & time <= as.POSIXct(c("2018-09-01 00:00:00"))) %>% 
  # Truncate negative values to 0 [not sure why there are negative values...]
  mutate(value = if_else(value < 0, 0, value)) %>% 
  # Average all 4 ports
  group_by(fire, aspect, time) %>%
  summarise(value = mean(value)) 


ecdf_fn <- function(x){
  Fn <- ecdf(unique(x))
  pctls <- Fn(x)
  return(data.frame('value' = x, 'prctl' = pctls))
}

ecdf_soil <- soil_sum %>% 
  group_by(fire, aspect) %>% 
  do(quantile(.$value, seq(0.1,0.9, by = 0.1)))

ecdf_soil <- soil_sum %>% 
  nest(-fire,-aspect) %>%
  mutate(Quantiles = map(data, ~ quantile(.$value, c(.5, .75, .90, .99))),
         Quantiles = map(Quantiles, ~ bind_rows(.) %>% gather())) %>% 
  unnest(Quantiles)



colVals <- c('Flat' = '#009E73','North' = '#0072B2','South' = '#E69F00')

ggplot(soil_sum) +
  stat_ecdf(aes(x = value, color = aspect, group = interaction(aspect, fire)), size = 1) +
  geom_rug(data = sample_n(soil_sum, 150), aes(x = value), alpha = 0.3) +
  scale_color_manual(values = colVals, name = 'aspect') +
  #facet_grid(~fire) +
  theme_bw(base_size = 12)

ggplot(soil_sum) +
  geom_density(aes(x = value, fill = aspect, group = interaction(aspect, fire)), alpha = 0.5) +
  scale_fill_manual(values = colVals, name = 'aspect') +
  facet_grid(~fire) +
  theme_bw(base_size = 12)



# Vapor pressure defecit -------------------------------------------------------
source('code/read_atmos_data.R') #creats 'atmos_df'
atmos_sum <- atmos_df %>%
  # Filter dates to just summer
  filter(time >= as.POSIXct(c("2018-06-01 00:00:00")) & time <= as.POSIXct(c("2018-09-01 00:00:00")),
         aspect != 'Grizz') 




# # ATMOS data


