source('code/read_soil_data.R')

# Decide on a time period of interest:
start_time <- as.POSIXct(c("2018-06-01 00:00:00"))
end_time <- as.POSIXct(c("2018-10-01 00:00:00"))


# Select just soil moisture during summer and average to hourly.
soil_preds <- soil_df %>% 
  # no missing values 
  filter(!is.na(value)) %>% 
  # Just the summer 
  filter(time >= start_time & time <= end_time) %>% 
  # Truncate negative values to 0 [not sure why there are negative values...]
  mutate(value = if_else(value < 0, 0, value)) 
  
ggplot(soil_preds, aes(x = port, y = value)) +
  geom_boxplot() +
  facet_wrap(~variable, scales = 'free')

ggplot(soil_preds, aes(x = value, group = port)) +
  stat_ecdf() +
  facet_wrap(~variable, scales = 'free')

