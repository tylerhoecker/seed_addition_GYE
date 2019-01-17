# Import soil data
source('code/read_soil_data.R')

# Select just soil moisture during summer and average to hourly.
soil_preds <- soil_df %>% 
  # no missing values 
  filter(!is.na(value)) %>% 
  # Just the summer 
  filter(time >= start_time & time <= end_time) %>% 
  # Truncate negative values to 0 [not sure why there are negative values...]
  mutate(value = if_else(value < 0, 0, value)) %>% 
  # Average all 4 ports
  group_by(fire, aspect, time, variable) %>%
  summarise(value = mean(value)) %>% 
# Caclulate 50th and 75th percentiles.
  group_by(fire, aspect, variable) %>% 
  summarise(q50 = quantile(value, 0.50),
            q75 = quantile(value, 0.75),
            min = min(value),
            max = max(value)) %>% 
  gather(quant, value, q50:max) %>% 
  unite(temp, variable, quant) %>% 
  spread(temp, value)

# This is a very neat approach using purrr, 
# but I don't really understand what's happening, so I'm using the method above for now instead.

# ecdf_soil <- soil_sum %>% 
#   nest(-fire,-aspect) %>%
#   mutate(Quantiles = map(data, ~ quantile(.$value, c(.5, .75, .90, .99))),
#          Quantiles = map(Quantiles, ~ bind_rows(.) %>% gather())) %>% 
#   unnest(Quantiles) %>% 
#   filter(key == '75%')

# colVals <- c('Flat' = '#009E73','North' = '#0072B2','South' = '#E69F00')
# 
# ggplot() +
#   geom_hline(data = soil_sum, aes(yintercept = 0.5)) +
#   geom_vline(data = ecdf_soil, aes(xintercept = mois_q50, color = aspect)) + 
#   stat_ecdf(data = soil_sum, aes(x = value, color = aspect, group = interaction(aspect, fire)), size = 1) +
#   #geom_rug(data = sample_n(soil_sum, 150), aes(x = value), alpha = 0.3) +
#   scale_color_manual(values = colVals, name = 'aspect') +
#   facet_grid(~fire) +
#   theme_bw(base_size = 12)

