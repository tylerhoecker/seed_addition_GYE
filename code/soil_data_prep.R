# Import soil data
source('code/read_soil_data.R')

# Select just soil moisture during summer and average to hourly.
soil_preds <- soil_df %>% 
  group_by(fire, aspect, variable) %>% 
  # Issue with Maple soil moisture - sensor not calibrated correctly... >:o
  mutate(value = if_else(fire == 'Maple' & variable == 'mois', value-10, value)) %>% 
  summarise(q50 = quantile(value, 0.50),
            q75 = quantile(value, 0.75),
            min = min(value),
            max = max(value),
            dry_hours = sum(value < 10),
            hot_hours = sum(value > 30)) %>% 
  gather(quant, value, -c(fire,aspect,variable)) %>% 
  unite(temp, variable, quant) %>% 
  spread(temp, value) %>% 
  dplyr::select(-mois_hot_hours, -temp_dry_hours)

# soil_df %>%
#   filter(variable == 'mois') %>%
#   #mutate(value = if_else(fire == 'Maple', value-5, value)) %>% 
#   ggplot() +
#   geom_histogram(aes(x = value)) +
#   geom_vline(xintercept = 10) +
#   facet_grid(aspect~fire) +
#   theme_bw()

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

