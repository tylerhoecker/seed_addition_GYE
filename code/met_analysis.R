source('code/read_soil_data.R')

soil_data <- soil_df %>% 
  mutate(value = if_else(variable == 'mois', value * 100, value)) %>% 
  group_by(site, aspect, date = floor_date(time, 'hour'), variable) %>%
  summarise_at('value', c('min','mean','max')) 
  # summarise(low = quantile(value, 0.05, na.rm = T),
  #           med = quantile(value, 0.50, na.rm = T),
  #           high = quantile(value, 0.90, na.rm = T)) 

## ATMOS (meteorological) data --------------------------------------------
source('code/read_atmos_data.R') #creats 'atmos_df'

atmos_data <- atmos_df %>% 
  gather(variable, value, -site,-aspect,-time) %>%
  group_by(site, aspect, date = floor_date(time, 'day'), variable) %>% 
  summarise_at('value', c('min','mean','max')) 
# 
#   summarise(low = quantile(value, 0.05, na.rm = T),
#             med = quantile(value, 0.50, na.rm = T),
#             high = quantile(value, 0.90, na.rm = T)) 


abiotic_df <- full_join(soil_data, atmos_data) 

air_soil_diff <- abiotic_df %>% 
  filter(variable %in% c('temp','air_temp')) %>%
  select(-low, -high) %>% 
  spread(variable, med) %>% 
  mutate(diff = temp - air_temp) %>% 
  filter(!is.na(diff))

ggplot(air_soil_diff) +
  geom_line(aes(x = date, y = diff, color = aspect)) +
  facet_wrap(~site, ncol = 4) +
  scale_color_manual(values = colVals2, name = 'aspect') +
  labs(x = 'Day', y = bquote('Soil and air temp. difference ('*~degree*C*')')) +
  theme_bw(base_size = 14) +
  theme(strip.background = element_blank(),
        #strip.text.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

abiotic_df_sel <- abiotic_df %>% 
  filter(variable %in% c('rel_hum'),
         aspect != 'Old')

met_time <- 
  ggplot(abiotic_df_sel, aes(x = date, y = mean)) +
  #geom_ribbon(aes(x = date, ymin = min, ymax = max, fill = aspect), alpha = 0.6) +
  geom_line(aes(color = aspect)) +
  scale_color_manual(values = colVals2) +
  scale_fill_manual(values = colVals2) +
  scale_x_datetime(limits = as.POSIXct(c("2018-06-15 00:00:00", "2018-08-15 00:00:00"))) +
  facet_wrap(~variable+site, ncol = 4) +
  theme_bw(base_size = 14) +
  theme(strip.background = element_blank(),
        #strip.text.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
met_time


