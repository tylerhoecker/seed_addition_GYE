# Import soil data
source('code/read_soil_data.R')
soil_rounded <- soil_df %>% 
  filter(time >= as.POSIXct(c("2018-06-20 00:00:00")) & time <= as.POSIXct(c("2018-08-20 00:00:00"))) %>% 
  group_by(site, aspect, date = floor_date(time, '14 days'), variable) %>%
  summarise_at('value', c('min','mean','max')) 
# ATMOS data
source('code/read_atmos_data.R') #creats 'atmos_df'
atmos_rounded <- atmos_df %>% 
  gather(variable, value, -site,-aspect,-time) %>%
  group_by(site, aspect, date = floor_date(time, '14 days'), variable) %>% 
  summarise_at('value', c('min','mean','max'))


abiotic_rounded <- full_join(soil_rounded, atmos_rounded) 


# Makes 'seedlings' , 'counts', 'counts_full'
source('code/read_seedling_data.R')

counts_rounded <- counts_full %>% 
  mutate(aspect = Hmisc::capitalize(aspect)) %>% 
  ungroup() %>% 
  mutate(date = floor_date(date, '14 days'))


soil_seedling_df <- 
  right_join(abiotic_rounded, counts_rounded) %>% 
  na.omit()

colVals2 <- c('Flat' = '#009E73','North' = '#0072B2','South' = '#E69F00')

ggplot(filter(soil_seedling_df, variable == 'rel_hum'), 
       aes(x = mean, y = prop*100)) +
  geom_point(aes(fill = aspect), shape = 21, size = 3) +
  geom_smooth(method = 'lm', se = T, color = 'black') +
  facet_wrap(~species+variable, scales = 'free', ncol = 1) +
  scale_fill_manual(values = colVals2) +
  labs(x = bquote('Min. 2-week soil mois. (mm'*~m^-3*')'), y = 'Percent of living seedlings') +
  theme_bw(base_size = 14) +
  theme(strip.background = element_blank())



# Correlatoin
corr_df <- soil_seedling_df %>% 
  filter(species == 'pico') %>% 
  filter(variable == 'mois') %>% 
  ungroup() %>% 
  select(min, prop) 

Hmisc::rcorr(as.matrix(corr_df), type="pearson") 


#----------
ggplot(soil_rounded, aes(x = date, y = mean, color = aspect)) +
  #geom_ribbon(aes(x = date, ymin = low, ymax = high, fill = aspect), alpha = 0.6) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = colVals2) +
  scale_fill_manual(values = colVals2) +
  #scale_x_datetime(limits = as.POSIXct(c("2018-06-15 00:00:00", "2018-08-15 00:00:00"))) +
  facet_wrap(~variable+site, scales = 'free_y', ncol = 4) 

