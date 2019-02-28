library(tidyverse)


## Seed-frame data ---------------------------------------------------------

# Read in data from Kobo
source('code/read_seedling_data.R')

# Summarize counts
counts <- seedlings %>% 
  filter(variable == 'height', species != 'control', value > 0) %>% 
  group_by(date, fire, species, aspect) %>%
  summarize(count = n(),
            prop = n()/(5*50))

allMeasures <- seedlings %>% 
  group_by(date, fire, species, aspect) %>% 
  filter(variable == 'height', value < 0 | is.na(value)) %>% 
  summarise(count = 0)

 counts_full <- 
  full_join(counts, allMeasures) %>% 
  group_by(date, fire, species, aspect) %>% 
  filter(species != 'control') %>% 
  summarise(count = sum(count),
            prop = count/(5*50)) 

   
# Summarize measurements
meas_summ <- seedlings %>% 
  group_by(date, fire, species, variable, aspect) %>% 
  summarize(med = quantile(value, 0.50, na.rm = T),
            lower = quantile(value, 0.05, na.rm = T),
            upper = quantile(value, 0.95, na.rm = T))


## Soil sensor data -------------------------------------------------------
source('code/read_soil_data.R')

soil_data <- soil_df %>% 
  #mutate(value = if_else(variable == 'mois', value * 100, value)) %>% 
  group_by(site, aspect, date = floor_date(time, 'day'), variable) %>%
  summarise_at('value', c('min','mean','max')) 

## ATMOS (meteorological) data --------------------------------------------
source('code/read_atmos_data.R') #creats 'atmos_df'

atmos_data <- atmos_df %>% 
  gather(variable, value, -site,-aspect,-time) %>%
  group_by(site, aspect, date = floor_date(time, 'week'), variable) %>% 
  summarise_at('value', c('min','mean','max'))
  

# Time series plotting ---------

colVals <- c('flat' = '#009E73','north' = '#0072B2','south' = '#E69F00')
legLabs <- c('Flat','North','South')

# Counts, through time 
seedlings_time <- 
  ggplot(counts_full) +
  geom_line(aes(x = date, y = count, color = aspect), size = 1) +
  geom_point(aes(x = date, y = count, fill = aspect), shape = 21, size =2) +
  facet_wrap(~species+fire, ncol = 4, scales = 'free') +
  scale_y_continuous(sec.axis = sec_axis(~.*0.4, name = "Percent of planted")) +
  scale_x_datetime(limits = as.POSIXct(c("2018-06-15 00:00:00", "2018-10-30 00:00:00"))) +
  scale_color_manual(values = colVals, name = 'aspect') +
  scale_fill_manual(values = colVals, name = 'aspect') +
  labs(x = 'Date', y = 'Count of living seedlings') +  
  theme_bw(base_size = 14) +
  theme(strip.background = element_blank(),
        #strip.text.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

# Abiotic through time
colVals2 <- c('Flat' = '#009E73','North' = '#0072B2','South' = '#E69F00')

atmos_time <- 
  ggplot(filter(atmos_data, site != "Berry-Grrizz", variable == 'air_temp')) +
  #geom_ribbon(aes(x = date, ymin = min, ymax = max, fill = aspect), alpha = 0.6) +
  geom_line(aes(x = date, y = mean, color = aspect), size = 1) +
  facet_wrap(~site, ncol = 4) +
  scale_x_datetime(limits = as.POSIXct(c("2018-06-15", "2018-10-30"))) +
  #coord_cartesian(ylim = c(-5,28)) +
  scale_color_manual(values = colVals2) +
  scale_fill_manual(values = colVals2) +
  labs(x = 'Day', y = bquote(atop('Daily mean','air temp. ('*~degree*C*')'))) +
  theme_bw(base_size = 14) + 
  theme(strip.background = element_blank(),
    strip.text.x = element_text(face = 'bold'),
    axis.title.x = element_blank(),
    axis.text.x = element_blank())

soil_temp_time <- 
  ggplot(filter(soil_data, variable == 'temp')) +
  #geom_ribbon(aes(x = date, ymin = min, ymax = max, fill = aspect), alpha = 0.6) +
  geom_line(aes(x = date, y = mean, color = aspect), size = 1) +
  facet_wrap(~variable+site, ncol = 4) +
  scale_x_datetime(limits = as.POSIXct(c("2018-06-15", "2018-10-30"))) +
  #coord_cartesian(ylim = c(0,45)) +
  scale_color_manual(values = colVals2) +
  scale_fill_manual(values = colVals2) +
  labs(x = 'Day') + #, y = bquote('Daily mean and range ('*~degree*C*')')) +  #('*mu*'m'*~m^2*','*~degree*C*')'))) + 
  theme_bw(base_size = 14) + 
  theme(strip.background = element_blank(),
        strip.text.x = element_blank())
        #axis.title.x = element_blank(),
        #axis.text.x = element_blank())
soil_mois_time <- 
  ggplot(filter(soil_data, variable == 'mois')) +
  #geom_ribbon(aes(x = date, ymin = min, ymax = max, fill = aspect), alpha = 0.6) +
  geom_line(aes(x = date, y = mean, color = aspect), size = 1) +
  facet_wrap(~variable+site, ncol = 4) +
  scale_x_datetime(limits = as.POSIXct(c("2018-06-15", "2018-10-30"))) +
  coord_cartesian(ylim = c(0.0,0.3)) +
  scale_color_manual(values = colVals2) +
  scale_fill_manual(values = colVals2) +
  labs(x = 'Day') +  #y = bquote('Daily mean and range (mm'*~m^3*')')) + 
  theme_bw(base_size = 14) + 
  theme(strip.background = element_blank(),
        #strip.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())


cowplot::plot_grid(soil_mois_time, soil_temp_time, 
                     rel_heights = c(1.1,1), nrow = 2, align = 'v')

# Proportion alive Plots --------

prop_alive <- seedlings %>% 
  filter(variable == 'height') %>% 
  group_by(date, site, species, aspect, frameID) %>%
  summarize(count = n(),
            prop = n()/(50))

ggplot(prop_alive) +
  #geom_line(aes(x = date, y = prop, color = aspect, group = date), size = 5, alpha = 0.5) +
  geom_point(aes(x = date, y = prop, color = aspect), size = 5, alpha = 0.5) +
  facet_wrap(~species+site, ncol = 4) +
  scale_color_manual(values = colVals, name = 'aspect') +
  theme_bw(base_size = 14) +
  labs(x = 'Measurement date', y = 'Proportion alive of planted')
  




###########################################################################
# Measurements, through time
ggplot(meas_summ) +
  #geom_ribbon(aes(x = date, ymin = lower, ymax = upper, fill = aspect, linetype = species), alpha = 0.5) +
  geom_line(aes(x = date, y = med, color = aspect, linetype = species), size = 1.5) +
  facet_wrap(~site+variable, scales = 'free_y', ncol = 4) +
  scale_fill_manual(values = colVals, name = 'aspect') +
  scale_color_manual(values = colVals, name = 'aspect') +
  labs(x = 'Date', y = 'Count PICO') +
  theme_bw(base_size = 14)
  
  
  
  
meas_time_pico <- 
  ggplot(filter(meas_summ, species == 'pico')) +
  #geom_ribbon(aes(x = date, ymin = lower, ymax = upper, fill = aspect), alpha = 0.5) +
  geom_line(aes(x = date, y = med, color = aspect), size = 1.5) +
  facet_wrap(~site+variable, scales = 'free_y', ncol = 2) +
  scale_fill_manual(values = colVals, name = 'aspect') +
  scale_color_manual(values = colVals, name = 'aspect') +
  labs(x = 'Date', y = 'Count PICO') +
  theme_bw(base_size = 14)

meas_time_psme <- 
  ggplot(filter(meas_summ, species == 'pmse')) +
    #geom_ribbon(aes(x = date, ymin = lower, ymax = upper, fill = aspect), alpha = 0.5) +
    geom_line(aes(x = date, y = med, color = aspect), size = 1.5) +
    facet_wrap(~site+variable, scales = 'free_y', ncol = 2) +
    scale_fill_manual(values = colVals, name = 'aspect') +
    scale_color_manual(values = colVals, name = 'aspect') +
    labs(x = 'Date', y = 'Count PSME') +
    theme_bw(base_size = 14)


# Measurements, full disctributions 
meas_dist_psme <- 
  ggplot(filter(seedlings, species == 'pmse')) +
  geom_density(aes(x = value, fill = aspect), alpha = 0.5) +
  scale_fill_manual(values = colVals, name = 'aspect', labels = legLabs) +
  facet_wrap(~variable + site, scales = 'free', ncol = 4) +
  theme_bw(base_size = 14) +
  labs(x = 'measurement (mm)', title = 'PSME')

meas_dist_pico <- 
  ggplot(filter(seedlings, species == 'pico')) +
  geom_density(aes(x = value, fill = aspect), alpha = 0.5) +
  scale_fill_manual(values = colVals, name = 'aspect', labels = legLabs) +
  facet_wrap(~variable + site, scales = 'free', ncol = 4) +
  theme_bw(base_size = 14) +
  labs(x = 'measurement (mm)', title = 'PICO')


# Frame progress
bgs_df <- seedlings %>% 
  filter(site == 'Berry-Glade', species == 'pico', 
         species == 'pico',  variable == 'height') 

death_dates <- death_dates %>% 
  filter(site == 'Berry-Glade', species == 'pico', 
         species == 'pico',  variable == 'height') 

ggplot(bgs_df) +
  geom_point(aes(x = x_cell, y = y_cell, size = value, fill = value),
             shape = 21) +
  geom_point(data = death_dates, shape = 23,
             aes(x = x_cell, y = y_cell, size = 20), fill = 'darkred') +
  scale_fill_viridis_c() +
  scale_y_continuous(breaks = seq(1,10,1)) +
  facet_wrap(~aspect+frameID, nrow = 3) +
  theme_bw(base_size = 14) +
  transition_states(value, transition_length = 2, state_length = 1) +
  #ease_aes('linear') +
  shadow_mark()


###

# death_dates <- read_csv(this_version) %>% 
#   select(-starts_with('height_header'),-starts_with('basal_header'), 
#          -ends_with('_note'), -`_id`,-`_uuid`,-`_submission_time`,-`_index`) %>% 
#   gather(cell, value, -date,-Fire,-Aspect,-Species,-Direction,-frameID,-Notes) %>% 
#   separate(cell, into = c('variable','y_cell','x_cell'), sep = '_') %>%
#   #unite(x_cell, y_cell, col = 'cell') %>% 
#   filter(Species != 'control_pico', Species != 'control') %>% 
#   modify_at('date', as.POSIXct) %>% 
#   modify_at('y_cell', as.integer) %>% 
#   mutate(Fire = recode(Fire, 'berry_glade'='Berry-Glade','berry_huck'='Berry-Huck', 'buffalo'='Buffalo', 'maple'='Maple'),
#          value = if_else(variable == 'basal', value/100, as.numeric(value))) %>%
#   rename(aspect = Aspect,
#          species = Species,
#          site = Fire,
#          rep = Direction) %>% 
#   filter(value == '-999') 


  