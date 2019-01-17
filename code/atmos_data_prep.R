# Import ATMOS data
source('code/read_atmos_data.R') #creats 'atmos_df'

# Summarize ATMOS data
atmos_preds <- atmos_df %>% 
  # Just summer
  filter(time >= start_time & time <= end_time,
         # Ignore 'Grizz' for now
         aspect != 'Grizz',
         # Remove NAs
         !is.na(air_temp)) %>% 
  group_by(fire, aspect) %>%
  # Saturation vapor pressure (es) is calculated automatically by the ATMOS 41 using the 
  # Magnus-Tetens equation in Buck 1981, and used to calculate RH as: RH = ea / (es * air temp).
  # es is not saved by the data logger, but is easily back-calculated using the 
  # supplied values for ea and RH, as: es = ea / RH.
  # See ATMOS 41 manual: http://library.metergroup.com/Manuals/20635_ATMOS41_Manual_Web.pdf 
  mutate(vpd = (atms_press / rel_hum) - atms_press) %>% 
  select(air_temp, vpd) %>% 
  gather(variable, value, air_temp, vpd) %>%
  group_by(fire, aspect, variable) %>% 
  summarise(q50 = quantile(value, 0.50),
            q75 = quantile(value, 0.75),
            min = min(value),
            max = max(value)) %>% 
  gather(quant, value, q50:max) %>% 
  unite(temp, variable, quant) %>% 
  spread(temp, value)





