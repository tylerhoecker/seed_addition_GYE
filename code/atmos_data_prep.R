library(lubridate)
# Import ATMOS data
source('code/read_atmos_data.R') #creats 'atmos_df'

# Summarize ATMOS data
atmos_df <- atmos_df %>% 
  # Just summer
  filter(time >= start_time & time <= end_time,
         # Ignore 'Grizz' for now
         aspect != 'Grizz',
         # Remove NAs
         !is.na(air_temp)) %>% 
  # Summarize at hourly, to align with soil moisture
  mutate(time = floor_date(time, 'hour')) %>% 
  group_by(fire, aspect, time) %>% 
  summarize_all(mean) %>% 
  # Truncate RH at 1
  mutate(rel_hum = if_else(rel_hum >= 1, 1, rel_hum),
         # The ATMOS 41 measures RH and temperature and computes vapor pressure (es) 
         # using the Magnus-Tetens eq. from Buck 1981. es is not saved by the 
         # data logger, so must be re-calculated using: 
         # es = 0.6108 * exp(17.502 * T / (T + 240.97)). Then, ea = RH*es; then VPD = es - ea. 
         # ATMOS 41 manual: http://library.metergroup.com/Manuals/20635_ATMOS41_Manual_Web.pdf 
         es = 0.6108 * exp((17.502 * air_temp) / (air_temp + 240.97)),
         vpd = es - rel_hum * es) 

# Caclulate percentiles and pull out just air temp and vpd
atmos_preds <- atmos_df %>% 
  dplyr::select(air_temp, vpd, solar, precip) %>% 
  gather(variable, value, air_temp:precip) %>%
  group_by(fire, aspect, variable) %>% 
  summarise(q50 = quantile(value, 0.50),
            q75 = quantile(value, 0.75),
            min = min(value),
            max = max(value)) %>% 
  gather(quant, value, q50:max) %>% 
  unite(temp, variable, quant) %>% 
  spread(temp, value)





