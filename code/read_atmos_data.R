# This script detects all files in the data/met_stations direcotry,
# reads them in (automatically produced by xls fiels the METER ATMOS 
# weather station + Em60 data logger.) 
# It re-orders columns to put the most relevant variables first

library(tidyverse)
library(lubridate)
library(readxl)

read_atmos <- function(file_name){
  data <- read_excel(file_name,
                     na = '#N/A',
                     skip = 3,
                     col_names = FALSE) %>%
    select(time = 1, 
           air_temp = 9, precip = 3, rel_hum = 10, atms_press = 11, solar = 2, 
           wind_dir = 6, wind_speed = 7, wind_gust = 8, 
           light_n = 4, light_dist = 5, 
           precip_rate = 14, RH_temp = 15,
           xaxis = 12, yaxis = 13)
  return(data)
}

raw_files <- list.files(file.path('data','met_stations'),full.names = TRUE)

atmos_df <- map(raw_files, read_atmos) %>% 
  `names<-` (str_extract(raw_files, '[[:upper:]][[:alnum:]]+-*[[:upper:]]*[[:alnum:]]+_[[:upper:]][[:alnum:]]*')) %>%
  bind_rows(.id = 'site') %>%   
  #mutate(time = parse_date_time(.$time, orders = 'mdy hs')) %>%
  separate(site, into = c('fire','aspect'), sep = "_") %>% 
  modify_at(c('fire','aspect', 'variable'), as_factor) %>% 
  mutate(aspect = fct_relevel(aspect, 'North','South','Grizz'))
# END
  
