# This script detects all files in the data/soil directory,
# (automatic ouput of METER 5TM soil sensors and Em50 data logger), and
# reads them in.
# The script assumes that "Port 4" is an empty port.
# Ports 1-3 are inside seed-addition frames, port 5 is outside. Ports are 
# renamed 1-4, with 4 becoming "empty" and then removed.
# This does not work with the "Berry-Old" sites because their ports were 
# arranged differently.
# After read-in, the sript does some wrangling to transpose columns for 
# each port and varibale into key-columns for each variable (temp, mois), 
# port #, and value

library(tidyverse)
library(lubridate)
library(readxl)

read_soil <- function(file_name){
  data <- read_excel(file_name,
                     col_types = c('date',rep('text',9)),
                     na = '#N/A',
                     skip = 3,
                     col_names = FALSE) %>%
    select(time = 1, 
           mois_1 = 2, temp_1 = 3, 
           mois_2 = 4, temp_2 = 5,
           mois_3 = 6, temp_3 = 7,
           mois_4 = 9, temp_4 = 10)
  data <- data %>% 
    modify_at(2:9, as.numeric)
  return(data)
}

raw_files <- list.files(file.path('data','soil_sensors'),full.names = TRUE)

soil_df <- map(raw_files, read_soil) %>% 
  `names<-` (str_extract(raw_files, '[[:upper:]][[:alnum:]]+-*[[:upper:]]*[[:alnum:]]+_[[:upper:]][[:alnum:]]*')) %>%
  bind_rows(.id = 'site') %>%   
  separate(site, into = c('fire','aspect'), sep = "_") %>%
  #separate(time, into = c('date','hour'), sep = " ") %>%
  #mutate(date = as.POSIXct(date)) %>%
  gather(key, value, -fire,-aspect,-time) %>%
  separate(key, into = c('variable','port'), sep = '_') %>% 
  modify_at(c('fire','aspect', 'variable'), as_factor) %>% 
  mutate(aspect = fct_relevel(aspect, 'North','Flat','South'))


# END


