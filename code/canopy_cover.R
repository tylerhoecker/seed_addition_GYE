library(tidyverse)
library(lubridate)
library(readxl)

# Import soil data, at this point filter for entire season 
start_time <- as.POSIXct(c("2018-08-01 00:00:00"))
end_time <- as.POSIXct(c("2018-10-01 00:00:00"))

read_soil <- function(file_name){
  data <- read_excel(file_name,
                     col_types = c('date',rep('text',9)),
                     na = '#N/A',
                     skip = 3,
                     col_names = FALSE) %>%
    dplyr::select(time = 1, 
                  mois_1 = 2, temp_1 = 3, 
                  mois_2 = 4, temp_2 = 5,
                  mois_3 = 6, temp_3 = 7,
                  mois_4 = 9, temp_4 = 10)
  data <- data %>% 
    modify_at(2:9, as.numeric)
  return(data)
}

raw_files <- list.files(file.path('data','canopy_soil_sensors'),full.names = TRUE)

soil_df <- map(raw_files, read_soil) %>% 
  `names<-` (str_extract(raw_files, '[[:upper:]][[:alnum:]]+-*[[:upper:]]*[[:alnum:]]+_[[:upper:]][[:alnum:]]*')) %>%
  bind_rows(.id = 'site') %>%   
  separate(site, into = c('fire','aspect'), sep = "_") %>%
  gather(key, value, -fire,-aspect,-time) %>%
  separate(key, into = c('variable','port'), sep = '_') %>% 
  modify_at(c('fire','aspect', 'variable'), as_factor) %>% 
  mutate(aspect = fct_relevel(aspect, 'North','South')) %>% 
  # no missing values 
  #filter(!is.na(value)) %>% 
  # Just the summer 
  filter(time >= start_time & time <= end_time) %>% 
  # Truncate negative values to 0 [not sure why there are negative values...]
  mutate(value = if_else(value < 0 & variable == 'mois', as.numeric(NA), value)) %>% 
  # Average all 4 ports
  group_by(fire, aspect, time, variable) %>%
  summarise(value = mean(value, na.rm = T)) 


soil_data <- soil_df %>% 
  separate(time, into = c('date','hour'), sep = " ") %>%
  mutate(date = as.Date(date),
         variable = if_else(variable == 'mois','Soil water content','Soil temperature')) %>%
  group_by(fire, aspect, date, variable) %>% # Change site for aspect
  summarize(dayMin = min(value, na.rm = T),
            dayMean = mean(value, na.rm = T),
            dayMax = max(value, na.rm = T)) %>% 
  mutate(dayMin = ifelse(is.infinite(dayMin), NA, dayMin),
         dayMax = ifelse(is.infinite(dayMax), NA, dayMax))


colVals <- rev(c('South' = '#E69F00', 'North' = '#0072B2'))
#legLabs <- rev(c('South','North','Flat'))
alphaVals <- 0.4
sizeVals <- 1

soil_time_plot <- 
  ggplot(soil_data, aes(x = date)) +
  #geom_line(aes(y = dayMin, color = aspect, linetype = fire), size = 0.5) +
  #geom_line(aes(y = dayMax, color = aspect, linetype = fire), size = 0.5) +
  # geom_ribbon(aes(ymin = dayMin, ymax = dayMax, fill = aspect),
  #             alpha = alphaVals) +
  geom_line(aes(y = dayMean, color = aspect, linetype = fire), size = 0.85) +
  scale_fill_manual('Aspect', values = colVals) +
  scale_color_manual('Aspect', values = colVals) +
  scale_linetype('Canopy cover', labels = c('Absent', 'Present')) +
  facet_wrap(~variable, scales = 'free') +
  theme_bw(base_size = 14) +
  labs(x = '', y = bquote('Daily mean & range ('*~m^3*''*~m^-3*','*~degree *C*')')) +
  theme(strip.background = element_blank(),
        plot.subtitle = element_text(hjust = 0.5, face = 'bold')) +
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%b")






