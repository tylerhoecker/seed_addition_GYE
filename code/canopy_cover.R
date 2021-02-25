library(tidyverse)
library(lubridate)
library(readxl)

# Import soil data, at this point filter for entire season 
start_time <- as.POSIXct(c("2019-06-01 00:00:00"))
end_time <- as.POSIXct(c("2019-10-01 00:00:00"))

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

raw_files <- list.files(file.path('data','canopy_soil_sensors'), pattern = '*.xls',  full.names = TRUE)

soil_df <- map(raw_files, read_soil) %>% 
  `names<-` (str_extract(raw_files, '[[:upper:]][[:alnum:]]+-*[[:upper:]]*[[:alnum:]]+_[[:upper:]][[:alnum:]]*')) %>%
  bind_rows(.id = 'site') %>%   
  separate(site, into = c('fire','aspect'), sep = "_") %>%
  gather(key, value, -fire,-aspect,-time) %>%
  separate(key, into = c('variable','port'), sep = '_') %>% 
  modify_at(c('fire','aspect', 'variable'), as_factor) %>% 
  mutate(aspect = fct_relevel(aspect, 'North','Flat','South')) %>% 
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
         variable = if_else(variable == 'mois','Soil moisture','Soil temperature')) %>%
  group_by(fire, aspect, date, variable) %>% # Change site for aspect
  summarize(dayMin = min(value, na.rm = T),
            dayMean = mean(value, na.rm = T),
            dayMax = max(value, na.rm = T)) %>% 
  mutate(dayMin = ifelse(is.infinite(dayMin), NA, dayMin),
         dayMax = ifelse(is.infinite(dayMax), NA, dayMax)) %>% 
  mutate(variable = factor(variable, levels = c('Soil temperature', 'Soil moisture')))


colVals <- rev(c('South' = '#E69F00', 'North' = '#0072B2','Flat' = '#009E73'))
#legLabs <- rev(c('South','North','Flat'))
alphaVals <- 0.4
sizeVals <- 1

soil_time_plot <- 
  ggplot(soil_data, aes(x = date)) +
  #geom_line(aes(y = dayMin, color = aspect, linetype = fire), size = 0.5) +
  #geom_line(aes(y = dayMax, color = aspect, linetype = fire), size = 0.5) +
  # geom_ribbon(aes(ymin = dayMin, ymax = dayMax, fill = aspect),
  #             alpha = alphaVals) +
  geom_smooth(aes(y = dayMean, color = aspect, fill = aspect, linetype = fire), se = F, size = 0.85, span = 0.12) +
  #geom_line(aes(y = dayMean, color = aspect, fill = aspect, linetype = fire), size = 0.85) +
  
  scale_fill_manual('Aspect', values = colVals) +
  scale_color_manual('Aspect', values = colVals) +
  scale_linetype('Fire return interval', labels = c('Short', 'Long')) +
  facet_grid(variable~aspect, scales = 'free') +
  theme_bw(base_size = 14) +
  labs(x = '', y = bquote('Mean soil conditions ('*~m^3*''*~m^-3*','*~degree *C*')')) +
  theme(strip.background = element_blank(),
        plot.subtitle = element_text(hjust = 0.5, face = 'bold')) +
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%b")


soil_data %>% 
  #filter(date >= as.POSIXct(c("2019-06-15 00:00:00")) & date <= as.POSIXct(c("2019-08-15 00:00:00"))) %>% 
  group_by(fire, aspect, variable) %>%
  summarize(dayLower = sd(dayMean),
            dayUpper = sd(dayMean),
            dayMean = mean(dayMean, na.rm = T)) %>% 
  ggplot(., aes(x = aspect, fill = aspect, alpha = fire)) +
  geom_col(aes(y = dayMean), position = position_dodge(width = 0.9),
           color = 'black') +
  geom_errorbar(aes(ymin = dayMean - dayLower, ymax = dayMean + dayUpper),
                size = 0.75, width = 0.3, position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = colVals, name = 'Aspect') +
  scale_alpha_manual(values = c(1,0.5)) + 
  # scale_fill_manual(values = colVals, name = 'Aspect') +
  # scale_x_discrete(labels = c('Germination', 'Survival (yr 1)', 'Survival (yr 2)', 'Establishment')) +
  # geom_vline(xintercept = c(1.5,2.5,3.5), color = 'grey90') +
  # geom_text(aes(y = Upper + 0.03, label = letters), position = position_dodge(width = 0.9),
  #           color = 'black', size = 4) +
  # # geom_text(aes(y = -0.03, label = substring(aspect, 1, 1)), 
  # #           position = position_dodge(width = 0.9), color = 'grey30') +
  facet_wrap(~variable, nrow = 1, scales = 'free') +
  labs(y = 'Proportion of seeds') +
  theme_bw(base_size = 14) +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = 'bold', size = 13),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.subtitle = element_text(hjust = 0.5, face = 'bold'),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        #legend.position = c(.43,.8),
        legend.background = element_blank(),
        legend.title = element_text(size = 12)  ) 

soil_time_plot


####
temp <- 
  ggplot(filter(soil_data, variable == 'Soil temperature'), aes(x = date)) +
  #geom_line(aes(y = dayMin, color = aspect, linetype = fire), size = 0.5) +
  #geom_line(aes(y = dayMax, color = aspect, linetype = fire), size = 0.5) +
  # geom_ribbon(aes(ymin = dayMin, ymax = dayMax, fill = aspect),
  #             alpha = alphaVals) +
  geom_smooth(aes(y = dayMean, color = aspect, fill = aspect, linetype = fire), se = F, size = 0.85, span = 0.12) +
  #geom_line(aes(y = dayMean, color = aspect, fill = aspect, linetype = fire), size = 0.85) +
  
  scale_fill_manual('Aspect', values = colVals) +
  scale_color_manual('Aspect', values = colVals) +
  scale_linetype('Fire return interval', labels = c('Short', 'Long')) +
  #facet_grid(variable~aspect, scales = 'free') +
  theme_bw(base_size = 14) +
  labs(x = '', y = bquote('Mean soil temperature ('*~degree *C*')')) +
  theme(strip.background = element_blank(),
        plot.subtitle = element_text(hjust = 0.5, face = 'bold')) +
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%b")

mois <- 
  ggplot(filter(soil_data, variable == 'Soil moisture'), aes(x = date)) +
  #geom_line(aes(y = dayMin, color = aspect, linetype = fire), size = 0.5) +
  #geom_line(aes(y = dayMax, color = aspect, linetype = fire), size = 0.5) +
  # geom_ribbon(aes(ymin = dayMin, ymax = dayMax, fill = aspect),
  #             alpha = alphaVals) +
  geom_smooth(aes(y = dayMean, color = aspect, fill = aspect, linetype = fire), se = F, size = 0.85, span = 0.12) +
  #geom_line(aes(y = dayMean, color = aspect, fill = aspect, linetype = fire), size = 0.85) +
  
  scale_fill_manual('Aspect', values = colVals, guide = F) +
  scale_color_manual('Aspect', values = colVals, guide = F) +
  scale_linetype('Fire return interval', labels = c('Short', 'Long'), guide = F) +
  #facet_grid(variable~aspect, scales = 'free') +
  theme_bw(base_size = 14) +
  labs(x = '', y = bquote('Mean soil moisture ('*~m^3*''*~m^-3*')')) +
  theme(strip.background = element_blank(),
        plot.subtitle = element_text(hjust = 0.5, face = 'bold')) +
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%b")

library(cowplot)
plot_grid(mois, temp, nrow = 1, rel_widths = c(0.8,1))


soil_time_plot





