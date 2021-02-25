library(cowplot)
# This function reads the xls files automatically produced by the METER Em50 data logger. 
# The script assumes that "Port 4" is an empty port
# Ports 1-3 are inside seed-addition frames, port 5 is outside. Ports are renamed 1-4, with 4 becoming "empty" and then removed.
# This does not work with the "Berry-Old" sites because their ports were arranged differently.

#---------------------------------------------------------------------------
# Edited on 2/8 to use updated version of soil_df
# Decide on a time period of interest:
start_time <- as.POSIXct(c("2018-06-01 00:00:00"))
end_time <- as.POSIXct(c("2018-10-01 00:00:00"))


# Load soil data for sure in either method. Produces `soil_df`
source('code/read_soil_data.R')

soil_data <- soil_df %>% 
  #mutate(time = parse_date_time(.$time, orders = 'mdy hs')) %>%
  separate(time, into = c('date','hour'), sep = " ") %>%
  mutate(date = as.Date(date),
         #value = if_else(variable == 'mois', value / 100, value),
         variable = if_else(variable == 'mois','Soil moisture','Soil temperature')) %>%
  # mutate(aspect = as.factor(aspect),
  #        measType = if_else(measType == 'mois',"Moisture","Temperature")) %>%
  group_by(fire, aspect, date, variable) %>% # Change site for aspect
  #drop_na() %>% 
  summarize(dayMin = min(value, na.rm = T),
            dayMean = mean(value, na.rm = T),
            dayMax = max(value, na.rm = T)) %>% 
  mutate(dayMin = ifelse(is.infinite(dayMin), NA, dayMin),
         dayMax = ifelse(is.infinite(dayMax), NA, dayMax)) 
# %>% 
#   group_by(variable, aspect) %>% 
#   summarise_if(is.numeric, mean, na.rm = T)


colVals <- rev(c('South' = '#E69F00', 'North' = '#0072B2','Flat' = '#009E73'))
#legLabs <- rev(c('South','North','Flat'))
alphaVals <- 0.4
sizeVals <- 1

moisture_2018 <- 
  ggplot(filter(soil_data, variable == 'Soil moisture')) +
  # geom_ribbon(aes(x = date, ymin = dayMin, ymax = dayMax, fill = aspect),
  #             alpha = alphaVals) +
  geom_line(aes(x = date, y = dayMean, color = aspect), size = 0.85) +
  scale_fill_manual('Aspect', values = colVals) +
  scale_color_manual('Aspect', values = colVals) +
  facet_grid(~fire) +
  theme_bw(base_size = 14) +
  labs(x = '', y = bquote('Mean soil moisture ('*~m^3*''*~m^-3*')')) + #,'*~degree *C*'
  theme(strip.background = element_blank(),
        plot.subtitle = element_text(hjust = 0.5, face = 'bold')) +
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%b") +
  theme(panel.grid.minor = element_blank())

temperature_2018 <- 
  ggplot(filter(soil_data, variable == 'Soil temperature')) +
  # geom_ribbon(aes(x = date, ymin = dayMin, ymax = dayMax, fill = aspect),
  #             alpha = alphaVals) +
  geom_line(aes(x = date, y = dayMean, color = aspect), size = 0.85) +
  scale_fill_manual('Aspect', values = colVals) +
  scale_color_manual('Aspect', values = colVals) +
  facet_grid(~fire) +
  theme_bw(base_size = 14) +
  labs(x = '', y = bquote('Mean soil temperature ('*~degree *C*')')) + #,'*~degree *C*'
  theme(strip.background = element_blank(),
        plot.subtitle = element_text(hjust = 0.5, face = 'bold')) +
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%b") +
  theme(panel.grid.minor = element_blank())

# Rerun again with 19 data...

plot_grid(moisture_2018, moisture_2019, temperature_2018, temperature_2019, nrow = 4)

##------
# soilSouth <- filter(soil_data, aspect == 'South')
# soilFlat <- filter(soil_data, aspect == 'Flat')
# soilNorth <- filter(soil_data, aspect == 'North')
# 
# seriesPlot <- 
#   ggplot() +
#   #South
#   geom_ribbon(data = soilSouth,
#               aes(x = date, ymin = dayMin, ymax = dayMax, fill = aspect),
#               alpha = alphaVals) +
#   geom_line(data = soilSouth,
#             aes(x = date, y = dayMean, color = aspect), 
#             size = sizeVals) +
#   #Flat
#   geom_ribbon(data = soilFlat,
#               aes(x = date, ymin = dayMin, ymax = dayMax, fill = aspect),
#               alpha = alphaVals) +
#   geom_line(data = soilFlat,
#             aes(x = date, y = dayMean, color = aspect), 
#             size = sizeVals) +
#   #North
#   geom_ribbon(data = soilNorth,
#               aes(x = date, ymin = dayMin, ymax = dayMax, fill = aspect),
#               alpha = alphaVals) +
#   geom_line(data = soilNorth,
#             aes(x = date, y = dayMean, color = aspect), 
#             size = sizeVals) +
#   
#   facet_wrap(~fire+variable, scales = 'free_y', ncol = 2) +
#   scale_fill_manual(values = colVals, name = 'Aspect', labels = legLabs) +
#   scale_color_manual(values = colVals, name = 'Aspect', labels = legLabs) +
#   labs(x = 'Day', y = 'Daily mean and range of measurement\n (mm/m3, deg. C)') +
#   theme_bw() +
#   theme(legend.position="none")
# 
# temphistPlot <- 
#   ggplot(filter(soilData, measType == 'Temperature')) +
#   geom_histogram(aes(x = dayMean, fill = aspect), 
#                  bins = 50, position = 'identity', alpha = alphaVals) +
#   geom_histogram(aes(x = dayMean, color = aspect), 
#                  bins = 50, fill = 'transparent', position = 'identity') +
#   xlim(0,50) +
#   coord_flip() +
#   facet_wrap(~measType, scales = 'free_y') +
#   scale_fill_manual(values = colVals, name = 'Aspect', labels = legLabs) +
#   scale_color_manual(values = colVals, name = 'Aspect', labels = legLabs) +
#   labs(y = 'Count') +
#   theme_th() +
#   theme(axis.title.y = element_blank(),
#         legend.position = c(0.75,0.8),
#         legend.background = element_rect('transparent'))
# 
# moisthistPlot <- 
#   ggplot(filter(soilData, measType == 'Moisture')) +
#   geom_histogram(aes(x = dayMean, fill = aspect), 
#                  bins = 50, position = 'identity', alpha = alphaVals) +
#   geom_histogram(aes(x = dayMean, color = aspect), 
#                  bins = 50, fill = 'transparent', position = 'identity') +
#   xlim(0,0.4) +
#   coord_flip() +
#   facet_wrap(~measType, scales = 'free_y') +
#   scale_fill_manual(values = colVals, name = 'Aspect', labels = legLabs) +
#   scale_color_manual(values = colVals, name = 'Aspect', labels = legLabs) +
#   labs(y = 'Count') +
#   theme_th() +
#   theme(axis.title.y = element_blank(),
#         legend.position="none")
# 
# plot_grid(seriesPlot,moisthistPlot,temphistPlot, 
#           ncol = 3, rel_widths = c(1,0.25,0.25))
# 
# 
# # ---------
# library(tidyverse)
# library(lubridate)
# 
# read_soil <- function(file_name){
#   data <- readxl::read_excel(file_name,
#                              skip = 3, 
#                              na = '#N/A',
#                              col_types = c('date', rep('numeric',9)),
#                              col_names = c('time',	
#                                            'mois_1', 'temp_1',
#                                            'mois_2', 'temp_2',
#                                            'mois_3', 'temp_3',
#                                            'empty',
#                                            'mois_4',	'temp_4')) %>% 
#     select(-empty)
#   return(data)
# }
# 
# raw_files <- list.files(file.path('data','soil_sensors'),full.names = TRUE)
# 
# soil_df <- map(raw_files, read_soil) %>% 
#   `names<-` (str_extract(raw_files, paste(file.path('data','soil_sensors'),".*[[:space:]]"))) %>%
#   bind_rows(.id = 'site') 
# 
# 
# 
# 
# soil_data <- soil_df %>% 
#   #mutate(time = parse_date_time(.$time, orders = 'mdy hs')) %>%
#   separate(site, into = c('site','aspect'), sep = 2) %>%
#   separate(time, into = c('date','hour'), sep = " ") %>%
#   mutate(date = as.POSIXct(date)) %>%
#   gather(key, value, -site,-aspect,-date,-hour) %>%
#   separate(key, into = c('variable','port'), sep = '_') %>%
#   # mutate(aspect = as.factor(aspect),
#   #        measType = if_else(measType == 'mois',"Moisture","Temperature")) %>%
#   select(site, aspect, date, hour, port, variable, value) %>%
#   group_by(site, aspect, date, variable) %>% # Change site for aspect
#   #drop_na() %>% 
#   summarize(dayMin = min(value, na.rm = T),
#             dayMean = mean(value, na.rm = T),
#             dayMax = max(value, na.rm = T)) 
# 
# 
# 
# colVals <- rev(c('South' = '#E69F00', 'North' = '#0072B2','Flat' = '#009E73'))
# #legLabs <- rev(c('South','North','Flat'))
# alphaVals <- 0.4
# sizeVals <- 1
# 
# soild_data_2018 = soil_data %>% 
#   filter(year(date) == '2018', month(date) > 6)
# 
# 
# ggplot(soil_data) +
#   geom_ribbon(aes(x = date, ymin = dayMin, ymax = dayMax, fill = aspect), alpha = alphaVals) +
#   geom_line(aes(x = date, y = dayMean, color = aspect), size = sizeVals) +
#   facet_wrap(~site+variable, scales = 'free_y', ncol = 2) +
#   scale_fill_manual(values = colVals, name = 'Aspect') +
#   scale_color_manual(values = colVals, name = 'Aspect') +
#   labs(x = 'Day', y = 'Daily mean and range of measurement\n (mm/m3, deg. C)') +
#   theme_bw() +
#   theme(legend.position="none")
# 
# 
# # Ridgeline plot!
# soil_data_summer <- soil_df %>% 
#   separate(site, into = c('site','aspect'), sep = 2) %>%
#   separate(time, into = c('date','hour'), sep = " ") %>%
#   mutate(date = as.POSIXct(date)) %>%
#   gather(key, value, -site,-aspect,-date,-hour) %>%
#   separate(key, into = c('variable','port'), sep = '_') %>%
#   select(site, aspect, date, hour, port, variable, value) %>%
#   group_by(site, aspect, month(date), variable) %>% 
#   filter(month(date) > 5, month(date) < 10) 
# 
# 
# 
# 
# ggplot(soil_data_summer) +
#   ggridges::geom_density_ridges(aes(x = value, y = site, fill = aspect),
#                                 alpha = alphaVals) +
#   scale_fill_manual(values = colVals, name = 'Aspect', labels = legLabs) +
#   scale_color_manual(values = colVals, name = 'Aspect', labels = legLabs) +
#   facet_wrap(~variable, scales = 'free') +
#   theme_bw(base_size = 14)
# 
# 
# # By Site
# soilData <- map(paste0('data/soil_sensors/',sites,'_',dataTime,'.csv'), read_csv) %>% 
#   `names<-` (sites) %>%
#   bind_rows(.id = 'site') %>%
#   mutate(time = parse_date_time(.$time, orders = 'mdy hs')) %>%
#   separate(site, into = c('site','aspect'), sep = '_') %>%
#   separate(time, into = c('date','hour'), sep = " ") %>%
#   mutate(date = as.POSIXct(date)) %>%
#   gather(key, value, 5:ncol(.)) %>%
#   mutate(aspect = as.factor(aspect),
#          measType = as.factor(sub("_.*","",.$key)),
#          port = sub(".*_","",.$key)) %>%
#   mutate(measType = if_else(measType == 'mois',"Moisture","Temperature")) %>%
#   select(site,aspect,date,hour,port,measType,value) %>%
#   group_by(site,aspect,date,measType) %>% # Change site for aspect
#   summarize(dayMin = min(value),
#             dayMean = mean(value),
#             dayMax = max(value)) 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ggplot(soild_data_2018) +
#   geom_ribbon(aes(x = date, ymin = dayMin, ymax = dayMax, fill = aspect), alpha = alphaVals) +
#   geom_line(aes(x = date, y = dayMean, color = aspect), size = sizeVals) +
#   facet_wrap(~site+variable, scales = 'free_y', ncol = 2) +
#   scale_fill_manual(values = colVals, name = 'Aspect', labels = legLabs) +
#   scale_color_manual(values = colVals, name = 'Aspect', labels = legLabs) +
#   labs(x = 'Day', y = 'Daily mean and range of measurement\n (mm/m3, deg. C)') +
#   theme_bw() +
#   theme(legend.position="none")
# 
# 
# 
# 
# 
# 
# 
# 
# 
