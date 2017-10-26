library(tidyverse)
library(lubridate)

# Define the month and year that the data were downloaded
dataTime <- '2017-09'

sites <- sub('(.*?_.*?)_.*','\\1',list.files('data'))

soilData <- map(paste0('data/',sites,'_',dataTime,'.csv'), read_csv) %>% 
  `names<-` (sites) %>%
  bind_rows(.id = 'site') %>%
  mutate(time = parse_date_time(.$time, orders = 'mdy hs')) %>%
  separate(site, into = c('site','aspect'), sep = '_') %>%
  separate(time, into = c('date','hour'), sep = " ") %>%
  mutate(date = as.POSIXct(date)) %>%
  gather(key, value, 5:ncol(.)) %>%
  mutate(aspect = as.factor(aspect),
         measType = as.factor(sub("_.*","",.$key)),
         port = sub(".*_","",.$key)) %>%
  mutate(measType = if_else(measType == 'mois',"Moisture","Temperature")) %>%
  select(site,aspect,date,hour,port,measType,value) %>%
  group_by(aspect,date,measType) %>%
  summarize(dayMin = min(value),
         dayMean = mean(value),
         dayMax = max(value)) 

soilSouth <- filter(soilData, aspect == 'S')
soilFlat <- filter(soilData, aspect == 'F')
soilNorth <- filter(soilData, aspect == 'N')

colVals <- c('S' = '#E69F00', 'F' = '#009E73', 'N' = '#0072B2')
legLabs <- c('South','Flat','North')
alphaVals <- 0.4
sizeVals <- 1

ggplot() +
  #South
  geom_ribbon(data = soilSouth,
              aes(x = date, ymin = dayMin, ymax = dayMax, fill = aspect),
              alpha = alphaVals) +
  geom_line(data = soilSouth,
            aes(x = date, y = dayMean, color = aspect), 
            size = sizeVals) +
  #Flat
  geom_ribbon(data = soilFlat,
              aes(x = date, ymin = dayMin, ymax = dayMax, fill = aspect),
              alpha = alphaVals) +
  geom_line(data = soilFlat,
            aes(x = date, y = dayMean, color = aspect), 
            size = sizeVals) +
  #North
  geom_ribbon(data = soilNorth,
              aes(x = date, ymin = dayMin, ymax = dayMax, fill = aspect),
              alpha = alphaVals) +
  geom_line(data = soilNorth,
            aes(x = date, y = dayMean, color = aspect), 
            size = sizeVals) +
  
  facet_wrap(~measType, scales = 'free_y') +
  scale_fill_manual(values = colVals, name = 'Aspect', labels = legLabs) +
  scale_color_manual(values = colVals, name = 'Aspect', labels = legLabs) +
  labs(x = 'Day', y = 'Daily mean and range of measurement')
  theme_bw(base_size = 12)








#######=------------------




ggplot() +
  geom_ribbon(data = soilSouth,
              aes(x = date, ymin = dayMin, ymax = dayMax, fill = aspect),
              fill = fillColors[1], alpha = 0.5) +
  geom_line(data = soilSouth,
            aes(x = date, y = dayMean, color = aspect), 
            color = fillColors[1], size = 1) +
  # geom_ribbon(data = soilFlat,
  #             aes(x = date, ymin = dayMin, ymax = dayMax, fill = aspect),
  #             fill = fillColors[2], alpha = 0.5) +
  # geom_line(data = soilFlat,
  #           aes(x = date, y = dayMean, color = aspect), 
  #           color = fillColors[2], size = 2) +
  geom_ribbon(data = soilNorth,
              aes(x = date, ymin = dayMin, ymax = dayMax, fill = aspect),
              fill = fillColors[3], alpha = 0.5) +
  geom_line(data = soilNorth,
            aes(x = date, y = dayMean, color = aspect), 
            color = fillColors[3], size = 1) +
  facet_wrap(~measType, scales = 'free_y') +
  #scale_fill_manual('S' = 'red', 'N' = 'blue', 'F' = 'green') +
  #scale_color_brewer(palette = 'Set1')
  theme_bw(base_size = 12)


ggplot(soilData, aes(x = date, y = dayMean)) +
  geom_line(aes(color = aspect)) +
  facet_wrap(~measType, scales = 'free_y')




grepl('_.*',soilData$site)


filter(soilData, sub(".*_","",soilData$key))
ifelse(grepl("mois_.*",key),'mois','temp')