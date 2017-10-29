library(tidyverse)
library(lubridate)
library(cowplot)

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

colVals <- rev(c('S' = '#E69F00', 'F' = '#009E73', 'N' = '#0072B2'))
legLabs <- rev(c('Flat','North','South'))
alphaVals <- 0.4
sizeVals <- 1

relevel(soilData$aspect, "S")

seriesPlot <- 
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
  labs(x = 'Day', y = 'Daily mean and range of measurement\n (mm/m3, deg. C)') +
  theme_th() +
  theme(legend.position="none")

temphistPlot <- 
ggplot(filter(soilData, measType == 'Temperature')) +
  geom_histogram(aes(x = dayMean, fill = aspect), 
                 bins = 50, position = 'identity', alpha = alphaVals) +
  geom_histogram(aes(x = dayMean, color = aspect), 
                 bins = 50, fill = 'transparent', position = 'identity') +
  xlim(0,50) +
  coord_flip() +
  facet_wrap(~measType, scales = 'free_y') +
  scale_fill_manual(values = colVals, name = 'Aspect', labels = legLabs) +
  scale_color_manual(values = colVals, name = 'Aspect', labels = legLabs) +
  labs(y = 'Count') +
  theme_th() +
  theme(axis.title.y = element_blank(),
        legend.position = c(0.75,0.8),
        legend.background = element_rect('transparent'))

moisthistPlot <- 
  ggplot(filter(soilData, measType == 'Moisture')) +
  geom_histogram(aes(x = dayMean, fill = aspect), 
                 bins = 50, position = 'identity', alpha = alphaVals) +
  geom_histogram(aes(x = dayMean, color = aspect), 
                 bins = 50, fill = 'transparent', position = 'identity') +
  xlim(0,0.4) +
  coord_flip() +
  facet_wrap(~measType, scales = 'free_y') +
  scale_fill_manual(values = colVals, name = 'Aspect', labels = legLabs) +
  scale_color_manual(values = colVals, name = 'Aspect', labels = legLabs) +
  labs(y = 'Count') +
  theme_th() +
  theme(axis.title.y = element_blank(),
        legend.position="none")
  
plot_grid(seriesPlot,moisthistPlot,temphistPlot, 
          ncol = 3, rel_widths = c(1,0.25,0.25))
 