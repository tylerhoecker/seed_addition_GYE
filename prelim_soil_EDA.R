library(tidyverse)

dataTime <- '2017-09'



sites <- sub('(.*?_.*?)_.*','\\1',list.files('data'))

soilData <- lapply(paste0('data/',sites,'_',dataTime,'.csv'), read.csv(skip = 3))


read.csv("data/BF17_F_2017-09.csv", skip = 3)
