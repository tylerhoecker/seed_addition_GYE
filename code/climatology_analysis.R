library(lubridate)
library(tidyverse)
library(cowplot)


climate_recent_raw <- read_csv('data/climate_2018_2019.csv')
# https://www.ncdc.noaa.gov/cdo-web/search

climate_normals_raw <- read_csv('data/climate_normals.csv')

climate_2018 <- climate_recent_raw %>% 
  separate(DATE, into = c('YEAR','DATE')) %>% 
  mutate(DATE = as.integer(DATE)) %>% 
  dplyr::select(name = NAME, month = DATE, year = YEAR, temp = TAVG, precip = PRCP) 
  

climate_normals  <- climate_normals_raw %>% 
  dplyr::select(name = NAME, month = DATE, temp = `MLY-TAVG-NORMAL`, temp_sd = `MLY-TAVG-STDDEV`,
         precip = `MLY-PRCP-NORMAL`, precip_25 = `MLY-PRCP-25PCTL`, precip_75 = `MLY-PRCP-75PCTL`) %>% 
#   summarise_at(vars(temp,precip), list(mean = ~mean(., na.rm = T), 
#                                     sd = ~sd(., na.rm = T))) %>% 
  mutate(year = '1980-2010') %>% 
  filter(name != 'Tower Falls')

climatology <- full_join(climate_2018, climate_normals) 


temp <- 
  ggplot(climatology, aes(x = month, y = temp)) +
    geom_col(aes(fill = year), position = 'dodge') +
    geom_errorbar(aes(ymin = temp - temp_sd/sqrt(26), ymax = temp + temp_sd/sqrt(26), group = year),
                  position = position_dodge2(width = 0.2, padding = 0.8)) +
    scale_fill_brewer('Year', palette = 'Dark2', guide = F) +
    scale_x_continuous(breaks = c(6,7,8,9), labels = c('June','July','Aug.','Sept.')) +
    theme_bw(base_size = 12) +
    facet_wrap(~name) +
    labs(y = 'Mean summer montly temperature (deg. C)') +
    theme(axis.title.x = element_blank(),
          strip.background = element_blank())

precip <- 
  ggplot(climatology, aes(x = month, y = precip)) +
  geom_col(aes(fill = year), position = 'dodge') +
  geom_errorbar(aes(ymin = precip_25, ymax = precip_75, group = year),
                position = position_dodge2(width = 0.2, padding = 0.8)) +
  scale_fill_brewer('Year', palette = 'Dark2') +
  scale_x_continuous(breaks = c(6,7,8,9), labels = c('June','July','Aug.','Sept.')) +
  theme_bw(base_size = 12) +
  facet_wrap(~name) +
  labs(y = 'Mean summer monthly precipitation (mm)') +
  theme(#legend.position = c(0.91,0.91),
        legend.background = element_blank(),
        axis.title.x = element_blank(),
        strip.background = element_blank())

plot_grid(temp, precip, rel_widths = c(1, 1.3))




snotel <- read_csv('data/snotel.csv') %>% 
  mutate(date = parse_date(date, format = '%b-%y')) %>% 
  mutate(SWE = SWE/10) #%>% 
  #filter(month(date) %in% c(2,3,4,5)) %>% 
  #group_by(date = year(date)) %>% 
  #summarise(SWE = mean(SWE)) %>% 
  #mutate(date = parse_date(as.character(date), format = '%Y'))

month_means <- snotel %>% 
  group_by(station_name, month(date)) %>% 
  summarise(mean = mean(SWE, na.rm = T)) %>% 
  filter(`month(date)` == 5)

ggplot(snotel, aes(x = date, y = SWE)) +
  geom_hline(data = month_means, aes(yintercept = mean), linetype = 'dashed') +
  geom_line(size = 1) +
  #geom_line(aes(x = date, y = med_SWE), color = 'blue', size = 1) +
  geom_point(data = filter(snotel, month(date) == 05), aes(x = date, y = SWE), shape = 21, fill = 'grey50', size = 2.5) +
  geom_text_repel(data = filter(snotel, month(date) == 05 & year(date) == 2018), 
            aes(x = date, y = SWE, label = 'May 2018'), nudge_y = 20, nudge_x = 10) +
  facet_wrap(~station_name) +
  theme_bw(base_size = 14) +
  labs(x = 'Year', y = 'Snow Water Equivalent (cm)')



climate_2018_raw <- read_csv('data/climate_gy_2018.csv')

#climate_2018 <- climate_2018_raw %>% 
#   summarise_at(vars(temp,precip), list(mean = ~mean(., na.rm = T), 
#                                      sd = ~sd(., na.rm = T))) %>% 
separate(DATE, into = c('YEAR','DATE')) %>% 
  mutate(DATE = as.integer(DATE)) %>% 
  dplyr::select(name = NAME, date = DATE, temp = TAVG, precip = PRCP) %>% 
  mutate(version = '2018') 

climate_normals_raw <- read_csv('data/climate_normals_gy.csv')

climate_2018 <- climate_recent_raw %>% 
  #   summarise_at(vars(temp,precip), list(mean = ~mean(., na.rm = T), 
  #                                      sd = ~sd(., na.rm = T))) %>% 
  separate(DATE, into = c('YEAR','DATE')) %>% 
  mutate(DATE = as.integer(DATE)) %>% 
  dplyr::select(name = NAME, month = DATE, year = YEAR, temp = TAVG, precip = PRCP) 




climate_normals  <- climate_normals_raw %>% 
  dplyr::select(name = NAME, month = DATE, temp = `MLY-TAVG-NORMAL`, temp_sd = `MLY-TAVG-STDDEV`,
                precip = `MLY-PRCP-NORMAL`, precip_25 = `MLY-PRCP-25PCTL`, precip_75 = `MLY-PRCP-75PCTL`) %>% 
  #   summarise_at(vars(temp,precip), list(mean = ~mean(., na.rm = T), 
  #                                     sd = ~sd(., na.rm = T))) %>% 
  mutate(year = '1980-2010') %>% 
  filter(name != 'Tower Falls')





