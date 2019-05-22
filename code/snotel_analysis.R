library(lubridate)

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
