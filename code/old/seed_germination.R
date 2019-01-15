library(tidyverse)


data_path <- '/Users/tylerhoecker/GitHub/seed_addition_GYE/data'

data <- read_csv(paste0(data_path,"/seed_germination_data.csv"))

data$date <- as.Date(data$date, "%m/%d/%y")

data %>% 
  group_by(species, date) %>% 
  summarise(low = quantile(count, 0.10)/20,
            mean = quantile(count, 0.50)/20,
            high = quantile(count, 0.90)/20) %>% 
  ggplot() +
  # geom_ribbon(aes(x = date, ymin = low, ymax = high, fill = species), 
  #             alpha = 0.5) +
  geom_line(aes(x = date, y = mean, color = species), size = 1) +
  geom_point(aes(x = date, y = mean, fill = species), size = 2, shape = 21) +
  scale_color_manual(values = c('red4','dodgerblue3')) +
  scale_fill_manual(values = c('red4','dodgerblue3')) +
  
  #scale_x_date(limits = as.Date(c('2017-10-06', '2017-11-17'))) +
  labs(x = 'Date', y = 'Proportion of seeds germinated') +
  theme_bw(base_size = 14)
