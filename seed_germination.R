library(tidyverse)


data_path <- '/Users/tylerhoecker/GitHub/seed_addition_GYE/data'

data <- read_csv(paste0(data_path,"/seed_germination_data.csv"))

data$date <- as.Date(data$date, "%m/%d/%y")

data %>% 
  group_by(species, date) %>% 
  summarise(low = quantile(count, 0.10)/20,
            mean = mean(count)/20,
            high = quantile(count, 0.90)/20) %>% 
  ggplot() +
  geom_ribbon(aes(x = date, ymin = low, ymax = high, fill = species), 
              alpha = 0.5) +
  geom_line(aes(x = date, y = mean, color = species)) +
  labs(x = 'Date', y = 'Proportion of seeds germinated') +
  theme_bw(base_size = 14)
