library(lubridate)
# Import seedling data
source('code/read_seedling_data.R')

# Surival and germination figuring-------------------------------------------
# Germinated
alive <- seedlings %>% 
  filter(variable == 'height', species != 'control') %>% 
  mutate(alive = if_else(is.na(value), 0, 1)) %>% 
  group_by(date, fire, aspect, species, frameID) %>% 
  summarise(alive = sum(alive, na.rm = T) / n()) %>% 
  ungroup() %>% 
  mutate(day = date(date)) %>% 
  select(-date)

# Might need to address some frames that go up...
ggplot(alive) +
  geom_line(aes(x = day, y = alive, group = frameID)) +
  facet_wrap(aspect~fire, scales = 'free')


# Load and prep soil moisture and temperature predictor variables
source('code/read_soil_data.R') # creates `soil_df` (complete) 

# Decide on a time period of interest:
start_time <- as.POSIXct(c("2018-06-01 00:00:00"))
end_time <- as.POSIXct(c("2018-11-01 00:00:00"))

soil_df <- soil_df %>% 
  # no missing values 
  filter(!is.na(value)) %>% 
  # Just the summer 
  filter(time >= start_time & time <= end_time) %>% 
  # Truncate negative values to 0 [not sure why there are negative values...]
  mutate(value = if_else(value < 0, 0, value)) %>% 
  # Average all 4 ports
  mutate(day = date(time))  %>% 
  group_by(fire, aspect, variable, day) %>%
  summarise(value = mean(value))   

sample_days <- alive %>% 
  group_by(fire, aspect, day) %>% 
  summarise(end = unique(day),
            start = end - 13) %>% 
  select(fire, aspect, start, end)

test <- 
  full_join(sample_days, soil_df) %>%
  filter(day >= start & day < end) %>% 
  group_by(fire, aspect, start, end, variable) %>% 
  summarise(value = mean(value)) %>% 
  mutate(day = end) %>% 
  full_join(alive) %>% 
  filter(!is.na(value)) %>% 
  filter(variable == 'mois')

library(gganimate)

ggplot(test, aes(x = alive, y = value, color = aspect)) +
  geom_point() +
  geom_smooth(color = 'black') +
  transition_states(day) 


# MONDAY
# go back and keep hourly measures, then do means. Calculate cumulative measures too
# then try using all groups, should be the same
# Random effect for site to account for repeat measures...? Or group to site-level then random site.



ggplot(test) +
  geom_point(aes(x = value, y = alive, color = aspect)) +
  facet_grid(~variable)
