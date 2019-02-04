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
  summarise(value = mean(value)) %>%
  # Concert soil moisture (VMC) to %
  mutate(value = if_else(variable == 'mois', value*100, value))  

sample_days <- alive %>% 
  group_by(fire, aspect, day) %>% 
  summarise(end = unique(day),
            start = end - 13) %>% 
  select(fire, aspect, start, end)

alive_sub <- alive %>% 
  filter(fire == 'Berry-Glade',
         aspect == 'North',
         species == 'pico')
soil_sub <- soil_df %>% 
  filter(fire == 'Berry-Glade',
         aspect == 'North')

test <- 
  full_join(sample_days, soil_sub) %>%
  filter(day >= start & day < end) %>% 
  group_by(fire, aspect, start, ed)



