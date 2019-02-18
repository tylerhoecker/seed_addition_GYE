library(lubridate)
# Import seedling data
source('code/read_seedling_data.R')

# Surival and germination figuring-------------------------------------------
# Germinated
alive <- seedlings %>% 
  filter(variable == 'height', species != 'control') %>% 
  mutate(alive = if_else(is.na(value), 0, 1)) %>% 
  group_by(date, fire, aspect, species, frameID) %>% 
  summarise(alive = sum(alive, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(day = date(date)) %>% 
  dplyr::select(-date)

# Might need to address some frames that go up...
ggplot(alive) +
  geom_line(aes(x = day, y = alive, group = frameID)) +
  facet_wrap(aspect~fire, scales = 'free')


# Load and prep soil moisture and temperature predictor variables -------------
source('code/read_soil_data.R') # creates `soil_df` (complete) 

# Decide on a time period of interest:
start_time <- as.POSIXct(c("2018-06-01 00:00:00"))
end_time <- as.POSIXct(c("2018-11-01 00:00:00"))

soil_sum <- soil_df %>% 
  # no missing values 
  filter(!is.na(value)) %>% 
  # Just the period of interest 
  filter(time >= start_time & time <= end_time) %>% 
  # Grouping column by day 
  mutate(day = date(time))  %>% 
  group_by(fire, aspect, variable, day) %>%
  summarise(mean_val = mean(value),
            min_val = min(value),
            max_val = max(value))  


# Calculate two-week metrics ---------------------------------------------------
sample_days <- alive %>% 
  group_by(fire, aspect, day) %>% 
  summarise(end = unique(day),
            start = end - 13) %>% 
  dplyr::select(fire, aspect, start, end)

repeat_df <- 
  full_join(sample_days, soil_sum) %>%
  filter(day >= start & day < end) %>% 
  group_by(fire, aspect, start, end, variable) %>% 
  summarise(mean_val = mean(mean_val),
            min_val = min(min_val),
            max_val = max(max_val)) %>% 
  full_join(alive, by = c('fire', 'aspect', 'end' = 'day')) %>% 
  filter(!is.na(variable)) %>%  
  gather(stat, value, mean_val, min_val, max_val) %>% 
  unite(variable, variable, stat, sep = '_') %>% 
  spread(variable, value) %>% 
  mutate(species = as.factor(species))
  

# MONDAY
# go back and keep hourly measures, then do means. Calculate cumulative measures too
# then try using all groups, should be the same
# Random effect for site to account for repeat measures...? Or group to site-level then random site.

# MODELING ---------------------------------------------------------------------
repeat_model <- 
  glmer(formula = alive ~ species*(mois_mean_val + mois_mean_val + mois_max_val + 
                                  temp_mean_val + temp_min_val + temp_max_val) + 
                                    (1|fire) + (1|frameID),
        family = poisson(link = 'log'), 
        data = repeat_df,
        control=glmerControl(optimizer="bobyqa"),
        na.action="na.fail")

glmm_dredge <- dredge(repeat_model, m.lim = c(0,3))

top_model <- get.models(glmm_dredge, subset = 1)[[1]]
summary(top_model)
plot(top_model)

# Fixed Effects of top model
plot(allEffects(top_model, residuals = TRUE))


ggplot(test) +
  geom_point(aes(x = value, y = alive, color = aspect)) +
  facet_grid(~variable)
