# Import and manipulate data sources---------------------------------------

# Import soil data
source('code/read_soil_data.R')
soil_sum <- soil_df %>% 
  filter(time >= as.POSIXct(c("2018-06-01 00:00:00")) & time <= as.POSIXct(c("2018-09-01 00:00:00"))) %>% 
  group_by(fire, aspect, time, variable) %>%
  summarise(value = mean(value)) %>% 
  mutate(value = if_else(variable == 'mois', round(value, 2), round(value))) %>% 
  group_by(fire, aspect, variable, value) %>% 
  summarise(hours = n()) %>% 
  filter(!is.na(value))

# ggplot(filter(soil_sum, variable == 'mois')) +
#   geom_histogram(aes(x = value)) +
#   facet_grid(variable~aspect) +
#   theme_bw()

ggplot(soil_sum, aes(x = hours, y = value)) +
  geom_point() +
  geom_smooth(se = F) +
  facet_grid(variable~aspect, scales = 'free')

temp <- filter(soil_sum, aspect == 'North', variable == 'mois') %>% 
  ungroup() %>% 
  select(y = hours, x = value)

# Log-log exponential model
exp_model <- lm(log(y) ~ log(x), data = temp)
summary(exp_model)

newX <- seq(min(temp$x),max(temp$x),0.001) 
prediction <- predict(exp_model, list(x = newX), se.fit = T)
newY <- prediction$fit
# Plot
#par(mfrow = c(1,2))
plot(temp$x, temp$y, pch=16, xlab = 'soil moisture', ylab = 'total hours', main = 'North',
     ylim = c(0,500), xlim = c(0,0.30))
lines(newX, exp(newY)-1, lwd=2, col = "red", xlab = "Time (s)", ylab = "Counts")






# # ATMOS data
# source('code/read_atmos_data.R') #creats 'atmos_df'
# atmos_sum <- atmos_df %>% 
#   filter(time >= as.POSIXct(c("2018-06-20 00:00:00")) & time <= as.POSIXct(c("2018-08-20 00:00:00")),
#          aspect != 'Grizz') %>% 
#   group_by(fire, aspect) %>%
#   summarise_at(vars(air_temp:solar),
#                funs(low = quantile(., 0.05), 
#                     med = quantile(., 0.50), 
#                     high = quantile(., 0.95)), na.rm = T) %>% 
#   mutate(solar_high = replace(solar_high, solar_high > 900 | solar_high < 840, NA)) 


