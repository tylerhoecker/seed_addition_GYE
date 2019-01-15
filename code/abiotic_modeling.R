# Import and manipulate data sources---------------------------------------

# Import soil data
source('code/read_soil_data.R')
soil_sum <- soil_df %>% 
  filter(time >= as.POSIXct(c("2018-06-20 00:00:00")) & time <= as.POSIXct(c("2018-08-20 00:00:00"))) %>% 
  group_by(site, aspect, time, variable) %>%
  summarise(value = mean(value)) %>% 
  spread(variable, value) %>% 
  group_by(site, aspect) %>% 
  summarise_at(vars(mois, temp),
               funs(low = quantile(., 0.05), 
                    med = quantile(., 0.50), 
                    high = quantile(., 0.95))
               , na.rm = T) 

# ATMOS data
source('code/read_atmos_data.R') #creats 'atmos_df'
atmos_sum <- atmos_df %>% 
  filter(time >= as.POSIXct(c("2018-06-20 00:00:00")) & time <= as.POSIXct(c("2018-08-20 00:00:00")),
         aspect != 'Grizz') %>% 
  group_by(site, aspect) %>%
  summarise_at(vars(air_temp:solar),
               funs(low = quantile(., 0.05), 
                     med = quantile(., 0.50), 
                     high = quantile(., 0.95)), na.rm = T) %>% 
  mutate(solar_high = replace(solar_high, solar_high > 900 | solar_high < 840, NA)) 
  

# Import seedling data
source('code/read_seedling_data.R')


# Surival and germination figuring-------------------------------------------
# Germinated
germination <- seedlings %>% 
  filter(variable == 'height', species != 'control') %>% 
  select(site, aspect, species, frameID, cell, value) %>% 
  mutate(germinated = if_else(is.na(value), 0, 1)) %>% 
  unite(frameID, cell, col = 'uniqID', remove = FALSE) %>%
  group_by(site, aspect, species, frameID, uniqID) %>% 
  summarise(germinated = max(germinated)) %>% 
  group_by(site, aspect, species, frameID) #%>% 
  #mutate(problem = if_else(sum(germinated) == 0, 'Yes','No')) %>% 
  #filter(problem == 'No') 
  

# Survived
survival <- seedlings %>%
  filter(variable == 'height', species != 'control') %>% 
  mutate(survived = if_else(date > as.POSIXct("2018-10-01") & value > 0, 1, 0)) %>% 
  select(site, aspect, species, frameID, cell, survived) %>% 
  unite(frameID, cell, col = 'uniqID') %>%
  group_by(site, aspect, species, uniqID) %>% 
  summarise(survived = max(survived, na.rm = T))  
  

establishment_df <- full_join(germination, survival) %>% 
  ungroup()

estb_abio_df <- establishment_df %>% 
  full_join(atmos_sum, by = c('site','aspect')) %>% 
  full_join(soil_sum, by = c('site','aspect')) %>% 
  select(site, aspect, species, frameID, uniqID, germinated, survived,
         air_temp_med, solar_high, solar_med, atms_press_med,
         temp_high, temp_med, temp_low, 
         mois_high, mois_med, mois_low, 
         rel_hum_med, rel_hum_high) 



# Exploratory Analysis ----------------------------------------------------
temp <- estb_abio_df %>%
  gather(variable, value, -site, -aspect, -species, -frameID, -uniqID, -germinated, -survived) 
  
ggplot(temp, aes(factor(survived), y = value, fill=factor(survived))) +
  geom_boxplot() +
  facet_wrap(~variable, scales="free_y", nrow = 3) +
  scale_fill_grey(start = 0.8, end = 0.4) +
  theme_bw(base_size = 10) 

colVals <- c('Flat' = '#009E73','North' = '#0072B2','South' = '#E69F00')

ggplot(filter(estb_abio_df, species == 'pico'), 
       aes(x = mois_med, y = survived)) +
  geom_jitter(aes(fill = aspect), shape = 21, alpha = 0.5, size = 2) +
  scale_fill_manual(values = colVals, name = 'aspect') +
  scale_y_continuous(breaks = c(0,1), labels = c(0,1)) +
  theme_bw(base_size = ) 


# THE MODEL - mixed-effects logisitic regression --------------------------
library(lme4)

m <- glm(germinated ~ mois_med + temp_med + air_temp_med + solar_high + rel_hum_high + 
           as.factor(species), 
           data = as.data.frame(estb_abio_df), family = 'binomial')
summary(m)
confint(m)
exp(coef(m))


m <- glmer(survived ~ mois_med + temp_med + air_temp_med + solar_high + rel_hum_high + 
             (1 | species) + (1 | site), 
           data = as.data.frame(estb_abio_df), family = 'binomial', nAGQ=0)
summary(m)


se <- sqrt(diag(vcov(m)))
# table of estimates with 95% CI
tab <- cbind(Est = fixef(m), LL = fixef(m) - 1.96 * se, UL = fixef(m) + 1.96 *
                se)

exp(tab)





# Mixed effects Poissin regression
model.rand = glmer(count ~ observer*log2photo + (1|flock), family = poisson, nAGQ = 25, data = snowgeese)
# Residuals for mixed effects model
snowgeese$yhat <- predict(model.rand, type = "link")
resid <- residuals(model.rand, type = "deviance")
p <- ggplot(snowgeese, aes(x = yhat, y = resid))
p <- p + geom_point(aes(shape = observer, colour = observer))
p <- p + xlab("Predicted Values") + ylab("Deviance Residuals")
p




### EXTRA
covariates <- germ_abiotic_df %>% 
  ungroup() %>% 
  na.omit() %>% 
  as.data.frame()

GGally::ggpairs(covariates)

ggplot(temp, aes(x = aspect, y = value)) +
  geom_jitter(alpha = .1) +
  geom_violin(aes(fill = aspect), alpha = .75) +
  scale_fill_manual(values = colVals, name = 'aspect') +
  facet_wrap(~variable, scales = 'free')


