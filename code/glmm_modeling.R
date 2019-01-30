# Over-arching Questions: 
# What is the right way to decide which time period to use for germination, and which to use for suvrival, establishment?
# Should separate models be estimated for each species, or should there be a parameter for species?
# Should there be two separate models, germination and survival, or one, establishment? Two makes a bit more sense ecologically, but one makes more sense practically.
# After extensively examing ECDF, settled on q50 for all, which is just median... 
# explored options for curve-fitting and AUC for cumulative stress, but not clear
# option emerged. Is median fine for this simple non-temporal model?
#-------------------------------------------------------------------------------

# Decide on a time period of interest:
start_time <- as.POSIXct(c("2018-06-01 00:00:00"))
end_time <- as.POSIXct(c("2018-10-01 00:00:00"))

# Load and prep soil moisture and temperature predictor variables
# Calculates Q50 and Q75 and minimum for moisture, Q50 and Q75 and maximum for temperature
source('code/soil_data_prep.R') # creates `soil_df` (complete) and `soil_preds` (summarized)

# Load and prep meteorological data (air temperature and VPD)
#source('code/atmos_data_prep.R') # creates `atmos_df` (complete) and `atmos_preds` (summarized)

# Run seedling import/summary scripts. Creates `germination`, `final`, `proprotions`, `seed_per`.
source('code/read_summarize_seedling.R')

# Read in topographic indices from CSV (done using built-in GDAL DEM tools in QGIS3: https://www.gdal.org/gdaldem.html)
dem_idx <- read_csv('data/dem_indices.csv') %>% 
  # Convert aspect to continuous measure
  mutate(dev_north = if_else(aspect_deg > 180, abs(aspect_deg - 360), aspect_deg))

# Start with a dataframe of seedling data and soil moisture and temperature
complete_df <- proportions %>% 
  full_join(., soil_preds, by = c('fire','aspect')) %>% 
  #full_join(., atmos_preds) %>% # Comment out for germination, because no ATMOS data that early...
  full_join(., dem_idx) %>% 
  filter(version == 'original') %>% #,species == 'pico'
  rename(proportion = value) %>% 
  # Dealing only with 'establishment'
  filter(period == 'establishment') %>% 
  unite(site, fire, aspect, remove = F) %>% 
  ungroup() %>% 
  modify_at(c('site', 'fire', 'aspect'), as.factor) 

# Exploratory Analysis ----------------------------------------------------
colVals <- c('Flat' = '#009E73','North' = '#0072B2','South' = '#E69F00')


# Linear models
library(broom)

# Look at all possible variables
complete_df %>%
  mutate(count = as.integer(proportion*50)) %>% 
  select(-c(utm_zone, easting, northing, elev, aspect_deg)) %>% 
  gather(., variable, value, -(site:proportion),-count) %>% 
  
  ggplot(aes(x = value, y = count)) +
  geom_point(aes(color = aspect)) +
  geom_smooth(method = 'lm', color = 'black') +
  scale_color_manual(values = colVals) +
  facet_wrap(~variable, scales = 'free') +
  theme_bw(base_size = 12)

# Just plot the ones I will use moving forward
complete_df %>%
  mutate(count = as.integer(proportion*50)) %>% 
  select(-c(utm_zone, easting, northing, elev)) %>% 
  gather(., variable, value, -(site:proportion),-count) %>% 
  ggplot(aes(x = value, y = count)) +
  geom_point(aes(color = aspect)) +
  geom_smooth(method = 'lm', color = 'black') +
  scale_color_manual(values = colVals) +
  facet_wrap(~variable, scales = 'free') +
  theme_bw(base_size = 12)

# Make a table that summarizes the linear models
results_1 <- complete_df %>% 
  mutate(count = as.integer(proportion*50)) %>% 
  select(-c(utm_zone, easting, northing)) %>% 
  gather(., variable, value, -(site:proportion),-count) %>%
  group_by(variable) %>% 
  do(tidy(lm(count ~ value, data = .))) %>% # Glance helpful for fit statistics
  filter(term == 'value') %>% 
  arrange(p.value) %>% 
  select(-term) %>% 
  mutate_if(is.numeric, round, 2)

results_2 <- complete_df %>% 
  mutate(count = as.integer(proportion*50)) %>% 
  select(-c(utm_zone, easting, northing)) %>% 
  gather(., variable, value, -(site:proportion),-count) %>%
  group_by(variable) %>% 
  do(glance(lm(count ~ value, data = .))) %>% # Glance helpful for fit statistics
  select(r.squared, adj.r.squared, sigma) %>% 
  mutate_if(is.numeric, round, 2)

# Save this table to a PDF
pdf("lm_predictor_table.pdf", height=11, width=8.5)
full_join(results_1, results_2) %>% 
  gridExtra::grid.table()
dev.off()

# Examine correlation structure among predictors that seem significant
library(GGally)
complete_df %>% 
  mutate(count = as.integer(proportion*50)) %>% 
  ungroup() %>% 
  select(count, temp_max, tri, dev_north, mois_q75) %>% 
  ggpairs() + theme_bw()
# Must eliminate slope because of high correlation with TRI (0.993), all other
# correlations are less than |0.5|


# THE MODELS ---------------------------
model_df <- complete_df %>% 
  #filter(species == 'psme') %>% #,species == 'pico'
  mutate(count = as.integer(proportion*50)) %>% 
  select(site, fire, aspect, species, frameID, count,
         temp_max, tri, dev_north, mois_q50) %>%
  # Re-scaling the predictors eliminates the error about rescaling from glm
  # the results are very similar except for the p-value for the intercept
  mutate_at(vars(temp_max, mois_q50, tri), scale) %>% 
  as.data.frame() 

  
# Method 1: GLMM with a log-link and Poisson family.
library(lme4)
m <- glmer(count ~ species*(temp_max + mois_q50) + (1|fire) + (1|fire:aspect),
           family = poisson(link = 'log'), 
           data = model_df,
           control=glmerControl(optimizer="bobyqa"))
summary(m)
dotplot(ranef(m, condVar = T))
qqmath(ranef(m))
plot(m)
library(effects)
plot(allEffects(m, residuals = TRUE))
VarCorr(m)
library(MuMIn)
r.squaredGLMM(m)

# Explore effects
effects <- allEffects(m)

# Method using zero-inflated approach
library(glmmTMB)
m <- glmmTMB(count ~ species + temp_max + mois_q50 + dev_north + tri + (1|fire) + (1|fire:aspect),
             ziformula = ~.,
             family = poisson(link = 'log'), 
             REML = T,
             data = model_df)
summary(m)
plot(allEffects(m, residuals = TRUE))


# There is a Bayesian version, but I got confused quickly. 
# m <- stan_glmer(count ~ mois_q50 + temp_q50 + tri + (1|species) + (1|fire) + (1|site),
#                 family = poisson(link = 'log'), 
#                 data = model_df)
# summary(m)
# pairs(m)


# Method 2: GAM with negative binomial
# This approach gives different results for moisture
library(mgcv)
m <- gam(count ~  s(temp_max) + s(tri) + s(aspect_deg) + s(mois_q75), 
         data = model_df, 
         family = nb(theta = NULL, link = "log"), method = "REML")
summary(m)
plot(m)

