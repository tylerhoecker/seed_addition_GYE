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
source('code/atmos_data_prep.R') # creates `atmos_df` (complete) and `atmos_preds` (summarized)

# Run seedling import/summary scripts. Creates `germination`, `final`, `proprotions`, `seed_per`.
source('code/read_summarize_seedling.R')

# Read in topographic indices from CSV (done using built-in GDAL DEM tools in QGIS3: https://www.gdal.org/gdaldem.html)
dem_idx <- read_csv('data/dem_indices.csv')

# Start with a dataframe of seedling data and soil moisture and temperature
complete_df <- proportions %>% 
  full_join(., soil_preds, by = c('fire','aspect')) %>% 
  full_join(., atmos_preds) %>% # Comment out for germination, because no ATMOS data that early...
  full_join(., dem_idx) %>% 
  filter(version == 'original',
         species == 'pico') %>% 
  rename(proportion = value) %>% 
  # Dealing only with 'establishment'
  filter(period == 'survival') %>% 
  unite(site, fire, aspect, remove = F) %>% 
  ungroup() %>% 
  modify_at(c('site', 'fire', 'aspect'), as.factor) 

# Exploratory Analysis ----------------------------------------------------
colVals <- c('Flat' = '#009E73','North' = '#0072B2','South' = '#E69F00')

# Correlation

library(GGally)
complete_df %>% 
  ungroup() %>% 
  select(proportion, mois_q50, temp_q50, air_temp_q50, vpd_q50, tpi, tri, aspect_deg) %>% 
  ggpairs()


# THE MODELS ---------------------------
model_df <- complete_df %>% 
  mutate(count = as.integer(proportion*50)) %>% 
  select(site, fire, aspect, species, frameID, count, 
         starts_with('mois_'),  starts_with('temp_'),
         dem_elev, tri, tpi, slope, aspect_deg) %>% 
  as.data.frame()
  
# Method 1: GLMM with a log-link and Poisson family.
# Questions: 
# Should species be a fixed or random effect? 
# Is the correlation between mois and temp too high (0.77)? 
# How do I interpret correlation between the intercept and the other fixed effects? 
# Model fails to converge when using fire and site as fixed effects. Why? 
# Parameter estimates are quite different if I use fire vs site as random effects. Which should I use?
# Observations:
# Using site (fire + aspect, the lowest grouping varibale) seems to absorb some of the abiotic effects.
# The sign of the temp estimate is counter-intuitive, high median temps increase chance of establishment?
# Similarly, for germination, increase in temp decreases chances of germination... but soil moisture not significant?

library(lme4)
m <- glmer(count ~ mois_q50 + temp_q50 + tri + (1|fire),
           family = poisson(link = 'log'), 
           data = model_df)
summary(m)
plot(m)

# There is a Bayesian version, but I got confused quickly. 
# m <- stan_glmer(count ~ mois_q50 + temp_q50 + tri + (1|species) + (1|fire) + (1|site),
#                 family = poisson(link = 'log'), 
#                 data = model_df)
# summary(m)
# pairs(m)


# Method 2: GAM with negative binomial
# The models produce very different results... so its difficult to interperet
m <- gam(count ~  s(mois_q75) + s(temp_q75) + s(tri), 
         data = model_df, 
         family = nb(theta = NULL, link = "log"), method = "REML")
summary(m)
plot(m)

