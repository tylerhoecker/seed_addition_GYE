# Decide on a time period of interest:
start_time <- as.POSIXct(c("2018-06-01 00:00:00"))
end_time <- as.POSIXct(c("2018-09-01 00:00:00"))


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
  full_join(., atmos_preds) %>% 
  full_join(., dem_idx) %>% 
  filter(version == 'original') %>% 
  rename(proportion = value) %>% 
  # Dealing only with 'establishment'
  filter(period == 'establishment') %>% 
  unite(site, fire, aspect, remove = F) %>% 
  ungroup() %>% 
  modify_at(c('site', 'fire', 'aspect'), as.factor) 

# Exploratory Analysis ----------------------------------------------------
colVals <- c('Flat' = '#009E73','North' = '#0072B2','South' = '#E69F00')

# Correlation

library(GGally)
complete_df %>% 
  ungroup() %>% 
  select(proportion, mois_q50, temp_q75, air_temp_q50, vpd_q50, tpi, tri, aspect_deg) %>% 
  ggpairs()


# THE MODELS ---------------------------
model_df <- complete_df %>% 
  mutate(count = as.integer(proportion*50)) %>% 
  select(site, fire, aspect, species, frameID, count, 
         starts_with('mois_'),  starts_with('temp_'),
         dem_elev, tri, tpi, slope, aspect_deg) %>% 
  as.data.frame()
  

library(lme4)
m <- glmer(count ~ species + mois_q50 + temp_q50 + tri + (1|fire) ,
           family = poisson(link = 'log'), 
           data = model_df)
summary(m)
plot(m)

m <- stan_glmer(count ~ mois_q50 + temp_q50 + tri + (1|species) + (1|fire) + (1|site),
                family = poisson(link = 'log'), 
                data = model_df)
summary(m)
pairs(m)



