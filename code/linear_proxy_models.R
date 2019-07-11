library(ggrepel)
library(tidyverse)
library(broom)
library(raster)
library(rgdal)
library(spatialEco)
library(tidyverse)
library(DescTools)
library(MuMIn)


# Import raster data
dem <- raster( "/Users/tylerhoecker/Box Sync/PhD/GIS/establishment_map/dem_10m.tif") 
# Import experimental plot locatons
site_points <- readOGR(dsn = "/Users/tylerhoecker/Box Sync/PhD/GIS/establishment_map/site_locations_shp/") 

# Force CRS to be identical, even though they are already the same. MUST BE THE SAME!
crs(site_points) <- crs(dem)

# Calculate terrain indices from DEM
aspect <- terrain(dem, opt = 'aspect', unit = 'degrees')
# Convert aspect to continuous measure per Beers et al. 1966 J. For.
dev_ne <- cos( (45*pi/180) - (aspect*pi/180) ) + 1 
slope <- terrain(dem, opt = 'slope', unit = 'degrees')
tpi <- terrain(dem, opt = 'TPI')
#tpi9 <- tpi(dem, scale = 9)
hli <- hli(dem, check = T)
#hsp <- hsp(dem)
#curv <- curvature(dem, type = 'total')

# Save these values to a dataframe with the site names
terrain_ind_df <- site_points@data %>% 
  rename(aspect = plot) %>% 
  dplyr::select(fire, aspect)

terrain_ind_df$elev = extract(dem, site_points)
terrain_ind_df$dev_ne = extract(dev_ne, site_points)
terrain_ind_df$slope = extract(slope, site_points)
terrain_ind_df$tpi = extract(tpi, site_points)
terrain_ind_df$hli = extract(hli, site_points)

# Import response variables (seedling germination, survival)--------------------
source('code/read_summarize_seedling.R')
seedling_response <- proportions  
  #filter(version == 'asinsqrt') %>% 
  #dplyr::select(-version) %>% 

# Combine response and predictor variables -------------------------------------
complete_df <-  seedling_response %>% 
  mutate(Weights = 250) %>% 
  full_join(., terrain_ind_df) %>% 
  modify_at(c('species'), as.factor)  

rescaled_df <- complete_df %>% 
  ungroup() %>%
  mutate_at(vars(-c(fire,aspect,species,period,version,value,Weights)), scale) 

# Linear models ----------------------------------------------------------------
rescaled_df %>%
  filter(version == 'asinsqrt') %>% 
  filter(period == 'Establishment') %>% 
  filter(species == 'PICO') %>% 
  drop_na() %>% 
  do(dredge(lm(value ~ dev_ne + tpi + hli,
               data = ., na.action = 'na.fail'), 
            m.lim = c(0,3), 
            extra = list("R^2", "*" = function(x) {
              s <- summary(x)
              c(Rsq = s$r.squared, adjRsq = s$adj.r.squared, F = s$fstatistic[[1]])
            }),
            rank = 'AIC')) %>% 
  filter(delta <= 2) %>% 
  write_csv(., 'linear_proxy_model_germ.csv')


# Prediction
# Estimate a glmm of survival, with a zero-inflation component, save results
to_model <- filter(complete_df, version == 'asinsqrt' & period == 'Establishment' & species == 'PICO')

estab_model <- lm(value ~ dev_ne, data = to_model)

summary(estab_model)

names(dev_ne) <- 'dev_ne'

# Predict on this new raster!
estab_predict_rast <- predict(dev_ne, estab_model, progress = 'text')

# Back-transform the response
estab_predict_rast <- sin(estab_predict_rast)^2

writeRaster(estab_predict_rast, 'estab_predict.tif', progress = 'text')










# GLM -------------------------------------------------------------------------
rescaled_df %>%
  # Can't seem to get dredge to work when there are NAs, so have to do germination separate
  filter(version == 'original') %>% 
  filter(period == 'Establishment') %>% 
  filter(species == 'PICO') %>% 
  do(dredge(glm(value ~ dev_ne + tpi + hli,
                data = ., 
                na.action = 'na.fail', 
                family = binomial(link = "logit"), 
                weights = Weights),
            m.lim = c(0,3), 
            rank = 'AIC')) %>% 
  filter(delta <= 2) %>% 
  filter(logLik == max(logLik)) %>% 
  # This identifies the terms that were used in the models for each species
  gather(term, estimate, mois_q50, temp_q50, N, veg) %>%
  filter(!is.na(estimate)) %>%
  # And then makes a formula out of them. Probably won't work when there's more than 1 model
  summarise(form = paste0('value', '~', paste0(term, collapse = '+'))) %>%
  full_join(rescaled_df)

test <- rescaled_df %>%
  # Can't seem to get dredge to work when there are NAs, so have to do germination separate
  filter(version == 'original') %>% 
  filter(period == 'Establishment') %>% 
  filter(species == 'PICO') 
test_glm <-
  glm(value ~ dev_ne + tpi + hli,
      data = test,
      family = binomial(link = "logit"),
      weights = Weights)

round(Pseudo.R2(test_glm),2)
PseudoR2(test_glm, which = 'all')

# Save this summary of germination models
estab_models <- estab_model_selection %>%
  filter(period == 'Establishment') %>%
  drop_na() %>%
  group_by(species,period) %>%
  do(tidy(glm(as.formula(.$form),
              data = .,
              na.action = 'na.fail',
              family = binomial(link = "logit"),
              weights = surv_Weights))) 



