library(ggrepel)
library(tidyverse)
library(broom)
library(raster)
library(rgdal)
library(spatialEco)
library(tidyverse)
library(DescTools)
library(MuMIn)
select <- dplyr::select


# Skip all this - it's slow!
# ------------------------------------------------------------------------------
# Import raster data
raw_dem_files <- list.files("/Users/tylerhoecker/Box/Work/PhD/GIS/establishment_map/raster/raw_dems/", full.names = T)
raw_dem_list <- lapply(raw_dem_files, raster)
raw_dem_list$fun <- mean
large_dem <- do.call(mosaic, raw_dem_list)

# Import experimental plot locatons
site_points <- readOGR(dsn = "/Users/tylerhoecker/Box/Work/PhD/GIS/establishment_map/site_locations_shp/") 
# Transform to same as DEMs
site_points_wgs <- spTransform(site_points, crs(raw_dem_list[[1]]))

# Reburn areas
reburn_perims <- readOGR("/Users/tylerhoecker/Box/Work/PhD/GIS/establishment_map/overlap_old_2016.shp")
reburn_perims_wgs <- spTransform(reburn_perims, crs(raw_dem_list[[1]]))

smaller_dem <- crop(large_dem, reburn_perims_wgs)

compareCRS(smaller_dem, site_points_wgs)

# Calculate terrain indices from DEM
aspect <- terrain(smaller_dem, opt = 'aspect', unit = 'degrees')
# Convert aspect to continuous measure per Beers et al. 1966 J. For.
dev_ne <- cos( (45*pi/180) - (aspect*pi/180) ) + 1 
tpi <- terrain(smaller_dem, opt = 'TPI')
hli <- hli(smaller_dem, check = T)

terrain_stack <- stack(dev_ne, smaller_dem, tpi, hli) 
names(terrain_stack) <- c('dev_ne','elev','tpi','hli')
writeRaster(terrain_stack, 'terrain_stack', bylayer = T, format = 'GTiff', overwrite = T)

# Save these values to a dataframe with the site names
terrain_ind_df <- site_points@data %>% 
  rename(aspect = plot) %>% 
  dplyr::select(fire, aspect)

terrain_ind_df$elev = raster::extract(smaller_dem, site_points_wgs)
terrain_ind_df$dev_ne = raster::extract(dev_ne, site_points_wgs)
terrain_ind_df$tpi = raster::extract(tpi, site_points_wgs)
terrain_ind_df$hli = raster::extract(hli, site_points_wgs)

saveRDS(terrain_ind_df, 'data/terrain_ind_df.Rds')
# ------------------------------------------------------------------------------
# Instead:
terrain_ind_df <- readRDS('data/terrain_ind_df.Rds')
terrain_rasters <- c('terrain_stack_1.tif','terrain_stack_2.tif','terrain_stack_3.tif','terrain_stack_4.tif')
terrain_stack <- paste0("/Users/tylerhoecker/Box/Work/PhD/GIS/establishment_map/raster/", terrain_rasters) %>% 
  stack()
names(terrain_stack) <- names(terrain_stack) <- c('dev_ne','elev','tpi','hli')

# Import response variables (seedling germination, survival)--------------------
source(file.path('code','read_summarize_seedling.R'))

proportions <- full_join(germination, survival) %>% 
  group_by(fire, aspect, species, frameID) %>% # Adjust by frame or site: +/- frameID
  summarise(Germination = sum(germinated, na.rm = T) / n(),
            Survival_yr1 = sum(survived_yr1, na.rm = T) / sum(germinated, na.rm = T),
            Survival_yr2 = sum(survived_yr2, na.rm = T) / sum(germinated, na.rm = T),
            Survival_1to2 = sum(survived_yr2, na.rm = T) / sum(survived_yr1, na.rm = T),
            Establishment_yr1 = sum(survived_yr1, na.rm = T) / n(),
            Establishment_yr2 = sum(survived_yr2, na.rm = T) / n()) %>%
  # Average again...
  group_by(fire, aspect, species) %>% 
  summarise_all(mean, na.rm = T) %>% 
  dplyr::select(-frameID)

# Combine response and predictor variables -------------------------------------
complete_df <- proportions %>%
  ungroup() %>% 
  # Add informaiton for weights for binomial regression
  mutate(germ_estab_Weights = as.integer(250),
         surv_Weights = as.integer(Germination * 250)) %>% 
  full_join(., terrain_ind_df) %>% 
  modify_at(c('fire','species','texture'), as.factor) %>% 
  filter(species == 'PICO')

# Rescale data
rescaled_df <- complete_df %>%
  mutate_at(vars(elev, dev_ne, tpi, hli),  ~ scale(.)[,1]) %>% 
  as.data.frame(stringsAsFactors = F) 

# GLMs -------------------------------------------------------------------------
models_df <- data.frame('period' = c('Germination','Survival_yr1', 'Survival_yr2','Establishment_yr1','Establishment_yr2'),
                        'weights' = c('germ_estab_Weights', 'surv_Weights', 'surv_Weights','germ_estab_Weights','germ_estab_Weights'),
                        stringsAsFactors = FALSE)

delta_val <- 2

model_avg <- function(period, weights){

  # Remove NA's where neccessary - dredge doesn't work if data lengths are different among models
  if (period == 'Germination'){
    model_data <- rescaled_df
  } else if (period == 'Survival_yr1'){
    model_data <- rescaled_df %>% drop_na(Survival_yr1)
  } else if (period == 'Survival_yr2'){
    model_data <- rescaled_df %>% drop_na(Survival_yr2)
  } else if (period == 'Establishment_yr1'){
    model_data <- rescaled_df %>% drop_na(Establishment_yr1)
  } else if (period == 'Establishment_yr2'){
    model_data <- rescaled_df %>% drop_na(Establishment_yr2)
  } else{
    stop("FAIL! - Period is incorrect")
  }

  # Get model formula and weights from master data frame
  model_form <- as.formula(paste0(period, ' ~ dev_ne + elev + tpi + hli'))
  model_data$model_Ws <- as.numeric(model_data[[weights]])

  # Fit the global model
  global_model <- glm(formula = model_form, data = model_data,
                      na.action = 'na.fail', family = binomial(link = "logit"),
                      weights = model_Ws)

  # Save p-values from global model
  global_LRT <- anova(global_model, test="LRT")
  LRT_Ps <- data.frame('period' =  period,
                       'term' = rownames(global_LRT)[-1],
                       'LRT_p' = round(global_LRT[,'Pr(>Chi)'][-1], 3),
                       stringsAsFactors = F)

  # Perform all-subsets model selection based on AICc, min 1 max 3 predictors
  dredge_obj <- dredge(global_model, m.lim = c(1,2), rank = 'AICc')
  print(dredge_obj)

  if( length(which(dredge_obj[,'delta'] < delta_val)) > 1 ){
    avg_obj <- model.avg(dredge_obj, subset = delta < delta_val)
    coefs <- avg_obj$coefficients[2,]
  }else{
    avg_obj <- get.models(dredge_obj, subset = delta < delta_val)[[1]]
    coefs <- avg_obj$coefficients
  }

  results <-  data.frame(
    'period' = period,
    'term' = names(coefs),
    'estimate' = unname(coefs),
    'lower' = confint(avg_obj)[,1],
    'upper' = confint(avg_obj)[,2],
    'AICc' = AICc(global_model))

  rownames(results) <- c()

  results <- results %>%
    #mutate(term = ifelse(term == 'speciesPSME', 'species', as.character(term))) %>%
    full_join(., LRT_Ps)

  return(results)
}

model_results <- pmap_df(models_df, model_avg)

plot_results <- model_results %>%
  filter(term != '(Intercept)') %>%
  mutate(period = factor(period,
                         levels = c('Germination','Survival_yr1','Survival_yr2','Survival_1to2','Establishment_yr1','Establishment_yr2'),
                         labels = c('Germination','Survival (yr 1)','Survival (yr 2)','Survival_1to2','Establishment_yr1','Establishment'))) %>%
  filter(period != 'Establishment_yr1')


# Plot summarized terms
ggplot(plot_results, aes(x = term, y = estimate)) +
  geom_errorbar(aes(ymin = lower, ymax = upper, width = 0), position = position_dodge(width=0.7), size = 0.7) +
  geom_point(size = 3, position = position_dodge(width=0.7)) +
  geom_hline(aes(yintercept = 0), linetype = 'dashed') +
  coord_flip(ylim = c(-2.5, 2.5)) +
  theme_bw(base_size = 16) +
  labs(y = 'Effect size (log-odds)', x = '') +
  facet_wrap(~period, nrow = 1) +
  theme(title = element_text(size = 12)) +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = 'bold'),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text(face = 'bold'))


#-------------------------------------------------------------------------------
# Prediction -------------------------------------------------------------------
# Just for PICO establishment year 2, function not neccesary
# Fit the global model on original-scale predictors
global_model <- glm(Establishment_yr2 ~ dev_ne + elev + tpi + hli, data = complete_df, 
                    na.action = 'na.fail', family = binomial(link = "logit"), 
                    weights = germ_estab_Weights)

# Perform all-subsets model selection based on AICc, min 1 max 2 predictors
dredge_obj <- dredge(global_model, m.lim = c(1,2), rank = 'AICc')
dredge_obj

# Changed: should not do prediction with averaged coeffecients (wasn't but being explicit): http://atyre2.github.io/2017/06/16/rebutting_cade.html
# if( length(which(dredge_obj[,'delta'] < delta_val)) > 1 ){
#   avg_obj <- model.avg(dredge_obj, subset = delta < delta_val)
# }else{
#   avg_obj <- get.models(dredge_obj, subset = delta < delta_val)[[1]]
# }

# Pick top model, reformulate
top_model <- glm(formula = Establishment_yr2 ~ dev_ne + tpi, family = binomial(link = "logit"), 
                 data = complete_df, weights = germ_estab_Weights, na.action = "na.fail")

r.squaredGLMM(top_model)

# Mask DEM to only reburned areas - don't run takes too long
#-------------------------------------------------------------------------------
compareCRS(reburn_perims_wgs, terrain_stack)
terrain_reburn <- mask(terrain_stack, reburn_perims_wgs)
plot(terrain_reburn)
writeRaster(terrain_reburn, '/Users/tylerhoecker/Box/Work/PhD/GIS/establishment_map/raster/terrain_raster_reburn.tif', overwrite = T)
#-------------------------------------------------------------------------------
# Instead:
terrain_reburn <- stack("/Users/tylerhoecker/Box/Work/PhD/GIS/establishment_map/raster/terrain_raster_reburn.tif")
names(terrain_reburn) <- c('dev_ne','elev','tpi','hli')

# Predict on this new raster!
estab_predict <- predict(terrain_reburn, top_model, type = 'response', progress = 'text')

# Mask to stockable area
stockable <- raster("/Users/tylerhoecker/Box/Work/PhD/GIS/establishment_map/raster/stockable_reburn.tif")

# Project the prediction raster back to UTM crs
estab_predict_utm <- projectRaster(estab_predict, stockable)

stockable <- resample(stockable, estab_predict_utm)
estab_predict_stock <- mask(estab_predict_utm, stockable)

writeRaster(estab_predict_stock, 'estab_predict_pico_glm.tif', progress = 'text', overwrite = T)






## OLD
# Re-create GLMs with unscaled predictors
germ_top_model$formula
glm_proxy_germ <-
  glm(germ_top_model$formula,
      family = binomial(link = "logit"), 
      weights = Weights,
      data = complete_df)

summary(glm_proxy_germ)
# Estimate a glmm of survival, with a zero-inflation component, save results
glm_proxy_surv <- 
  glm(surv_top_model$formula,
      family = binomial(link = "logit"), 
      weights = Germination*50,
      data = complete_df)

# Make a stack of the rasters of predictors in final models
terrain_stack <- stack(dev_ne, dem, tpi, hli) 
names(terrain_stack) <- c('dev_ne','elev','tpi','hli')

# Predict on this new raster!
surv_predict_rast <- predict(terrain_stack, glm_proxy_surv, 
                            type = 'response',
                            const = data.frame(species = 'PICO', Weights = 50, Germination = 0.5),
                            progress = 'text')

germ_predict_rast <- predict(terrain_stack, glm_proxy_germ, 
                             type = 'response',
                             const = data.frame(species = 'PICO', Weights = 50, Germination = 0.5),
                             progress = 'text')

establishment_rast <- surv_predict_rast * germ_predict_rast

plot(establishment_rast)

writeRaster(establishment_rast, 'estab_predict.tif', progress = 'text')

# This takes a long time!
# terrain_stack_df <- as.data.frame(terrain_stack, xy = T, centroids = T) 
# 
# terrain_stack_df <- terrain_stack_df %>%
#   rename(dev_ne = layer.1, elev = dem_10m, hli = layer.2)





# OLD
# Estimate a glmm of germination, with no zero-inflation component, save results
# Estimate a glm of germination, save results
germ_proxy_global <- 
  glm(Germination ~ species + dev_ne + tpi + hli,
      family = binomial(link = "logit"), 
      weights = germ_estab_Weights,
      data = rescaled_df,
      na.action = 'na.fail')
germ_proxy_dredge <- dredge(germ_proxy_global)
germ_top_model <- get.models(germ_proxy_dredge, subset = 1)[[1]]
summary(germ_top_model)
PseudoR2(germ_top_model)

surv_proxy_global <- 
  glm(Survival ~ species + dev_ne + tpi + hli,
      family = binomial(link = "logit"), 
      weights = surv_Weights,
      data = rescaled_df,
      na.action = 'na.fail')
surv_proxy_dredge <- dredge(surv_proxy_global)
surv_top_model <- get.models(surv_proxy_dredge, subset = 1)[[1]]
summary(surv_top_model)
PseudoR2(surv_top_model)


glm_proxy_germ_df <- germ_top_model %>% 
  tidy(.) %>% 
  mutate(model = 'Germination')# Combine these two models into a single df and change labeling, for plotting

glm_proxy_surv_df <- surv_top_model %>% 
  tidy(.) %>% 
  mutate(model = 'Survival')# Combine these two models into a single df and change labeling, for plotting


model_results <- full_join(glm_proxy_germ_df, glm_proxy_surv_df) %>% 
  filter(term != '(Intercept)') %>%
  mutate(term = fct_reorder(term, estimate, max)) %>%
  mutate(term = fct_recode(term, 'Species (PSME)' = "speciesPSME",
                           'Deviation from NE' = 'dev_ne',
                           'TPI' = 'tpi',
                           'HLI' ='hli')) %>% 
  mutate(ci_low = estimate - (1.96*std.error),
         ci_high = estimate + (1.96*std.error)) 











