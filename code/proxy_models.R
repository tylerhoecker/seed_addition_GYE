library(MuMIn) # For psuedo-r-squared 
library(glmmTMB) # For zero-inflated
library(lme4)
library(effects)
source('code/r2glmmTMB.R') # For Ben Bolker's adaptation of r-squared for glmmTMB
library(ggrepel)
library(tidyverse)
library(broom.mixed)
library(DHARMa)
library(cowplot)
#library(stars)
library(raster)
library(rgdal)
library(spatialEco)
library(tidyverse)
library(DescTools)

# Import raster data
dem <- raster('data/geospatial/dem_10m.tif') 
# Import experimental plot locatons
site_points <- readOGR(dsn = 'data/geospatial/site_locations_shp/') 

# Force CRS to be identical, even though they are already the same. MUST BE THE SAME!
crs(site_points) <- crs(dem)

# Calculate terrain indices from DEM
aspect <- terrain(dem, opt = 'aspect', unit = 'degrees')
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
terrain_ind_df$asp_deg = extract(aspect, site_points)
terrain_ind_df$slope = extract(slope, site_points)
terrain_ind_df$tpi = extract(tpi, site_points)
terrain_ind_df$tpi9 = extract(tpi9, site_points)
terrain_ind_df$hli = extract(hli, site_points)
terrain_ind_df$hsp = extract(hsp, site_points)
terrain_ind_df$curv = extract(curv, site_points)

# Convert aspect to continuous measure per Beers et al. 1966 J. For.
terrain_ind_df <- terrain_ind_df %>% 
  mutate(dev_ne = cos( (45*pi/180) - (asp_deg*pi/180) ) + 1 )

# Import response variables (seedling germination, survival)--------------------
source('code/read_summarize_seedling.R')
seedling_response <- proportions %>% 
  filter(version == 'original') %>% 
  dplyr::select(-version) %>% 
  spread(period, value) 

# Combine response and predictor variables -------------------------------------
complete_df <-  seedling_response %>% 
  mutate(Weights = 50) %>% 
  full_join(., terrain_ind_df) %>% 
  modify_at(c('species'), as.factor)  
  

# Modeling! --------------------------------------------------------------------
rescaled_df <- complete_df %>% 
  ungroup() %>%
  mutate_at(vars(-c(fire,aspect,species,Germination,Survival,Establishment,Weights)), funs(as.numeric(scale(.)))) 

# GLMMs -------------------------------------------------------------------------

# Estimate a glmm of germination, with no zero-inflation component, save results
germ_proxy_global <- 
  glm(Survival ~ species + dev_ne + elev + tpi + tpi9 + hsp,
      family = binomial(link = "logit"), 
      weights = Germination*50,
      data = rescaled_df,
      na.action = 'na.fail')
germ_proxy_dredge <- dredge(germ_proxy_global)
germ_top_model <- get.models(germ_proxy_dredge, subset = 1)[[1]]
summary(germ_top_model)
PseudoR2(germ_top_model)

surv_proxy_global <- 
  glm(Survival ~ species + dev_ne + elev + tpi + tpi9,
      family = binomial(link = "logit"), 
      weights = Germination*50,
      data = rescaled_df,
      na.action = 'na.fail')
surv_proxy_dredge <- dredge(surv_proxy_global)
surv_top_model <- get.models(surv_proxy_dredge, subset = 1)[[1]]
summary(surv_top_model)
PseudoR2(surv_top_model)

# Estimate a glm of germination, save results
glmm_proxy_germ <-
  glm(Germination ~ species + dev_ne + elev + tpi,
      family = binomial(link = "logit"), 
      weights = Weights,
      data = rescaled_df)
summary(glmm_proxy_germ)
DescTools::PseudoR2(glmm_proxy_germ)  
  
glmm_proxy_germ_df <- glmm_proxy_germ %>% 
  tidy(.) %>% 
  mutate(model = 'Germination')


# Estimate a glmm of survival, with a zero-inflation component, save results
glmm_proxy_surv <- 
  glm(Survival ~ species + dev_ne + elev + tpi,
      family = binomial(link = "logit"), 
      weights = Germination*50,
      data = rescaled_df)
summary(glmm_proxy_surv)
DescTools::PseudoR2(glmm_proxy_surv)  
  
glmm_proxy_surv_df <- glmm_proxy_surv %>% 
  tidy(.) %>% 
  mutate(model = 'Survival')


# Combine these two models into a single df and change labeling, for plotting
model_results <- full_join(glmm_proxy_germ_df, glmm_proxy_surv_df) %>% 
  filter(term != '(Intercept)') %>%
  mutate(term = fct_reorder(term, estimate, max)) %>%
  mutate(term = fct_recode(term, 'Species (PSME)' = "speciesPSME",
                           'Deviation from NE' = 'dev_ne',
                           'Elevation' = 'elev',
                           'TPI' = 'tpi9')) %>% 
  mutate(ci_low = estimate - (1.96*std.error),
         ci_high = estimate + (1.96*std.error)) 

ggplot(model_results, aes(x = term, y = estimate)) +
  geom_errorbar(aes(ymin = estimate-std.error, ymax = estimate+std.error),
                size = 1, width = 0, color = 'grey50') +
  geom_point(aes(), fill = 'grey50', color = 'black', shape = 21, size = 3.2) +
  geom_text_repel(aes(label = round(estimate,2)), 
                  segment.alpha = 0, nudge_x = .3, show.legend = F) +
  geom_hline(aes(yintercept = 0), linetype = 'dashed') +
  coord_flip() +
  theme_bw(base_size = 14) +
  labs(y = 'Effect size (log-odds)', x = '') +
  facet_wrap(~model) +
  #scale_color_manual('Model', values = c('black','red3'), labels = c('Conditional','Zero-inflation') ) +
  #scale_fill_manual('Model', values = c('grey50','red3'), labels = c('Conditional','Zero-inflation') ) +
  theme(title = element_text(size = 12)) +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = 'bold'))


#-------------------------------------------------------------------------------
# Prediction -------------------------------------------------------------------

# This takes a long time!
terrain_stack <- stack(dev_ne, dem, tpi) %>% 
  as.data.frame(xy = T, centroids = T)

#writeRaster(terrain_stack, filename="terrain_indices.tif", options="INTERLEAVE=BAND", overwrite=TRUE)



