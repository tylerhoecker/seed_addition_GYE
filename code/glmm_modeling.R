# Load and prep soil moisture and temperature predictor variables---------------
# Decide on a time period of interest:
start_time <- as.POSIXct(c("2018-06-01 00:00:00"))
end_time <- as.POSIXct(c("2018-10-01 00:00:00"))

# Calculates quantiles and cumulative predictors
############ NOTE THE ADJUSTMENT MADE TO MAPLE SITES!! CHECK SCRIPT ############
source('code/soil_data_prep.R') # creates `soil_df` (complete) and `soil_preds` (summarized)

# Load seedling data and calculate counts for each response---------------------
source('code/read_seedling_data.R')
seedling_response <- seedlings %>% 
  filter(variable == 'height', species != 'control') %>%
  mutate(germinated = if_else(is.na(value), 0, 1),
         established = if_else(date > as.POSIXct("2018-10-01") & value > 0, 1, 0)) %>% 
  group_by(fire, aspect, species) %>% 
  summarise_at(vars(germinated, established), sum, na.rm = T)

# Read in topographic indices from CSV (done using built-in GDAL DEM tools in QGIS3: https://www.gdal.org/gdaldem.html)
dem_idx <- read_csv('data/dem_indices.csv') %>% 
  # Convert aspect to continuous measure per Beers et al. 1966 J. For.
  mutate(dev_ne = cos( (45*pi/180) - (aspect_deg*pi/180) ) + 1 ) %>% 
  dplyr::select(-c(utm_zone, easting, northing, aspect_deg)) 

# Elevation
dem_idx %>% 
  group_by(fire) %>% 
  summarise(m = mean(dem_elev)) %>% 
  arrange(m)

# Create a dataframe that combines the seedling responses, measured soil predictors, and DEM stuff
complete_df <- seedling_response %>% 
  full_join(., soil_preds, by = c('fire','aspect')) %>% 
  #full_join(., atmos_preds) %>% # Comment out for germination, because no ATMOS data that early...
  full_join(., dem_idx, by = c('fire','aspect')) %>% 
  # Dealing only with 'establishment'
  ungroup() %>% 
  modify_at(c('site', 'fire', 'aspect','species'), as.factor) 

# Exploratory Analysis ---------------------------------------------------------

colVals <- c('Flat' = '#009E73','North' = '#0072B2','South' = '#E69F00')
# Look at all possible variables
complete_df %>%
  gather(variable, value, -(fire:established)) %>% 
  # Linear relationships much stronger without Maple!!
  #filter(fire != 'Maple') %>% 
  
  ggplot(aes(x = value, y = established)) +
  geom_point(aes(color = aspect)) +
  geom_smooth(method = 'lm', aes(linetype = species), color = 'black', se = F) +
  scale_color_manual(values = colVals) +
  facet_wrap(~variable, scales = 'free') +
  theme_bw(base_size = 12)

# Examine correlation structure among predictors that seem significant
# library(GGally)
# complete_df %>% 
#   ungroup() %>% 
#   select(count, temp_max, tri, dev_ne, mois_q75) %>% 
#   ggpairs() + theme_bw()
# Must eliminate slope because of high correlation with TRI (0.993), all other
# correlations are less than |0.5|




## THE MODELS ------------------------------------------------------------------
library(MuMIn) # For psuedo-r-squared 
library(lme4)  # For glmer
library(effects) # For glmm effects plotting


model_df <- complete_df %>% 
  #filter(species == 'psme') %>% #,species == 'pico'
  #filter(fire != 'Maple') %>% 
  
  # Re-scaling the predictors eliminates the error about rescaling from glm
  # the results are very similar except for the p-value for the intercept
  mutate_at(vars(temp_hot_hours, temp_q50, temp_q75, temp_max, mois_dry_hours, mois_q50, mois_q75, 
                 dev_ne, tri, slope, dem_elev), scale) 

model_fn <- function(x){
  glmer(formula = as.formula(x),
        family = poisson(link = 'log'), 
        data = model_df,
        control=glmerControl(optimizer="bobyqa"))
}



glob_mod <- 
  glmer(formula = established ~ species*(mois_q50 + mois_q75 + mois_dry_hours + temp_q50 + temp_q75 + temp_max) + (1|fire),
        family = poisson(link = 'log'), 
        data = model_df,
        control=glmerControl(optimizer="bobyqa"),
        na.action="na.fail")

test <- dredge(glob_mod, m.lim = c(0,4))


# Selecting the best GLMM using sensor data ------------------------------------
# Make a list of formulas, all logical combinations of soil data
soil_formulas <- 
  c("established ~ species + (1|fire)", 
    "established ~ species*(mois_dry_hours + (1|fire))",
    "established ~ species*(mois_q50 + (1|fire))",
    "established ~ species*(mois_q75 + (1|fire))",
    "established ~ species*(temp_hot_hours + (1|fire))", 
    "established ~ species*(temp_max + (1|fire))",
    "established ~ species*(temp_q50 + (1|fire))",
    # multi-term
    "established ~ species*(mois_q50 + temp_max + (1|fire))",
    "established ~ species*(mois_q50 + temp_q50 + (1|fire))",
    "established ~ species*(mois_q50 + temp_hot_hours + (1|fire))",
    "established ~ species*(mois_q75 + temp_max + (1|fire))",
    "established ~ species*(mois_q75 + temp_q50 + (1|fire))",
    "established ~ species*(mois_q75 + temp_hot_hours + (1|fire))",
    "established ~ species*(mois_dry_hours + temp_hot_hours + (1|fire))",
    "established ~ species*(mois_dry_hours + temp_max + (1|fire))",
    "established ~ species*(mois_dry_hours + temp_q50 + (1|fire))")


soil_models <- tibble(
    forms = soil_formulas,
    model_obj = map(soil_formulas, model_fn),
    fit = map(model_obj, glance),
    terms = map(model_obj, tidy),
    marg_pR2 = map_dbl(map(model_obj, r.squaredGLMM), mean(1:3)),
    comp_pR2 = map_dbl(map(model_obj, r.squaredGLMM), mean(4:6))) %>% 
  unnest(fit) %>% 
  arrange(AIC) 
soil_models$forms

summary(soil_models$model_obj[[1]])


# Fixed Effects of top model
plot(allEffects(soil_models$model_obj[[1]], residuals = TRUE))

# Residuals of top model
plot(soil_models$model_obj[[1]])


# Selecting the best GLMM using DEM data ------------------------------------
# Make a list of formulas, all logical combinations of soil data

predictors <- c('dev_ne', 'tri', 'slope', 'dem_elev')

expand.grid(predictors, predictors, predictors, predictors)

dem_formulas <- 
  c("established ~ species + (1|fire)", 
    "established ~ species*(dev_ne + (1|fire))",
    "established ~ species*(dev_ne + tri + (1|fire))",
    "established ~ species*(dev_ne + tri + slope + (1|fire))",
    "established ~ species*(dev_ne + tri + slope + dem_elev + (1|fire))",
    "established ~ species*(dev_ne + slope + dem_elev, (1|fire))",
    "established ~ species*(dev_ne + slope + dem_elev, (1|fire))",
    
    "established ~ species*(dev_ne + dem_elev + (1|fire))",
    "established ~ species*(tri + slope + (1|fire))",
    "established ~ species*(tri + dem_elev + (1|fire))",
    "established ~ species*(slope + dem_elev + (1|fire))")




dem_models <- tibble(
  forms = dem_formulas,
  model_obj = map(dem_formulas, model_fn),
  fit = map(model_obj, glance),
  terms = map(model_obj, tidy),
  marg_pR2 = map_dbl(map(model_obj, r.squaredGLMM), mean(1:3)),
  comp_pR2 = map_dbl(map(model_obj, r.squaredGLMM), mean(4:6))) %>% 
  unnest(fit) %>% 
  arrange(AIC) 
dem_models$forms

summary(dem_models$model_obj[[1]])

# Fixed Effects of top model
library(effects)
plot(allEffects(dem_models$model_obj[[1]], residuals = TRUE))

# Residuals of top model
plot(dem_models$model_obj[[1]])






# Method using zero-inflated approach-------------------------------------------
library(glmmTMB)
m <- glmmTMB(count ~ species + temp_max + temp_q50 + mois_q50 + dev_ne + tri + (1|fire),
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


m <- glm(count ~ fire + species + temp_max +  mois_q75 +  dev_ne +  tri, 
           family = poisson(link = 'log'), 
           data = model_df)

summary(m)



#######
# Make a table that summarizes the linear models
# Linear models
library(broom)

# results_1 <- complete_df %>% 
#   mutate(count = as.integer(proportion*50)) %>% 
#   select(-c(utm_zone, easting, northing)) %>% 
#   gather(., variable, value, -(site:proportion),-count) %>%
#   group_by(variable) %>% 
#   do(tidy(lm(count ~ value, data = .))) %>% # Glance helpful for fit statistics
#   filter(term == 'value') %>% 
#   arrange(p.value) %>% 
#   select(-term) %>% 
#   mutate_if(is.numeric, round, 2)
# 
# results_2 <- complete_df %>% 
#   mutate(count = as.integer(proportion*50)) %>% 
#   select(-c(utm_zone, easting, northing)) %>% 
#   gather(., variable, value, -(site:proportion),-count) %>%
#   group_by(variable) %>% 
#   do(glance(lm(count ~ value, data = .))) %>% # Glance helpful for fit statistics
#   select(r.squared, adj.r.squared, sigma) %>% 
#   mutate_if(is.numeric, round, 2)
# 
# # Save this table to a PDF
# pdf("lm_predictor_table.pdf", height=11, width=8.5)
# full_join(results_1, results_2) %>% 
#   gridExtra::grid.table()
# dev.off()
# 
# 
# formulas %>% 
#   map(model_fn) %>% 
#   map(glance) %>% 
#   bind_rows() %>% 
#   mutate(forms = formulas)
# 
