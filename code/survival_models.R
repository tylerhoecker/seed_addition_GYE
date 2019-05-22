# Load and prep soil moisture and temperature predictor variables---------------
# Decide on a time period of interest:
start_time <- as.POSIXct(c("2018-07-08 00:00:00"))
end_time <- as.POSIXct(c("2018-10-01 00:00:00"))

source('code/soil_data_prep.R') # creates `soil_df` (complete) and `soil_preds` (summarized)

# Response variables
source('code/read_summarize_seedling.R')
seedling_response <- proportions %>% 
  filter(version == 'original') %>% 
  dplyr::select(-version) %>% 
  spread(period, value) 

# Create a dataframe that combines the seedling responses, measured soil predictors, and DEM stuff
complete_df <- seedling_response %>% 
  full_join(., soil_preds, by = c('fire','aspect')) %>% 
  #full_join(., dem_idx, by = c('fire','aspect')) %>% 
  # Dealing only with 'establishment'
  ungroup() %>% 
  modify_at(c('site', 'fire', 'aspect','species'), as.factor) 

# Exploratory Analysis ---------------------------------------------------------
# colVals <- c('Flat' = '#009E73','North' = '#0072B2','South' = '#E69F00')
# 
# complete_df %>%
#   gather(variable, value, -(fire:Establishment)) %>% 
#   filter(variable %in% c('temp_q50', 'mois_q50')) %>% 
# 
#   ggplot(aes(x = value, y = Survival)) +
#   geom_point(aes(color = aspect, shape = fire)) +
#   geom_smooth(method = 'lm', aes(linetype = species), color = 'black', se = F) +
#   scale_color_manual(values = colVals) +
#   facet_wrap(~variable, scales = 'free') +
#   theme_bw(base_size = 12)

## THE MODELS ------------------------------------------------------------------
library(MuMIn) # For psuedo-r-squared 
library(lme4)  # For glmer
library(effects) # For glmm effects plotting
library(glmmTMB) # For zero-inflated
source('code/r2glmmTMB.R') # For Ben Bolker's adaptation of r-squared for glmmTMB

# Rescale predictors
model_df <- complete_df %>% 
  # filter(fire != 'Maple') %>% 
  # mutate_at(vars(-c(fire,aspect,species,germinated,established)), scale) 
  mutate_at(vars(-c(fire,aspect,species,Germination,Survival,Establishment)), scale) 

# Zero-inflated GLMM using glmmTMB----------------------------------------------
glmm_survival <- glmer(Survival ~ species + mois_q50 + temp_q50 + (1|fire),
                       family = 'binomial', 
                       weights = Germination*50,
                       data = model_df)
summary(glmm_survival)
r.squaredGLMM(glmm_survival)


model_results <- 
  summary(glmm_survival)$coefficients$cond %>% 
  as.tibble(rownames = 'term') %>% 
  rename(sigma = `Std. Error`, coeffecient = Estimate, zval = `z value`, pval = `Pr(>|z|)`) %>% 
  filter(term != '(Intercept)') %>% 
  mutate(term = if_else(term == 'speciesPSME', 'Species (PSME)',
                        if_else(term == 'mois_q50', 'Soil moisture', 
                                if_else(term == 'temp_q50', 'Soil temperature', term)))) %>% 
  mutate(model = 'Survival')


germ_model_results <- readRDS('germ_model_results.rds') %>% 
  mutate(model = 'Germination')

both_model_results <- 
  full_join(model_results, germ_model_results) 

ggplot(both_model_results) +
  geom_pointrange(aes(x = term, y = coeffecient, ymin = coeffecient-2*sigma, ymax = coeffecient+2*sigma)) +
  geom_text(aes(x = term, y = coeffecient, label = round(coeffecient,2)), nudge_x = .3) +
  geom_hline(aes(yintercept = 0), linetype = 'dashed') +
  coord_flip() +
  theme_bw(base_size = 14) +
  labs(y = 'Effect size', x = '') +
  facet_wrap(~model) +
  theme(title = element_text(size = 12)) +
  theme(strip.background = element_blank(),
          strip.text = element_text(face = 'bold'))


# Make plots of predictions/model results
# First, fit the model on unscaled variables
glmm_survival <- glmer(Survival ~ species + mois_q50 + temp_q50 + (1|fire),
                       family = 'binomial', 
                       weights = Germination*50,
                       data = complete_df)

# Make a data frame of predictions
new_data <- 
  expand.grid(mois_q50 = seq(.02,.14,.002),
              temp_q50 = seq(10,20,4),
              species =  c("PSME", "PICO"),
              fire = c("Berry-Glade","Berry-Huck","Buffalo","Maple"),
              Germination = 1) %>% 
  mutate(pred_probs = predict(glmm_survival, type = 'response', newdata = .))  %>% 
  group_by(species) %>%  
  mutate(line_group = paste(temp_q50,fire))

# pred_low = pred_probs - predict(ziglmm_survival, type = 'response', newdata = ., se.fit = T)$se.fit,
# pred_high = pred_probs + predict(ziglmm_survival, type = 'response', newdata = ., se.fit = T)$se.fit

complete_df <- complete_df %>% 
  predict(ziglmm_survival, type = 'response', newdata = )

  
# Plot predictions
ggplot() +
  geom_jitter(data = complete_df,
             aes(x = mois_q50, y = Survival, size = Germination*50), alpha = 0.5) +
  # geom_ribbon(data = new_data, aes(x = mois_q50, ymin = pred_low, ymax = pred_high, group = fire), 
  #             alpha = 0.3) +
  geom_line(data = new_data, aes(x = mois_q50, y = pred_probs, color = temp_q50, group = line_group), 
            size = 0.75) +
  facet_wrap(~species) +
  scale_x_continuous(breaks = seq(0.04,0.12,.02)) +
  scale_radius("Seeds\ngerminated", range = c(1,5)) +
  labs(x = bquote('Median soil moisture (vwc, '*~m^3*''*~m^-3*')')) +
  theme_bw(base_size = 14) +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = 'bold'))

# Model space plotting----------------------------------------------------------
# Plot a 2-parameter model space based on the top model.

label_df <- complete_df %>% 
  unite(name, fire,aspect, sep = ' ', remove = F) %>% 
  group_by(name, aspect, mois_q50, temp_q50) %>% 
  summarise() 


plotObj <- 
ggplot(complete_df) +
  # geom_point(data = label_df, aes(x = mois_q50, y = temp_q50, color = aspect), 
  #            shape = 16, alpha = 0.3, size = 25, stroke = 1.5) +
  # geom_point(data = label_df, aes(x = mois_q50, y = temp_q50, color = aspect), 
  #            shape = 21, size = 25, stroke = 0.7) +
  geom_point(data = label_df, aes(x = mois_q50, y = temp_q50), 
             shape = 21, color = 'black', fill = 'grey80', alpha = 0.3, size = 16, stroke = 0.7) +
  #scale_color_manual(values = colVals, guide = F) +
  geom_jitter(aes(x = mois_q50, y = temp_q50, fill = Survival, size = Germination * 50),
              shape = 21, width = 0.004, height = 0.2, alpha = 0.8) +
  #geom_text(data = label_df, aes(x = mois_q50/100, y = temp_q50, label = name), nudge_x = -0.001) +
  scale_x_continuous(limits = c(0.033,0.12), breaks = seq(0.03,0.12,0.03)) +
  scale_y_continuous(limits = c(12,18), breaks = seq(12,18,2)) +
  scale_fill_viridis_c('Survival rate') +
  scale_radius("Seeds\ngerminated", range = c(1,10)) +
  facet_wrap(~species) +
  theme_bw(base_size = 14) +
  labs(x = bquote('Median soil moisture (vwc, '*~m^3*''*~m^-3*')'), y = bquote('Median soil temperature ('*~degree *C*')')) +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = 'bold'))

legend <- cowplot::get_legend(plotObj)

plot(legend)

# ------------------------------------------------------------------------------
# # Still need to justify with a pre-hoc test of zero inflation?
# ziglmm_survival <- glmmTMB(Survival ~ species + mois_q50 + temp_q50 + (1|fire),
#                            ziformula =  ~ 1,
#                            family = 'binomial', 
#                            weights = Germination*50,
#                            data = model_df)
# summary(ziglmm_survival)
# my_rsq(ziglmm_survival)
# 

# All-subsets model selection approach... this isn't really working....
# Dredge approach applied to ziglmmm's - takes while to run dredge here 
# library(glmmTMB)
# m <- glmmTMB(Establishment ~ species*(mois_min + mois_q50 + temp_q50 + temp_max) + (1|fire),
#              ziformula = ~ .,
#              family = 'binomial', 
#              weights = Germination, 
#              data = model_df)
# glmm_dredge <- dredge(m, m.lim = c(0,4))
# glmm_dredge
# top_model <- get.models(glmm_dredge, subset = 1)[[1]]
# summary(top_model)
# plot(allEffects(top_model, residuals = TRUE))
# r.squaredGLMM(top_model)
# 
# 
# 
# # Selecting the best GLMM using DEM data ------------------------------------
# ###################################################################################
# # Make a list of formulas, all logical combinations of soil data
# 
# predictors <- c('dev_ne', 'tri', 'slope', 'dem_elev')
# expand.grid(predictors, predictors, predictors, predictors)
# 
# dem_formulas <- 
#   c("established ~ species + (1|fire)", 
#     "established ~ species*(dev_ne + (1|fire))",
#     "established ~ species*(dev_ne + tri + (1|fire))",
#     "established ~ species*(dev_ne + tri + slope + (1|fire))",
#     "established ~ species*(dev_ne + tri + slope + dem_elev + (1|fire))",
#     "established ~ species*(dev_ne + slope + dem_elev, (1|fire))",
#     "established ~ species*(dev_ne + slope + dem_elev, (1|fire))",
#     
#     "established ~ species*(dev_ne + dem_elev + (1|fire))",
#     "established ~ species*(tri + slope + (1|fire))",
#     "established ~ species*(tri + dem_elev + (1|fire))",
#     "established ~ species*(slope + dem_elev + (1|fire))")
# 
# 
# 
# 
# dem_models <- tibble(
#   forms = dem_formulas,
#   model_obj = map(dem_formulas, model_fn),
#   fit = map(model_obj, glance),
#   terms = map(model_obj, tidy),
#   marg_pR2 = map_dbl(map(model_obj, r.squaredGLMM), mean(1:3)),
#   comp_pR2 = map_dbl(map(model_obj, r.squaredGLMM), mean(4:6))) %>% 
#   unnest(fit) %>% 
#   arrange(AIC) 
# dem_models$forms
# 
# summary(dem_models$model_obj[[1]])
# 
# # Fixed Effects of top model
# library(effects)
# plot(allEffects(dem_models$model_obj[[1]], residuals = TRUE))
# 
# # Residuals of top model
# plot(dem_models$model_obj[[1]])
# 
# 
# 
# 
# 
# 
# 
# 
# # There is a Bayesian version, but I got confused quickly. 
# # m <- stan_glmer(count ~ mois_q50 + temp_q50 + tri + (1|species) + (1|fire) + (1|site),
# #                 family = poisson(link = 'log'), 
# #                 data = model_df)
# # summary(m)
# # pairs(m)
# 
# 
# m <- glm(count ~ fire + species + temp_max +  mois_q75 +  dev_ne +  tri, 
#            family = poisson(link = 'log'), 
#            data = model_df)
# 
# summary(m)
# 
# 
# 
# #######
# # Make a table that summarizes the linear models
# # Linear models
# library(broom)
# 
# # results_1 <- complete_df %>% 
# #   mutate(count = as.integer(proportion*50)) %>% 
# #   select(-c(utm_zone, easting, northing)) %>% 
# #   gather(., variable, value, -(site:proportion),-count) %>%
# #   group_by(variable) %>% 
# #   do(tidy(lm(count ~ value, data = .))) %>% # Glance helpful for fit statistics
# #   filter(term == 'value') %>% 
# #   arrange(p.value) %>% 
# #   select(-term) %>% 
# #   mutate_if(is.numeric, round, 2)
# # 
# # results_2 <- complete_df %>% 
# #   mutate(count = as.integer(proportion*50)) %>% 
# #   select(-c(utm_zone, easting, northing)) %>% 
# #   gather(., variable, value, -(site:proportion),-count) %>%
# #   group_by(variable) %>% 
# #   do(glance(lm(count ~ value, data = .))) %>% # Glance helpful for fit statistics
# #   select(r.squared, adj.r.squared, sigma) %>% 
# #   mutate_if(is.numeric, round, 2)
# # 
# # # Save this table to a PDF
# # pdf("lm_predictor_table.pdf", height=11, width=8.5)
# # full_join(results_1, results_2) %>% 
# #   gridExtra::grid.table()
# # dev.off()
# # 
# # 
# # formulas %>% 
# #   map(model_fn) %>% 
# #   map(glance) %>% 
# #   bind_rows() %>% 
# #   mutate(forms = formulas)
# # library(rpart)
# # library(rpart.plot)
# # 
# # cart_model = rpart(Survival ~ species + mois_min + mois_q50 + temp_q50 + temp_max, 
# #                    data = complete_df, 
# #                    method ="class")
# # printcp(cart_model)
# # prp(cart_model)
# # 
# # No interactions, otherwise too many paramters
# global_mod <- 
#   glmer(Survival ~ species + mois_min + mois_dry_hours + mois_q50 + temp_max + temp_hot_hours + temp_q50 + (1|fire),
#         weights = Germination * 50,
#         data = model_df,
#         family="binomial",
#         control=glmerControl(optimizer="bobyqa"),
#         na.action="na.fail")
# 
# glmm_dredge <- dredge(global_mod, m.lim = c(0,5))
# glmm_dredge
# 
# top_model <- get.models(glmm_dredge, subset = 1)[[1]]
# summary(top_model)
# 
# # With interactions using terms from top model
# global_mod <- 
#   glmer(#Survival ~ species*(mois_min+mois_dry_hours+mois_q50+temp_q50) + (1|fire),
#     Survival ~ species*(mois_min + mois_q50 + temp_q50) + (1 | fire),
#     weights = Germination * 50,
#     data = model_df,
#     family="binomial",
#     control=glmerControl(optimizer="bobyqa"),
#     na.action="na.fail")
# 
# summary(global_mod)$AIC
# r.squaredGLMM(global_mod)
# 
# # Fixed Effects of top model
# plot(allEffects(global_mod, residuals = TRUE))
# 
# # Residuals of top model
# plot(top_model)
# r.squaredGLMM(top_model)
# 
# # Read in topographic indices from CSV (done using built-in GDAL DEM tools in QGIS3: https://www.gdal.org/gdaldem.html)
# dem_idx <- read_csv('data/dem_indices.csv') %>% 
#   # Convert aspect to continuous measure per Beers et al. 1966 J. For.
#   mutate(dev_ne = cos( (45*pi/180) - (aspect_deg*pi/180) ) + 1 ) %>% 
#   dplyr::select(-c(utm_zone, easting, northing, aspect_deg)) 
# 
# # Elevation
# dem_idx %>% 
#   group_by(fire) %>% 
#   summarise(m = mean(dem_elev)) %>% 
#   arrange(m)
# 
# 
