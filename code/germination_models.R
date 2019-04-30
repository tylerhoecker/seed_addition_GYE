# Load and prep soil moisture and temperature predictor variables---------------
# Decide on a time period of interest:
start_time <- as.POSIXct(c("2018-06-01 00:00:00"))
end_time <- as.POSIXct(c("2018-06-15 00:00:00"))

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
  modify_at(c('site', 'fire', 'aspect','species'), as.factor) %>% 
  mutate(Weights = 50)

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
  filter(!is.na(mois_q50)) %>% 
  # filter(fire != 'Maple') %>% 
  # mutate_at(vars(-c(fire,aspect,species,germinated,established)), scale) 
  mutate_at(vars(-c(fire,aspect,species,Germination,Survival,Establishment)), scale) %>% 
  mutate(Weights = 50)

# Zero-inflated GLMM using glmmTMB----------------------------------------------
ziglmm_germination <- glmmTMB(Germination ~ species + mois_q50 + temp_q50 + (1 | fire),
                           ziformula =  ~ 1,
                           family = 'binomial', 
                           weights = Weights,
                           data = model_df)
summary(ziglmm_germination)
my_rsq(ziglmm_germination)

model_results <- 
  summary(ziglmm_germination)$coefficients$cond %>% 
  as.tibble(rownames = 'term') %>% 
  rename(sigma = `Std. Error`, coeffecient = Estimate, zval = `z value`, pval = `Pr(>|z|)`) %>% 
  filter(term != '(Intercept)') %>% 
  mutate(term = if_else(term == 'speciesPSME', 'Species (PSME)',
                        if_else(term == 'mois_q50', 'Soil moisture', 
                                if_else(term == 'temp_q50', 'Soil temperature', term))))

ggplot(model_results) +
  geom_pointrange(aes(x = term, y = coeffecient, ymin = coeffecient-2*sigma, ymax = coeffecient+2*sigma)) +
  geom_text(aes(x = term, y = coeffecient, label = round(coeffecient,2)), nudge_x = .3) +
  coord_flip() +
  theme_bw(base_size = 14) +
  labs(y = 'Effect size', x = '', title = 'Germination') +
  theme(title = element_text(size = 12))

# Make plots of predictions/model results
# First, fit the model on unscaled variables
ziglmm_germination <- glmmTMB(Germination ~ species + mois_q50 + temp_q50 + (1 | fire),
                           ziformula =  ~ (1 | fire),
                           family = 'binomial', 
                           weights = Weights,
                           data = complete_df)
summary(ziglmm_germination)
# Make a data frame of predictions
new_data <- 
  expand.grid(mois_q50 = seq(.08,.16,.02),
              temp_q50 = seq(11.5,16,0.25),
              species =  c("PSME", "PICO"),
              fire = c("Berry-Glade","Berry-Huck","Buffalo","Maple"),
              Weights = 50) %>% 
  mutate(pred_probs = predict(ziglmm_germination, type = 'response', newdata = .)) %>% 
  mutate(line_group = paste(mois_q50,fire))

# Plot predictions
ggplot() +
  geom_jitter(data = complete_df, aes(x = temp_q50, y = Germination), 
              shape = 21, fill = 'grey50', alpha = 0.7, size = 2.1) +
  geom_line(data = new_data, aes(x = temp_q50, y = pred_probs, color = mois_q50, group = line_group),
            size = 0.75) +
  facet_wrap(~species) +
  #scale_x_continuous(breaks = seq(0.04,0.12,.02)) +
  scale_color_distiller(bquote('Median soil \nmoisture (vwc, '*~m^3*''*~m^-3*')'), 
                        palette = 'BrBG', direction = 1) +
  #scale_fill_distiller(palette = 'RdBu', guide = F) +
  labs(x = bquote('Median soil temperature ('*~degree *C*')')) +
  theme_bw(base_size = 14) +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = 'bold'))

# Model space plotting----------------------------------------------------------
# Plot a 2-parameter model space based on the top model.

label_df <- complete_df %>% 
  unite(name, fire,aspect, sep = ' ', remove = F) %>% 
  group_by(name, aspect, mois_q50, temp_q50) %>% 
  summarise() 


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

# legend <- cowplot::get_legend(plotObj)
# plot(legend)