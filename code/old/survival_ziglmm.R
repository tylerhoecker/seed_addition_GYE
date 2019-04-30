# Load and prep soil moisture and temperature predictor variables---------------
# Decide on a time period of interest:
start_time <- as.POSIXct(c("2018-06-01 00:00:00"))
end_time <- as.POSIXct(c("2018-10-01 00:00:00"))

source('code/soil_data_prep.R') # creates `soil_df` (complete) and `soil_preds` (summarized)

# Response variables
source('code/read_summarize_seedling.R')
seedling_response <- proportions %>% 
  filter(version == 'original') %>% 
  select(-version) %>% 
  spread(period, value) 


# Create a dataframe that combines the seedling responses, measured soil predictors, and DEM stuff
complete_df <- seedling_response %>% 
  full_join(., soil_preds, by = c('fire','aspect')) %>% 
  ungroup() %>% 
  modify_at(c('site', 'fire', 'aspect','species'), as.factor) 

library(MuMIn) # For psuedo-r-squared 
library(lme4)  # For glmer
library(effects) # For glmm effects plotting
library(glmmTMB) # For zero-inflated

# Rescale predictors
model_df <- complete_df %>% 
  mutate_at(vars(-c(fire,aspect,species,Germination,Survival,Establishment)), scale) 

# Selecting the best GLMM using sensor data ------------------------------------
# Zero-inflated GLMM using glmmTMB-----------------------------------------------
# Still need to justify with a pre-hoc test of zero inflation.
# What does it mean that ZI model is just intercept?
# Parameter estimates are almost identical to regular glmm.

# Dredge approach applied to ziglmmm's - takes while to run dredge here.
library(glmmTMB)
m <- glmmTMB(Survival ~ species*(mois_min + mois_q50*temp_q50 + temp_max) + (1|fire),
             ziformula = ~ .,
             family = 'binomial', 
             weights = Germination*50, 
             data = model_df)

# Determine best model, based on AIC, with 0-4 terms.
glmm_dredge <- dredge(m, m.lim = c(0,5))
glmm_dredge
top_model <- get.models(glmm_dredge, subset = 1)[[1]]
summary(top_model)
plot(allEffects(top_model, residuals = TRUE))
r.squaredGLMM(top_model)

# The best ziglmm model
# The minimum moisture term is hard to understand... remove on the basis of covariance?
best_m <- glmmTMB(Survival ~ species + mois_min + mois_q50 + temp_q50 + (1 | fire),
             ziformula =  ~ (1 | fire),
             family = binomial(link="logit"), 
             weights = Germination*50, 
             data = model_df)
best_m$sdr
summary(best_m)$AIC
plot(allEffects(best_m, residuals = TRUE))


library(mgcv)

gamsin <- gam(Survival ~ species + s(mois_min) + s(mois_q50) + s(temp_q50) + s(temp_max),
              family = binomial(link="logit"), 
              weights = Germination*50, 
              data = model_df)

summary(gamsin)
plot(gamsin)
# Plot the effect sizes
ziglmm_efs <- summary(best_m)$coefficients$cond %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "effect") %>% 
  as.tibble() %>% 
  rename(effect_size = Estimate, error = `Std. Error`, z_val = `z value`, p_val = `Pr(>|z|)`) %>% 
  filter(effect != '(Intercept)') 

ggplot(ziglmm_efs, aes(reorder(effect,-effect_size), y = effect_size)) +
  geom_pointrange(aes(ymin = effect_size - error, ymax = effect_size + error)) +
  theme_bw() +
  labs(x = 'Effect size', y = '')


ziglmm_df <- allEffects(best_m, residuals = TRUE, ) %>% 
  as.data.frame() %>% 
  bind_rows(.id = 'term') %>% 
  as.tibble() %>% 
  !is.na(species,mois_min,mois_q50,temp_q50)

Effect(best_m)
