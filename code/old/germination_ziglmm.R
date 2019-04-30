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

# Modeling
library(MuMIn) # For psuedo-r-squared 
library(lme4)  # For glmer
library(effects) # For glmm effects plotting
library(glmmTMB) # For zero-inflated

# Rescale predictors
model_df <- complete_df %>% 
  mutate_at(vars(-c(fire,aspect,species,Germination,Survival,Establishment)), scale) 


m <- glmmTMB(Survival ~ species*(mois_min + mois_q50 + temp_q50 + temp_max) + mois_q50:temp_q50 + (1|fire),
             ziformula = ~ .,
             family = 'binomial', 
             weights = Germination * 50, 
             data = model_df)

# Determine best model, based on AIC, with 0-4 terms. Takes a while.
glmm_dredge <- dredge(m, m.lim = c(0,5))
glmm_dredge
top_model <- get.models(glmm_dredge, subset = 1)[[1]]
summary(top_model)
plot(allEffects(top_model, residuals = TRUE))
r.squaredGLMM(top_model)


