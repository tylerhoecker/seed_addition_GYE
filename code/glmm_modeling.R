# Latest and great modeling effort as of 4/30/19
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
library(broom.mixed)


# Import soil data, at this point filter for entire season ---------------------
start_time <- as.POSIXct(c("2018-06-01 00:00:00"))
end_time <- as.POSIXct(c("2018-10-01 00:00:00"))
source('code/read_soil_data.R')

# Filter to germination and survival periods, and calculate median values -----
germ_period <- soil_df %>% 
  # Convert to proportion
  #mutate(value = if_else(variable == 'mois', value/100, value)) %>%
  # Filter to "germination period"
  filter(time >= "2018-06-01 00:00:00" & time <= "2018-06-15 00:00:00") %>% 
  # Calculate the median soil moisture 
  group_by(fire, aspect, variable) %>% 
  summarise(q50_germ = quantile(value, 0.50, na.rm = T)) %>% 
  gather(quant, value, -c(fire,aspect,variable)) %>% 
  unite(temp, variable, quant) %>% 
  spread(temp, value) 

surv_period <- soil_df %>% 
  # Convert to proportion
  #mutate(value = if_else(variable == 'mois', value/100, value)) %>%
  # Filter to "germination period"
  filter(time >= "2018-07-08 00:00:00" & time <= "2018-10-01 00:00:00") %>% 
  # Calculate the median soil moisture 
  group_by(fire, aspect, variable) %>% 
  summarise(q50_surv = quantile(value, 0.50, na.rm = T)) %>% 
  gather(quant, value, -c(fire,aspect,variable)) %>% 
  unite(temp, variable, quant) %>% 
  spread(temp, value) 

soil_preds <- full_join(surv_period, germ_period)

# Percent cover data
pct_cover <- read_csv('data/pct_cover.csv') %>% 
  group_by(fire, aspect) %>% 
  summarise_all(mean) %>% 
  mutate(veg = forb + gram) %>% 
  select(fire, aspect, veg)

ggplot(pct_cover) +
  geom_col(aes(x = paste(fire,aspect), y = veg))

# Import response variables (seedling germination, survival)--------------------
source('code/read_summarize_seedling.R')
seedling_response <- proportions %>% 
  filter(version == 'original') %>% 
  dplyr::select(-version) %>% 
  spread(period, value) 

seedling_response %>% 
  group_by(species, aspect) %>% 
  summarise_if(is.numeric, mean)

# Combine response and predictor variables -------------------------------------
complete_df <-  full_join(seedling_response,soil_preds) %>% 
  full_join(pct_cover) %>% 
  modify_at(c('species'), as.factor) %>% 
  mutate(Weights = 50)


# GLMMs on standardized predictors  --------------------------------------------
rescaled_df <- complete_df %>% 
  ungroup() %>%
  mutate_at(vars(-c(fire,aspect,species,Germination,Survival,Establishment,Weights)), scale) 

# Save the standard deviations used in the rescaling for unstandardization later
sd_all = complete_df %>% 
  ungroup() %>% 
  select(mois_q50_germ, mois_q50_surv, temp_q50_germ, temp_q50_surv, veg) %>% 
  map(., sd, na.rm = T) %>% 
  stack() %>% 
  rename(term = ind, stdevs = values)

# Estimate a glmm of germination, with no zero-inflation component, save results
glmm_germ_df <- glmmTMB(Germination ~ species + mois_q50_germ + temp_q50_germ + veg + (1|fire), 
                        ziformula = ~ 1,
                        family = binomial(link = "logit"), 
                        weights = Weights,
                        data = rescaled_df) %>% 
  tidy(.) %>% 
  mutate(model = 'Germination')

# Estimate a glmm of survival, with a zero-inflation component, save results
glmm_surv_df <- glmmTMB(Survival ~ species + mois_q50_surv + temp_q50_surv + veg + (1|fire), 
                        ziformula = ~ 1,
                        family = binomial(link = "logit"), 
                        weights = Germination*50,
                        data = rescaled_df) %>% 
  tidy(.) %>% 
  mutate(model = 'Survival')
  

# Combine these two models into a single df and change labeling, for plotting
model_results <- full_join(glmm_germ_df, glmm_surv_df) %>% 
  filter(effect != 'ran_pars',
         term != '(Intercept)') %>% 
  full_join(., sd_all) %>% 
  mutate(term = fct_recode(term, 'Species (PSME)' = "speciesPSME",
                                 'Soil moisture' = 'mois_q50_germ',
                                 'Soil moisture' = 'mois_q50_surv',
                                 'Soil temperature' = 'temp_q50_germ',
                                 'Soil temperature' = 'temp_q50_surv',
                                 'Veg. cover' = 'veg')) %>% 
  mutate(term = fct_reorder(term, estimate, max)) %>% 
  mutate(ci_low = estimate - (1.96*std.error),
         ci_high = estimate + (1.96*std.error)) #%>% 
         # Multiplying zero-inflation estimates by -1, to make intuitive for visualization
        # estimate = if_else(component == 'zi', estimate*-1, estimate)) %>% 
  # Change transformation for different interpretation: 'exp' to exponentiate log-odds to odds, 'plogis' to transform log-odds to probability
  #mutate_at(vars(estimate, ci_low, ci_high), plogis) 


# Plotting effect sizes --------------------------------------------------------

# Point-range plots of standard error estimate. 1 or 2 x SE for CI?
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


# Preditcion (models on original scale of predictors) --------------------------

# First, fit the models on unscaled variables ----------------------------------
# Estimate a glmm of germination, with no zero-inflation component, save results
glmm_germ_pred <- glmmTMB(Germination ~ species + mois_q50_germ + temp_q50_germ + veg + (1|fire), 
                        ziformula = ~ 1,
                        family = binomial(link = "logit"), 
                        weights = Weights,
                        data = complete_df) 

# Estimate a glmm of survival, with a zero-inflation component, save results
glmm_surv_pred <- glmmTMB(Survival ~ species + mois_q50_surv + temp_q50_surv + veg + (1|fire), 
                        ziformula = ~ 1,
                        family = binomial(link = "logit"), 
                        weights = Germination*50,
                        data = complete_df) 

# Make a and plot predictions --------------------------------------------------
new_germ <- 
  expand.grid(mois_q50_germ = seq(.02,.18,length.out = 10),
              temp_q50_germ = seq(12,16,length.out = 1000),
              species =  c("PSME", "PICO"),
              fire = NA,
              veg = 25,
              Weights = 50) %>% 
  mutate(pred_probs = predict(glmm_germ_pred, type = 'response', newdata = ., allow.new.levels=TRUE)) %>% 
  mutate(line_group = paste(mois_q50_germ,fire))

germ_plot <- 
ggplot() +
  geom_errorbar(data = complete_df, aes(x = temp_q50_germ, y = Germination, group = fire), width = 0,
                        stat = "summary", fun.ymin = min, fun.ymax = max, fun.y = median) +
  geom_point(data = complete_df, aes(x = temp_q50_germ, y = Germination),
              shape = 21, fill = 'grey50', alpha = 0.7, size = 2.1) +
  geom_line(data = new_germ, aes(x = temp_q50_germ, y = pred_probs, color = mois_q50_germ, group = line_group),
            size = 0.75) +
  facet_wrap(~species) +
  scale_color_distiller(bquote('Median soil \nmoisture (vwc, '*~m^3*''*~m^-3*')'), 
                        palette = 'BrBG', direction = 1) +
  theme_bw(base_size = 14) +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = 'bold'),
        #legend.position =  c(0.65,0.7),
        legend.background = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = margin(t = 10, r = 5, b = 5, l = 10, unit = "pt"),
        legend.justification = 'left')


new_surv <- 
  expand.grid(mois_q50_surv = seq(.02,.18,length.out = 10),
              temp_q50_surv = seq(12,19,length.out = 1000),
              species =  c("PSME", "PICO"),
              fire = NA,#c("Berry-Glade","Berry-Huck","Buffalo","Maple"),
              veg = 25,
              Germination = 0.5) %>% 
  mutate(pred_probs = predict(glmm_surv_pred, type = 'response', newdata = ., allow.new.levels=TRUE))  %>% 
  group_by(species) 

surv_plot <- 
ggplot() +
  geom_errorbar(data = complete_df, aes(x = temp_q50_surv, y = Survival, group = fire), width = 0,
                  stat = "summary", fun.ymin = min, fun.ymax = max, fun.y = median) +
  geom_point(data = complete_df, aes(x = temp_q50_surv, y = Survival, size = Germination*50), 
                  shape = 21, fill = 'grey50', alpha = 0.7) +
  geom_line(data = new_surv, 
            aes(x = temp_q50_surv, y = pred_probs, color = mois_q50_surv, group = mois_q50_surv), 
            size = 0.75) +
  facet_wrap(~species) +
  scale_radius("Seeds\ngerminated", range = c(1,10)) +
  scale_color_distiller(bquote('Median soil \nmoisture (vwc, '*~m^3*''*~m^-3*')'),
                        palette = 'BrBG', direction = 1, guide = F) +
  labs(x = bquote('Median soil temperature ('*~degree *C*')')) +
  scale_x_continuous(breaks = seq(12,19,1)) +
  # labs(x = bquote('Median soil moisture (vwc, '*~m^3*''*~m^-3*')')) +
  # scale_color_distiller(bquote('Median soil temperature ('*~degree *C*')'),
  #                       palette = 'RdBu', direction = -1) +
  theme_bw(base_size = 14) +
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        plot.margin = margin(t = 5, r = 5, b = 10, l = 10, unit = "pt"),
        legend.justification = 'left')

plot_grid(germ_plot, surv_plot, align = 'v', ncol = 1)
  

# Interpretffects on scale of predictors ------------------------------------------
# Estimate a glmm of germination, with no zero-inflation component, save results
model_results %>%
  # For all continuous variable estimates, unstandardize the coeffecient back to original scale by dividing by the standard deviations
  mutate_at(.vars = vars(estimate, ci_low, ci_high),
            .funs = funs(if_else(term != 'Species (PSME)', ./stdevs, estimate))) %>% 
  # Then, exponential the unstandardized estimates (log-odds) to obtain the odds [ratio]
  mutate_at(.vars = vars(estimate, ci_low, ci_high),
                  .funs = funs(exp))

  
  
