library(tidyverse)
library(DescTools)
library(MuMIn)
library(broom)

# Predictor variables ----------------------------------------------------------

# Import soil data, at this point filter for entire season 
start_time <- as.POSIXct(c("2018-06-01 00:00:00"))
end_time <- as.POSIXct(c("2019-10-01 00:00:00"))
source('code/read_soil_data.R')

# Filter to germination and survival periods, and calculate median values 
soil_predictors <- soil_df %>%
  ungroup() %>% 
  #spread(variable, value) %>% 
  mutate(period = if_else(time >= "2018-06-01 00:00:00" & time <= "2018-06-15 00:00:00", 'germ',
                          if_else(time >= "2018-07-01 00:00:00" & time <= "2018-10-01 00:00:00", 'year1', 
                                  if_else(time >= "2019-06-01 00:00:00" & time <= "2019-10-01 00:00:00", 'year2',
                                          'winter')))) %>% 
  # Calculate the median soil moisture 
  group_by(fire, aspect, variable, period) %>% 
  summarise(value = quantile(value, 0.50, na.rm = T)) %>% 
  filter(period != 'winter') %>% 
  unite(var_per, period, variable, sep = '_') %>% 
  spread(var_per, value) 

# Percent cover data
pct_cover <- read_csv('data/pct_cover.csv') %>% 
  group_by(fire, aspect) %>% 
  summarise_all(mean) %>% 
  mutate(veg = forb + gram) %>% 
  dplyr::select(fire, aspect, veg)

# Soil properties/texture
soil_props <- read_csv('data/soil_properties.csv') 

# Site info 
terrain <- read_csv('data/terrain_idx.csv')

# Import response variables (seedling germination, survival)--------------------
source('code/read_summarize_seedling.R')

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
  full_join(soil_predictors) %>% 
  full_join(pct_cover) %>% 
  full_join(soil_props) %>%
  full_join(terrain) %>% 
  modify_at(c('fire','species','texture'), as.factor) %>% 
  dplyr::select(fire, aspect, species, 
                starts_with('germ'), starts_with('Surv'), starts_with('Estab'), 
                starts_with('year1'), starts_with('year2'), 
                N, organic, veg)

temp <- 
  complete_df %>%
  group_by(species, aspect) %>%
  summarise_at(vars(Germination, Survival_yr1, Survival_yr2, Establishment_yr2), mean, na.rm = T) 


# vars(germ_temp, germ_mois, year1_temp, year1_mois, year2_temp, year2_mois)
# round(temp[2,-1] - temp[3,-1], 3)

complete_df %>% 
  group_by(fire, aspect) %>% 
  summarise_if(is.numeric, mean, na.rm = T) %>% 
  full_join(soil_props) %>%  
  dplyr::select(fire, aspect, veg, Sand, Silt, Clay, organic, N, germ_mois, germ_temp, year1_mois, year1_temp, year2_mois, year2_temp) %>% 
  write_csv(., 'predictor_table.csv')


#Scatter plots
library(GGally)
complete_df %>%
  filter(species == 'PSME') %>%
  dplyr::select(germ_mois, germ_temp, year1_mois, year1_temp, year2_mois, year2_temp, N, organic, veg) %>%
  as.matrix() %>% 
  Hmisc::rcorr()
  # ggpairs()

# Rescale data
rescaled_df <- complete_df %>%
  mutate_at(vars(germ_mois, germ_temp, year1_mois, year1_temp, year2_mois, year2_temp, N, organic, veg),  ~ scale(.)[,1]) %>% 
  as.data.frame(stringsAsFactors = F)
# Generalized linear models (logit link) ---------------------------------------

# Model selection for eah response (separated because possible predictors and weights vary by response) ---------------------------------------------------------------------
#average_models <- function(x){

models_df <- data.frame(expand.grid('period' = c('Germination','Survival_yr1', 'Survival_yr2','Establishment_yr1','Establishment_yr2'),
                                    'species' = c('PICO','PSME'), stringsAsFactors = FALSE),
                        form = rep(c('Germination ~ germ_temp + germ_mois + organic',
                                     'Survival_yr1 ~ year1_temp + year1_mois + organic + N + veg',
                                     'Survival_yr2 ~ year1_temp + year1_mois + year2_temp + year2_mois + organic + N + veg',
                                     'Establishment_yr1 ~ year1_temp + year1_mois + organic + N + veg',
                                     'Establishment_yr2 ~ year1_temp + year1_mois + year2_temp + year2_mois + organic + N + veg'),2),
                        weights = c('germ_estab_Weights', 'surv_Weights', 'surv_Weights','germ_estab_Weights','germ_estab_Weights'), 
                        stringsAsFactors = FALSE) 

delta_val <- 2

model_avg <- function(x, dataframe){
  
  # Subset dataframe to species
  data_spp <- dataframe[dataframe[,'species'] == x[['species']],]
  
  # Remove NA's where neccessary - dredge doesn't work if data lengths are different among models
  if (x[['period']] == 'Germination'){
    model_data <- as.data.frame(data_spp[!is.na(data_spp[,'germ_mois']),])
    exclude_terms <- expression(germ_temp || germ_mois || organic)
  } else if (x[['period']] == 'Survival_yr1'){
    model_data <- data_spp[!is.na(data_spp[,'Survival_yr1']),]
    exclude_terms <- expression(!( (N && organic) ))
    } else if (x[['period']] == 'Survival_yr2'){
    model_data <- data_spp[!is.na(data_spp[,'Survival_yr2']) & !is.na(data_spp[,'year2_mois']),]
    exclude_terms <- expression(!( (year1_temp && year2_temp) || (year1_temp && year2_mois) || (year2_temp && year2_mois) ))
  } else if (x[['period']] == 'Establishment_yr1'){
    model_data <- data_spp
    exclude_terms <- expression(!( (N && organic) ))
    } else if (x[['period']] == 'Establishment_yr2'){
    model_data <- data_spp[!is.na(data_spp[,'year2_mois']),]
    exclude_terms <- expression(!( (year1_temp && year2_temp) || (year1_temp && year2_mois) || (year2_temp && year2_mois) ))
  } else{
    stop("FAIL! - Period is incorrect")
  }
  
  # Get model formula and weights from master data frame
  model_form <- as.formula(x[['form']])
  model_data$model_Ws <- model_data[,x[['weights']]]
  
  # Fit the global model
  global_model <- glm(formula = model_form, data = model_data,
                      family = binomial(link = "logit"), weights = model_Ws, 
                      na.action = 'na.fail')
  
  # Save p-values from global model
  global_LRT <- anova(global_model, test="LRT")
  LRT_Ps <- data.frame('period' =  x[['period']],
                       'species' = x[['species']],
                       'term' = rownames(global_LRT)[-1],
                       'LRT_p' = round(global_LRT[,'Pr(>Chi)'][-1], 3),
                       stringsAsFactors = F)

  # Perform all-subsets model selection based on AICc, min 1 max 3 predictors
  exclude_terms <- expression(!( (year1_temp && year2_temp) || 
                                 (year1_temp && year2_mois) ||
                                 (year2_temp && year2_mois) ))
  
  if (x[['period']] == 'Germination'){
    dredge_obj <- dredge(global_model, 
                         m.lim = c(1,2), 
                         rank = 'AICc')
  } else if (x[['period']] == 'Survival_yr1'){
    dredge_obj <- dredge(global_model, 
                         m.lim = c(1,2), 
                         rank = 'AICc')
  } else if (x[['period']] == 'Survival_yr2'){
    dredge_obj <- dredge(global_model, 
                         m.lim = c(1,2), 
                         rank = 'AICc',
                         subset = exclude_terms)    
  } else if (x[['period']] == 'Establishment_yr1'){
    dredge_obj <- dredge(global_model, 
                         m.lim = c(1,2), 
                         rank = 'AICc')
  } else if (x[['period']] == 'Establishment_yr2'){
    dredge_obj <- dredge(global_model, 
                         m.lim = c(1,2), 
                         rank = 'AICc',
                         subset = exclude_terms)  
  } else{
    stop("FAIL! - Period is incorrect")
  }
  
  print(paste0(x[['period']],'-', x[['species']]))
  print(dredge_obj)

  if( length(which(dredge_obj[,'delta'] < delta_val)) > 1 ){
    avg_obj <- model.avg(dredge_obj, subset = delta < delta_val)
  }else{
    avg_obj <- get.models(dredge_obj, subset = delta < delta_val)[[1]]
  }

  print(avg_obj)

  results <-  data.frame(
    'period' = x[['period']],
    'species' = x[['species']],
    'term' = row.names(coefTable(avg_obj)),
    'estimate' = coefTable(avg_obj)[,'Estimate'],
    'lower' = confint(avg_obj)[,1],
    'upper' = confint(avg_obj)[,2])

  results <- full_join(results, LRT_Ps)

  return(results)
}  

model_results_l <- apply(models_df, 1, model_avg, dataframe = rescaled_df)
model_results <- bind_rows(model_results_l) 

model_results %>% 
  dplyr::select(species, period, term, estimate, lower, upper, LRT_p) %>% 
  mutate_if(is.numeric, round, 3) %>% 
  mutate(lRT_p = ifelse(LRT_p < 0.001, '< 0.001', LRT_p)) %>% 
  filter(term != '(Intercept)') %>% 
  mutate(term = factor(term, 
                       levels = c('N','veg','organic','year2_temp','year1_temp','germ_temp','year2_mois','year1_mois','germ_mois'),
                       labels = c('N','Veg.','SOM','Temp. (yr 2)','Temp. (yr 1)','Temp. (germ.)','Mois. (yr 2)','Mois. (yr 1)','Mois. (germ.)'))) %>% 
  write_csv(., 'glm_model_avging_results.csv')



# Combine model info and plot --------------------------------------------------
plot_results <- model_results %>% 
  filter(term != '(Intercept)') %>% 
  mutate(term = factor(term, 
                       levels = c('N','veg','organic','year2_temp','year1_temp','germ_temp','year2_mois','year1_mois','germ_mois'),
                       labels = c('N','Veg.','SOM','Temp. (yr 2)','Temp. (yr 1)','Temp. (germ.)','Mois. (yr 2)','Mois. (yr 1)','Mois. (germ.)')),
         period = fct_relevel(period, 'Germination','Survival_yr1','Survival_yr2','Establishment_yr1','Establishment_yr2')) %>% 
  # group_by(period, term) %>% 
  # summarise(estimate = mean(estimate, na.rm = T),
  #           lower = mean(lower, na.rm = T),
  #           upper = mean(upper, na.rm = T)) %>% 
  # ungroup() %>% 
  mutate(period = factor(period, 
                         levels = c('Germination','Survival_yr1','Survival_yr2','Survival_1to2','Establishment_yr1','Establishment_yr2'),
                         labels = c('Germination','Survival (yr 1)','Survival (yr 2)','Survival_1to2','Establishment_yr1','Establishment'))) %>% 
  filter(period != 'Establishment_yr1', term != 'N') %>%
  mutate(estimate_lab = ifelse(LRT_p < 0.01, estimate, NA))


plot_results[plot_results == -Inf] = NA
plot_results[plot_results == Inf] = NA
  #mutate_at(vars(estimate, lower, upper), exp) %>% 

#colors <- c("#0072B2","#D55E00") purple = '#9900CC',
colors <- c('#4daf4a','#996633','#cc3300','#cc3300','#cc3300','#3385ff','#3385ff','#3385ff','#3385ff')


# Plot summarized terms
ggplot(plot_results, aes(x = term, y = estimate, fill = term, shape = species)) +
  # geom_blank(aes(ymin = lower, ymax = upper),
  #   size = 0.5, width = 0, position=position_dodge(width=0.7)) +
  geom_errorbar(aes(ymin = lower, ymax = upper, width = 0), position = position_dodge(width=0.7), size = 0.7) +
  geom_point(size = 3, position = position_dodge(width=0.7)) +
  # geom_text(aes(size = 2, y = lower-0.6, label = round(estimate_lab,2)), 
  #           show.legend = F, position= position_dodge(width=0.8)) +
  geom_hline(aes(yintercept = 0), linetype = 'dashed') +
  scale_shape_manual('Species', values = c(21, 24), labels = c('Lodgepole pine', 'Douglas-fir')) +
  scale_x_discrete(labels = c(mois_q50 = 'Soil mois.',
                              temp_q50 = 'Soil temp.',
                              veg = 'Veg. cover',
                              organic = 'SOM')) +
  coord_flip(ylim = c(-2.5, 2.5)) + 
  theme_bw(base_size = 16) +
  labs(y = 'Effect size (log-odds)', x = '') +
  facet_wrap(~period, nrow = 1) +
  scale_fill_manual(values = colors, guide = F) +
  theme(title = element_text(size = 12)) +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = 'bold'),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text(face = 'bold'))



# ODDS RATIOS ---------------------------------------------------------
# Re-Do model averaging with unscaled predictors

# Change this one at a time to calculate odds ratio results
library(oddsratio)

delta_val <- 2

odds_ratio_fn <- function(x, dataframe){
  
  # Subset dataframe to species
  data_spp <- dataframe[dataframe[,'species'] == x[['species']],]
  
  # Remove NA's where neccessary - dredge doesn't work if data lengths are different among models
  if (x[['period']] == 'Germination'){
    model_data <- as.data.frame(data_spp[!is.na(data_spp[,'germ_mois']),])
  } else if (x[['period']] == 'Survival_yr1'){
    model_data <- data_spp[!is.na(data_spp[,'Survival_yr1']),]
  } else if (x[['period']] == 'Survival_yr2'){
    model_data <- data_spp[!is.na(data_spp[,'Survival_yr2']) & !is.na(data_spp[,'year2_mois']),]
  } else if (x[['period']] == 'Establishment_yr1'){
    model_data <- data_spp
  } else if (x[['period']] == 'Establishment_yr2'){
    model_data <- data_spp[!is.na(data_spp[,'year2_mois']),]
  } else{
    stop("FAIL! - Period is incorrect")
  }
  
  # Get model formula and weights from master data frame
  model_form <- as.formula(x[['form']])
  model_data$model_Ws <- model_data[,x[['weights']]]
  
  # Fit the global model
  global_model <- glm(formula = model_form, data = model_data, na.action = 'na.fail', 
                      family = binomial(link = "logit"), weights = model_Ws)
  
  # Perform all-subsets model selection based on AICc, min 1 max 3 predictors
  dredge_obj <- dredge(global_model, m.lim = c(1,2), rank = 'AICc')
  
  if( length(which(dredge_obj[,'delta'] < delta_val)) > 1 ){
    avg_obj <- model.avg(dredge_obj, subset = delta < delta_val, fit = T)
    }else{
    avg_obj <- get.models(dredge_obj, subset = delta < delta_val)[[1]]
    }
  
  or_df <- 
    coefTable(avg_obj) %>% 
    data.frame(term = row.names(.), .,
               unit = NA, 
               period = x[['period']],
               species = x[['species']],
               n_models = length(which(dredge_obj[,'delta'] < delta_val))) %>% 
    mutate(unit = ifelse(term %in% c('germ_mois','year1_mois','year2_mois'), 0.01,
                         ifelse(term == 'veg', 10, 1)),
           odds_ratio = round(exp(Estimate * (unit)), 2)) %>% 
    filter(term != '(Intercept)') %>% 
    dplyr::select(species, period, n_models, term, log_odds = Estimate, odds_ratio, unit)
  
  return(or_df)
}  

or_results_l <- apply(models_df, 1, odds_ratio_fn, 
                      dataframe = as.data.frame(complete_df, stringsasfactors = F)) 

or_results <- bind_rows(or_results_l) 

write_csv(or_results, 'odds_ratio_results.csv')


# Combine the rescaled and original scale model results (log-odds and odds-ratio results)
complete_model_table <- 
  full_join(model_results, or_results) %>% 
  filter(term != '(Intercept)') %>% 
  dplyr::select(species, period, n_models, term, estimate, lower, upper, LRT_p, odds_ratio, unit) %>% 
  write_csv(., 'complete_model_table.csv')


## OLD
# # Response curves...
# prediction <- 
#   expand.grid(mois_q50 = seq(.02,.16,length.out = 10),
#               temp_q50 = seq(12,19,length.out = 1000),
#               Weights = 250) %>% 
#   mutate(pred_probs = predict(test_glm, 
#                               type = 'response', 
#                               newdata = .))  
# 
# ggplot() +
#   geom_point(data = test, aes(x = temp_q50, y = value), 
#              shape = 21, fill = 'grey50', size = 3) +
#   geom_line(data = prediction, 
#             aes(x = temp_q50, y = pred_probs, color = mois_q50, group = mois_q50), 
#             size = 0.75) +
#   facet_wrap(~species) +
#   scale_color_distiller(bquote('Median soil \nmoisture (vwc, '*~m^3*''*~m^-3*')'),
#                         palette = 'BrBG', direction = 1, guide = F) +
#   labs(x = bquote('Median soil temperature ('*~degree *C*')'),
#        y = 'Germination') +
#   annotate('text', x = 12.3, y = 0.8, label = 'PSME', size = 5, fontface = 'bold') +
#   #scale_x_continuous(breaks = seq(12,19,1)) +
#   # labs(x = bquote('Median soil moisture (vwc, '*~m^3*''*~m^-3*')')) +
#   # scale_color_distiller(bquote('Median soil temperature ('*~degree *C*')'),
#   #                       palette = 'RdBu', direction = -1) +
#   theme_bw(base_size = 12) +
#   theme(strip.background = element_blank(),
#         strip.text = element_blank(),
#         plot.margin = margin(t = 5, r = 5, b = 10, l = 10, unit = "pt"),
#         legend.justification = 'left')
# 
# #### old
# germ_models <- rescaled_df %>%
#   # Can't seem to get dredge to work when there are NAs, so have to do germination separate
#   filter(period == 'Germination') %>% 
#   group_by(species, period) %>% 
#   do(dredge(glm(value ~ mois_q50 + temp_q50 + organic,
#                 data = ., 
#                 na.action = 'na.fail', 
#                 family = binomial(link = "logit"), 
#                 weights = Weights),
#             m.lim = c(0,2), 
#             rank = 'AICc')) 
# 
# surv1_models <- rescaled_df %>%
#   # Can't seem to get dredge to work when there are NAs, so have to do germination separate
#   filter(period == 'Survival_yr1') %>% 
#   group_by(species, period) %>% 
#   do(dredge(glm(value ~ mois_q50 + temp_q50 + organic + veg,
#                 data = ., 
#                 na.action = 'na.fail', 
#                 family = binomial(link = "logit"), 
#                 weights = Weights),
#             m.lim = c(0,2), 
#             rank = 'AICc')) 
# 
# estab1_models <- rescaled_df %>%
#   filter(period == 'Establishment_yr1') %>%
#   group_by(species, period) %>%
#   do(dredge(glm(value ~ mois_q50 + temp_q50 + organic + veg,
#                 data = ., 
#                 na.action = 'na.fail', 
#                 family = binomial(link = "logit"), 
#                 weights = Weights),
#             m.lim = c(0,2), 
#             rank = 'AICc')) 
# 
# surv2_models <- rescaled_df %>%
#   # Can't seem to get dredge to work when there are NAs, so have to do germination separate
#   filter(period == 'Survival_yr2') %>% 
#   group_by(species, period) %>% 
#   do(dredge(glm(value ~ mois_q50 + temp_q50 + N + veg,
#                 data = ., 
#                 na.action = 'na.fail', 
#                 family = binomial(link = "logit"), 
#                 weights = Weights),
#             m.lim = c(0,2), 
#             rank = 'AICc')) 
# 
# estab2_models <- rescaled_df %>%
#   filter(period == 'Establishment_yr2') %>%
#   group_by(species, period) %>%
#   do(dredge(glm(value ~ mois_q50 + temp_q50 + organic + N + veg,
#                 data = ., 
#                 na.action = 'na.fail', 
#                 family = binomial(link = "logit"), 
#                 weights = Weights),
#             m.lim = c(0,2), 
#             rank = 'AICc')) 
# 
# 
# top_models <- germ_models %>%  
#   full_join(., surv1_models) %>% 
#   full_join(estab1_models) %>% 
#   full_join(., surv2_models) %>% 
#   full_join(estab2_models) %>% 
#   dplyr::select(species, period, mois_q50, temp_q50, veg, organic, N, df, logLik, AICc, delta) %>%
#   # Keep only models within 2 AIC units, as equally supported 
#   filter(delta <= 2) 
# 
# 
# # Save formulas for all models within 2 AIC units, for each species and response
# model_formulas <- top_models %>% 
#   group_by(species, period) %>% 
#   mutate(modelnum = row_number(delta)) %>% 
#   # This identifies the terms that were used in the models for each species
#   gather(term, estimate, mois_q50, temp_q50, veg, organic, N) %>%
#   filter(!is.na(estimate)) %>%
#   # And then makes a formula out of them, for each species and model
#   group_by(species, period, modelnum) %>% 
#   mutate(form = paste0('value', '~', paste0(term, collapse = '+'))) %>%
#   # Joins them with data
#   full_join(rescaled_df) %>% 
#   # Drop rows with NA (they are mois/temp values during germination period for sites with offline loggers)
#   drop_na()
# # Save this summary of survival model terms
# model_terms <- model_formulas %>% 
#   group_by(species, period, modelnum) %>%
#   do(
#     tidy(
#       model.avg(
#         glm(as.formula(.$form), data = ., na.action = 'na.fail', family = binomial(link = "logit"), weights = Weights)))) %>% 
#   group_by(species, period, term) %>% 
#   # Stop and print here to view un-averaged model coeffecients, SE, P vals
#   # As shown in Table 2
#   # print(n = 50)
#   
#   # Average coeffecients from models with 2 AIC units, to present effect sizes from averages
#   summarize_at(vars(estimate, std.error, statistic, p.value), mean, na.rm = T) %>% 
#   mutate_at(vars(estimate, std.error, statistic, p.value), round, 3)
# 
# model_terms %>% 
#   mutate(exp(estimate)) %>% 
#   mutate((1-exp(estimate))*-1)
# 
# model_stats <- model_formulas %>% 
#   group_by(species, period, modelnum) %>%
#   do(glance(glm(as.formula(.$form),
#                 data = .,
#                 na.action = 'na.fail',
#                 family = binomial(link = "logit"),
#                 weights = Weights))) 
# test <-   lmtest::waldtest(global_model, glm(value ~ 1, data = dataframe, family = binomial(link = "logit"), weights = Weights))
# 
# wald_stats <- model_formulas %>% 
#   group_by(species, period, modelnum) %>%
#   do(lmtest::waldtest(
#     glm(as.formula(.$form), data = ., family = binomial(link = "logit"), weights = Weights),
#     glm(value ~ 1,data = ., family = binomial(link = "logit"), weights = Weights))) 
# 
# model_stats <- model_stats %>% 
#   full_join(., wald_stats) %>% 
#   drop_na() %>% 
#   mutate_if(is.numeric, round, 3)
# 
# full_stuff <- expand.grid(species = c('PICO','PSME'), 
#                           period = c('Germination','Survival_yr1','Survival_yr2','Survival_both','Establishment_yr1','Establishment_yr2'),
#                           term = c('mois_q50','temp_q50','veg','organic'))
# 
# model_info <- model_terms %>% 
#   filter(term != '(Intercept)') %>% 
#   full_join(., full_stuff) %>% 
#   mutate(term = factor(term, levels = c('N','veg','organic','mois_q50','temp_q50')))
# 
# ggplot(model_info, aes(x = term, y = estimate, fill = species)) +
#   geom_errorbar(aes(color = species,
#                     ymin = estimate-std.error*1.96, ymax = estimate+std.error*1.96),
#                 size = 0.5, width = 0.2, position=position_dodge(width=0.7)) +
#   geom_point(shape = 21, size = 2, position=position_dodge(width=0.7)) +
#   geom_text(aes(color = species, y = estimate-std.error-0.45, label = round(estimate,2)),
#             show.legend = F, position= position_dodge(width=0.7)) +
#   geom_hline(aes(yintercept = 0), linetype = 'dashed') +
#   scale_x_discrete(labels = c(mois_q50 = 'Soil mois.',
#                               temp_q50 = 'Soil temp.',
#                               veg = 'Veg. cover',
#                               organic = 'SOM')) +
#   coord_flip(ylim = c(-1.7,2)) +
#   theme_bw(base_size = 14) +
#   labs(y = 'Effect size (log-odds)', x = '') +
#   facet_wrap(~period, nrow = 2) +
#   scale_color_manual('Species', values = colors) +
#   scale_fill_manual('Species', values = colors) +
#   theme(title = element_text(size = 12)) +
#   theme(strip.background = element_blank(),
#         strip.text = element_text(face = 'bold'))
# 
# # PSEUDO R2 FOR LOGISTICS REGRESSION
# # Have to manually create GLM objects to use the pseudo r2 functions
# # test <- rescaled_df %>%
# #   filter(period == 'Germination', species == 'PSME') %>%
# #   drop_na()
# # test_glm <-
# #   glm(value ~ mois_q50 + temp_q50,
# #       data = test,
# #       na.action = 'na.fail',
# #       family = binomial(link = "logit"),
# #       weights = Weights)
# # 
# # test_null <-
# #   glm(value ~ 1,
# #       data = test,
# #       na.action = 'na.fail',
# #       family = binomial(link = "logit"),
# #       weights = Weights)
# # 
# # round(Pseudo.R2(test_glm),2)
# # lmtest::waldtest(test_glm)
# # lmtest::lrtest(test_glm)
# # nagelkerke(test_glm)
# # 
# # 
# # Pseudo.R2(test_glm)
# # PseudoR2(test_glm, which = 'all')
# # hoslem.test(test_glm$y, test_glm$fitted.values)
# 
# 
