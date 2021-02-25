library(tidyverse)
library(DescTools)
library(MuMIn)
library(broom)

# Run data prep script - produces rescaled_df, among other things
source('code/model_data_prep.R')

# Predictor variables ----------------------------------------------------------
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
  
  print(model_data)
  # Get model formula and weights from master data frame
  model_form <- as.formula(x[['form']])
  model_data$model_Ws <- model_data[,x[['weights']]]
  
  # Fit the global model
  global_model <- glm(formula = model_form, data = model_data, na.action = 'na.fail', family = binomial(link = "logit"), weights = model_Ws)
  
  # Save p-values from global model
  global_LRT <- anova(global_model, test="LRT")
  LRT_Ps <- data.frame('period' =  x[['period']],
                       'species' = x[['species']],
                       'term' = rownames(global_LRT)[-1],
                       'LRT_p' = round(global_LRT[,'Pr(>Chi)'][-1], 3),
                       stringsAsFactors = F)

  # Perform all-subsets model selection based on AICc, min 1 max 3 predictors
  dredge_obj <- dredge(global_model, m.lim = c(1,2), rank = 'AICc')
  print(paste0(x[['period']],'-', x[['species']]))
  print(dredge_obj)

  if( length(which(dredge_obj[,'delta'] < delta_val)) > 1 ){
    avg_obj <- model.avg(dredge_obj, subset = delta < delta_val)
    coefs <- avg_obj$coefficients[2,]
  }else{
    avg_obj <- get.models(dredge_obj, subset = delta < delta_val)[[1]]
    coefs <- avg_obj$coefficients
  }

  print(avg_obj)
  
  results <-  data.frame(
    'period' = x[['period']],
    'species' = x[['species']],
    'term' = names(coefs),
    'estimate' = unname(coefs),
    'lower' = confint(avg_obj)[,1],
    'upper' = confint(avg_obj)[,2],
    'AICc' = AICc(global_model))

  rownames(results) <- c()

  results <- full_join(results, LRT_Ps)

  return(results)
}  

#model_avg(x = models_df[5,], dataframe = rescaled_df)

model_results_l <- apply(models_df, 1, model_avg, dataframe = rescaled_df)

model_results <- bind_rows(model_results_l) 


# Combine model info and plot --------------------------------------------------
plot_results <- model_results %>% 
  filter(term != '(Intercept)') %>% 
  mutate(term = factor(term, 
                       levels = c('N','veg','organic','year2_temp','year1_temp','germ_temp','year2_mois','year1_mois','germ_mois'),
                       labels = c('N','Veg.','SOM','Temp. (Yr 2)','Temp. (Yr 1)','Temp. (Germ)','Mois. (Yr 2)','Mois. (Yr 1)','Mois. (Germ.)')),
         period = fct_relevel(period, 'Germination','Survival_yr1','Survival_yr2','Establishment_yr1','Establishment_yr2'),
         period = fct_recode(period, 'Germination' = 'Germination', 
                             'Survival (Yr 1)' = 'Survival_yr1',
                             'Survival (Yr 2' = 'Survival_yr2',
                             'Establishment (Yr 2)' = 'Establishment_yr2')) %>% 
  group_by(period, term) %>% 
  summarise(estimate = mean(estimate, na.rm = T),
            lower = mean(lower, na.rm = T),
            upper = mean(upper, na.rm = T)) %>% 
  filter(period != 'Establishment_yr1')

plot_results[plot_results == NaN] = NA
plot_results[plot_results == NaN] = NA
  #mutate_at(vars(estimate, lower, upper), exp) %>% 

#colors <- c("#0072B2","#D55E00")
colors <- c('#9900cc','#4daf4a','#996633','#cc3300','#cc3300','#cc3300','#3385ff','#3385ff','#3385ff')


# Plot summarized terms
ggplot(plot_results, aes(x = term, y = estimate, fill = term)) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
    size = 0.5, width = 0, position=position_dodge(width=0.7)) +
  geom_point(shape = 21, size = 3, position = position_dodge(width=0.7)) +
  geom_text(aes(#group = species,
    size = 2, y = estimate-0.5,
    label = round(estimate,2)),
    show.legend = F, position= position_dodge(width=1.5)) +
  geom_hline(aes(yintercept = 0), linetype = 'dashed') +
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
        panel.grid.minor = element_blank())

