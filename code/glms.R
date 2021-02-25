# This code is good as of 7/9/2019 and is based on understanding gain from looking at linear models on transformed responses
library(tidyverse)
library(DescTools)
library(MuMIn)
library(ggrepel)
library(broom)

# Predictor variables ----------------------------------------------------------

# Import soil data, at this point filter for entire season 
start_time <- as.POSIXct(c("2018-06-01 00:00:00"))
end_time <- as.POSIXct(c("2018-10-01 00:00:00"))
source('code/read_soil_data.R')


# Filter to germination and survival periods, and calculate median values 
germ_period <- soil_df %>% 
  # Filter to "germination period"
  filter(time >= "2018-06-01 00:00:00" & time <= "2018-06-15 00:00:00") %>% 
  # Calculate the median soil moisture 
  group_by(fire, aspect, variable) %>% 
  summarise(q50 = quantile(value, 0.50, na.rm = T)) %>% 
  gather(quant, value, -c(fire,aspect,variable)) %>% 
  unite(temp, variable, quant) %>% 
  spread(temp, value) %>% 
  mutate(period = 'Germination')

surv_period <- soil_df %>% 
  # Filter to "survival period"
  filter(time >= "2018-07-08 00:00:00" & time <= "2018-10-01 00:00:00") %>% 
  # Calculate the median soil moisture 
  group_by(fire, aspect, variable) %>% 
  summarise(q50 = quantile(value, 0.50, na.rm = T)) %>% 
  gather(quant, value, -c(fire,aspect,variable)) %>% 
  unite(temp, variable, quant) %>% 
  spread(temp, value) %>% 
  mutate(period = 'Survival')

estab_period <- surv_period %>% 
  mutate(period = 'Establishment')

soil_preds <- full_join(surv_period, germ_period) %>% 
  full_join(estab_period)

soil_preds %>% 
  group_by(aspect, period) %>% 
  summarise_at(vars(mois_q50, temp_q50), funs(mean(., na.rm = TRUE))) %>% 
  filter(period != 'Establishment') %>% 
  arrange(period)

# Percent cover data
pct_cover <- read_csv('data/pct_cover.csv') %>% 
  group_by(fire, aspect) %>% 
  summarise_all(mean) %>% 
  mutate(veg = forb + gram) %>% 
  dplyr::select(fire, aspect, veg)

# Soil properties/texture
soil_props <- read_csv('data/soil_properties.csv') 


# Import response variables (seedling germination, survival)--------------------
source('code/read_summarize_seedling.R')

proportions <- full_join(germination, final) %>% 
  group_by(fire, aspect, species) %>% # Adjust by frame or site: +/- frameID
  summarise(Germination = sum(germinated, na.rm = T) / n(),
            Survival = sum(final, na.rm = T) / sum(germinated, na.rm = T),
            Establishment = Germination * Survival) %>% 
  gather(period, value, Germination, Survival, Establishment) %>% 
  mutate(value = if_else(is.na(value), 0, value)) %>% 
  # Transform data, then show both ways (all fires and aspects together for clarity)
  # Using arsine-square-root transform per Ives 2018 sensu Larson and Marx 1981
  mutate(asinsqrt = asin(sign(value) * sqrt(abs(value))),
         #logit = log( (value/(1-value)) )
         logit = car::logit(value, adjust=0)) %>%
  rename(original = value) %>% 
  gather(version, value, original, asinsqrt, logit) %>% 
  mutate(period = fct_relevel(period, 'Germination','Survival','Establishment'))

seedling_response <- proportions %>% 
  filter(version == 'original') %>% 
  dplyr::select(-version)

std_e <- function(x) sd(x)/sqrt(length(x))

seedling_response %>% 
  group_by(species, period, aspect) %>% 
  summarise(mean = round(mean(value, na.rm = T), 2),
            se = round(sd(value)/sqrt(length(value)),2)) %>% 
  filter(species == 'PSME')

# Combine response and predictor variables -------------------------------------
complete_df <- seedling_response %>%
  spread(period,value) %>% 
  # Add informaiton for weights for binomial regression
  mutate(germ_estab_Weights = as.integer(250),
         surv_Weights = as.integer(Germination * 250)) %>% 
  gather(period, value, Germination, Survival, Establishment) %>% 
  full_join(soil_preds) %>% 
  full_join(pct_cover) %>% 
  full_join(soil_props) %>% 
  modify_at(c('species','period','texture'), as.factor) %>% 
  ungroup() %>% 
  mutate(period = fct_relevel(period, 'Germination','Survival','Establishment'),
         Weights = if_else(period %in% c('Germination', 'Establishment'),
                   germ_estab_Weights, # else (survival):
                   surv_Weights)) %>% 
  dplyr::select(fire, aspect, species, period, value, Weights, mois_q50, temp_q50, veg, organic, N)

rescaled_df <- complete_df %>% 
  ungroup() %>%
  mutate_at(vars(-c(fire,aspect,species,period,value,Weights)), scale) 


# Generalized linear models (logit link) ---------------------------------------

# Model selection for eah response (separated because possible predictors and weights vary by response) ---------------------------------------------------------------------
germ_models <- rescaled_df %>%
  # Can't seem to get dredge to work when there are NAs, so have to do germination separate
  filter(period == 'Germination') %>% 
  drop_na() %>% 
  group_by(species, period) %>% 
  do(dredge(glm(value ~ mois_q50 + temp_q50 + organic,
                data = ., 
                na.action = 'na.fail', 
                family = binomial(link = "logit"), 
                weights = Weights),
            m.lim = c(0,2), 
            rank = 'AICc')) 

surv_models <- rescaled_df %>%
  # Can't seem to get dredge to work when there are NAs, so have to do germination separate
  filter(period == 'Survival') %>% 
  group_by(species, period) %>% 
  do(dredge(glm(value ~ mois_q50 + temp_q50 + N + veg,
                data = ., 
                na.action = 'na.fail', 
                family = binomial(link = "logit"), 
                weights = Weights),
            m.lim = c(0,2), 
            rank = 'AICc')) 

estab_models <- rescaled_df %>%
  filter(period == 'Establishment') %>%
  group_by(species, period) %>%
  do(dredge(glm(value ~ mois_q50 + temp_q50 + organic + N + veg,
                data = ., 
                na.action = 'na.fail', 
                family = binomial(link = "logit"), 
                weights = Weights),
            m.lim = c(0,2), 
            rank = 'AICc')) 

top_models <- germ_models %>%  
  full_join(., surv_models) %>% 
  full_join(estab_models) %>% 
  dplyr::select(species, period, mois_q50, temp_q50, veg, organic, N, df, logLik, AICc, delta) %>%
  # Keep only models within 2 AIC units, as equally supported 
  filter(delta <= 2) 


# Save formulas for all models within 2 AIC units, for each species and response
model_formulas <- top_models %>% 
  group_by(species, period) %>% 
  mutate(modelnum = row_number(delta)) %>% 
  # This identifies the terms that were used in the models for each species
  gather(term, estimate, mois_q50, temp_q50, veg, organic, N) %>%
  filter(!is.na(estimate)) %>%
  # And then makes a formula out of them, for each species and model
  group_by(species, period, modelnum) %>% 
  mutate(form = paste0('value', '~', paste0(term, collapse = '+'))) %>%
  # Joins them with data
  full_join(rescaled_df) %>% 
  # Drop rows with NA (they are mois/temp values during germination period for sites with offline loggers)
  drop_na()

# Save this summary of survival model terms
model_terms <- model_formulas %>% 
  group_by(species, period, modelnum) %>%
  do(tidy(glm(as.formula(.$form),
              data = .,
              na.action = 'na.fail',
              family = binomial(link = "logit"),
              weights = Weights))) %>% 
  group_by(species, period, term) %>% 
  # Stop and print here to view un-averaged model coeffecients, SE, P vals
  # As shown in Table 2
  # print(n = 50)
  
  # Average coeffecients from models with 2 AIC units, to present effect sizes from averages
  summarize_at(vars(estimate, std.error, statistic, p.value), mean, na.rm = T) %>% 
  mutate_at(vars(estimate, std.error, statistic, p.value), round, 3)

model_terms %>% 
  mutate(exp(estimate)) %>% 
  mutate((1-exp(estimate))*-1)

model_stats <- model_formulas %>% 
  group_by(species, period, modelnum) %>%
  do(glance(glm(as.formula(.$form),
                data = .,
                na.action = 'na.fail',
                family = binomial(link = "logit"),
                weights = Weights))) 

wald_stats <- model_formulas %>% 
  group_by(species, period, modelnum) %>%
  do(lmtest::waldtest(
    glm(as.formula(.$form), data = ., family = binomial(link = "logit"), weights = Weights),
    glm(value ~ 1,data = ., family = binomial(link = "logit"), weights = Weights))) 

model_stats <- model_stats %>% 
  full_join(., wald_stats) %>% 
  drop_na() %>% 
  mutate_if(is.numeric, round, 3)


# Combine model info and plot --------------------------------------------------
full_stuff <- expand.grid(species = c('PICO','PSME'), 
                          period = c('Germination','Survival','Establishment'),
                          term = c('mois_q50','temp_q50','veg','organic'))

model_info <- model_terms %>% 
  filter(term != '(Intercept)') %>% 
  full_join(., full_stuff) %>% 
  mutate(term = factor(term, levels = c('veg','organic','mois_q50','temp_q50')))


colors <- c("#0072B2","#D55E00")

# Plot summarized terms

ggplot(model_info, aes(x = term, y = estimate, fill = species)) +
  geom_errorbar(aes(color = species,
                    ymin = estimate-std.error*1.96, ymax = estimate+std.error*1.96),
                size = 0.5, width = 0.2, position=position_dodge(width=0.7)) +
  geom_point(shape = 21, size = 2, position=position_dodge(width=0.7)) +
  geom_text(aes(color = species, y = estimate-std.error-0.45, label = round(estimate,2)),
                  show.legend = F, position= position_dodge(width=0.7)) +
  geom_hline(aes(yintercept = 0), linetype = 'dashed') +
  scale_x_discrete(labels = c(mois_q50 = 'Soil mois.',
                              temp_q50 = 'Soil temp.',
                              veg = 'Veg. cover',
                              organic = 'SOM')) +
  coord_flip(ylim = c(-1.7,2)) +
  theme_bw(base_size = 14) +
  labs(y = 'Effect size (log-odds)', x = '') +
  facet_wrap(~period) +
  scale_color_manual('Species', values = colors) +
  scale_fill_manual('Species', values = colors) +
  theme(title = element_text(size = 12)) +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = 'bold'))


  # PSEUDO R2 FOR LOGISTICS REGRESSION
# Have to manually create GLM objects to use the pseudo r2 functions
# test <- rescaled_df %>%
#   filter(period == 'Germination', species == 'PSME') %>%
#   drop_na()
# test_glm <-
#   glm(value ~ mois_q50 + temp_q50,
#       data = test,
#       na.action = 'na.fail',
#       family = binomial(link = "logit"),
#       weights = Weights)
# 
# test_null <-
#   glm(value ~ 1,
#       data = test,
#       na.action = 'na.fail',
#       family = binomial(link = "logit"),
#       weights = Weights)
# 
# round(Pseudo.R2(test_glm),2)
# lmtest::waldtest(test_glm)
# lmtest::lrtest(test_glm)
# nagelkerke(test_glm)
# 
# 
# Pseudo.R2(test_glm)
# PseudoR2(test_glm, which = 'all')
# hoslem.test(test_glm$y, test_glm$fitted.values)


# Prediction surfaces! ---------------------------------------------------------

# Change this one at a time to calculate odds ratio results
test <- complete_df %>%
  filter(period == 'Establishment', species == 'PSME') %>%
  drop_na()
test_glm <-
  glm(value ~ veg + temp_q50,
      data = test,
      family = binomial(link = "logit"),
      weights = Weights)

or_glm(data = test,
       model = test_glm,
       incr = list(temp_q50 = 1, veg = 10))

prediction <- 
  expand.grid(mois_q50 = seq(.02,.16,length.out = 10),
              temp_q50 = seq(12,19,length.out = 1000),
              Weights = 250) %>% 
  mutate(pred_probs = predict(test_glm, 
                              type = 'response', 
                              newdata = .))  

ggplot() +
  geom_point(data = test, aes(x = temp_q50, y = value), 
             shape = 21, fill = 'grey50', size = 3) +
  geom_line(data = prediction, 
            aes(x = temp_q50, y = pred_probs, color = mois_q50, group = mois_q50), 
            size = 0.75) +
  facet_wrap(~species) +
  scale_color_distiller(bquote('Median soil \nmoisture (vwc, '*~m^3*''*~m^-3*')'),
                        palette = 'BrBG', direction = 1, guide = F) +
  labs(x = bquote('Median soil temperature ('*~degree *C*')'),
       y = 'Germination') +
  annotate('text', x = 12.3, y = 0.8, label = 'PSME', size = 5, fontface = 'bold') +
  #scale_x_continuous(breaks = seq(12,19,1)) +
  # labs(x = bquote('Median soil moisture (vwc, '*~m^3*''*~m^-3*')')) +
  # scale_color_distiller(bquote('Median soil temperature ('*~degree *C*')'),
  #                       palette = 'RdBu', direction = -1) +
  theme_bw(base_size = 12) +
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        plot.margin = margin(t = 5, r = 5, b = 10, l = 10, unit = "pt"),
        legend.justification = 'left')


