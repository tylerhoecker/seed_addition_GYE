library(tidyverse)
# Run seedling import/summary scripts. Creates `germination`, `final`, `proprotions`, `seed_per`.
source('code/read_summarize_seedling.R')

# Set up as proportions and add transformed verions
proportions_frame <- full_join(germination, survival) %>% 
  group_by(fire, aspect, species, frameID) %>% # Adjust by frame or site: +/- frameID
  summarise(Germination = sum(germinated, na.rm = T) / n(),
            Survival_yr1 = sum(survived_yr1, na.rm = T) / sum(germinated, na.rm = T),
            Survival_yr2 = sum(survived_yr2, na.rm = T) / sum(germinated, na.rm = T),
            Establishment_yr1 = sum(survived_yr1, na.rm = T) / n(),
            Establishment_yr2 = sum(survived_yr2, na.rm = T) / n()) %>% 
  group_by(fire, aspect, species) %>% 
  summarise_if(is.numeric, mean, na.rm = T) %>% 
  gather(period, value, Germination, Survival_yr1, Survival_yr2, Establishment_yr1, Establishment_yr2) %>% 
  
  # Transform data, then show both ways (all fires and aspects together for clarity)
  # Using arsine-square-root transform per Ives 2018 sensu Larson and Marx 1981
  mutate(asinsqrt = asin(sign(value) * sqrt(abs(value))),
         logit = car::logit(value, adjust=0)) %>% # OR? logit = log( (value/(1-value)) )
  rename(original = value) %>% 
  gather(version, value, original, asinsqrt, logit) %>% 
  mutate(period = fct_relevel(period, 'Germination','Survival_yr1', 'Survival_yr2','Establishment_yr1','Establishment_yr2')) %>% 
  filter(version == 'asinsqrt') 


# Aggregate to site level and perform traditional two-way anova by aspect
proportions_frame %>%      
  group_by(species, period) %>% 
  # do(tidy(cld(glht(aov(value ~ aspect, data = .), linfct = mcp(aspect = 'Tukey')))))  
  do(tidy(aov(value ~ aspect, data = .))) %>% 
  filter(term != 'Residuals') 

# ------------------------------------------------------------------------------
# THIS DOESN'T WORK ANYMORE...

  
# Don't aggregate, estimate linear mixed model on transformed response using site as random effect
# Interestingly, this model can be applied to the original response scale and produces the same result
proportions %>%
  filter(version == 'asinsqrt') %>% 
  group_by(species, period) %>% 
  # do(tidy(cld(glht(lmer(value ~ aspect + (1|site), data = .), linfct = mcp(aspect = 'Tukey'))))) %>%
  do(tidy(Anova(lmer(value ~ fire + (1|site), data = .)))) %>%
  mutate_if(is.numeric, round, 4) 



# Don't aggregate estimate generalized linear model on original data using binomial/logit link
# I think there may be problems with this approach because of the zeros
full_join(germination, final) %>% 
  mutate(site = paste(fire,aspect, sep = '_')) %>% 
  group_by(site, fire, aspect, species, frameID) %>% # Adjust by frame or site: +/- frameID
  summarise(Germination = sum(germinated) / n(),
            Survival = sum(final) / sum(germinated),
            Establishment = Germination * Survival) %>%
  mutate(germ_estab_Weights = as.integer(50),
         surv_Weights = as.integer(Germination * 50)) %>% 
  gather(period, value, Germination, Survival, Establishment) %>% 
  mutate(period = fct_relevel(period, 'Germination','Survival','Establishment'),
         Weights = if_else(period %in% c('Germination', 'Establishment'), germ_estab_Weights, surv_Weights)) %>% 
  group_by(species, period) %>% 
  do(tidy(Anova(glmer(value ~ aspect + (1|site),
                        data = ., family = binomial(link = "logit"), weights = Weights),
                test.statistic = 'Chisq'))) %>%
  # do(tidy(cld(glht(glmer(value ~ fire + (1|site),
  #                       data = ., family = binomial(link = "logit"), weights = Weights),
  #                      linfct = mcp(fire = 'Tukey'))))) %>%
  mutate_if(is.numeric, round, 4) 










