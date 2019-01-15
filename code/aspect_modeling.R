source('code/read_seedling_data.R')
# Convert proportions back to counts for GLM------------------------------------
counts <- props_frameID_long %>% 
  mutate(value = as.integer(value * 50))

# ------------------------------------------------------------------------------
# Q1.1: Is germination and survival significantly different among aspects and sites in each species?
# ------------------------------------------------------------------------------

# Generalized linear model (GLM) using a Poisson link function. This model predicts the counts instead of the proporitons.
library(lme4)

glm_aspect <- counts %>%
  group_by(species, period) %>% 
  do(tidy(glmer(value ~ aspect + (1 | fire) + (1 | fire:aspect:frameID), data = ., family = "poisson"))) %>% 
  filter(p.value <= 0.10) 

glm_aspect_glance <- counts %>%
  group_by(species, period) %>% 
  do(glance(glmer(value ~ aspect + (1 | fire:aspect:frameID), data = ., family = "poisson"))) 

temp <- counts %>%
  filter(species == 'pico', period == 'establishment') %>%
  glmer(value ~ aspect + aspect + (1 | fire) + (1 | fire:aspect:frameID), data = ., family = "poisson")

summary(temp)

# For RMarkdown
# Specified as count of germination/survival/establishment as a function of aspect and fire (as fixed effects), with random effect for frame nested in aspect nested in fire. `glmer(value ~ aspect + fire + (1 | fire:aspect:frameID), data = ., family = "poisson")` 
# Unclear: fire is significant when specified as a fixed effect, but not when it is listed as random effect. No significance for nested random effect of frameID. I think I am specifying the model correctly, but not 100%.
# 
# 
# ```{r, echo=FALSE, code = readLines("/Users/tylerhoecker/GitHub/seed_addition_GYE/code/analysis_modeling.R")}
# ```
# 
# ```{r, echo=FALSE}
# knitr::kable(glm_aspect, digits = 3, caption = "GLMM coeffecients")
# knitr::kable(glm_aspect_glance, digits = 3, caption = "GLMM statistics")
# ```



# # THE MODEL - mixed-effects logisitic regression --------------------------
# 
# m <- glm(germinated ~ mois_med + temp_med + air_temp_med + solar_high + rel_hum_high +
#            as.factor(species),
#            data = as.data.frame(estb_abio_df), family = 'poisson')
# summary(m)
# confint(m)
# exp(coef(m))
# 
# 
# m <- glmer(survived ~ mois_med + temp_med + air_temp_med + solar_high + rel_hum_high +
#              (1 | species) + (1 | site),
#            data = as.data.frame(estb_abio_df), family = 'binomial', nAGQ=0)
# summary(m)
# 
# 
# se <- sqrt(diag(vcov(m)))
# # table of estimates with 95% CI
# tab <- cbind(Est = fixef(m), LL = fixef(m) - 1.96 * se, UL = fixef(m) + 1.96 *
#                 se)
# 
# exp(tab)





