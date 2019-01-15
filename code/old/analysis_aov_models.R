source('code/read_summarize_plot_seedling.R')
# ------------------------------------------------------------------------------
# Q1.1: Is germination, survival and establishment significantly different among aspects and sites in each species?
# Anwer: Yes, between N-S for PICO @ 0.01, F-S for PICO @ 0.05, and N-S for PSME at 0.05.
# ------------------------------------------------------------------------------

# Transform data, then show both ways (all fires and aspects together for clarity)
# Using arsine-square-root transform per Ives 2018 sensu Larson and Marx 1981
proportions <- proportions %>% 
  mutate(asinsqrt = asin(sqrt(value))) %>%
  rename(original = value) %>% 
  gather(version, value, asinsqrt, original)

# Plot distributions of values
ggplot(proportions) +
  geom_histogram(aes(x = value), binwidth = 0.05, fill = 'grey10') +
  facet_grid(version~species+period) +
  coord_cartesian(ylim = c(0,30)) +
  theme_bw(base_size = 12)


# Now do ANOVA and Tukey's Multiple comparisons --------------------------------
library(broom)
# One-way ANOVA test for differences in means among aspects, for each species: PICO different, PSME not
aov_aspect_fire_pico <- proportions %>%
  filter(version == 'asinsqrt') %>% 
  group_by(species, period) %>% 
  do(tidy(aov(value ~ aspect*fire, data = .))) %>% 
  filter(species == 'pico')

aov_aspect_fire_psme <- proportions %>%
  filter(version == 'asinsqrt') %>% 
  group_by(species, period) %>% 
  do(tidy(aov(value ~ aspect*fire, data = .))) %>% 
  filter(species == 'psme')

# Tukey multiple pairwise comparisons
tukey_aspect <- proportions %>%
  filter(version == 'asinsqrt') %>% 
  group_by(species, period) %>% 
  do(tidy(TukeyHSD(aov(value ~ aspect, data = .)))) 

tukey_aspect_cld <- proportions %>%
  filter(version == 'asinsqrt') %>% 
  group_by(species, period) %>% 
  do(tidy(cld(glht(aov(value ~ aspect, data = .), linfct = mcp(aspect = 'Tukey')))))  

tukey_fire <- proportions %>%
  filter(version == 'asinsqrt') %>% 
  group_by(species, period) %>% 
  do(tidy(TukeyHSD(aov(value ~ fire, data = .)))) 

tukey_fire_cld <- proportions %>%
  filter(version == 'asinsqrt') %>% 
  group_by(species, period) %>% 
  do(tidy(cld(glht(aov(value ~ fire, data = .), linfct = mcp(fire = 'Tukey')))))  


# Visualize  general pattern ----------------------------------------------------
colVals <- c('Flat' = '#009E73','North' = '#0072B2','South' = '#E69F00')

# Full breakdown (, aspect, period)
dodge <- position_dodge(width = 0.9)

ggplot(proportions, aes(x = fire, y = value, fill = aspect, group = aspect)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") + 
  stat_summary(fun.data = mean_se, geom = "errorbar", position = dodge, width = 0.25) +
  scale_fill_manual(values = colVals, name = 'aspect') +
  facet_grid(species~period) +
  coord_cartesian(ylim = c(0,0.8)) +
  theme_bw(base_size = 14) +
  theme(strip.background = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


# By aspect, sites group (aspect, period)
ggplot(proportions) +
  stat_summary(aes(x = aspect, y = value, fill = aspect, group = aspect),
               fun.y = mean, geom = "bar", position = "dodge") + 
  stat_summary(aes(x = aspect, y = value, fill = aspect, group = aspect),
               fun.data = mean_se, geom = "errorbar", position = dodge, width = 0.25) +
  geom_text(data = tukey_aspect_cld, aes(x = lhs, y = -0.05, label = letters), fontface = 'bold') +
  scale_fill_manual(values = colVals, name = 'aspect') +
  facet_grid(species~period) +
  coord_cartesian(ylim = c(-0.06,0.81)) +
  theme_bw(base_size = 14) +
  theme(strip.background = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


# By fire (fire, period)
ggplot(proportions, aes(x = fire, y = value)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") + 
  stat_summary(fun.data = mean_se, geom = "errorbar", position = dodge, width = 0.25) +
  geom_text(data = tukey_fire_cld, aes(x = lhs, y = -0.05, label = letters), fontface = 'bold') +
  facet_grid(species~period) +
  coord_cartesian(ylim = c(-0.06,1)) +
  theme_bw(base_size = 14) +
  theme(strip.background = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))



# # Multiple linear regression
# mlr_aspect <- proportions %>%
#   filter(version == 'asinsqrt') %>% 
#   group_by(species, period) %>% 
#   do(tidy(lm(value ~ aspect*fire, data = .))) %>% 
#   filter(p.value <= 0.10)
# 
# mlr_aspect_glance <- proportions %>%
#   filter(version == 'asinsqrt') %>% 
#   group_by(species, period) %>% 
#   do(glance(lm(value ~ aspect*fire, data = .))) %>% 
#   filter(p.value <= 0.10)



# # -----------------------------------------------------------------------------
# # Q2: Do germination and survival vary by site?
# # One-way ANOVA test for differences in means among aspects, for each species: PICO different, PSME not
# aov_site <- proportions %>%
#   filter(version == 'asinsqrt') %>% 
#   group_by(species, period) %>% 
#   do(tidy(aov(value ~ fire, data = .))) %>% 
#   filter(p.value <= 0.10) %>% 
#   select(-term)
# 
# # Tukey multiple pairwise comparisons: S-F almost for PICO, S-N very for PICO, S-N almost for PSME
# tukey_site <- proportions %>%
#   filter(version == 'asinsqrt') %>% 
#   group_by(species, period) %>% 
#   do(tidy(TukeyHSD(aov(value ~ fire, data = .)))) %>% 
#   filter(adj.p.value <= 0.10) %>% 
#   select(-term)
# 
# 
# # LM version
# anova_lm_aspect <- proportions %>%
#   filter(version == 'asinsqrt') %>% 
#   group_by(species, period) %>% 
#   do(tidy(anova(lm(value ~ aspect, data = .)))) %>% 
#   filter(p.value <= 0.10) 
# 
# lm_aspect <- proportions %>%
#   filter(version == 'asinsqrt') %>% 
#   group_by(species, period) %>% 
#   do(tidy(lm(value ~ aspect, data = .))) %>% 
#   filter(p.value <= 0.10) 
# 
# lm_aspect_glance <- proportions %>%
#   filter(version == 'asinsqrt') %>% 
#   group_by(species, period) %>% 
#   do(glance(lm(value ~ aspect, data = .))) %>% 
#   filter(p.value <= 0.10) 
# 
# 
# # Linear model on original scale of response, for interpretibility
# lm_aspect_orig <- proportions %>%
#   filter(version == 'original') %>% 
#   group_by(species, period) %>% 
#   do(tidy(lm(value ~ aspect, data = .))) %>% 
#   filter(p.value <= 0.10) 
# 


