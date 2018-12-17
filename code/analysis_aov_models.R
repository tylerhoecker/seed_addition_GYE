source('code/read_summarize_plot_seedling.R')
# ------------------------------------------------------------------------------
# Q1.1: Is germination, survival and establishment significantly different among aspects and sites in each species?
# Anwer: Yes, between N-S for PICO @ 0.01, F-S for PICO @ 0.05, and N-S for PSME at 0.05.
# ------------------------------------------------------------------------------

# Transform data, then show both ways (all fires and aspects together for clarity)
# Using arsine-square-root transform per Ives 2018 sensu Larson and Marx 1981
proportions <- props_frameID_long %>% 
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

# One-way ANOVA test for differences in means among aspects, for each species: PICO different, PSME not
aov_aspect <- proportions %>%
  filter(version == 'asinsqrt') %>% 
  group_by(species, period) %>% 
  do(tidy(aov(value ~ aspect, data = .))) %>% 
  filter(p.value <= 0.10) %>% 
  select(-term)


# Tukey multiple pairwise comparisons: S-F almost for PICO, S-N very for PICO, S-N almost for PSME
tukey_aspect <- proportions %>%
  filter(version == 'asinsqrt') %>% 
  group_by(species, period) %>% 
  do(tidy(TukeyHSD(aov(value ~ aspect, data = .)))) %>% 
  filter(adj.p.value <= 0.10) %>% 
  select(-term)


# -----------------------------------------------------------------------------
# Q2: Do germination and survival vary by site?
# One-way ANOVA test for differences in means among aspects, for each species: PICO different, PSME not
aov_site <- proportions %>%
  filter(version == 'asinsqrt') %>% 
  group_by(species, period) %>% 
  do(tidy(aov(value ~ fire, data = .))) %>% 
  filter(p.value <= 0.10) %>% 
  select(-term)

# Tukey multiple pairwise comparisons: S-F almost for PICO, S-N very for PICO, S-N almost for PSME
tukey_site <- proportions %>%
  filter(version == 'asinsqrt') %>% 
  group_by(species, period) %>% 
  do(tidy(TukeyHSD(aov(value ~ fire, data = .)))) %>% 
  filter(adj.p.value <= 0.10) %>% 
  select(-term)


# LM version
anova_lm_aspect <- proportions %>%
  filter(version == 'asinsqrt') %>% 
  group_by(species, period) %>% 
  do(tidy(anova(lm(value ~ aspect, data = .)))) %>% 
  filter(p.value <= 0.10) 

lm_aspect <- proportions %>%
  filter(version == 'asinsqrt') %>% 
  group_by(species, period) %>% 
  do(tidy(lm(value ~ aspect, data = .))) %>% 
  filter(p.value <= 0.10) 

lm_aspect_glance <- proportions %>%
  filter(version == 'asinsqrt') %>% 
  group_by(species, period) %>% 
  do(glance(lm(value ~ aspect, data = .))) %>% 
  filter(p.value <= 0.10) 


# Linear model on original scale of response, for interpretibility
lm_aspect_orig <- proportions %>%
  filter(version == 'original') %>% 
  group_by(species, period) %>% 
  do(tidy(lm(value ~ aspect, data = .))) %>% 
  filter(p.value <= 0.10) 

# Multiple linear regression
mlr_aspect <- proportions %>%
  filter(version == 'asinsqrt') %>% 
  group_by(species, period) %>% 
  do(tidy(lm(value ~ aspect*fire, data = .))) %>% 
  filter(p.value <= 0.10)

mlr_aspect_glance <- proportions %>%
  filter(version == 'asinsqrt') %>% 
  group_by(species, period) %>% 
  do(glance(lm(value ~ aspect*fire, data = .))) %>% 
  filter(p.value <= 0.10)


