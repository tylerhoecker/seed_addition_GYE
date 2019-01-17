# This script conducts AOV of seedling germination and survival data by aspect and site.
# Transforms data with arc-sine square root
# Conducts AOV and Tukey's Multiple comparisons. Does Tukey's separately so as to identify groups, for plotting purposes.
# Makes plots that are perfect as of 1/15/18

# Run seedling import/summary scripts. Creates `germination`, `final`, `proprotions`, `seed_per`.
source('code/read_summarize_seedling.R')

# ------------------------------------------------------------------------------
# Q1.1: Is germination, survival and establishment significantly different among aspects and sites in each species?
# Anwer: Yes, between N-S for PICO @ 0.01, F-S for PICO @ 0.05, and N-S for PSME at 0.05.
# ------------------------------------------------------------------------------

# Plot distributions of values
ggplot(proportions) +
  geom_histogram(aes(x = value), binwidth = 0.05, fill = 'grey10') +
  facet_grid(version~species+period) +
  coord_cartesian(ylim = c(0,30)) +
  theme_bw(base_size = 12)


# Now do ANOVA and Tukey's Multiple comparisons --------------------------------
library(broom)
library(multcomp)
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
  #coord_cartesian(ylim = c(0,0.8)) +
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

