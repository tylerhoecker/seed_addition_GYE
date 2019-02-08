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
  geom_histogram(aes(x = value), fill = 'grey10') +
  facet_grid(version~species+period) +
  coord_cartesian(ylim = c(0,30)) +
  theme_bw(base_size = 12) +
  labs(x = 'Proportion', y = 'Count')


# Now do ANOVA and Tukey's Multiple comparisons --------------------------------
library(broom)
library(multcomp)
# One-way ANOVA test for differences in means among aspects, for each species: PICO different, PSME not
aov_aspect_fire_model <- proportions %>%
  filter(version == 'asinsqrt') %>% 
  group_by(species, period) %>% 
  do(glance(aov(value ~ aspect*fire, data = .))) %>% 
  mutate_if(is.numeric, round, 4) 

aov_aspect_fire_terms <- proportions %>%
  filter(version == 'asinsqrt') %>% 
  group_by(species, period) %>% 
  do(tidy(aov(value ~ aspect*fire, data = .))) %>% 
  mutate_if(is.numeric, round, 4) 


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
# Plot the original values 
props_plot <- proportions %>% 
  filter(version == 'original')

dodge <- position_dodge(width = 0.9)

ggplot(props_plot, aes(x = fire, y = value, fill = aspect, group = aspect)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") + 
  stat_summary(fun.data = mean_se, geom = "errorbar", position = dodge, width = 0.25) +
  scale_fill_manual(values = colVals, name = 'aspect') +
  facet_grid(species~period) +
  #coord_cartesian(ylim = c(0,0.8)) +
  theme_bw(base_size = 14) +
  theme(strip.background = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  labs(x = 'Fire', y = 'Proportion')

# Showing +/- 1.96* standard error...
statistics_df <- props_plot %>% 
  filter(version == 'original') %>% 
  group_by(fire, aspect, species, period) %>% 
  summarise(mean = mean(value),
            se = sd(value)/sqrt(n()),
            lower = mean-(1.96*se),
            upper = mean+(1.96*se))

ggplot(statistics_df) +
  geom_col(aes(x = fire, y = mean, fill = aspect, group = aspect), position = "dodge") +
  geom_errorbar(aes(x = fire, ymin = lower, ymax = upper, group = aspect), position = dodge, width = 0.25) +
  scale_fill_manual(values = colVals, name = 'aspect') +
  facet_grid(species~period) +
  #coord_cartesian(ylim = c(0,0.8)) +
  theme_bw(base_size = 14) +
  theme(strip.background = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


# By aspect 
text_ys <- c(0.3,0.3,0.2, 0.7,0.65,0.2, 0.2,0.2,0.1, 
             0.2,0.2,0.2, 0.4,0.25,0.25, 0.1,0.1,0.1)

ggplot(props_plot) +
  stat_summary(aes(x = aspect, y = value, fill = aspect, group = aspect),
               fun.y = mean, geom = "bar", position = "dodge") + 
  stat_summary(aes(x = aspect, y = value, fill = aspect, group = aspect),
               fun.data = mean_se, geom = "errorbar", position = dodge, width = 0.25) +
  geom_text(data = tukey_aspect_cld, aes(x = lhs, y = text_ys, label = letters), fontface = 'bold') +
  scale_fill_manual(values = colVals, name = 'aspect') +
  facet_grid(species~period) +
  coord_cartesian(ylim = c(-0.06,0.75)) +
  theme_bw(base_size = 14) +
  theme(strip.background = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  labs(x = '', y = 'proportion of seeds')


# By fire 
text_ys <- c(0.35,0.3,0.2,0.3,    0.7,0.3,0.6,0.7,    0.25,0.15,0.15,0.15, 
             0.2,0.15,0.15,0.15,  0.3,0.25,0.45,0.35, 0.1,0.1,0.1,0.1)

ggplot(props_plot, aes(x = fire, y = value)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") + 
  stat_summary(fun.data = mean_se, geom = "errorbar", position = dodge, width = 0.25) +
  geom_text(data = tukey_fire_cld, aes(x = lhs, y = text_ys, label = letters), fontface = 'bold') +
  facet_grid(species~period) +
  coord_cartesian(ylim = c(-0.06,0.75)) +
  theme_bw(base_size = 14) +
  theme(strip.background = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  labs(x = 'fire', y = 'proportion of seeds')

