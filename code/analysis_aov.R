# This script conducts AOV of seedling germination and survival data by aspect and site.
# Transforms data with arc-sine square root
# Conducts AOV and Tukey's Multiple comparisons. Does Tukey's separately so as to identify groups, for plotting purposes.
# Makes plots that are perfect as of 1/15/18

# Run seedling import/summary scripts. Creates `germination`, `final`, `proprotions`, `seed_per`.
source('code/read_summarize_seedling.R')
# Plot distributions of values
count_histos <- 
  ggplot(proportions) +
  geom_histogram(aes(x = value), fill = 'grey10', bins = 10) +
  facet_wrap(version~species+period, scales = 'free', ncol = 6) +
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

aov_species <- proportions %>%
  filter(version == 'asinsqrt') %>% 
  group_by(period) %>% 
  do(glance(aov(value ~ species*(aspect + fire), data = .))) %>% 
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


# PLOTTING ----------------------------------------------------
library(cowplot)

colVals <- c('Flat' = '#009E73','North' = '#0072B2','South' = '#E69F00')

# Plot the original values 
props_plot <- proportions %>% 
  filter(version == 'original') 

# Dataframes with Tukey labels
aspect_group_labels <- proportions %>% 
  full_join(tukey_aspect_cld) %>% 
  filter(version == 'original') %>% 
  group_by(species, aspect, lhs, period, letters) %>% 
  summarise(y_position = max(value) + 0.05) %>% 
  filter(aspect == lhs) 

fire_group_labels <- proportions %>% 
  full_join(tukey_fire_cld) %>% 
  filter(version == 'original') %>% 
  group_by(species, fire, lhs, period, letters) %>% 
  summarise(y_position = max(value) + 0.05) %>% 
  filter(fire == lhs) 

# Aspect ----------------------------------------------------------------------
aspect_plot_fn <- function(subset, y_axis, x_axis, title, multsize, label_ys){
    
  ggplot(filter(props_plot, period == subset), 
         aes(x = aspect, y = value, color = aspect, fill = aspect)) +
    geom_jitter(alpha = 0.6, size = 1*multsize) +
    stat_summary(color = 'black', size = 0.3*multsize,
                 fun.data = "mean_cl_boot", geom = "pointrange") +
    scale_color_manual(values = colVals, name = 'aspect', guide = F) +
    scale_fill_manual(values = colVals, name = 'aspect', guide = F) +
    geom_text(data = filter(aspect_group_labels, period == subset),
      aes(x = aspect, y = y_position, label = letters), 
              color = 'black', size = 2*multsize, fontface = 'bold') +
    facet_wrap(~species) +
    labs(y = 'Proportion of seeds', subtitle = title) +
    theme_bw(base_size = 14) +
    theme(strip.background = element_blank(),
          plot.subtitle = element_text(hjust = 0.5, face = 'bold'),
          axis.text.x = x_axis,
          axis.title.x = element_blank(),
          axis.title.y = y_axis) }

estab_plot <- 
  aspect_plot_fn(subset = 'Establishment',
                 y_axis = element_blank(),
                 x_axis = element_text(angle = 45, hjust = 1, vjust = 1),
                 title = 'Establishment', 
                 multsize = 2)

surv_plot <- 
  aspect_plot_fn(subset = 'Survival',
                 y_axis = element_blank(), 
                 x_axis = element_text(angle = 45, hjust = 1, vjust = 1),
                 title = 'Survival', 
                 multsize = 2)

germ_plot <-
  aspect_plot_fn(subset = 'Germination',
                 y_axis = element_text(), , 
                 x_axis = element_text(angle = 45, hjust = 1, vjust = 1),
                 title = 'Germination', 
                 multsize = 2)
  
#small <- plot_grid(germ_plot, surv_plot, ncol = 1, rel_heights = c(1,1.15))
#plot_grid(estab_plot, small, ncol = 2, rel_widths = c(2.3,1))
aspect_plot <- plot_grid(germ_plot, surv_plot, estab_plot, ncol = 3)

# By fire  --------------------------------------------------------------------
fire_plot_fn <- function(subset, y_axis, x_axis, title, multsize, label_ys){
  
  ggplot(filter(props_plot, period == subset), 
         aes(x = fire, y = value)) +
    geom_jitter(color = 'grey40', alpha = 0.6, size = 1*multsize) +
    stat_summary(color = 'black', size = 0.3*multsize,
                 fun.data = "mean_cl_boot", geom = "pointrange") +
    scale_color_manual(values = colVals, name = 'aspect', guide = F) +
    scale_fill_manual(values = colVals, name = 'aspect', guide = F) +
    geom_text(data = filter(fire_group_labels, period == subset),
              aes(x = fire, y = y_position, label = letters), 
              color = 'black', size = 2*multsize, fontface = 'bold') +
    facet_wrap(~species) +
    labs(y = 'Proportion of seeds', subtitle = title) +
    theme_bw(base_size = 11) +
    theme(strip.background = element_blank(),
          plot.subtitle = element_text(hjust = 0.5, face = 'bold'),
          axis.text.x = x_axis,
          axis.title.x = element_blank(),
          axis.title.y = y_axis) }

estab_plot <- 
  fire_plot_fn(subset = 'Establishment',
                 y_axis = element_text(), 
                 x_axis = element_text(angle = 45, hjust = 1, vjust = 1),
                 title = 'Establishment', 
                 multsize = 2)

surv_plot <- 
  fire_plot_fn(subset = 'Survival',
                 y_axis = element_blank(), 
                 x_axis = element_text(angle = 45, hjust = 1, vjust = 1),
                 title = 'Survival', 
                 multsize = 2)

germ_plot <-
  fire_plot_fn(subset = 'Germination',
                 y_axis = element_blank(), 
               x_axis = element_text(angle = 45, hjust = 1, vjust = 1),
               title = 'Germination', multsize = 2)

#small <- plot_grid(germ_plot, surv_plot, ncol = 1)
#plot_grid(estab_plot, small, ncol = 2, rel_widths = c(2.3,1))
fire_plot <- plot_grid(germ_plot, surv_plot, estab_plot, ncol = 3)



# #--------------------
# # BAR PLOTS
# dodge <- position_dodge(width = 0.9)
# 
# # By aspect 
# text_ys <- c(0.3,0.3,0.2, 0.7,0.65,0.2, 0.2,0.2,0.1, 
#              0.2,0.2,0.2, 0.4,0.25,0.25, 0.1,0.1,0.1)
# 
# ggplot(props_plot) +
#   stat_summary(aes(x = aspect, y = value, fill = aspect, group = aspect),
#                fun.y = mean, geom = "bar", position = "dodge") + 
#   stat_summary(aes(x = aspect, y = value, fill = aspect, group = aspect),
#                fun.data = mean_se, geom = "errorbar", position = dodge, width = 0.25) +
#   geom_text(data = tukey_aspect_cld, aes(x = lhs, y = text_ys, label = letters), fontface = 'bold') +
#   scale_fill_manual(values = colVals, name = 'Aspect') +
#   facet_grid(species~period) +
#   coord_cartesian(ylim = c(-0.06,0.75)) +
#   theme_bw(base_size = 14) +
#   theme(strip.background = element_blank(),
#         axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
#   labs(x = '', y = 'Proportion of seeds')
# 
# 
# text_ys <- c(0.35,0.3,0.2,0.3,    0.7,0.3,0.6,0.7,    0.25,0.15,0.15,0.15, 
#              0.2,0.15,0.15,0.15,  0.3,0.25,0.45,0.35, 0.1,0.1,0.1,0.1)
# 
# 
# ggplot(props_plot, aes(x = fire, y = value)) +
#   stat_summary(fun.y = mean, geom = "bar", position = "dodge") + 
#   stat_summary(fun.data = mean_se, geom = "errorbar", position = dodge, width = 0.25) +
#   geom_text(data = tukey_fire_cld, aes(x = lhs, y = text_ys, label = letters), fontface = 'bold') +
#   facet_grid(species~period) +
#   coord_cartesian(ylim = c(-0.06,0.75)) +
#   theme_bw(base_size = 14) +
#   theme(strip.background = element_blank(),
#         axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
#   labs(x = 'fire', y = 'proportion of seeds')
# # Showing +/- 1.96* standard error...
# statistics_df <- props_plot %>% 
#   filter(version == 'original') %>% 
#   group_by(fire, aspect, species, period) %>% 
#   summarise(mean = mean(value),
#             se = sd(value)/sqrt(n()),
#             lower = mean-(1.96*se),
#             upper = mean+(1.96*se))
# 
# ggplot(statistics_df) +
#   geom_col(aes(x = fire, y = mean, fill = aspect, group = aspect), position = "dodge") +
#   geom_errorbar(aes(x = fire, ymin = lower, ymax = upper, group = aspect), position = dodge, width = 0.25) +
#   scale_fill_manual(values = colVals, name = 'aspect') +
#   facet_grid(species~period) +
#   #coord_cartesian(ylim = c(0,0.8)) +
#   theme_bw(base_size = 14) +
#   theme(strip.background = element_blank(),
#         axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
# 
# 
# ggplot(props_plot, aes(x = fire, y = value, fill = aspect, group = aspect)) +
#   stat_summary(fun.y = mean, geom = "bar", position = "dodge") + 
#   stat_summary(fun.data = mean_se, geom = "errorbar", position = dodge, width = 0.25) +
#   scale_fill_manual(values = colVals, name = 'aspect') +
#   facet_grid(species~period) +
#   #coord_cartesian(ylim = c(0,0.8)) +
#   theme_bw(base_size = 14) +
#   theme(strip.background = element_blank(),
#         axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
#   labs(x = 'Fire', y = 'Proportion')
# 
# 
# 
