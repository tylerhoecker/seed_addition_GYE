# This script conducts AOV of abiotic conditions.
# Conducts AOV and Tukey's Multiple comparisons. Does Tukey's separately so as to identify groups, for plotting purposes.

# Run soil data import
start_time <- as.POSIXct(c("2018-06-01 00:00:00"))
end_time <- as.POSIXct(c("2018-10-01 00:00:00"))
source('code/soil_data_prep.R') # creates `soil_df` (complete) and `soil_preds` (summarized)

# Now do ANOVA and Tukey's Multiple comparisons --------------------------------
library(broom)
library(multcomp)
# One-way ANOVA test for differences in means among aspects, for each species: PICO different, PSME not
soil_df %>% 
  group_by(variable) %>% 
  do(tidy(aov(value ~ aspect*fire, data = .)))
  



soil_aov_df <- soil_preds %>% 
  gather(pred, value, -c(fire,aspect)) %>% 
  group_by(pred) 

aov_soil_model <- soil_aov_df %>% 
  do(glance(aov(value ~ aspect, data = .))) %>% 
  mutate_if(is.numeric, round, 4) 

aov_soil_terms <- soil_aov_df %>%
  do(tidy(aov(value ~ aspect, data = .))) %>% 
  mutate_if(is.numeric, round, 4) 

# Tukey multiple pairwise comparisons
tukey_aspect <- soil_aov_df %>%
  do(tidy(TukeyHSD(aov(value ~ aspect, data = .)))) 

tukey_aspect_cld <- soil_aov_df %>%  
  do(tidy(cld(glht(aov(value ~ aspect, data = .), linfct = mcp(aspect = 'Tukey')))))  


# PLOTTING ----------------------------------------------------
library(cowplot)

colVals <- c('Flat' = '#009E73','North' = '#0072B2','South' = '#E69F00')


# Dataframes with Tukey labels
aspect_group_labels <- soil_aov_df %>% 
  full_join(tukey_aspect_cld) %>% 
  group_by(aspect, lhs, letters) %>% 
  summarise(y_position = max(value) + 0.05) %>% 
  filter(aspect == lhs) 

# Aspect ----------------------------------------------------------------------
ggplot(soil_df) +
  ggridges::geom_density_ridges(aes(x = value, y = fire, fill = aspect),
                                alpha = 0.5) +
  scale_fill_manual(values = colVals, name = 'Aspect') +
  scale_color_manual(values = colVals, name = 'Aspect') +
  facet_wrap(~variable, scales = 'free') +
  theme_bw(base_size = 14)



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
        axis.title.y = y_axis) 

