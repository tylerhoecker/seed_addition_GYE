library(raster)
library(tidyverse)
library(rgdal)

estab_prob <- raster("/Users/tylerhoecker/Box/Work/PhD/GIS/establishment_map/result/estab_predict_pico_glm.tif")

estab_prob_df <- 
  rasterToPoints(estab_prob, fun = function(x) {x > 0}, progress = 'text') %>% 
  as_tibble() %>% 
  rename(value = 'estab_predict_pico_glm') 

mean(estab_prob_df$value)
std_e <- function(x) sd(x)/sqrt(length(x))
std_e(estab_prob_df$value)

# Save ECDF and add to df
estab_prob_df$ecdf = ecdf(estab_prob_df$value)(estab_prob_df$value)


ggplot(estab_prob_df) +
  stat_ecdf(aes(x = value), geom = 'line', color = 'black', size = 1) +
  theme_bw(base_size = 12) +
  labs(x = bquote('Predicted establishment rate'), 
       y = 'Proportion of short-interval burn area') +
  coord_cartesian(xlim = c(0,0.3)) +
  #scale_x_continuous(breaks = seq(0,0.14,0.02)) +
  theme(legend.position = c(0.8,0.25),
        legend.background = element_blank(), 
        legend.key = element_blank())


# Various seed delivery densities at 25m per Gill 
density_low <- 1 * 10000 
density_med <- 5 * 10000 
density_high <- 10 *10000



estab_densities <- estab_prob_df %>%
  #sample_n(50000) %>% 
  # Dummy column for discrete colors on ECDF that correspond to map
  mutate(value_disc = if_else(value < 0.01, 0.01, 
                         if_else(value >= 0.01 & value < 0.03, 0.03, 
                                 if_else(value >= 0.03 & value < 0.05, 0.05, 
                                         if_else(value >= 0.05, 0.2, value))))) %>%
  mutate(low = value * density_low,
         med = value * density_med, 
         high = value * density_high) %>%
  dplyr::select(ecdf, value, value_disc, low, med, high) %>% 
  gather(level, density, low,med,high) %>% 
  mutate(level = factor(level)) %>% 
  mutate(level = fct_relevel(level, 'high','med','low')) %>% 
  arrange(ecdf)

estab_densities %>% 
  group_by(level) %>% 
  summarise_at(vars(density), list(q1 = ~quantile(., probs = 0.05), 
                                   q2 = ~quantile(., probs =  0.50), 
                                   q3 = ~quantile(., probs = 0.95)))
  
  

# ECDF
ggplot(estab_densities) +
  #geom_vline(aes(xintercept = 1480), linetype = 'dashed') +
  #geom_vline(aes(xintercept = 424), linetype = 'dashed') +
  #stat_ecdf(aes(x = density, group = level), geom = 'line', color = 'black', size = 0.2) +
  geom_point(data = slice(estab_densities, which(row_number() %% 1700 == 1)),
             aes(x = density, y = ecdf, fill = as.factor(value_disc), group = level),
             shape = 21, size = 2.2, stroke = 0.3) +
  scale_fill_manual('Probability of\nseedling\nestablishment',
                     values = c('#67000d','#c03133', '#fd865b','#737373'),
                     labels = c('< 1%', '1-3%', '3-5%', '> 5%')) +
  scale_x_continuous(breaks = c(50, 424, 1480, 5000, 10000)) +
  theme_bw(base_size = 12) +
  labs(x = bquote('Predicted tree density (stems '*~ha^-1*')'), 
       y = 'Proportion of landscape') +
  coord_cartesian(xlim = c(0,10000)) +
  theme(legend.position = c(0.8,0.25),
        legend.background = element_blank(), 
        legend.key = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust =1))


ggplot(estab_densities) +
  stat_ecdf(aes(x = value, group = level, fill = value), geom = 'line', size = 2) +
  theme_bw(base_size = 12) 

