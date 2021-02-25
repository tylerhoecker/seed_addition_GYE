# Latest and great modeling effort as of 4/30/19
library(ggrepel)
library(tidyverse)
library(cowplot)
library(ggcorrplot)
library(multcomp)


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


# Correlation matrix
corr <- complete_df %>% 
  ungroup() %>% 
  dplyr::select(starts_with('mois'),starts_with('temp'),veg,organic,N) %>% 
  cor(use = "pairwise.complete.obs", method = 'spearman')

pvals <- complete_df %>% 
  ungroup() %>% 
  select(starts_with('mois'),starts_with('temp'),veg,Clay,organic,N, Germination,Survival) %>% 
  cor_pmat(use = "pairwise.complete.obs")

ggcorrplot(corr, type = "lower", lab = TRUE, p.mat = pvals, insig = 'blank')

# Scatterplots
r2vals <- complete_df %>% 
  ungroup() %>% 
  select(species, Germination, Survival, starts_with('mois'),starts_with('temp'), veg, Clay, organic, N) %>% 
  gather(predictor, value, -species, -Germination, -Survival) %>% 
  group_by(species, predictor) %>% 
  summarise(germ_r2 = round(cor.test(Germination, value, use = "pairwise.complete.obs", method = 'spearman')$estimate, 2),
            germ_pval = round(cor.test(Germination, value, use = "pairwise.complete.obs", method = 'spearman')$p.value, 3),
            surv_r2 = round(cor.test(Survival, value, use = "pairwise.complete.obs", method = 'spearman')$estimate, 2),
            surv_pval = round(cor.test(Survival, value, use = "pairwise.complete.obs", method = 'spearman')$p.value, 3)) 

scatterplot_df <- rescaled_df %>%
  ungroup() %>% 
  select(species, Survival, Germination, starts_with('mois'),starts_with('temp'), veg, Clay, organic, N) %>% 
  gather(predictor, value, -species, -Survival, -Germination) 

germ_scatter_plots <- 
  ggplot(scatterplot_df, aes(x = value, y = Germination)) +
  geom_point() +
  geom_smooth(method = 'lm', color = 'black', se = F) +
  geom_text(data = r2vals, aes(x = 0, 0.3, label = paste('r2 =', germ_r2, 'p =',germ_pval, sep = ' '))) +
  facet_grid(species~predictor) +
  theme_bw()

surv_scatter_plots <- 
  ggplot(scatterplot_df, aes(x = value, y = Survival)) +
  geom_point() +
  geom_smooth(method = 'lm', color = 'black', se = F) +
  geom_text(data = r2vals, aes(x = 0, 0.8, label = paste('r2 =', surv_r2, 'p =',surv_pval, sep = ' '))) +
  facet_grid(species~predictor) +
  theme_bw()

plot_grid(germ_scatter_plots, surv_scatter_plots, nrow = 2)


# Refined scatterplots of key predictors
proportions <- full_join(germination, final) %>% 
  group_by(fire, aspect, species, frameID) %>% # Adjust by frame or site: +/- frameID
  summarise(Germination = sum(germinated, na.rm = T) / n(),
            Survival = sum(final, na.rm = T) / sum(germinated, na.rm = T),
            Establishment = sum(final, na.rm = T) / n()) %>% 
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
  dplyr::select(-version) %>% 
  spread(period, value) 


pretty_plots <-  full_join(seedling_response,soil_preds) %>% 
  full_join(pct_cover) %>% 
  full_join(soil_props) %>% 
  modify_at(c('species'), as.factor) %>% 
  ungroup() %>% 
  select(species,Germination, Survival, starts_with('mois'), starts_with('temp')) %>% 
  gather(key, value, -species, -Germination, -Survival) %>% 
  separate(key, into = c('variable','delete','period'), sep = '_') %>% 
  select(-delete)

cbp2 <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot(filter(pretty_plots, period == 'germ'), aes(x = value, y = Germination)) +
  stat_summary(aes(fill = species), fun.data = 'mean_cl_boot', shape = 21, size = 0.75, alpha = 0.7) +
  geom_smooth(data = filter(pretty_plots, period == 'germ', species == 'PICO', variable == 'temp'),
              method = 'lm', se = F, size = 1.5, color = cbp2[6]) +
  facet_grid(~variable, scales = 'free_x') +
  scale_fill_manual('Species', values=cbp2[c(6,7)]) +
  scale_color_manual('Species', values=cbp2[c(6,7)]) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = 'bold'))

ggplot(filter(pretty_plots, period == 'surv'), aes(x = value, y = Survival)) +
  stat_summary(aes(fill = species), fun.data = 'mean_cl_boot', shape = 21, size = 0.75, alpha = 0.7) +
  geom_smooth(data = filter(pretty_plots, period == 'surv', species == 'PSME', variable == 'mois'),
              method = 'lm', se = F, size = 1.5, color = cbp2[7]) +
  facet_grid(~variable, scales = 'free_x') +
  scale_fill_manual('Species', values=cbp2[c(6,7)]) +
  scale_color_manual('Species', values=cbp2[c(6,7)]) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = 'bold'))


# Abiotic - aspect relationships
# Read in topographic indices from CSV (done using built-in GDAL DEM tools in QGIS3: https://www.gdal.org/gdaldem.html)
terrain_df <- read_csv('data/terrain_idx.csv') 

daily_soil <- soil_df %>% 
  group_by(fire, aspect, floor_date(time, 'days'), variable) %>% 
  summarise_if(is.numeric, mean) %>% 
  spread(variable, value)

abiotic_df <- full_join(terrain_df, daily_soil)


# Correlation matrix
corr <- abiotic_df %>% 
  ungroup() %>% 
  dplyr::select(-fire,-aspect,-`floor_date(time, "days")`) %>% 
  cor(use = "pairwise.complete.obs", method = 'spearman')

pvals <-  abiotic_df %>% 
  ungroup() %>%
  dplyr::select(-fire,-aspect,-`floor_date(time, "days")`) %>% 
  cor_pmat(use = "pairwise.complete.obs", method = 'spearman')

ggcorrplot(corr, type = "lower", lab = TRUE, p.mat = pvals)


# ANOVA abiotic by aspect

daily_soil <- soil_df %>% 
  group_by(fire, aspect, floor_date(time, 'days'), variable) %>% 
  summarise_if(is.numeric, mean)

# Tukey multiple pairwise comparisons
tukey_aspect <- daily_soil %>%
  group_by(variable) %>% 
  do(tidy(TukeyHSD(aov(value ~ aspect, data = .)))) 

tukey_aspect_cld <- daily_soil %>%
  group_by(variable) %>%
  do(tidy(cld(glht(aov(value ~ aspect, data = .), linfct = mcp(aspect = 'Tukey')))))  

# Dataframes with Tukey labels
aspect_group_labels <- daily_soil %>% 
  full_join(., tukey_aspect_cld) %>% 
  group_by(variable, aspect, lhs, letters) %>% 
  summarise() %>% 
  mutate(y_position = if_else(variable == 'mois', 0.15, 22)) %>% 
  filter(aspect == lhs) 


colVals <- c('Flat' = '#009E73','North' = '#0072B2','South' = '#E69F00')

ggplot(daily_soil) +
  #geom_boxplot(notch = T) +
  stat_summary(aes(x = aspect, y = value, group = fire),
               fill = 'grey80', size = 0.5, shape = 21, position = position_dodge(width = 0.5),
               fun.data = "mean_cl_boot", geom = "pointrange") +
  stat_summary(aes(x = aspect, y = value, fill = aspect), 
               size = 1, shape = 21, position = position_dodge(width = 0.5),
               fun.data = "mean_cl_boot", geom = "pointrange") +
  scale_color_manual('Aspect', values = rep('black', 4)) +
  scale_fill_manual('Aspect', values = colVals) +
  geom_text(data = aspect_group_labels,
            aes(x = aspect, y = y_position, label = letters),
            color = 'black', size = 5) +
  facet_wrap(~variable, scales = 'free_y') +
  theme_bw(base_size = 14) +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = 'bold'))



terrain_plots <-  soil_preds %>% 
  full_join(terrain_df) %>% 
  modify_at(c('species'), as.factor) %>% 
  ungroup() %>% 
  dplyr::select(starts_with('mois'), starts_with('temp'), aspect, dev_ne, tpi, hli) %>% 
  gather(key, value, -aspect, -dev_ne, -tpi, -hli) %>% 
  separate(key, into = c('variable','delete','period'), sep = '_') %>% 
  dplyr::select(-delete)

ggplot(terrain_plots) +
  geom_point(aes(x = dev_ne, y = value, color = aspect)) +
  facet_wrap(~variable, scales = 'free_y')  +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = 'bold'))


