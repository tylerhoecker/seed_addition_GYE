# Import soil data, at this point filter for entire season ---------------------
start_time <- as.POSIXct(c("2018-06-01 00:00:00"))
end_time <- as.POSIXct(c("2018-10-01 00:00:00"))
source('code/read_soil_data.R')

# Filter to germination and survival periods, and calculate median values -----
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

# Import response variables (seedling germination, survival)--------------------
source('code/read_summarize_seedling.R')
seedling_response <- proportions %>% 
  filter(version == 'original') %>% 
  dplyr::select(-version) %>% 
  group_by(fire, aspect, species, period) %>% 
  summarise_if(is.numeric, mean) %>% 
  dplyr::select(-frameID)

# Combine response and predictor variables -------------------------------------
complete_df <-  full_join(seedling_response,soil_preds) %>% 
  modify_at(c('species','period'), as.factor) %>% 
  mutate(period = fct_relevel(period, 'Germination','Survival','Establishment'))

# Plot a 2-parameter model space based on the top model.
plotObj <- 
  ggplot(complete_df, aes(x = mois_q50, y = temp_q50, size = value)) +
  geom_point(shape = 21, color = 'black', fill = 'grey80', stroke = 0.7) +
  #scale_x_continuous(limits = c(0.033,0.12), breaks = seq(0.03,0.12,0.03)) +
  #scale_y_continuous(limits = c(12,18), breaks = seq(12,18,2)) +
  scale_radius("Proportion", range = c(1,6)) + #, 
  facet_grid(species~period) +
  theme_bw(base_size = 14) +
  labs(x = 'Soil moisture (vwc)', y = bquote('Soil temperature ('*~degree *C*')')) +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = 'bold'))
plotObj

legend <- cowplot::get_legend(plotObj)

plot(legend)