counts <- seedlings %>% 
  filter(variable == 'height', species != 'control', value > 0) %>% 
  group_by(date, site, species, aspect) %>%
  summarize(count = n(),
            prop = n()/(5*50))

allMeasures <- seedlings %>% 
  group_by(date, site, species, aspect) %>% 
  filter(variable == 'height', value < 0 | is.na(value)) %>% 
  summarise(count = 0)

counts_aspect <- 
  full_join(counts, allMeasures) %>% 
  group_by(date, site, species, aspect) %>% 
  filter(species != 'control') %>% 
  summarise(count = sum(count),
            prop = count/(5*50)) %>%
  ungroup() %>% 
  group_by(species, aspect) %>% 
  summarise(median = quantile(prop, 0.50, na.rm = T),
            low = quantile(prop, 0.10, na.rm = T),
            high = quantile(prop, 0.90, na.rm = T),
            n = n())

all_by_aspect <- seedlings %>% 
  mutate(value = if_else(is.na(value), 0, value)) %>% 
  filter(value >= 0 & value <= 100)

colVals <- c('flat' = '#009E73','north' = '#0072B2','south' = '#E69F00')
legLabs <- c('Flat','North','South')

ggplot(counts_aspect) +
  #geom_col(aes(x = aspect, y = median, fill = aspect)) +
  #geom_errorbar(aes(x = aspect, ymin = low, ymax = high)) +
  geom_boxplot(aes(x = aspect, y = count, fill = aspect)) +
  scale_fill_manual(values = colVals, name = 'aspect') +
  labs(x = 'Aspect', y = 'Count of living seedlings') +  
  facet_wrap(~species, ncol = 1) +
  theme_bw(base_size = 14) +
  theme(strip.background = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


germination <- seedlings %>%
  filter(!is.na(value), species != 'control') %>% 
  select(aspect, species, frameID, cell) %>% 
  unite(frameID, cell, col = 'uniqID') %>% 
  group_by(aspect, species) %>% 
  summarise(germs = length(unique(uniqID)))

final_count <- seedlings %>%
  filter(value > 0, species != 'control') %>% 
  filter(date > as.POSIXct("2018-10-01")) %>% 
  select(aspect, species, frameID, cell) %>% 
  unite(frameID, cell, col = 'uniqID') %>% 
  group_by(aspect, species) %>% 
  summarise(final = length(unique(uniqID)))

survival <- full_join(germination, final_count) %>% 
  mutate(pct_surv = final / germs)

ggplot(survival) +
  geom_col(aes(x = aspect, y = pct_surv*100, fill = aspect)) +
  #geom_errorbar(aes(x = aspect, ymin = low, ymax = high)) +
  #geom_boxplot(aes(x = aspect, y = count, fill = aspect)) +
  #scale_y_continuous(sec.axis = sec_axis(~./(50*5*4), name = "Percent seeds germinated")) +
  scale_fill_manual(values = colVals, name = 'aspect') +
  #scale_y_continuous(limits = c(0,150), breaks = seq(0,150,50)) +
  labs(x = 'Aspect', y = 'Percent germinants survived') +  
  facet_wrap(~species, ncol = 1, scales = 'free_y') +
  theme_bw(base_size = 14) +
  theme(strip.background = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))




