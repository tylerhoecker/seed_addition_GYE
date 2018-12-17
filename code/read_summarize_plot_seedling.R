# Import seedling data
source('code/read_seedling_data.R')



# Surival and germination figuring-------------------------------------------
# Germinated
germination <- seedlings %>% 
  filter(variable == 'height', species != 'control') %>% 
  mutate(germinated = if_else(is.na(value), 0, 1)) %>% 
  group_by(fire, aspect, species, frameID, cell) %>% 
  summarise(germinated = max(germinated)) 

# Survived
final <- seedlings %>%
  filter(variable == 'height', species != 'control') %>% 
  mutate(survived = if_else(date > as.POSIXct("2018-10-01") & value > 0, 1, 0)) %>% 
  group_by(fire, aspect, species, frameID, cell) %>% 
  summarise(final = max(survived, na.rm = T)) 

# Calculate survival
seeds_per = 50
trays_per = 5

# Proportion of germination, survival and their product, establishment for each frame. No mean or errors can be calculated here.
props_by_frameID <- full_join(germination, final) %>% 
  group_by(fire, aspect, species, frameID) %>% 
  summarise(germination = sum(germinated) / seeds_per,
            survival = sum(final) / sum(germinated),
            establishment = sum(final) / seeds_per)

props_frameID_long <- props_by_frameID %>% 
  gather(period, value, germination, survival, establishment)

# Grouping by fire ("fire") and aspect .
props_by_site <- props_frameID_long %>% 
  group_by(fire, aspect, species, period) %>% 
  summarise_at(vars(value),
               funs('mean' = mean(., na.rm=T), 'stdev' = sd(., na.rm = T)))

props_by_aspect <- props_frameID_long %>% 
  group_by(aspect, species, period) %>% 
  summarise_at(vars(value),
               funs('mean' = mean(., na.rm=T), 'stdev' = sd(., na.rm = T)))

props_by_fire <- props_frameID_long %>% 
  group_by(fire, species, period) %>% 
  summarise_at(vars(value),
               funs('mean' = mean(., na.rm=T), 'stdev' = sd(., na.rm = T)))

# Visualize  general pattern ----------------------------------------------------
colVals <- c('Flat' = '#009E73','North' = '#0072B2','South' = '#E69F00')

# Full breakdown (, aspect, period)
dodge <- position_dodge(width = 0.9)

ggplot(props_by_site) +
  geom_col(aes(x = fire, y = mean, fill = aspect), position = 'dodge') +
  geom_errorbar(aes(x = fire, ymin = mean - stdev, ymax = mean + stdev, group = aspect), 
                position = dodge, width = 0.25) +
  scale_fill_manual(values = colVals, name = 'aspect') +
  scale_color_manual(values = colVals, name = 'aspect') +
  facet_grid(species~period) +
  coord_cartesian(ylim = c(0,0.8)) +
  theme_bw(base_size = 14) +
  theme(strip.background = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

# By aspect, sites group (aspect, period)
ggplot(props_by_aspect) +
  geom_col(aes(x = aspect, y = mean, fill = aspect), position = 'dodge') +
  geom_errorbar(aes(x = aspect, ymin = mean - stdev, ymax = mean + stdev), 
                position = dodge, width = 0.25) +
  scale_fill_manual(values = colVals, name = 'aspect') +
  coord_cartesian(ylim = c(0,0.8)) +
  facet_grid(species~period) +
  theme_bw(base_size = 14) +
  theme(strip.background = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

# By fire (fire, period)
ggplot(props_by_fire) +
  geom_col(aes(x = fire, y = mean), position = 'dodge', fill = 'grey45') +
  geom_errorbar(aes(x = fire, ymin = mean - stdev, ymax = mean + stdev), 
                position = dodge, width = 0.25) +
  facet_grid(species~period) +
  coord_cartesian(ylim = c(0,0.8)) +
  theme_bw(base_size = 14) +
  theme(strip.background = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


