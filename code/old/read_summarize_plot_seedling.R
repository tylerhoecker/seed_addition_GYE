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
  gather(period, value, germination, survival, establishment) %>% 
  mutate(period = factor(period, levels = c('germination','survival','establishment')))

# Grouping by fire ("fire") and aspect .
props_by_site <- props_frameID_long %>% 
  group_by(fire, aspect, species, period) %>% 
  summarise_at(vars(value),
               funs('mean' = mean(., na.rm=T), 'se' = 1.96*sd(., na.rm = T)/sqrt(n()) ))

props_by_aspect <- props_frameID_long %>% 
  group_by(aspect, species, period) %>% 
  summarise_at(vars(value),
               funs('mean' = mean(., na.rm=T), 'se' = 1.96*sd(., na.rm = T)/sqrt(n()) ))

props_by_fire <- props_frameID_long %>% 
  group_by(fire, species, period) %>% 
  summarise_at(vars(value),
               funs('mean' = mean(., na.rm=T), 'se' = 1.96*sd(., na.rm = T)/sqrt(n()) ))

# Visualize  general pattern ----------------------------------------------------
colVals <- c('Flat' = '#009E73','North' = '#0072B2','South' = '#E69F00')

# Full breakdown (, aspect, period)
dodge <- position_dodge(width = 0.9)

ggplot(props_frameID_long, aes(x = fire, y = value, fill = aspect, group = aspect)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") + 
  stat_summary(fun.data = mean_se, geom = "errorbar", position = dodge, width = 0.25) +
  scale_fill_manual(values = colVals, name = 'aspect') +
  facet_grid(species~period) +
  coord_cartesian(ylim = c(0,0.8)) +
  theme_bw(base_size = 14) +
  theme(strip.background = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

# Box plot
# ggplot(filter(props_frameID_long, period != 'survival')) +
#   geom_boxplot(aes(x = fire, y = value, fill = aspect)) +
#   scale_fill_manual(values = colVals, name = 'aspect') +
#   facet_grid(species~period, scales = 'free_y') +
#   theme_bw(base_size = 14) +
#   theme(strip.background = element_blank(),
#         axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

# By aspect, sites group (aspect, period)
ggplot(props_frameID_long, aes(x = aspect, y = value, fill = aspect, group = aspect)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") + 
  stat_summary(fun.data = mean_se, geom = "errorbar", position = dodge, width = 0.25) +
  geom_signif(comparisons = list(c('North','South'), c('Flat','South')), 
              y_position = c(0.75, 0.65), vjust = -0.2, tip_length = 0,
              test = 't.test', na.rm = T, 
              map_signif_level= c("0.001"=0.001,"0.01"=0.01, "0.05"=0.05, "n.s."=2)) +
  scale_fill_manual(values = colVals, name = 'aspect') +
  facet_grid(species~period) +
  coord_cartesian(ylim = c(0,.85)) +
  theme_bw(base_size = 14) +
  theme(strip.background = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

# By fire (fire, period)
ggplot(props_frameID_long, aes(x = fire, y = value)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") + 
  stat_summary(fun.data = mean_se, geom = "errorbar", position = dodge, width = 0.25) +
  geom_signif(comparisons = list(c('Berry-Glade','Maple'),
                                 c('Berry-Glade','Buffalo'),
                                 c('Berry-Glade','Berry-Huck'),
                                 c('Buffalo','Maple')), 
              y_position = c(0.85,0.75, 0.65, 0.65), vjust = -0.2, 
              test = 't.test', na.rm = T, 
              map_signif_level= c("0.001"=0.001,"0.01"=0.01, "0.05"=0.05, "n.s."=2)) +
  facet_grid(species~period) +
  coord_cartesian(ylim = c(0,1)) +
  theme_bw(base_size = 14) +
  theme(strip.background = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
