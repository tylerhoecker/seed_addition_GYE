# This script conducts AOV of seedling germination and survival data by aspect and site.
# Transforms data with arc-sine square root
# Conducts AOV and Tukey's Multiple comparisons. Does Tukey's separately so as to identify groups, for plotting purposes.
library(broom)
library(multcomp)
library(lme4)
select <- dplyr::select

# Run seedling import/summary scripts. Creates `germination`, `final`, `proprotions`, `seed_per`.
source('code/read_summarize_seedling.R')

# ------------------------------------------------------------------------------
# Set up the dataframe of proporitons. 
# Key elements:
# - data aggregated to site level, frames are psuedoreplicates
# - Weights for binomial models are based accordinly on 250 possible succeses
# - Dataframe is long-wise to work nicely with purrr/tidy model summaries
# ------------------------------------------------------------------------------
proportions <- full_join(germination, survival) %>% 
  group_by(fire, aspect, species, frameID) %>% # Adjust by frame or site: +/- frameID
  summarise(Germination = sum(germinated, na.rm = T) / n(),
            Survival_yr1 = sum(survived_yr1, na.rm = T) / sum(germinated, na.rm = T),
            Survival_yr2 = sum(survived_yr2, na.rm = T) / sum(germinated, na.rm = T),
            Survival_1to2 = sum(survived_yr2, na.rm = T) / sum(survived_yr1, na.rm = T),
            Establishment_yr1 = sum(survived_yr1, na.rm = T) / n(),
            Establishment_yr2 = sum(survived_yr2, na.rm = T) / n()) %>%
  # Average again...
  group_by(fire, aspect, species) %>% 
  summarise_all(mean, na.rm = T) %>% 
  select(-frameID) %>% 
  # Add weights for untransformed logit glmer version of ANOVA
  mutate(germ_estab_Weights = as.integer(250),
         surv_Weights = as.integer(Germination * 250)) %>% 
   gather(period, value, -fire, -aspect, -species, -germ_estab_Weights, -surv_Weights) %>% 
  # Transform data using arsine-square-root transform per Ives 2018 sensu Larson and Marx 1981
  mutate(asinsqrt = asin(sign(value) * sqrt(abs(value))),
         logit = car::logit(value),
         Weights = if_else(period %in% c('Germination', 'Establishment_yr1','Establishment_yr2'), germ_estab_Weights, surv_Weights)) %>% # OR? logit = log( (value/(1-value)) )
  select(-germ_estab_Weights, -surv_Weights) %>% 
  rename(original = value) %>% 
  gather(version, value, original, asinsqrt, logit) %>% 
  mutate(period = fct_relevel(period, 'Germination','Survival_yr1','Survival_yr2','Survival_1to2','Establishment_yr1','Establishment_yr2')) 
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Results summary for tables, etc.
# ------------------------------------------------------------------------------
std_e <- function(x) sd(x)/sqrt(length(x))

proportions %>% 
  filter(version == 'original') %>% 
  group_by(species, period, aspect, fire) %>% 
  summarise(mean = round(mean(value, na.rm = T), 2),
            se = round(sd(value, na.rm = T)/sqrt(length(value)),5)) %>% 
  view()
#write_csv('results_summary.csv')
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Linear ANOVA approach - transform data using arc-sine-sqrt and ANOVA by aspect 
# ------------------------------------------------------------------------------

# Rather than filter each time, new dataframe with transformed data
proportions_linear <- proportions %>% 
  filter(version == 'logit')

# Plot distributions of values
ggplot(proportions_linear) +
  geom_histogram(aes(x = value), fill = 'grey10', bins = 10) +
  facet_wrap(version~species+period, scales = 'free', ncol = 6) +
  coord_cartesian(ylim = c(0,10)) +
  theme_bw(base_size = 12) +
  labs(x = 'Proportion', y = 'Count')

# ANOVA test for differences in means among aspects
proportions_linear %>%
  group_by(species, period) %>% 
  do(glance(aov(value ~ aspect, data = .))) %>% 
  mutate_if(is.numeric, round, 4) %>% 
  write_csv('anova_linaer_logit_results.csv')

proportions_linear %>%
  group_by(species, period) %>% 
  do(tidy(aov(value ~ aspect, data = .))) %>% 
  filter(term != 'Residuals') %>% 
  mutate_if(is.numeric, round, 4) 

# Tukey multiple pairwise comparisons of significant differences
tukey_aspect <- proportions_linear %>%
  group_by(species, period) %>% 
  do(tidy(TukeyHSD(aov(value ~ aspect + fire, data = .), conf.level = 0.90))) 

tukey_aspect_cld <- proportions_linear %>%
  group_by(species, period) %>% 
  do(tidy(cld(glht(aov(value ~ aspect, data = .), linfct = mcp(aspect = 'Tukey')),
              level = 0.05))) %>% 
  ungroup() %>% 
  rename(aspect = lhs) %>% 
  mutate(period = fct_relevel(period, 'Germination','Survival_yr1','Survival_yr2','Survival_1to2','Establishment_yr1','Establishment_yr2'),
         species = ifelse(species == "PICO", 'Lodgepole pine', 'Douglas-fir')) %>% 
  mutate(letters = ifelse(period == 'Germination', NA, letters),
         letters = ifelse(species == 'Douglas-fir', NA, letters))

# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Comparative plotting of proportions by aspect - key figure for paper
# Commented code does different versions: barplot, point-range, etc.
# ------------------------------------------------------------------------------
# The colors for aspect
colVals <- c('Flat' = '#009E73','North' = '#0072B2','South' = '#E69F00')

# Filtering
proportions %>% 
  group_by(aspect, species, period) %>% 
  filter(version == 'original') %>% 
  mutate(value = ifelse(value == 'NaN', NA, value)) %>% 
  do(data.frame(rbind(Hmisc::smean.cl.boot(.$value, na.rm=TRUE)))) %>% 
  ungroup() %>% 
  filter(period != 'Survival_1to2', period != 'Establishment_yr1') %>% 
  mutate(species = ifelse(species == "PICO", 'Lodgepole pine', 'Douglas-fir')) %>% 
  left_join(., tukey_aspect_cld) %>% 
  mutate(aspect = fct_relevel(aspect, 'South','Flat','North')) %>% 
  # Begin plotting
  ggplot(., aes(x = period, y = Mean, fill = aspect)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper),
                size = 0.75, width = 0.3, position = position_dodge(width = 0.9)) +
  scale_color_manual(values = colVals, name = 'Aspect') +
  scale_fill_manual(values = colVals, name = 'Aspect') +
  scale_x_discrete(labels = c('Germination', 'Survival (yr 1)', 'Survival (yr 2)', 'Establishment')) +
  geom_vline(xintercept = c(1.5,2.5,3.5), color = 'grey90') +
  geom_text(aes(y = Upper + 0.03, label = letters), position = position_dodge(width = 0.9),
            color = 'black', size = 4) +
  # geom_text(aes(y = -0.03, label = substring(aspect, 1, 1)), 
  #           position = position_dodge(width = 0.9), color = 'grey30') +
  facet_wrap(~species, nrow = 1) +
  labs(y = 'Proportion of seeds') +
  theme_bw(base_size = 14) +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = 'bold', size = 13),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.subtitle = element_text(hjust = 0.5, face = 'bold'),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = c(.43,.8),
        legend.background = element_blank(),
        legend.title = element_text(size = 12)  ) 
  # ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Binomial "ANOVA"
# This is an alternative version of the ANOVA, more consistent with other modeling approach
# It uses a glm. GLM keeps original scale, mixed-effects considers fire only as a mixed effect

# This is a summary-style approach. It is convenient, but the p-values are highly deflated
proportions %>% 
  filter(version == 'original') %>% 
  group_by(species, period) %>% 
  do(tidy(car::Anova(glm(value ~ aspect, data = ., 
                         family = quasibinomial(link = "logit"), weights = Weights),
                     test = 'LR'))) %>%
  mutate_if(is.numeric, round, 5) %>% 
  filter(term == 'aspect')

# ------------------------------------------------------------------------------


# OLD
# PLOTTING ----------------------------------------------------
library(cowplot)

colVals <- c('Flat' = '#009E73','North' = '#0072B2','South' = '#E69F00')

# Plot the original values 
props_plot <- proportions %>% 
  filter(version == 'original') 

# Dataframes with Tukey labels
aspect_group_labels <- proportions %>% 
  full_join(., tukey_aspect_cld) %>% 
  filter(version == 'original') %>% 
  group_by(species, aspect, lhs, period, letters) %>% 
  summarise(y_position = max(value) + 0.01) %>% 
  filter(aspect == lhs) 

fire_group_labels <- proportions %>% 
  full_join(tukey_fire_cld) %>% 
  filter(version == 'original') %>% 
  group_by(species, fire, lhs, period, letters) %>% 
  summarise(y_position = max(value) + 0.01) %>% 
  filter(fire == lhs) 

props_plot %>% 
  filter(period != 'Survival_1to2', period != 'Establishment_yr1') %>% 
  mutate(species = ifelse(species == "PICO", 'Lodgepole pine', 'Douglas-fir')) %>% 
  ggplot(., aes(x = period, y = value, fill = aspect)) +
  #geom_boxplot(width = 0.3, position = position_dodge(width = 0.4)) +
  # stat_summary(size = 1, shape = 21, fun.data = 'mean_cl_boot', fun.args=(conf.int=.95),
  #              geom = "pointrange", position = position_dodge(width = 0.35)) +
  stat_summary(fun.y = 'mean', geom = "bar", position = 'dodge') +
  stat_summary(fun.data = 'mean_cl_boot', fun.args=(conf.int=.90), size = 0.75, width = 0.3,
               geom = "errorbar", position = position_dodge(width = 0.9)) +  
  scale_color_manual(values = colVals, name = 'aspect', guide = F) +
  scale_fill_manual(values = colVals, name = 'aspect', guide = F) +
  scale_x_discrete(labels = c('Germination', 'Survival (yr 1)', 'Survival (yr 2)', 'Establishment')) +
  # geom_text(data = aspect_group_labels,
  #           aes(x = aspect, y = 0.5, label = letters), 
  #           color = 'black', size = 2.5) +
  facet_wrap(~species, nrow = 1) +
  labs(y = 'Proportion of seeds') +
  theme_bw(base_size = 14) +
  theme(strip.background = element_blank(),
        #strip.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.subtitle = element_text(hjust = 0.5, face = 'bold'),
        axis.title.x = element_blank(),
        #axis.text.y = y_axis,
        #axis.title.y = element_blank()
        )


# Aspect ----------------------------------------------------------------------
aspect_plot_fn <- function(subset, y_axis, x_axis, strip_txt, title, multsize, label_ys){
    
  ggplot(filter(props_plot, period %in% subset), 
         aes(x = aspect, y = value, fill = aspect)) +
    geom_jitter(color = 'black', alpha = 0.3, size = 1*multsize, width = 0.2) +
    stat_summary(size = 0.5*multsize, shape = 21,
                 fun.data = 'mean_cl_boot', fun.args=(conf.int=.90), geom = "pointrange") +
    scale_color_manual(values = colVals, name = 'aspect', guide = F) +
    scale_fill_manual(values = colVals, name = 'aspect', guide = F) +
    geom_text(data = filter(aspect_group_labels, period == subset),
      aes(x = aspect, y = label_ys, label = letters), 
              color = 'black', size = 2.5*multsize) +
    facet_grid(species~period) +
    #coord_flip() +
    labs(y = 'Proportion of seeds', subtitle = title) +
    theme_bw(base_size = 14) +
    theme(strip.background = element_blank(),
          strip.text.x = element_blank(),
          strip.text.y = strip_txt,
          plot.subtitle = element_text(hjust = 0.5, face = 'bold'),
          axis.title.x = element_blank(),
          axis.text.y = y_axis,
          axis.title.y = element_blank()) }

estab_plot <- 
  aspect_plot_fn(subset = 'Establishment_yr2',
                 y_axis = element_blank(),
                 strip_txt = NULL,
                 title = 'Establishment - Year 2', 
                 label_ys = 0.3,
                 multsize = 2)

surv_plot <- 
  aspect_plot_fn(subset = 'Survival_yr1','Survival_yr2',
                 y_axis = element_blank(), 
                 strip_txt = element_blank(),
                 title = 'Survival - Year 2', 
                 label_ys = 0.8,
                 multsize = 2)

germ_plot <-
  aspect_plot_fn(subset = 'Germination',
                 y_axis = element_text(), 
                 strip_txt = element_blank(),
                 title = 'Germination', 
                 label_ys = 0.35,
                 multsize = 2)
  
#small <- plot_grid(germ_plot, surv_plot, ncol = 1, rel_heights = c(1,1.15))
#plot_grid(estab_plot, small, ncol = 2, rel_widths = c(2.3,1))
aspect_plot <- plot_grid(germ_plot, surv_plot, estab_plot, ncol = 3, rel_widths = c(1,0.9,1))
aspect_plot
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



# -----
# # Parametric bootstrap (compare full and reduced models), This is better because of small sample size
# 
# # Create dataframe to of each species x period model to fill
# models_df <- proportions[!duplicated(proportions[,c('species','period')]),
#                          c('species','period')] %>%
#   mutate(LRT_p = NA,
#          PB_p = NA,
#          South = NA,
#          Flat = NA,
#          North = NA)
# 
# 
# tukey_labels <- models_df %>% 
#   gather(aspect, tukey, South, Flat, North) %>% 
#   mutate(species = ifelse(species == "PICO", 'Lodgepole pine', 'Douglas-fir')) %>% 
#   filter(period != 'Survival_1to2', period != 'Establishment_yr1') 

# Loop through all models and do parametric bootstrap + post-hoc Tukey's
# for(i in 1:length(models_df[[1]])){
#   
#   # Filter dataframe for this iteration
#   model_dat <- proportions %>%
#     filter(version == 'original',
#            #species == 'PICO', period == 'Survival_yr1'
#            species == models_df[['species']][i], period == models_df[['period']][i]
#     ) %>% 
#     mutate(name = paste0(fire,aspect))
#   
#   # Remove NAs 
#   model_dat <- na.omit(model_dat)
#   
#   # Fit full and reduce models
#   fit_large <-
#     glmer(value ~ aspect + (1|name), data = model_dat,
#           family = binomial(link = "logit"), weights = Weights)
#   
#   fit_small <-
#     glmer(value ~ 1 + (1|name), data = model_dat,
#           family = binomial(link = "logit"), weights = Weights)
#   
#   # Perform parametric bootstrap comparing full and reduce to get p-values
#   # Perform the parametric bootstrap by simulating data with the full model.
#   # Fit dat with the full model
#   # Parametric bootstrap of H0
#   
#   # Tony Ives Approach
#   # nboot <- 100
#   # LLR.dat <- 2*(logLik(fit_large) - logLik(fit_small))[1]
#   # boot0 <- data.frame(LLR=rep(NA, nboot), converge=NA) 
#   # dat.boot <- model_dat
#   # 
#   # for(i in 1:nboot){
#   #   dat.boot$Y <- simulate(fit_small)[[1]]
#   #   mod.glm <- update(fit_large, data=dat.boot)
#   #   mod.glm0 <- update(fit_small, data=dat.boot)
#   #   boot0$LLR[i] <- 2*(logLik(mod.glm) - logLik(mod.glm0))[1]
#   #   boot0$converge[i] <- mod.boot0$converge
#   # }
#   # # Remove the cases when glm() did not converge
#   # boot0 <- boot0[boot0$converge == T,] pvalue <- mean(boot0$LLR > LLR.dat)
#   # 
#   
#   boot_Ps <- pbkrtest::PBmodcomp(fit_large, fit_small)
#   tukey <- cld(glht(fit_large, linfct = mcp(aspect = 'Tukey')))
#   
#   # Add the results to the dataframe
#   models_df[['LRT_p']][i] <- boot_Ps[['test']]['LRT','p.value']
#   models_df[['PB_p']][i] <- boot_Ps[['test']]['PBtest','p.value']
#   models_df[['South']][i] <- tukey$mcletters$Letters[1]
#   models_df[['Flat']][i] <- tukey$mcletters$Letters[2]
#   models_df[['North']][i] <- tukey$mcletters$Letters[3]
# }
# 
# models_df



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
