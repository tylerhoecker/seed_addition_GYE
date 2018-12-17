allometry_df <- read_csv('data/seedling_allometry.csv') 

allometry_df <- allometry_df %>% 
  mutate(total_mass = ag_mass + root_mass)


corr_mat <- allometry_df %>% 
  select(height,basal,ag_mass,root_mass,total_mass) 

corr_mat <- Hmisc::rcorr(as.matrix(corr_mat), type="pearson") 

# # Model 
ecophys_model = lm(log(total_mass) ~ log(basal^2) + log(height), data = allometry_df)  


rp_label <- function(r,p){
  r.input = r
  p.input = p
  text = substitute(italic(R)^2~"="~r.input*","~~italic(p)~"="~p.input,
                    list(r.input = format(r, digits = 2),
                         p.input = format(p, digits = 2)))
  as.character(as.expression(text))                 
}

# Plotting--------- moved to Rmd "reasearch_updates"

# r.text = corr_mat$r['total_mass','height'] * corr_mat$r['total_mass','height']
# p.text = corr_mat$P['total_mass','height']
# 
# ggplot(allometry_df, aes(x = total_mass, y = height)) +
#   geom_point(shape = 21, fill = 'black', size = 2.5, alpha = 0.7) +
#   geom_smooth(method = 'lm', color = 'red3') +
#   labs(y = 'Height (mm)', x = 'Total mass (g)') +
#   annotate('text', x = 0.2, y = 90, 
#             label = rp_label(r.text,p.text), parse = TRUE, size = 6) +
#   theme_bw(base_size = 14)

# r.text = corr_mat$r['total_mass','basal'] * corr_mat$r['total_mass','basal']
# p.text = corr_mat$P['total_mass','basal']
# 
# ggplot(allometry_df, aes(x = total_mass, y = basal)) +
#   geom_point(shape = 21, fill = 'black', size = 2.5, alpha = 0.7) +
#   geom_smooth(method = 'lm', color = 'red3') +
#   labs(y = 'Basal (mm)', x = 'Total mass (g)') +
#   annotate('text', x = 0.2, y = 2.5, 
#            label = rp_label(r.text,p.text), parse = TRUE, size = 6) +
#   theme_bw(base_size = 14)


# 
# summary(ecophys_model)
# 
# allometry_df <- allometry_df %>% 
#   mutate(resids = ecophys_model$residuals)
# 
# ggplot(allometry_df, aes(x = total_mass, y = resids)) +
#   geom_point() +
#   geom_smooth(se = 'F', color = 'black') +
#   labs(x = 'Total mass (g)', y = 'Model residuals') +
#   theme_bw(base_size = 14)

