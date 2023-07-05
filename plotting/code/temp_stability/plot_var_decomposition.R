
#  DESCRIPTION -------------------------------------------------------------

# Script that plots the results of theVariance Decomposition analysis
# Code adapted from plotting function of the specr package
# https://github.com/masurp/specr/tree/master
# Masur, Philipp K. & Scharkow, M. (2020). specr: Conducting and Visualizing Specification Curve Analyses. Available from https://CRAN.R-project.org/package=specr.


# Author(s): Alexandra Bagaini, Centre for Cognitive and Decision Sciences, Faculty of Psychology, University of Basel.


# PACKAGES ----------------------------------------------------------------


library(tidyverse)
library(patchwork)


# FUNCTIONS ---------------------------------------------------------------

source("helper_functions.R")


# FILES  ---------------------------------------------------

data_path <- c("analysis/output/temp_stability/") # where is the input file stored
output_path <- c("plotting/output/temp_stability/") # where to store the output 


# OMNIBUS PLOTTING ----------------------------------------------------------------

main_shapley_vals <- read_csv(paste0(data_path, "shapley_values_omni_retest.csv")) %>% 
  mutate(measure_category = "Omnibus")

predictors <- unique(main_shapley_vals$x)

w_df <- main_shapley_vals %>% 
  group_by(n_reg_with, x, measure_category ) %>% 
  summarise(w = 1/(length(predictors)*n())) 


t <- main_shapley_vals %>% 
  left_join(w_df, by = c("n_reg_with", "x", "measure_category")) %>% 
  group_by(x, measure_category) %>% 
  summarise(m = weighted.mean(r2adj_increment, w = w))%>% 
  rowwise() %>% 
  mutate(x_lbl = lbl_pred_replace(x),
         categ_lbl = lbl_categ_replace(x)) %>% 
  ungroup()


boot_shapley_vals <-  read_csv(paste0(data_path, "shapley_values_omni_retest_boot.csv")) %>% 
  mutate(measure_category = "Omnibus")


boot_w_df <- boot_shapley_vals %>% 
  group_by(n_reg_with, x, boot_num, measure_category) %>% 
  summarise(w = 1/(length(predictors)*n())) 


boot_t <- boot_shapley_vals %>% 
  left_join(boot_w_df, by = c("n_reg_with", "x", "boot_num", "measure_category")) %>% 
  group_by(x, boot_num, measure_category) %>% 
  summarise(m = weighted.mean(r2adj_increment, w = w)) %>% 
  group_by(x, measure_category) %>% 
  mean_qi(m, .width = c(.5,.8, .95)) %>% 
  pivot_wider(names_from = .width, values_from = c(.lower,.upper)) %>% 
  ungroup() %>% 
  rowwise() %>% 
  mutate(x_lbl = lbl_pred_replace(x),
         categ_lbl = lbl_categ_replace(x)) %>% 
  ungroup()


boot_t$categ_lbl <-  factor(boot_t$categ_lbl, levels= c('Measure', "Respondent", "Panel"))
t$categ_lbl <-  factor(t$categ_lbl, levels= c('Measure', "Respondent", "Panel"))

p_omni <- ggplot(boot_t, aes(x=reorder(x_lbl,m),y = m )) +
  geom_crossbar(aes(ymin = .lower_0.95, ymax = .upper_0.95), color = "NA", fill = "white", linewidth = .15,
                width = 0.3, alpha =  1) +
  geom_crossbar(aes(ymin = .lower_0.95, ymax = .upper_0.95), color = "NA", fill = "#23583A", linewidth = .15,
                width = 0.3, alpha =  .4) +
  geom_crossbar(aes(ymin = .lower_0.8, ymax = .upper_0.8), color = "NA", fill = "white",linewidth = .15,
                width = 0.3, alpha =  1) +
  geom_crossbar(aes(ymin = .lower_0.8, ymax = .upper_0.8), color = "NA", fill = "#23583A",linewidth = .15,
                width = 0.3, alpha =  .7) +
  geom_crossbar(aes(ymin =.lower_0.5, ymax = .upper_0.5), color = "NA", fill = "white",linewidth = .15,
                width = 0.3, alpha =  1) +
  geom_crossbar(aes(ymin =.lower_0.5, ymax = .upper_0.5), color = "NA", fill = "#23583A",linewidth = .15,
                width = 0.3, alpha =  1) +
  geom_point(data = t,
             aes(x = reorder(x_lbl,m), y = m),
             color = "grey30", alpha = 1,fill = "grey95",
             size = 1.5, shape = 21, stroke = .5) +
  labs(y = "Proportion of variance explained",
       fill = "", color = "", tag = "A",
       title = "") +
  coord_flip() +
  theme_void() +
  facet_grid(categ_lbl~., space = "free", scales = "free", switch = "y") +
  # geom_segment(aes(y = 0, yend = .2, x = -Inf, xend = -Inf), color = "grey80", size = 0.15, linetype = "solid")+
  # scale_x_discrete(expand = c(0.0,0.0)) +
  scale_y_continuous(limits = c(0,.20), breaks = seq(0,.20,.05),expand = c(0.0,0.0),
                     labels = c("0", format(seq(.05,.20,.05), nsmall = 2))) +
  theme(axis.text.x  = element_text(family = "Source Sans 3", size = 10, colour = "grey20",
                                    margin = margin(t = 5, b = 5),
                                    hjust=seq(0,1, length.out = 5)),
        axis.text.y = element_text(family = "Source Sans 3", size = 9.5, colour = "grey20",
                                   hjust = 1 ,margin = margin(l = 5, r = 10)),
        axis.title.x = element_text(family = "Source Sans 3 Light", size = 9, colour = "grey20",margin = margin(b = 5)),
        # panel.grid.major.x  = element_line(color = "grey80", size = 0.25, linetype = "solid"),
        # panel.grid.minor.x  = element_line(color = "grey80", size = 0.25, linetype = "solid"),
        plot.tag =  element_text(family = "Source Sans 3", size = 10,face = "bold",colour = "grey20",margin = margin(b = 10)),
        
        panel.grid = element_blank(),
        panel.grid.major.x = element_line(linewidth = .1, color = "grey80", linetype = "dotted"),
        # panel.grid.major.y = element_line(linewidth = .25, color = "grey80"),
        panel.background = element_rect(color = "grey50", size = .25),
        strip.text = element_text(family = "Source Sans 3 Light", size = 9, angle = 90,colour = "grey20"),
        strip.placement = "outside",
        plot.margin = margin(l = 10, r = 10, t = 10),
        plot.title.position = "plot",
        plot.title = element_blank(),
        plot.caption = element_text(family = "Source Sans 3", size = 9, lineheight = .75),
        axis.ticks.length = unit(0, "cm"),
        legend.title = element_blank(),
        legend.text = element_text(family = "Source Sans 3", face= "bold",size = 12, colour = "grey20"),
        legend.title.align=0.5,
        legend.margin = margin(0),
        legend.position = "none",
        legend.direction='horizontal',
        legend.spacing.y = unit(0, 'cm'),
        legend.spacing.x = unit(1.0, 'cm'),
        legend.key.size = unit(0.75,"line")) 

p_omni


# 
# 
# ggsave(plot = p_omni, filename = paste0(output_path, "shapley_decomp_omni_retest_fig.png"),
#        dpi = 300, width = 25, height = 10, units = "cm") 





# BY MEASURE CATEG. PLOTTING ----------------------------------------------------------------

## read shapley values from data set
main_shapley_vals_fre <- read_csv(paste0(data_path,"shapley_values_fre_retest.csv")) %>% 
  mutate(measure_category = "Frequency")

main_shapley_vals_pro <- read_csv(paste0(data_path,"shapley_values_pro_retest.csv")) %>% 
  mutate(measure_category = "Propensity")


main_shapley_vals_beh <- read_csv(paste0(data_path,"shapley_values_beh_retest.csv")) %>% 
  mutate(measure_category = "Behaviour")

main_shapley_vals <- bind_rows(main_shapley_vals_beh,
                               main_shapley_vals_fre,
                               main_shapley_vals_pro)



predictors <- unique(main_shapley_vals$x)
w_df <- main_shapley_vals %>% 
  group_by(n_reg_with, x, measure_category ) %>% 
  summarise(w = 1/(length(predictors)*n())) 


t <- main_shapley_vals %>% 
  left_join(w_df, by = c("n_reg_with", "x", "measure_category")) %>% 
  group_by(x, measure_category) %>% 
  summarise(m = weighted.mean(r2adj_increment, w = w))%>% 
  rowwise() %>% 
  mutate(x_lbl = lbl_pred_replace(x),
         categ_lbl = lbl_categ_replace(x)) %>% 
  ungroup() 



## read shapley values from boostrapped data set
boot_shapley_vals_fre <- read_csv(paste0(data_path,"shapley_values_fre_retest_boot.csv")) %>% 
  mutate(measure_category = "Frequency")

boot_shapley_vals_pro <- read_csv(paste0(data_path,"shapley_values_pro_retest_boot.csv")) %>% 
  mutate(measure_category = "Propensity")


boot_shapley_vals_beh <- read_csv(paste0(data_path,"shapley_values_beh_retest_boot.csv")) %>% 
  mutate(measure_category = "Behaviour")

boot_shapley_vals <- bind_rows(boot_shapley_vals_beh,
                               boot_shapley_vals_fre,
                               boot_shapley_vals_pro)


boot_w_df <- boot_shapley_vals %>% 
  group_by(n_reg_with, x, boot_num, measure_category) %>% 
  summarise(w = 1/(length(predictors)*n())) 

boot_t <- boot_shapley_vals %>% 
  left_join(boot_w_df, by = c("n_reg_with", "x", "boot_num", "measure_category")) %>% 
  group_by(x, boot_num, measure_category) %>% 
  summarise(m = weighted.mean(r2adj_increment, w = w)) %>% 
  group_by(x, measure_category) %>% 
  mean_qi(m, .width = c(.5,.8, .95)) %>% 
  pivot_wider(names_from = .width, values_from = c(.lower,.upper)) %>% 
  ungroup() %>% 
  rowwise() %>% 
  mutate(x_lbl = lbl_pred_replace(x),
         categ_lbl = lbl_categ_replace(x)) %>% 
  ungroup() 

t_sum <- t %>% group_by(x_lbl) %>% summarise(mn = mean(m, na.rm = T)) 
factor_order <- order(t_sum$mn, decreasing = FALSE)

boot_t$measure_category <-  factor(boot_t$measure_category, levels= rev(c('Propensity','Frequency','Behaviour')))
t$measure_category <-  factor(t$measure_category, levels= rev(c('Propensity','Frequency','Behaviour')))
boot_t$categ_lbl <-  factor(boot_t$categ_lbl, levels= c('Measure', "Respondent", "Panel"))
t$categ_lbl <-  factor(t$categ_lbl, levels= c('Measure', "Respondent", "Panel"))
boot_t$x_lbl <-  factor(boot_t$x_lbl, levels=t_sum$x_lbl[factor_order])
t$x_lbl <-  factor(t$x_lbl, levels=t_sum$x_lbl[factor_order])




line_segment_df <- t %>% distinct(x_lbl, categ_lbl) %>%  
  group_by(categ_lbl) %>% 
  reframe(y = 0,
          yend = .2,
          x = 1:n() -1 +.5,
          xend =  1:n() -1 + .5) %>% 
  crossing(measure_category = unique(t$measure_category))

p_categ <- ggplot(boot_t, aes(x=x_lbl,y = m, fill = measure_category, # color = measure_category,
                              group=measure_category )) +
  geom_crossbar(aes(ymin = .lower_0.95, ymax = .upper_0.95), 
                position = position_dodge(width = 0.9), linewidth = 0,
                fill = "white",
                width = 0.7, alpha =  1) +
  geom_crossbar(aes(ymin = .lower_0.95, ymax = .upper_0.95), 
                position = position_dodge(width = 0.9), linewidth = 0,
                width = 0.7, alpha =  .4) +
  geom_crossbar(aes(ymin = .lower_0.8, ymax = .upper_0.8), 
                position = position_dodge(width = 0.9),linewidth = 0,
                fill = "white",
                width = 0.7, alpha =  1) +
  geom_crossbar(aes(ymin = .lower_0.8, ymax = .upper_0.8), 
                position = position_dodge(width = 0.9),linewidth = 0,
                width = 0.7, alpha =  .7) +
  geom_crossbar(aes(ymin = .lower_0.5, ymax = .upper_0.5), 
                position = position_dodge(width = 0.9),linewidth = 0,
                fill = "white",
                width = 0.7, alpha =  1) +
  geom_crossbar(aes(ymin = .lower_0.5, ymax = .upper_0.5), 
                position = position_dodge(width = 0.9), linewidth = 0,
                width = 0.7, alpha =  1) +
  geom_point(data = t,
             aes(x = x_lbl, y = m, group = measure_category),
             color = "grey20", alpha = 1,  fill = "white",
             size = 2, shape = 21, stroke = .5, position = position_dodge(width = .9, preserve = "total")) +
  labs(y = "Proportion of variance explained", tag = "B",
       fill = "", color = "",
       title = "") +
  coord_flip(ylim = c(0,.2)) +
  theme_void() +
  facet_grid(categ_lbl~., space = "free", scales = "free", switch = "y") +
  # scale_fill_manual(values = c_palette) +
  # scale_color_manual(values = c_palette) +
  scale_color_manual("", 
                     breaks = c("Propensity", "Frequency", "Behaviour"),
                     values = c("#502a7a", "#1e9bae","#e07f00"), 
                     labels = c("Propensity", "Frequency", "Behaviour")) +
  scale_fill_manual("", 
                    breaks = c("Propensity", "Frequency", "Behaviour"),
                    values = c("#502a7a", "#1e9bae","#e07f00"), 
                    labels = c("Propensity", "Frequency", "Behaviour")) +
  geom_segment(data = line_segment_df, aes(x = x, xend = xend, yend = yend, y = y), color = "grey80", size = 0.15, linetype = "dotted") +
  scale_y_continuous(limits = c(0,.20), breaks = seq(0,.20,.05),expand = c(0.0,0.0),
                     labels = c("0", format(seq(.05,.20,.05), nsmall = 2))) +
  theme(axis.text.x  = element_text(family = "Source Sans 3", size = 10, colour = "grey20",
                                    margin = margin(t = 5, b = 5),
                                    hjust=seq(0,1, length.out = 5)),
        axis.text.y = element_text(family = "Source Sans 3", size = 9.5, colour = "grey20",
                                   hjust = 1 ,margin = margin(l = 5, r = 10)),
        axis.title.x = element_text(family = "Source Sans 3 Light", size = 9, colour = "grey20",margin = margin(b = 5)),
        # panel.grid.major.x  = element_line(color = "grey80", size = 0.25, linetype = "solid"),
        # panel.grid.minor.x  = element_line(color = "grey80", size = 0.25, linetype = "solid"),
        plot.tag =  element_text(family = "Source Sans 3", size = 10,face = "bold",colour = "grey20",margin = margin(b = 10)),
        
        panel.grid = element_blank(),
        panel.grid.major.x = element_line(linewidth = .1, color = "grey80", linetype = "dotted"),
        # panel.grid.major.y = element_line(linewidth = .25, color = "grey80"),
        plot.title.position = "plot",
        panel.background = element_rect(color = "grey50", size = .25),
        strip.text = element_text(family = "Source Sans 3 Light", size = 9, angle = 90,colour = "grey20"),
        strip.placement = "outside",
        plot.margin = margin(l = 10, r = 10),
        # panel.background  = element_rect(fill = NA, color = "grey70", size = .4),
        plot.title = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        legend.title = element_blank(),
        legend.text = element_text(family = "Source Sans 3", size = 10, colour = "grey20"),
        legend.title.align=0.5,
        legend.margin = margin(rep(5,3), r = 0),
        # legend.position = "top",
        legend.position = "top",
        legend.justification = "center",
        legend.direction='horizontal',
        legend.spacing.x = unit(1, 'cm'),
        # legend.background = element_rect(color = "grey80", fill = "NA", size = .15),
        legend.spacing.y = unit(.1, 'cm'),
        # legend.key = element_rect(size = 5),
        legend.key.height =  unit(0.25,"cm"),
        legend.key.width =  unit(1,"cm")
        # legend.key.size = unit(0.75,"line")
  ) +
  guides(color = "none",
         fill = guide_legend(label.position = "bottom",
                             ncol = 3,
                             override.aes = list(alpha = 1, color = NA)))



p_categ

# ggsave(plot = p_categ, filename = paste0(output_path, "shapley_decomp_retest_fig.png"),
#        dpi = 300, width = 25, height = 15, units = "cm") 




# COMBINE -----------------------------------------------------------------




p <- p_omni /p_categ + plot_layout(heights = c(0.75,1.25))

ggsave(plot = p, filename = paste0(output_path, "shapley_decomp_retest_fig.png"),
       dpi = 300, width = 25, height = 20, units = "cm")

