
#  DESCRIPTION -------------------------------------------------------------

# Script that plots the results of the Variance Decomposition analysis

# Author(s): Alexandra Bagaini, Centre for Cognitive and Decision Sciences, Faculty of Psychology, University of Basel.


# PACKAGES ----------------------------------------------------------------


library(tidyverse)


# FUNCTIONS ---------------------------------------------------------------

source("helper_functions.R")


# FILES  ---------------------------------------------------

data_path <- c("analysis/output/convergent_val/") # where is the input file stored
output_path <- c("plotting/output/convergent_val/") # where to store the output 


# OMNIBUS PLOTTING ----------------------------------------------------------------

main_shapley_vals <- read_csv(paste0(data_path, "shapley_values_intercor.csv")) %>% 
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


boot_shapley_vals <-  read_csv(paste0(data_path, "shapley_values_intercor_boot.csv")) %>% 
  mutate(measure_category = "Omnibus")


boot_w_df <- boot_shapley_vals %>% 
  group_by(n_reg_with, x, boot_num, measure_category) %>% 
  summarise(w = 1/(length(predictors)*n())) 


boot_t <- boot_shapley_vals %>% 
  left_join(boot_w_df, by = c("n_reg_with", "x", "boot_num", "measure_category")) %>% 
  group_by(x, boot_num, measure_category) %>% 
  summarise(m = weighted.mean(r2adj_increment, w = w)) %>% 
  group_by(x, measure_category) %>% 
  summarise(m_l95 = quantile(m, .025),
            m_u95 = quantile(m, .975),
            m_50 = quantile(m, .5),
            m_l50 = quantile(m, .25),
            m_u50 = quantile(m, .75),
            m_l80 = quantile(m, .10),
            m_u80 = quantile(m, .90)) %>% 
  rowwise() %>% 
  mutate(x_lbl = lbl_pred_replace(x),
         categ_lbl = lbl_categ_replace(x)) %>% 
  ungroup()


boot_t$categ_lbl <-  factor(boot_t$categ_lbl, levels= rev(c('Panel','Respondent','Measure')))
t$categ_lbl <-  factor(t$categ_lbl, levels= rev(c('Panel','Respondent','Measure')))

p_omni <- ggplot(boot_t, aes(x=reorder(x_lbl,m_50),y = m_50 )) +
  geom_crossbar(aes(ymin = m_l95, ymax = m_u95), color = "NA", fill = "white", linewidth = .15,
                width = 0.3, alpha =  1) +
  geom_crossbar(aes(ymin = m_l95, ymax = m_u95), color = "NA", fill = "#23583A", linewidth = .15,
                width = 0.3, alpha =  .4) +
  geom_crossbar(aes(ymin = m_l80, ymax = m_u80), color = "NA", fill = "white",linewidth = .15,
                width = 0.3, alpha =  1) +
  geom_crossbar(aes(ymin = m_l80, ymax = m_u80), color = "NA", fill = "#23583A",linewidth = .15,
                width = 0.3, alpha =  .7) +
  geom_crossbar(aes(ymin = m_l50, ymax = m_u50), color = "NA", fill = "white",linewidth = .15,
                width = 0.3, alpha =  1) +
  geom_crossbar(aes(ymin = m_l50, ymax = m_u50), color = "NA", fill = "#23583A",linewidth = .15,
                width = 0.3, alpha =  1) +
  geom_point(data = t,
             aes(x = reorder(x_lbl,m), y = m),
             color = "grey30", alpha = 1,fill = "grey95",
             size = 1.5, shape = 21, stroke = .5) +
  labs(y = "Proportion of variance explained",
       fill = "", color = "", 
       title = "") +
  coord_flip() +
  theme_void() +
  facet_grid(categ_lbl~., space = "free", scales = "free", switch = "y") +
  # geom_segment(aes(y = 0, yend = .2, x = -Inf, xend = -Inf), color = "grey80", size = 0.15, linetype = "solid")+
  # scale_x_discrete(expand = c(0.0,0.0)) +
  scale_y_continuous(limits = c(0,.15), breaks = seq(0,.15,.05),expand = c(0.0,0.0),
                     labels = c("0", format(seq(.05,.15,.05), nsmall = 2))) +
  theme(axis.text.x  = element_text(family = "Source Sans 3", size = 10, colour = "grey20",
                                    margin = margin(t = 5, b = 5),
                                    hjust=seq(0,1, length.out = 5)),
        axis.text.y = element_text(family = "Source Sans 3", size = 9.5, colour = "grey20",
                                   hjust = 1 ,margin = margin(l = 5, r = 10)),
        axis.title.x = element_text(family = "Source Sans 3 Light", size = 9, colour = "grey20",margin = margin(b = 5)),
        # panel.grid.major.x  = element_line(color = "grey80", size = 0.25, linetype = "solid"),
        # panel.grid.minor.x  = element_line(color = "grey80", size = 0.25, linetype = "solid"),
        plot.tag =  element_blank(),
        
        panel.grid = element_blank(),
        panel.grid.major.x = element_line(linewidth = .25, color = "grey80", linetype = "dotted"),
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
ggsave(plot = p_omni, filename = paste0(output_path, "shapley_decomp_convergent.png"),
       dpi = 300, width = 25, height = 10, units = "cm") 


