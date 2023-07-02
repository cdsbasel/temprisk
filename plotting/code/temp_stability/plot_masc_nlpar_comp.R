
# DESCRIPTION -------------------------------------------------------------

# Script to plot the estimated values of the paramaters of the MASC model
# using the risk pref data and the data used by Anusic and Schimmack. 


# Author(s): Alexandra Bagaini, Centre for Cognitive and Decision Sciences, Faculty of Psychology, University of Basel.


# PACKAGES ----------------------------------------------------------------

library(tidyverse)
library(ggtext)

# FUNCTIONS ---------------------------------------------------------------

source("helper_functions.R")
# DATA --------------------------------------------------------------------

data_path <- c("analysis/output/temp_stability/") # where is the  data stored
output_path <- c("plotting/output/temp_stability/") # where to store the output data

pred_df <- read_csv(paste0(data_path, "masc_nlpar_pred.csv"))

# PLOTTING NLPAR ------------------------------------------------------

palette_col <- c("Construct" = "#003d5b",
                 "Behaviour" = "#e07f00",
                 "Frequency" = "#1e9bae",
                 "Propensity" =  "#502a7a")

# ordering domains/constructs
pred_df_orderA <- pred_df %>% 
  filter(nlpar == "Reliability") %>% 
  filter(sub_component != "**Overall**") %>% 
  group_by(measure) %>% 
  arrange(.epred) %>% 
  mutate(rank = 1+1:n()) %>% 
  ungroup() %>% 
  select(sub_component, measure, rank )

pred_df_orderB <- pred_df %>% 
  filter(nlpar == "Reliability") %>% 
  filter(sub_component == "**Overall**") %>% 
  mutate(rank = 1) %>% 
  select(sub_component, measure, rank)

pred_df_order <- bind_rows(pred_df_orderB, pred_df_orderA)

pred_df <- pred_df %>% left_join(pred_df_order, by = c("measure", "sub_component"))


line_df_overall <- tibble(measure = c("Propensity", "Frequency", "Behaviour"),
                          sub_component = "**Overall**",
                          y = 0,
                          yend = 1)


line_df_overall$measure <- factor(line_df_overall$measure, levels = c("Propensity", "Frequency", "Behaviour", "Construct")) 
pred_df$nlpar <- factor(pred_df$nlpar, levels = c("Reliability", "Change", "Stab. Change"))
pred_df$measure <- factor(pred_df$measure, levels = c("Propensity", "Frequency", "Behaviour", "Construct")) 



p <- pred_df %>% ggplot(aes(fill = measure)) +
  geom_crossbar(aes(xmin = .lower_0.95, x = .epred, 
                    xmax = .upper_0.95, y = reorder(sub_component, rank)),
                fill = "white", color = "NA",
                linewidth = .15,width = 0.25, alpha =  1) +
  geom_crossbar(aes(xmin = .lower_0.95, x = .epred, 
                    xmax = .upper_0.95, y = reorder(sub_component, rank)),
                color = "NA",
                linewidth = .15,width = 0.25, alpha =  .3) +
  geom_crossbar(aes(xmin = .lower_0.8, x = .epred,
                    xmax = .upper_0.8, y = reorder(sub_component, rank)),
                fill = "white", color = "NA",
                linewidth = .15,width = 0.25, alpha =  1) +
  geom_crossbar(aes(xmin = .lower_0.8, x = .epred,
                    xmax = .upper_0.8, y = reorder(sub_component, rank)),
                color = "NA",
                linewidth = .15,width = 0.25, alpha =  .6) +
  geom_crossbar(aes(xmin = .lower_0.5, x = .epred, 
                    xmax = .upper_0.5, y = reorder(sub_component, rank)),
                fill = "white",color = "NA",
                linewidth = .15,width = 0.25, alpha =  1) +
  geom_crossbar(aes(xmin = .lower_0.5, x = .epred, 
                    xmax = .upper_0.5, y = reorder(sub_component, rank)),
                color = "NA",
                linewidth = .15,width = 0.25, alpha =  .9) +
  geom_point(aes(x = .epred, y = reorder(sub_component, rank)),
             fill = "white", color = "grey20",
             shape = 21, 
             stroke = .3, 
             size = 1.25) +
  facet_grid(measure~nlpar, scales = "free_y", space = "free",switch = "y") + # 
  # scale_x_continuous(limits = c(0,1), expand = c(0,0)) +
  scale_x_continuous(limits = c(0,1), expand = c(0,0), breaks = c(0,.5,1), labels = c("0","0.5" ,"1")) +
  scale_y_discrete(position = "left") +
  scale_fill_manual(values = palette_col)+
  theme_minimal() +
  geom_segment(data = line_df_overall, aes(y = sub_component, yend = sub_component,
                                           x = y, xend = yend), 
               linetype = "dashed", color = "grey70",
               linewidth = .2,
               position = position_nudge(y = .3))+
  # geom_rect(data = subset(pred_df_pro, x %in% c("**Overall**")), 
  #           fill = NA, colour = "black", size = .75, xmin = -Inf,xmax = Inf,
  #           ymin = -Inf,ymax = Inf) +
  theme(panel.grid = element_blank(),
        legend.position = "none",
        plot.title.position = "plot",
        strip.placement = "outside",
        plot.tag = element_text(family = "Source Sans 3", face = "bold", size = 11),
        strip.text.y = element_text(family = "Source Sans 3", face = "bold"),
        axis.title.x = element_text(family = "Source Sans 3", size = 9, color = "grey20"),
        plot.margin = margin(b = 10, t = 10, r = 15, l = 0),
        panel.spacing.x = unit(.1, "cm"),
        panel.spacing.y = unit(.25, "cm"),
        panel.grid.major.x  = element_line(linewidth = .25, color = "grey80", linetype = "dotted"),
        panel.grid.minor.x  = element_line(linewidth = .25, color = "grey80", linetype = "dotted"),
        
        panel.background = element_rect(linewidth = .25, color = "grey50", fill = "NA"),
        # plot.background = element_rect(linewidth = .25, color = "grey40", fill = "NA"),
        strip.text =  element_text(family = "Source Sans 3", face = "bold"),
        # plot.title = element_text(family = "Source Sans 3", face = "bold", hjust = 0,
        # size = 11, colour = "grey20", margin = margin(b = 7)),
        plot.title = element_blank(),
        # title = element_text(family = "Source Sans 3", face = "bold", size = 9),
        axis.text.x =  element_markdown(family = "Source Sans 3", color = "black", size = 8, hjust=c(0,.5, 1)),
        axis.text.y.left =  element_markdown(family = "Source Sans 3", angle = 0, hjust = 1, color = "grey20", size = 8), # hjust = c(0,.5,.5,.5,1)
        text = element_text(family = "Source Sans 3")) +
  labs(y = "", x = "Parameter Estimate", tag = "") 

p




# SAVE OUTPUT -----------------------------------------------------------

ggsave(plot = p, filename = paste0(output_path, "masc_param_comp_fig.png"), 
       dpi = 300, width = 19, height = 17, units = "cm") 

