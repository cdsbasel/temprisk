
# DESCRIPTION -------------------------------------------------------------

#Plotting the MASC model predictions of test-retest correlations split by domain-category-sample.
#Plotting MASC prediction for constrcuts

# Author(s): Alexandra Bagaini, Centre for Cognitive and Decision Sciences, Faculty of Psychology, University of Basel.



# PACKAGES ---------------------------------------------------------------

library(tidyverse)
library(brms)
library(patchwork)
library(tidybayes)
library(geomtextpath)
library(ggtext)

# DATA --------------------------------------------------------------------

data_w_path <- c("processing/output/temp_stability/") # where is the  data stored
output_data_path <- c("docs/images/") # where to store the output data
model_path <- c("analysis/output/temp_stability/") # where is the  data stored
source("helper_functions.R")
data_file <- "complete_agg_retest_yb10.csv" # name of retest data 
data_file_as <- "anusic_schimmack_2015.csv"
dat <- read_csv(paste0(data_w_path,data_file))
dat_as <- read_csv(paste0(data_w_path,data_file_as)) 

masc_list <- list(pro = read_rds(paste0(model_path, "masc_pro.rds")),
                  fre = read_rds(paste0(model_path, "masc_fre.rds")),
                  beh = read_rds(paste0(model_path, "masc_beh.rds")),
                  as = read_rds(paste0(model_path, "masc_as.rds")))

# PROPENSITY TIME --------------------------------------------------------

fit_masc <- masc_list[["pro"]]

domain_sample <- fit_masc$data %>% group_by(sample) %>% 
  mutate(age_dec_c = weighted.mean(age_dec_c, weights = (1/(sei^2)))) %>% 
  ungroup() %>% 
  distinct(domain_name,sample,age_dec_c)

nd <- crossing(domain_sample,
               time_diff_dec = seq(0, 20, .25)/10,
               female_prop_c = 0,
               sei = 0.1)
nd <- nd %>% mutate(age_dec_c2 = age_dec_c^2)

epred_draws_df_time <- fit_masc %>% 
  epred_draws(newdata = nd)






data_w_time <- dat %>% 
  filter(data_transform == "none" &
           age_group != "10-90" &
           gender_group != "all" & 
           rho_val == .5 &
           month_bin == 3 &
           age_bin == 10 & 
           min_n == 30 &
           cor_metric == "pearson" &
           measure_category == "pro") %>% 
  mutate(time_diff_dec = time_diff_bin/10,
         female_prop_c = case_when(gender_group == "male" ~ -0.5,
                                   gender_group == "female" ~ 0.5),
         age_dec_c = (mean_age - 40)/10,
         age_dec_c2 = age_dec_c^2) 


dom_vec<- unique(data_w_time$domain_name)


for (curr_domain in dom_vec) {
  
  plot_title <- rename_domain(curr_domain)
  
  
  
  p <-  ggplot(filter(epred_draws_df_time, domain_name == curr_domain)) +
    geom_point(data = filter(data_w_time, domain_name == curr_domain), aes(x = time_diff_dec*10, y = wcor),
               shape = 21,
               color = "#502a7a",
               fill = "grey50",
               size = 1,
               stroke = .2,
               alpha = .5) +
    stat_lineribbon(alpha = 1/4, aes(x = time_diff_dec*10, y = .epred, fill = "Overall", color = "Overall")) + 
    # geom_line(data = filter(epred_draws_agg_time, domain_name == curr_domain), 
    #           aes(x = time_diff_dec*10, y = .epred),
    #           color = "#502a7a",
    #           size = .5) +
    facet_wrap(sample~., ncol = 6) +
    theme_minimal() +
    # scale_color_manual(values = c("Overall" = "white" )) +
    scale_fill_manual(values = c("Overall" = "#502a7a" )) +
    # scale_linetype_manual(values = c("11", "42", "7212")) +
    scale_color_manual(values = c("Overall" = "#502a7a" )) +
    labs(y = "Retest Correlation", x = "Retest Interval (Years)", color = "", linetype = "", fill = "",
         title = paste0("Propensity: ", plot_title)) +
    theme(strip.placement = "outside",
          legend.position = "none", # c(0,0) bottom left, c(1,1) top-right.
          # legend.justification = c(0.5,0.5),
          legend.margin = margin(-.5,0,0,0, unit="cm"),
          legend.spacing.y = unit(0.15, 'cm'),
          legend.key.width = unit(1, "cm"),
          legend.key.size = unit(.3, "cm"),
          legend.text = element_text(family = "Source Sans 3", size = 8.5, color = "grey20"),
          # legend.background = theme_rect(fill = "white", colour = NA),
          text = element_text(family = "Source Sans 3", size = 9, color = "grey40"),
          axis.text.y = element_text( vjust=seq(0,1, length.out = 5)),
          axis.text.x = element_text( hjust=c(0,1)),
          title = element_text(family = "Source Sans 3", size = 9, color = "grey20"),
          plot.background = element_rect(color = NA, fill = "white"),
          # axis.title.x = element_text(margin = margin(t = 5)),
          # title = element_text(family = "Source Sans 3", size = 9, face = "bold", color = "grey20"),
          panel.spacing = unit(.5, "lines"),
          strip.text = element_text(family = "Source Sans 3", face = "bold", color = "grey20",
                                    margin = margin(b = 5)),
          plot.title = element_text(family = "Source Sans 3", face = "bold", hjust = 0,
                                    size = 11, colour = "grey20", margin = margin(b = 7, t = 5)),
          panel.grid = element_blank(),
          plot.title.position = "plot",
          plot.margin = margin(b = 5, r = 5, l = 5),
          panel.background = element_rect(color = "grey75", fill = "white", size = .4)) +
    guides(color = guide_legend(override.aes = list(size = .75)),
           fill =  guide_legend(override.aes = list(size = .75)),
           size = "none", linetype = guide_legend(override.aes = list(size = .75))) +
    coord_cartesian(ylim = c(0, 1), xlim = c(0,20))+
    scale_y_continuous(breaks = seq(0,1,0.25), expand = c(0,0)) +
    scale_x_continuous(breaks = c(0,20), expand = c(0,0))
  
  
  panel_num <- length(unique(epred_draws_df_time$sample[epred_draws_df_time$domain_name == curr_domain]))
  
  width = if_else(panel_num >= 6, 3.5*6,4.5*panel_num)
  height = if_else(panel_num > 6,4*ceiling(panel_num/6),5*ceiling(panel_num/6))
  
  ggsave(plot = p, filename = paste0(output_data_path,"p_masc_panel_pred_pro_",curr_domain, ".png"),
         width = width, height = height, dpi = 300, units = "cm")
  
}



# FREQUENCY TIME ---------------------------------------------------------------

fit_masc <- masc_list[["fre"]]

domain_sample <- fit_masc$data %>% group_by(sample) %>% 
  mutate(age_dec_c = weighted.mean(age_dec_c, weights = (1/(sei^2)))) %>% 
  ungroup() %>% 
  distinct(domain_name,sample,age_dec_c)

nd <- crossing(domain_sample,
               time_diff_dec = seq(0, 20, .25)/10,
               female_prop_c = 0,
               sei = 0.1)
nd <- nd %>% mutate(age_dec_c2 = age_dec_c^2)

epred_draws_df_time <- fit_masc %>% 
  epred_draws(newdata = nd)




data_w_time <-  dat %>% 
  filter(  data_transform == "none" &
            age_group != "10-90" &
            gender_group != "all" & 
            rho_val == .5 &
            month_bin == 3 &
            age_bin == 10 & 
            min_n == 30 &
            cor_metric == "pearson" &
            measure_category == "fre") %>% 
  mutate(time_diff_dec = time_diff_bin/10,
         wcor = if_else(wcor < 0, 0, wcor), # when aggregating, some correlations (15) were negative
         female_prop_c = case_when(gender_group == "male" ~ -0.5,
                                   gender_group == "female" ~ 0.5),
         age_dec_c = (mean_age - 40)/10,
         age_dec_c2 = age_dec_c^2) 





dom_vec<- unique(data_w_time$domain_name)




for (curr_domain in dom_vec) {
  
  plot_title <- rename_domain(curr_domain)
  
  
  
  p <-  ggplot(filter(epred_draws_df_time, domain_name == curr_domain)) +
    geom_point(data = filter(data_w_time, domain_name == curr_domain), aes(x = time_diff_dec*10, y = wcor),
               shape = 21,
               color = "#1e9bae",
               fill = "grey50",
               size = 1,
               stroke = .2,
               alpha = .5) +
    stat_lineribbon(alpha = 1/4, aes(x = time_diff_dec*10, y = .epred, fill = "Overall", color = "Overall")) + 
    # geom_line(data = filter(epred_draws_agg_time, domain_name == curr_domain), 
    #           aes(x = time_diff_dec*10, y = .epred),
    #           color = "#1e9bae",
    #           size = .5) +
    facet_wrap(sample~., ncol = 6) +
    theme_minimal() +
    # scale_color_manual(values = c("Overall" = "white" )) +
    scale_fill_manual(values = c("Overall" = "#1e9bae" )) +
    # scale_linetype_manual(values = c("11", "42", "7212")) +
    scale_color_manual(values = c("Overall" = "#1e9bae" )) +
    labs(y = "Retest Correlation", x = "Retest Interval (Years)", color = "", linetype = "", fill = "",
         title = paste0("Frequency: ", plot_title)) +
    theme(strip.placement = "outside",
          legend.position = "none", # c(0,0) bottom left, c(1,1) top-right.
          # legend.justification = c(0.5,0.5),
          legend.margin = margin(-.5,0,0,0, unit="cm"),
          legend.spacing.y = unit(0.15, 'cm'),
          legend.key.width = unit(1, "cm"),
          legend.key.size = unit(.3, "cm"),
          plot.background = element_rect(color = NA, fill = "white"),
          legend.text = element_text(family = "Source Sans 3", size = 8.5, color = "grey20"),
          # legend.background = theme_rect(fill = "white", colour = NA),
          text = element_text(family = "Source Sans 3", size = 9, color = "grey40"),
          axis.text.y = element_text( vjust=seq(0,1, length.out = 5)),
          axis.text.x = element_text( hjust=c(0,1)),
          title = element_text(family = "Source Sans 3", size = 9, color = "grey20"),
          # axis.title.x = element_text(margin = margin(t = 5)),
          # title = element_text(family = "Source Sans 3", size = 9, face = "bold", color = "grey20"),
          panel.spacing = unit(.5, "lines"),
          strip.text = element_text(family = "Source Sans 3", face = "bold", color = "grey20",
                                    margin = margin(b = 5)),
          plot.title = element_text(family = "Source Sans 3", face = "bold", hjust = 0,
                                    size = 11, colour = "grey20", margin = margin(b = 7, t = 5)),
          panel.grid = element_blank(),
          plot.title.position = "plot",
          plot.margin = margin(b = 5, r = 5, l = 5),
          panel.background = element_rect(color = "grey75", fill = "white", size = .4)) +
    guides(color = guide_legend(override.aes = list(size = .75)),
           fill =  guide_legend(override.aes = list(size = .75)),
           size = "none", linetype = guide_legend(override.aes = list(size = .75))) +
    coord_cartesian(ylim = c(0, 1), xlim = c(0,20))+
    scale_y_continuous(breaks = seq(0,1,0.25), expand = c(0,0)) +
    scale_x_continuous(breaks = c(0,20), expand = c(0,0))
  
  
  panel_num <- length(unique(epred_draws_df_time$sample[epred_draws_df_time$domain_name == curr_domain]))
  
  width = if_else(panel_num >= 6, 3.5*6,4.5*panel_num)
  height = if_else(panel_num > 6,4*ceiling(panel_num/6),5*ceiling(panel_num/6))
  
  
  ggsave(plot = p, filename = paste0(output_data_path,"p_masc_panel_pred_fre_",curr_domain, ".png"),
         width = width, height = height, dpi = 300, units = "cm")
  
  
}



# BEHAVIOUR: TIME ---------------------------------------------------------------


fit_masc <- masc_list[["beh"]]

domain_sample <- fit_masc$data %>% group_by(sample) %>% 
  mutate(age_dec_c = weighted.mean(age_dec_c, weights = (1/(sei^2)))) %>% #
  ungroup() %>% 
  distinct(domain_name,sample,age_dec_c)

nd <- crossing(domain_sample,
               time_diff_dec = seq(0, 20, .25)/10,
               female_prop_c = 0,
               sei = 0.1)
nd <- nd %>% mutate(age_dec_c2 = age_dec_c^2)

epred_draws_df_time <- fit_masc %>% 
  epred_draws(newdata = nd)



data_w_time <- dat %>% 
  filter(wcor > 0 & # remove negative values (MASC predictions do not og below 0)
           data_transform == "none" &
           age_group != "10-90" &
           gender_group != "all" & 
           rho_val == .5 &
           month_bin == 3 &
           age_bin == 10 & 
           min_n == 30 &
           cor_metric == "pearson" &
           measure_category == "beh") %>% 
  mutate(time_diff_dec = time_diff_bin/10,
         female_prop_c = case_when(gender_group == "male" ~ -0.5,
                                   gender_group == "female" ~ 0.5),
         age_dec_c = (mean_age - 40)/10,
         age_dec_c2 = age_dec_c^2) 


dom_vec<- unique(data_w_time$domain_name) 


for (curr_domain in dom_vec) {
  
  plot_title <- rename_domain(curr_domain)
  
  
  
  
  
  p <-  ggplot(filter(epred_draws_df_time, domain_name == curr_domain)) +
    geom_point(data = filter(data_w_time, domain_name == curr_domain), aes(x = time_diff_dec*10, y = wcor),
               shape = 21,
               color = "#e07f00",
               fill = "grey50",
               size = 1,
               stroke = .2,
               alpha = .5) +
    stat_lineribbon(alpha = 1/4, aes(x = time_diff_dec*10, y = .epred, fill = "Overall", color = "Overall")) + 
    # geom_line(data = filter(epred_draws_agg_time, domain_name == curr_domain), 
    #           aes(x = time_diff_dec*10, y = .epred),
    #           color = "#e07f00",
    #           size = .5) +
    facet_wrap(sample~., ncol = 6) +
    theme_minimal() +
    # scale_color_manual(values = c("Overall" = "white" )) +
    scale_fill_manual(values = c("Overall" = "#e07f00" )) +
    # scale_linetype_manual(values = c("11", "42", "7212")) +
    scale_color_manual(values = c("Overall" = "#e07f00" )) +
    labs(y = "Retest Correlation", x = "Retest Interval (Years)", color = "", linetype = "", fill = "",
         title = paste0("Behaviour: ", plot_title)) +
    theme(strip.placement = "outside",
          legend.position = "none", # c(0,0) bottom left, c(1,1) top-right.
          # legend.justification = c(0.5,0.5),
          legend.margin = margin(-.5,0,0,0, unit="cm"),
          legend.spacing.y = unit(0.15, 'cm'),
          legend.key.width = unit(1, "cm"),
          legend.key.size = unit(.3, "cm"),
          legend.text = element_text(family = "Source Sans 3", size = 8.5, color = "grey20"),
          # legend.background = theme_rect(fill = "white", colour = NA),
          text = element_text(family = "Source Sans 3", size = 9, color = "grey40"),
          axis.text.y = element_text( vjust=seq(0,1, length.out = 5)),
          axis.text.x = element_text( hjust=c(0,1)),
          title = element_text(family = "Source Sans 3", size = 9, color = "grey20"),
          # axis.title.x = element_text(margin = margin(t = 5)),
          # title = element_text(family = "Source Sans 3", size = 9, face = "bold", color = "grey20"),
          panel.spacing = unit(.5, "lines"),
          plot.background = element_rect(color = NA, fill = "white"),
          strip.text = element_text(family = "Source Sans 3", face = "bold", color = "grey20",
                                    margin = margin(b = 5)),
          plot.title = element_text(family = "Source Sans 3", face = "bold", hjust = 0,
                                    size = 11, colour = "grey20", margin = margin(b = 7, t = 5)),
          panel.grid = element_blank(),
          plot.title.position = "plot",
          plot.margin = margin(b = 5, r = 5, l = 5),
          panel.background = element_rect(color = "grey75", fill = "white", size = .4)) +
    guides(color = guide_legend(override.aes = list(size = .75)),
           fill =  guide_legend(override.aes = list(size = .75)),
           size = "none", linetype = guide_legend(override.aes = list(size = .75))) +
    coord_cartesian(ylim = c(0, 1), xlim = c(0,20))+
    scale_y_continuous(breaks = seq(0,1,0.25), expand = c(0,0)) +
    scale_x_continuous(breaks = c(0,20), expand = c(0,0))
  
  
  panel_num <- length(unique(epred_draws_df_time$sample[epred_draws_df_time$domain_name == curr_domain]))
  
  width = if_else(panel_num >= 6, 3.5*6,4.5*panel_num)
  height = if_else(panel_num > 6,4*ceiling(panel_num/6),5*ceiling(panel_num/6))
  
  ggsave(plot = p, filename = paste0(output_data_path,"p_masc_panel_pred_beh_",curr_domain, ".png"),
         width = width, height = height, dpi = 300, units = "cm")
  
}




# ANUSIC & SCHIMMACK  TIME -----------------------------------------------------------------

fit_masc <- masc_list[["as"]]


construct_sample <- fit_masc$data %>% group_by(construct) %>% 
  mutate(age_dec_c = weighted.mean(age_dec_c, weights = (1/(sei^2)))) %>% #
  ungroup() %>% 
  distinct(construct,age_dec_c)

nd <- crossing(construct_sample,
               time_diff_dec = seq(0, 15, .25)/10,
               female_prop_c = 0,
               se = 0.1)
nd <- nd %>% mutate(age_dec_c2 = age_dec_c^2)

epred_draws_df_time <- fit_masc %>% 
  epred_draws(newdata = nd) %>% 
  mutate( construct = case_when(construct == "pers" ~ "Personality",
                                construct == "life" ~ "Life Satisfaction",
                                construct == "affe" ~ "Affect",
                                construct == "self" ~ "Self-Esteem"))



dt <- dat_as %>% 
  fill(author, .direction = "down") 

dt <- dt[complete.cases(dt),] # 4 rows removed because of missing sample size
dt <- dt %>%  filter(age >= 10 & age <= 90)  # select only data from 10-90

# variable coding/transformation
dt <- dt %>%  
  mutate(se = sqrt((1 - retest^2)^2)/(n - 1),
         time_diff_bin =  time_binning(x = time_diff, year_bin =  3/12),
         time_diff_dec = time_diff_bin/10,
         female_prop_c = prop_female - .5,
         age_dec_c = (age - 40)/10, # setting another age as the mean
         age_dec_c2 = age_dec_c^2,
         construct = case_when(construct == "pers" ~ "Personality",
                               construct == "life" ~ "Life Satisfaction",
                               construct == "affe" ~ "Affect",
                               construct == "self" ~ "Self-Esteem")) 



p <-  ggplot(filter(epred_draws_df_time)) +
  geom_point(data = filter(dt), aes(x = time_diff_dec*10, y = retest),
             shape = 21,
             color = "darkblue",
             fill = "grey50",
             size = 1,
             stroke = .2,
             alpha = .5) +
  stat_lineribbon(alpha = 1/4, aes(x = time_diff_dec*10, y = .epred, fill = "Overall", color = "Overall")) + 
  # geom_line(data = filter(epred_draws_agg_time, domain_name == curr_domain), 
  #           aes(x = time_diff_dec*10, y = .epred),
  #           color = "#e07f00",
  #           size = .5) +
  facet_wrap(construct~., ncol = 4) +
  theme_minimal() +
  # scale_color_manual(values = c("Overall" = "white" )) +
  scale_fill_manual(values = c("Overall" = "darkblue" )) +
  # scale_linetype_manual(values = c("11", "42", "7212")) +
  scale_color_manual(values = c("Overall" = "darkblue" )) +
  labs(y = "Retest Correlation", x = "Retest Interval (Years)", color = "", linetype = "", fill = "",
       title = paste0("Constructs")) +
  theme(strip.placement = "outside",
        legend.position = "none", # c(0,0) bottom left, c(1,1) top-right.
        # legend.justification = c(0.5,0.5),
        legend.margin = margin(-.5,0,0,0, unit="cm"),
        legend.spacing.y = unit(0.15, 'cm'),
        legend.key.width = unit(1, "cm"),
        legend.key.size = unit(.3, "cm"),
        legend.text = element_text(family = "Source Sans 3", size = 8.5, color = "grey20"),
        # legend.background = theme_rect(fill = "white", colour = NA),
        text = element_text(family = "Source Sans 3", size = 9, color = "grey40"),
        axis.text.y = element_text( vjust=seq(0,1, length.out = 5)),
        axis.text.x = element_text( hjust=c(0,1)),
        title = element_text(family = "Source Sans 3", size = 9, color = "grey20"),
        # axis.title.x = element_text(margin = margin(t = 5)),
        # title = element_text(family = "Source Sans 3", size = 9, face = "bold", color = "grey20"),
        panel.spacing = unit(.5, "lines"),
        plot.background = element_rect(color = NA, fill = "white"),
        strip.text = element_text(family = "Source Sans 3", face = "bold", color = "grey20",
                                  margin = margin(b = 5)),
        plot.title = element_text(family = "Source Sans 3", face = "bold", hjust = 0,
                                  size = 11, colour = "grey20", margin = margin(b = 7, t = 5)),
        panel.grid = element_blank(),
        plot.title.position = "plot",
        plot.margin = margin(b = 5, r = 5, l = 5),
        panel.background = element_rect(color = "grey75", fill = "white", size = .4)) +
  guides(color = guide_legend(override.aes = list(size = .75)),
         fill =  guide_legend(override.aes = list(size = .75)),
         size = "none", linetype = guide_legend(override.aes = list(size = .75))) +
  coord_cartesian(ylim = c(0, 1), xlim = c(0,15))+
  scale_y_continuous(breaks = seq(0,1,0.25), expand = c(0,0)) +
  scale_x_continuous(breaks = c(0,15), expand = c(0,0))


ggsave(plot = p, filename = paste0(output_data_path,"p_masc_as", ".png"),
       width = 17, height = 7, dpi = 300, units = "cm")


