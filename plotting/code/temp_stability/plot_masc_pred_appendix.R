
# DESCRIPTION -------------------------------------------------------------

# Script to plot predictions of MASC for different measure categories of risk and domains.
# Creates separate plots (as a function of time and age as well as model parameters) and then combines them


# Author(s): Alexandra Bagaini, Centre for Cognitive and Decision Sciences, Faculty of Psychology, University of Basel.


# PACKAGES ---------------------------------------------------------------

library(tidyverse)
library(brms)
library(patchwork)
library(tidybayes)
library(geomtextpath)
library(ggtext)



# FUNCTIONS ---------------------------------------------------------------

source("helper_functions.R")

# DATA --------------------------------------------------------------------

model_path <- c("analysis/output/temp_stability/") # where the results are stored
output_path <- c("plotting/output/temp_stability/") # where to store the output 
data_path <- c("processing/output/temp_stability/") # where is the data

# list of fitted masc models
masc <- list(Propensity = read_rds(paste0(model_path, "masc_pro.rds")),
             Frequency = read_rds(paste0(model_path, "masc_fre.rds")),
             Behaviour  = read_rds(paste0(model_path, "masc_beh.rds")))


data_file <- "complete_agg_retest_yb10.csv"
dat <-read_csv(paste0(data_path,data_file))

# NLPAR CALC --------------------------------------------------------------

### BY DOMAIN FOR 40 YEAR OLDS  

pred_df_dom <- NULL
for(curr_meas in c("Propensity", "Frequency", "Behaviour")) {
  
  fit_masc <- masc[[curr_meas]]
  
  
  for (curr_nlpar in c("stabch","rel","change")) {
    
    
    nd <-  crossing(domain_name = unique(fit_masc$data$domain_name),
                    female_prop_c = 0,
                    time_diff_dec = 0,
                    sei = 0.01,
                    age_dec_c = 0) %>% 
      mutate(age_dec_c2 = age_dec_c^2)
    
    
    
    pred_df <-  nd %>%  add_epred_draws(fit_masc, nlpar = curr_nlpar, re_formula = NA) 
    
    if (curr_nlpar == "stabch") {
      pred_df <- pred_df %>%
        mutate(.epred = .epred^.1) 
    }
    
    
    
    pred_df <- pred_df %>% 
      group_by(domain_name) %>%
      mean_hdci(.epred,.width = c(.95,.8,.5)) %>% 
      pivot_wider(names_from = .width, values_from = c(.lower,.upper)) %>% 
      mutate(nlpar = curr_nlpar,
             measure = curr_meas,
             x = "all",
             categ = "all") %>% 
      select(categ, x, domain_name, measure, nlpar, .epred, dplyr::contains("er_"))
    
    
    
    
    pred_df_dom <- bind_rows(pred_df, pred_df_dom) 
    
  }
  
}



pred_df_dom <-pred_df_dom %>% 
  rowwise() %>% 
  mutate(x = case_when(x == "all" ~  "**Overall**"),
         domain_name = rename_domain(domain_name),
         nlpar = case_when(nlpar == "rel" ~ "Reliability",
                           nlpar == "change" ~ "Change",
                           nlpar == "stabch" ~"Stab. Change")) %>% 
  ungroup()




### BY AGE GROUP  BY DOMAIN   

pred_df_age <- NULL
for(curr_meas in c("Propensity", "Frequency", "Behaviour")) {
  
  
  fit_masc <- masc[[curr_meas]]
  
  
  for (curr_nlpar in c("stabch","rel","change")) {
    
    
    nd <-  crossing(domain_name = unique(fit_masc$data$domain_name),
                    female_prop_c = 0,
                    time_diff_dec = 0,
                    sei = 0.01,
                    age_dec_c = (c(15,25,35,45,55,65,75)-40)/10) %>% 
      mutate(age_dec_c2 = age_dec_c^2) %>% 
      mutate(row_id = paste0("V", 1:n()))
    
    
    
    
    fit_nlpar_age <-  nd %>%  add_epred_draws(fit_masc, nlpar = curr_nlpar, re_formula = NA)     
    
    if (curr_nlpar == "stabch") {
      fit_nlpar_age <- fit_nlpar_age %>%
        mutate(.epred = .epred^.1) 
    }
    
    
    
    fit_nlpar_age <- fit_nlpar_age %>%
      group_by(domain_name, age_dec_c) %>%
      mean_hdci(.epred,.width = c(.95,.8,.5)) %>% 
      pivot_wider(names_from = .width, values_from = c(.lower,.upper)) %>% 
      mutate(nlpar = curr_nlpar,
             measure = curr_meas,
             categ = "age",
             x = case_when(age_dec_c == -2.5 ~ "10-19",
                           age_dec_c == -1.5 ~ "20-29",
                           age_dec_c == -.5 ~ "30-39",
                           age_dec_c == 0.5  ~ "40-49",
                           age_dec_c == 1.5 ~ "50-59",
                           age_dec_c == 2.5 ~ "60-69",
                           age_dec_c == 3.5 ~ "70+"))%>% 
      select(categ, x,domain_name, measure, nlpar, .epred, dplyr::contains("er_"))
    
    
    pred_df <- fit_nlpar_age
    
    
    
    pred_df_age <- bind_rows(pred_df, pred_df_age) 
  }
  
}





pred_df_age <-pred_df_age %>% 
  mutate(domain_name = rename_domain(domain_name),
         nlpar = case_when(nlpar == "rel" ~ "Reliability",
                           nlpar == "change" ~ "Change",
                           nlpar == "stabch" ~"Stab. Change")) %>% 
  ungroup()





# BY GENDER COLLAPSED BY DOMAINS FOR 40 YEAR OLDS


pred_df_gend <- NULL
for(curr_meas in c("Propensity", "Frequency", "Behaviour")) {
  
  
  fit_masc <- masc[[curr_meas]]
  
  
  for (curr_nlpar in c("stabch","rel","change")) {
    
    
    nd <-  crossing(domain_name = unique(fit_masc$data$domain_name),
                    female_prop_c = c(-.5,.5),
                    time_diff_dec = 0,
                    sei = 0.01,
                    age_dec_c = 0) %>% 
      mutate(age_dec_c2 = age_dec_c^2)
    
    
    
    fit_nlpar_gend <-  nd %>%  add_epred_draws(fit_masc, nlpar = curr_nlpar, re_formula = NA)  
    
    
    
    if (curr_nlpar == "stabch") {
      fit_nlpar_gend <- fit_nlpar_gend %>%
        mutate(.epred = .epred^.1) 
    }
    
    
    
    fit_nlpar_gend <- fit_nlpar_gend %>% 
      group_by(domain_name, female_prop_c) %>%
      mean_hdci(.epred,.width = c(.95,.8,.5)) %>% 
      pivot_wider(names_from = .width, values_from = c(.lower,.upper)) %>% 
      mutate(nlpar = curr_nlpar,
             measure = curr_meas,
             categ = "gender",
             x = case_when(female_prop_c == .5 ~ "Female",
                           female_prop_c == -.5 ~ "Male"))%>% 
      select(categ, x,domain_name, measure, nlpar, .epred, dplyr::contains("er_"))
    
    
    
    pred_df <- fit_nlpar_gend
    
    
    
    pred_df_gend <- bind_rows(pred_df, pred_df_gend) 
  }
  
}


pred_df_gend <- pred_df_gend %>% 
  mutate(domain_name = rename_domain(domain_name),
         nlpar = case_when(nlpar == "rel" ~ "Reliability",
                           nlpar == "change" ~ "Change",
                           nlpar == "stabch" ~"Stab. Change")) %>% 
  ungroup()




pred_df <- bind_rows(pred_df_gend, pred_df_dom, pred_df_age)




# PROPENSITY TIME --------------------------------------------------------

fit_masc <- masc[["Propensity"]]

nd <- crossing(domain_name = unique(fit_masc$data$domain_name),
               time_diff_dec = seq(0, 20, .25)/10,
               female_prop_c = 0,
               sei = 0.1,
               age_dec_c = (c(seq(15,65,10), 75)-40)/10)
nd <- nd %>% mutate(age_dec_c2 = age_dec_c^2)

epred_draws_df_time <- fit_masc %>% 
  epred_draws(newdata = nd, re_formula = NA)%>% 
  mutate(age_group = case_when(age_dec_c == -2.5 ~ "10-19",
                               age_dec_c == -1.5 ~ "20-29",
                               age_dec_c == -.5 ~ "30-39",
                               age_dec_c == 0.5  ~ "40-49",
                               age_dec_c == 1.5 ~ "50-59",
                               age_dec_c == 2.5 ~ "60-69",
                               age_dec_c == 3.5 ~ "70+"))


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
         time_diff = time_diff_bin,
         age_group = case_when(age_group %in% c("70-79", "80-90")~ "70+",
                               TRUE ~ age_group)) 


# PROPENSITY AGE --------------------------------------------------------

fit_masc <- masc[["Propensity"]]

nd <- crossing(domain_name = unique(fit_masc$data$domain_name),
               time_diff_dec = c(.5,1,2, 5,10,15, 20)/10,
               female_prop_c = 0,
               sei = 0.1,
               age_dec_c = (c(seq(10,70,1))-40)/10)
nd <- nd %>% mutate(age_dec_c2 = age_dec_c^2)

epred_draws_df_age <- fit_masc %>% 
  epred_draws(newdata = nd, re_formula = NA) %>% 
  mutate(time_diff_dec_lbl = case_when(time_diff_dec == 1/10 ~ paste0((10*time_diff_dec), " year"),
                                       time_diff_dec > 1/10 ~ paste0((10*time_diff_dec), " years"),
                                       time_diff_dec < 1/10 ~ paste0((120*time_diff_dec), " months")))



data_w_age <- dat %>% 
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
         time_diff = time_diff_bin,
         age_dec = mean_age/10,
         age_dec_c = (mean_age - 40)/10,
         age_group = case_when(age_group %in% c("70-79", "80-90")~ "70+",
                               TRUE ~ age_group)) %>% 
  filter(time_diff_dec %in% nd$time_diff_dec)%>% 
  mutate(time_diff_dec_lbl = case_when(time_diff_dec == 1/10 ~ paste0((10*time_diff_dec), " year"),
                                       time_diff_dec > 1/10 ~ paste0((10*time_diff_dec), " years"),
                                       time_diff_dec < 1/10 ~ paste0((120*time_diff_dec), " months")))





# PROPENSITY COMBINE ------------------------------------------------------

p_list <- list()
dom_vec<- unique(fit_masc$data$domain_name)


for (curr_domain in dom_vec) {
  
  
  
  p_time <-  ggplot(filter(epred_draws_df_time, domain_name == curr_domain)) +
    geom_point(data = filter(data_w_time, domain_name == curr_domain), aes(x = time_diff_dec*10, y = wcor),
               shape = 21,
               color = "#502a7a",
               fill = "#502a7a",
               size = .75,
               alpha = .25,
               stroke = .5) +
    stat_lineribbon(alpha = 1/4, aes(x = time_diff_dec*10, y = .epred, fill = "Overall", color = "Overall")) + 
    facet_grid(.~age_group, switch =  "y") +
    theme_minimal() +
    # scale_color_manual(values = c("Overall" = "white" )) +
    scale_fill_manual(values = c("Overall" = "#502a7a" )) +
    scale_color_manual(values = c("Overall" = "#502a7a" )) +
    # scale_linetype_manual(values = c("11", "42", "7212")) +
    # scale_color_manual(values = c("Overall" = "#1e9bae" )) +
    labs(y = "Retest Correlation", x = "Retest Interval (Years)", color = "", linetype = "", fill = "",
         title = "Propensity") +
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
          strip.text = element_text(family = "Source Sans 3", face = "bold", color = "grey20",
                                    margin = margin(b = 5)),
          plot.title = element_blank(),
          panel.grid = element_blank(),
          plot.title.position = "plot",
          plot.margin = margin(b = 5, r = 5, l = 5),
          panel.background = element_rect(color = "grey75", fill = NA, size = .4)) +
    guides(color = guide_legend(override.aes = list(size = .75)),
           fill =  guide_legend(override.aes = list(size = .75)),
           size = "none", linetype = guide_legend(override.aes = list(size = .75))) +
    coord_cartesian(ylim = c(0, 1), xlim = c(0,20))+
    scale_y_continuous(breaks = seq(0,1,0.25), expand = c(0,0)) +
    scale_x_continuous(breaks = c(0,20), expand = c(0,0))
  
  
  # p_time
  
  p_age <-  ggplot(filter(epred_draws_df_age, domain_name == curr_domain)) +
    geom_point(data = filter(data_w_age, domain_name == curr_domain), aes(x =  40+(10*age_dec_c), y = wcor),
               shape = 21,
               color = "#502a7a",
               fill = "#502a7a",
               size = .75,
               alpha = .25,
               stroke = .5) +
    stat_lineribbon(alpha = 1/4, aes(x =  40+(10*age_dec_c), y = .epred), size = .5,
                    color = "#502a7a", fill = "#502a7a") + 
    facet_grid(.~reorder(time_diff_dec_lbl,time_diff_dec), switch =  "y") +
    theme_minimal() +
    
    labs(y = "Retest Correlation", x = "Age (Years)", color = "", linetype = "", fill = "",
         title = "Propensity") +
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
          strip.text = element_text(family = "Source Sans 3", face = "bold", color = "grey20",
                                    margin = margin(b = 5)),
          strip.text.y.left = element_blank(),
          plot.title = element_blank(),
          panel.grid = element_blank(),
          plot.title.position = "plot",
          plot.margin = margin(b = 0, r = 5, l = 5, t = 5),
          panel.background = element_rect(color = "grey75", fill = NA, size = .4)) +
    guides(color = guide_legend(override.aes = list(size = .75)),
           fill =  guide_legend(override.aes = list(size = .75)),
           size = "none", linetype = guide_legend(override.aes = list(size = .75))) +
    coord_cartesian(ylim = c(0, 1), xlim = c(10,70)) +
    scale_y_continuous(breaks = seq(0,1,0.25), expand = c(0,0)) +
    scale_x_continuous(breaks = c(10,70), expand = c(0,0))
  
  # p_age
  
  
  plot_title <- rename_domain(curr_domain)
  
  
  pred_df_pro <- pred_df %>% filter(measure == "Propensity" & domain_name == plot_title)
  
  pred_df_pro$categ <- factor(pred_df_pro$categ, levels = c("age", "gender", "all"))
  
  pred_df_pro$nlpar <- factor(pred_df_pro$nlpar, levels = c("Reliability", "Change", "Stab. Change"))
  
  
  pred_df_pro$x <- factor(pred_df_pro$x, levels = c("70+"        
                                                    ,"60-69"     
                                                    ,"50-59"      
                                                    ,"40-49"     
                                                    ,"30-39"      
                                                    ,"20-29"     
                                                    ,"10-19"      
                                                    ,"Female"    
                                                    , "Male"      
                                                    , "**Overall**"))   
  
  
  
  p_nlpar <- filter(pred_df_pro,  domain_name == plot_title) %>% ggplot() +
    geom_crossbar(aes(xmin = .lower_0.95, x = .epred, 
                      xmax = .upper_0.95, y = x),
                  fill = "white", color = "NA",
                  linewidth = .15,width = 0.25, alpha =  1) +
    geom_crossbar(aes(xmin = .lower_0.95, x = .epred, 
                      xmax = .upper_0.95, y = x),
                  fill = "#502a7a", color = "NA",
                  linewidth = .15,width = 0.25, alpha =  .3) +
    geom_crossbar(aes(xmin = .lower_0.8, x = .epred,
                      xmax = .upper_0.8, y = x),
                  fill = "white", color = "NA",
                  linewidth = .15,width = 0.25, alpha =  1) +
    geom_crossbar(aes(xmin = .lower_0.8, x = .epred,
                      xmax = .upper_0.8, y = x),
                  fill = "#502a7a",color = "NA",
                  linewidth = .15,width = 0.25, alpha =  .6) +
    geom_crossbar(aes(xmin = .lower_0.5, x = .epred, 
                      xmax = .upper_0.5, y = x),
                  fill = "white",color = "NA",
                  linewidth = .15,width = 0.25, alpha =  1) +
    geom_crossbar(aes(xmin = .lower_0.5, x = .epred, 
                      xmax = .upper_0.5, y = x),
                  fill = "#502a7a",color = "NA",
                  linewidth = .15,width = 0.25, alpha =  .9) +
    geom_point(aes(x = .epred, y =x),
               fill = "white", color = "grey20",
               shape = 21, 
               stroke = .25, 
               size = 1.1) +
    facet_grid(categ~nlpar, scales = "free_y", space = "free") + # switch = "y"
    # scale_x_continuous(limits = c(0,1), expand = c(0,0)) +
    scale_x_continuous(limits = c(0,1), expand = c(0,0), breaks = c(0,.5,1), labels = c("0","0.5" ,"1")) +
    scale_y_discrete(position = "left") +
    theme_minimal() +
    geom_rect(data = subset(pred_df_pro, x %in% c("**Overall**")), 
              fill = NA, colour = "black", size = .75, xmin = -Inf,xmax = Inf,
              ymin = -Inf,ymax = Inf) +
    theme(panel.grid = element_blank(),
          legend.position = "none",
          plot.title.position = "plot",
          strip.placement = "outside",
          strip.text.y = element_blank(),
          axis.title.x = element_text(family = "Source Sans 3", size = 9, color = "grey20"),
          plot.margin = margin(b = 10, t = 10, r = 15, l = 0),
          panel.spacing.x = unit(.3, "cm"),
          # panel.grid.major.x = element_line(linewidth = .2, color = "grey80", linetype = "solid"),
          panel.background = element_rect(linewidth = .25, color = "grey50", fill = "NA"),
          # plot.background = element_rect(linewidth = .25, color = "grey40", fill = "NA"),
          strip.text =  element_text(family = "Source Sans 3", face = "bold"),
          plot.title = element_text(family = "Source Sans 3", face = "bold", hjust = 0,
                                    size = 11, colour = "grey20", margin = margin(b = 7)),
          # plot.title = element_blank(),
          # title = element_text(family = "Source Sans 3", face = "bold", size = 9),
          axis.text.x =  element_markdown(family = "Source Sans 3", color = "black", size = 8, hjust=c(0,.5, 1)),
          axis.text.y.left =  element_markdown(family = "Source Sans 3", angle = 0, hjust = 1, color = "grey20", size = 8), # hjust = c(0,.5,.5,.5,1)
          text = element_text(family = "Source Sans 3")) +
    labs(y = "", x = "Parameter .epred", title = plot_title) 
  
  
  
  
  p_pred <- p_time/p_age
  p <- p_nlpar + p_pred + plot_layout(ncol = 2, widths = c(.5,1))
  
  p_list[[curr_domain]] <- p
  
  
}



p_listA <- p_list[1:3]
p_listB <- p_list[4:6]
p_listC <- p_list[7:9]


pA <- wrap_plots(p_listA,  ncol = 1)

ggsave(plot = pA, filename =paste0(output_path,"masc_pred_pro_figA.png"), dpi = 300, width = 25, height = 30, units = "cm") 

pB <- wrap_plots(p_listB,  ncol = 1)

ggsave(plot = pB, filename = paste0(output_path,"masc_pred_pro_figB.png"), dpi = 300, width = 25, height = 30, units = "cm") 

pC <- wrap_plots(p_listC,  ncol = 1)

ggsave(plot = pC, filename = paste0(output_path,"masc_pred_pro_figC.png"), dpi = 300, width = 25, height = 30, units = "cm") 


# FREQUENCY TIME ---------------------------------------------------------------

fit_masc <- masc[["Frequency"]]

nd <- crossing(domain_name = unique(fit_masc$data$domain_name),
               time_diff_dec = seq(0, 20, .25)/10,
               female_prop_c = 0,
               sei = 0.1,
               age_dec_c = (c(seq(15,65,10), 75)-40)/10)
nd <- nd %>% mutate(age_dec_c2 = age_dec_c^2)

epred_draws_df_time <- fit_masc %>% 
  epred_draws(newdata = nd, re_formula = NA)%>% 
  mutate(age_group = case_when(age_dec_c == -2.5 ~ "10-19",
                               age_dec_c == -1.5 ~ "20-29",
                               age_dec_c == -.5 ~ "30-39",
                               age_dec_c == 0.5  ~ "40-49",
                               age_dec_c == 1.5 ~ "50-59",
                               age_dec_c == 2.5 ~ "60-69",
                               age_dec_c == 3.5 ~ "70+"))


data_w_time <- dat %>% 
  filter(data_transform == "none" &
           age_group != "10-90" &
           gender_group != "all" & 
           rho_val == .5 &
           month_bin == 3 &
           age_bin == 10 & 
           min_n == 30 &
           cor_metric == "pearson" &
           measure_category == "fre") %>% 
  mutate(time_diff_dec = time_diff_bin/10,
         time_diff = time_diff_bin,
         panel = unlist(panel),
         wcor = if_else(wcor < 0, 0, wcor), # when aggregating, some correlations (15) were negative
         domain_name = unlist(domain_name),
         female_prop = case_when(gender_group == "male" ~ 0,
                                 gender_group == "female" ~ 1),
         female_prop_c = case_when(gender_group == "male" ~ -0.5,
                                   gender_group == "female" ~ 0.5),
         age_dec = mean_age/10,
         age_dec_c = (mean_age - 40)/10,
         age_dec_c2 = age_dec_c^2,
         age_dec_c3 = age_dec_c^3,
         age_dec_c2_dec = age_dec_c*.1,
         age_group = case_when(age_group %in% c("70-79", "80-90")~ "70+",
                               TRUE ~ age_group)) 




# FREQUENCY AGE --------------------------------------------------------

fit_masc <- masc[["Frequency"]]

nd <- crossing(domain_name = unique(fit_masc$data$domain_name),
               time_diff_dec = c(.5,1,2, 5,10,15, 20)/10,
               female_prop_c = 0,
               sei = 0.1,
               age_dec_c = (c(seq(10,70,1))-40)/10)
nd <- nd %>% mutate(age_dec_c2 = age_dec_c^2)

epred_draws_df_age <- fit_masc %>% 
  epred_draws(newdata = nd, re_formula = NA) %>% 
  mutate(time_diff_dec_lbl = case_when(time_diff_dec == 1/10 ~ paste0((10*time_diff_dec), " year"),
                                       time_diff_dec > 1/10 ~ paste0((10*time_diff_dec), " years"),
                                       time_diff_dec < 1/10 ~ paste0((120*time_diff_dec), " months")))


data_w_age <- dat %>% 
  filter( data_transform == "none" &
            age_group != "10-90" &
            gender_group != "all" & 
            rho_val == .5 &
            month_bin == 3 &
            age_bin == 10 & 
            min_n == 30 &
            cor_metric == "pearson" &
            measure_category == "fre") %>% 
  mutate(time_diff_dec = time_diff_bin/10,
         time_diff = time_diff_bin,
         panel = unlist(panel),
         domain_name = unlist(domain_name),
         wcor = if_else(wcor < 0, 0, wcor), # when aggregating, some correlations (15) were negative
         female_prop = case_when(gender_group == "male" ~ 0,
                                 gender_group == "female" ~ 1),
         female_prop_c = case_when(gender_group == "male" ~ -0.5,
                                   gender_group == "female" ~ 0.5),
         age_dec = mean_age/10,
         age_dec_c = (mean_age - 40)/10,
         age_dec_c2 = age_dec_c^2,
         age_group = case_when(age_group %in% c("70-79", "80-90")~ "70+",
                               TRUE ~ age_group)) %>%
  filter(time_diff_dec %in% nd$time_diff_dec) %>% 
  mutate(time_diff_dec_lbl = case_when(time_diff_dec == 1/10 ~ paste0((10*time_diff_dec), " year"),
                                       time_diff_dec > 1/10 ~ paste0((10*time_diff_dec), " years"),
                                       time_diff_dec < 1/10 ~ paste0((120*time_diff_dec), " months")))



# FREQUENCY COMBINE ------------------------------------------------------

p_list <- list()
dom_vec<-unique(fit_masc$data$domain_name)





for (curr_domain in dom_vec) {
  
  
  p_time <-  ggplot(filter(epred_draws_df_time, domain_name == curr_domain)) +
    geom_point(data = filter(data_w_time, domain_name == curr_domain), aes(x = time_diff_dec*10, y = wcor),
               shape = 21,
               color = "#1e9bae",
               fill = "#1e9bae",
               size = .75,
               alpha = .25,
               stroke = .5) +
    stat_lineribbon(alpha = 1/4, aes(x = time_diff_dec*10, y = .epred, fill = "Overall", color = "Overall")) + 
    facet_grid(.~age_group, switch =  "y") +
    theme_minimal() +
    # scale_color_manual(values = c("Overall" = "white" )) +
    scale_fill_manual(values = c("Overall" = "#1e9bae" )) +
    scale_color_manual(values = c("Overall" = "#1e9bae" )) +
    # scale_linetype_manual(values = c("11", "42", "7212")) +
    # scale_color_manual(values = c("Overall" = "#1e9bae" )) +
    labs(y = "Retest Correlation", x = "Retest Interval (Years)", color = "", linetype = "", fill = "",
         title = "Propensity") +
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
          strip.text = element_text(family = "Source Sans 3", face = "bold", color = "grey20",
                                    margin = margin(b = 5)),
          plot.title = element_blank(),
          panel.grid = element_blank(),
          plot.title.position = "plot",
          plot.margin = margin(b = 5, r = 5, l = 5),
          panel.background = element_rect(color = "grey75", fill = NA, size = .4)) +
    guides(color = guide_legend(override.aes = list(size = .75)),
           fill =  guide_legend(override.aes = list(size = .75)),
           size = "none", linetype = guide_legend(override.aes = list(size = .75))) +
    coord_cartesian(ylim = c(0, 1), xlim = c(0,20))+
    scale_y_continuous(breaks = seq(0,1,0.25), expand = c(0,0)) +
    scale_x_continuous(breaks = c(0,20), expand = c(0,0))
  
  
  
  
  p_age <-  ggplot(filter(epred_draws_df_age, domain_name == curr_domain)) +
    geom_point(data = filter(data_w_age, domain_name == curr_domain), aes(x =  40+(10*age_dec_c), y = wcor),
               shape = 21,
               color = "#1e9bae",
               fill = "#1e9bae",
               size = .75,
               alpha = .25,
               stroke = .5) +
    stat_lineribbon(alpha = 1/4, aes(x =  40+(10*age_dec_c), y = .epred), size = .5,
                    color = "#1e9bae", fill = "#1e9bae") + 
    facet_grid(.~reorder(time_diff_dec_lbl,time_diff_dec), switch =  "y") +
    theme_minimal() +
    
    labs(y = "Retest Correlation", x = "Age (Years)", color = "", linetype = "", fill = "",
         title = "Propensity") +
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
          strip.text = element_text(family = "Source Sans 3", face = "bold", color = "grey20",
                                    margin = margin(b = 5)),
          strip.text.y.left = element_blank(),
          plot.title = element_blank(),
          panel.grid = element_blank(),
          plot.title.position = "plot",
          plot.margin = margin(b = 0, r = 5, l = 5, t = 5),
          panel.background = element_rect(color = "grey75", fill = NA, size = .4)) +
    guides(color = guide_legend(override.aes = list(size = .75)),
           fill =  guide_legend(override.aes = list(size = .75)),
           size = "none", linetype = guide_legend(override.aes = list(size = .75))) +
    coord_cartesian(ylim = c(0, 1), xlim = c(10,70)) +
    scale_y_continuous(breaks = seq(0,1,0.25), expand = c(0,0)) +
    scale_x_continuous(breaks = c(10,70), expand = c(0,0))
  
  
  plot_title <- rename_domain(curr_domain)
  
  
  pred_df_fre <- pred_df %>% filter(measure == "Frequency" & domain_name == plot_title)
  
  pred_df_fre$categ <- factor(pred_df_fre$categ, levels = c("age", "gender", "all"))
  
  pred_df_fre$nlpar <- factor(pred_df_fre$nlpar, levels = c("Reliability", "Change", "Stab. Change"))
  
  
  pred_df_fre$x <- factor(pred_df_fre$x, levels = c("70+"        
                                                    ,"60-69"     
                                                    ,"50-59"      
                                                    ,"40-49"     
                                                    ,"30-39"      
                                                    ,"20-29"     
                                                    ,"10-19"      
                                                    ,"Female"    
                                                    , "Male"      
                                                    , "**Overall**"))   
  
  
  
  
  # scale_x_continuous(breaks = seq(0,20,5), expand = c(0,0)
  p_nlpar <- filter(pred_df_fre, domain_name == plot_title) %>% ggplot() +
    geom_crossbar(aes(xmin = .lower_0.95, x = .epred, 
                      xmax = .upper_0.95, y = x),
                  fill = "white", color = "NA",
                  linewidth = .15,width = 0.25, alpha =  1) +
    geom_crossbar(aes(xmin = .lower_0.95, x = .epred, 
                      xmax = .upper_0.95, y = x),
                  fill = "#1e9bae", color = "NA",
                  linewidth = .15,width = 0.25, alpha =  .3) +
    geom_crossbar(aes(xmin = .lower_0.8, x = .epred,
                      xmax = .upper_0.8, y = x),
                  fill = "white", color = "NA",
                  linewidth = .15,width = 0.25, alpha =  1) +
    geom_crossbar(aes(xmin = .lower_0.8, x = .epred,
                      xmax = .upper_0.8, y = x),
                  fill = "#1e9bae",color = "NA",
                  linewidth = .15,width = 0.25, alpha =  .6) +
    geom_crossbar(aes(xmin = .lower_0.5, x = .epred, 
                      xmax = .upper_0.5, y = x),
                  fill = "white",color = "NA",
                  linewidth = .15,width = 0.25, alpha =  1) +
    geom_crossbar(aes(xmin = .lower_0.5, x = .epred, 
                      xmax = .upper_0.5, y = x),
                  fill = "#1e9bae",color = "NA",
                  linewidth = .15,width = 0.25, alpha =  .9) +
    geom_point(aes(x = .epred, y =x),
               fill = "white", color = "grey20",
               shape = 21, 
               stroke = .25, 
               size = 1.1) +
    facet_grid(categ~nlpar , scales = "free_y", space = "free") + # switch = "y"
    # scale_x_continuous(limits = c(0,1), expand = c(0,0)) +
    scale_x_continuous(limits = c(0,1), expand = c(0,0), breaks = c(0,.5,1), labels = c("0","0.5" ,"1")) +
    scale_y_discrete(position = "left") +
    theme_minimal() +
    geom_rect(data = subset(pred_df_fre, x %in% c("**Overall**")), 
              fill = NA, colour = "black", size = .75, xmin = -Inf,xmax = Inf,
              ymin = -Inf,ymax = Inf) +
    theme(panel.grid = element_blank(),
          legend.position = "none",
          plot.title.position = "plot",
          strip.placement = "outside",
          strip.text.y = element_blank(),
          axis.title.x = element_text(family = "Source Sans 3", size = 9, color = "grey20"),
          plot.margin = margin(b = 10, t = 10, r = 15, l = 0),
          panel.spacing.x = unit(.3, "cm"),
          # panel.grid.major.x = element_line(linewidth = .2, color = "grey80", linetype = "solid"),
          panel.background = element_rect(linewidth = .25, color = "grey50", fill = "NA"),
          # plot.background = element_rect(linewidth = .25, color = "grey40", fill = "NA"),
          strip.text =  element_text(family = "Source Sans 3", face = "bold"),
          plot.title = element_text(family = "Source Sans 3", face = "bold", hjust = 0,
                                    size = 11, colour = "grey20", margin = margin(b = 7)),
          # plot.title = element_blank(),
          # title = element_text(family = "Source Sans 3", face = "bold", size = 9),
          axis.text.x =  element_markdown(family = "Source Sans 3", color = "black", size = 8, hjust=c(0,.5, 1)),
          axis.text.y.left =  element_markdown(family = "Source Sans 3", angle = 0, hjust = 1, color = "grey20", size = 8), # hjust = c(0,.5,.5,.5,1)
          text = element_text(family = "Source Sans 3")) +
    labs(y = "", x = "Parameter .epred", title = plot_title) 
  
  
  
  
  
  p_pred <- p_time/p_age
  p <- p_nlpar + p_pred + plot_layout(ncol = 2, widths = c(.5,1))
  
  
  p_list[[curr_domain]] <- p
  
  
}

p_listA <- p_list[1:4]
p_listB <- p_list[5:8]



pA <- wrap_plots(p_listA,  ncol = 1)

ggsave(plot = pA, filename =paste0(output_path,"masc_pred_fre_figA.png"), dpi = 300, width = 25, height = 40, units = "cm") 

pB <- wrap_plots(p_listB,  ncol = 1)

ggsave(plot = pB, filename = paste0(output_path,"masc_pred_fre_figB.png"), dpi = 300, width = 25, height = 40, units = "cm") 

# 
# ggsave(plot = p, filename =paste0(output_path,"masc_pred_fre_figW.png")
#        , dpi = 300, width = 25, height = 42, units = "cm") 



# BEHAVIOUR: TIME ---------------------------------------------------------------


fit_masc <- masc[["Behaviour"]]


nd <- crossing(domain_name = unique(fit_masc$data$domain_name),
               time_diff_dec = seq(0, 20, .25)/10,
               female_prop_c = 0,
               sei = 0.1,
               age_dec_c = (c(seq(15,65,10), 75)-40)/10)
nd <- nd %>% mutate(age_dec_c2 = age_dec_c^2)

epred_draws_df_time <- fit_masc %>% 
  epred_draws(newdata = nd, re_formula = NA)%>% 
  mutate(age_group = case_when(age_dec_c == -2.5 ~ "10-19",
                               age_dec_c == -1.5 ~ "20-29",
                               age_dec_c == -.5 ~ "30-39",
                               age_dec_c == 0.5  ~ "40-49",
                               age_dec_c == 1.5 ~ "50-59",
                               age_dec_c == 2.5 ~ "60-69",
                               age_dec_c == 3.5 ~ "70+"))


data_w_time <- dat %>% 
  filter(data_transform == "none" &
           age_group != "10-90" &
           gender_group != "all" & 
           rho_val == .5 &
           month_bin == 3 &
           age_bin == 10 & 
           min_n == 30 &
           cor_metric == "pearson" &
           measure_category == "beh") %>% 
  mutate(time_diff_dec = time_diff_bin/10,
         time_diff = time_diff_bin,
         age_dec = mean_age/10,
         age_dec_c = (mean_age - 40)/10,
         age_dec_c2 = age_dec_c^2,
         age_group = case_when(age_group %in% c("70-79", "80-90")~ "70+",
                               TRUE ~ age_group)) 





# BEHAVIOUR AGE --------------------------------------------------------

fit_masc <- masc[["Behaviour"]]

nd <- crossing(domain_name = unique(fit_masc$data$domain_name),
               time_diff_dec = c(.5,1,2, 5,10,15, 20)/10,
               female_prop_c = 0,
               sei = 0.1,
               age_dec_c = (c(seq(10,70,1))-40)/10)
nd <- nd %>% mutate(age_dec_c2 = age_dec_c^2)

epred_draws_df_age <- fit_masc %>% 
  epred_draws(newdata = nd, re_formula = NA) %>% 
  mutate(time_diff_dec_lbl = case_when(time_diff_dec == 1/10 ~ paste0((10*time_diff_dec), " year"),
                                       time_diff_dec > 1/10 ~ paste0((10*time_diff_dec), " years"),
                                       time_diff_dec < 1/10 ~ paste0((120*time_diff_dec), " months")))

data_w_age <- dat %>% 
  filter(data_transform == "none" &
           age_group != "10-90" &
           gender_group != "all" & 
           rho_val == .5 &
           month_bin == 3 &
           age_bin == 10 & 
           min_n == 30 &
           cor_metric == "pearson" &
           measure_category == "beh") %>% 
  mutate(time_diff_dec = time_diff_bin/10,
         time_diff = time_diff_bin,
         age_dec = mean_age/10,
         age_dec_c = (mean_age - 40)/10,
         age_dec_c2 = age_dec_c^2,
         age_group = case_when(age_group %in% c("70-79", "80-90")~ "70+",
                               TRUE ~ age_group)) %>% 
  filter(time_diff_dec %in% nd$time_diff_dec) %>% 
  mutate(time_diff_dec_lbl = case_when(time_diff_dec == 1/10 ~ paste0((10*time_diff_dec), " year"),
                                       time_diff_dec > 1/10 ~ paste0((10*time_diff_dec), " years"),
                                       time_diff_dec < 1/10 ~ paste0((120*time_diff_dec), " months")))


# BEHAVIOUR COMBINE ------------------------------------------------------


p_list <- list()
dom_vec <- unique(fit_masc$data$domain_name)


for (curr_domain in dom_vec) {
  
  
  p_time <-  ggplot(filter(epred_draws_df_time, domain_name == curr_domain)) +
    geom_point(data = filter(data_w_time, domain_name == curr_domain), aes(x = time_diff_dec*10, y = wcor),
               shape = 21,
               color = "#e07f00",
               fill = "#e07f00",
               size = .75,
               alpha = .25,
               stroke = .5) +
    stat_lineribbon(alpha = 1/4, aes(x = time_diff_dec*10, y = .epred, fill = "Overall", color = "Overall")) + 
    facet_grid(.~age_group, switch =  "y") +
    theme_minimal() +
    # scale_color_manual(values = c("Overall" = "white" )) +
    scale_color_manual(values = c("Overall" = "#e07f00" )) +
    scale_fill_manual(values = c("Overall" = "#e07f00" )) +
    # scale_linetype_manual(values = c("11", "42", "7212")) +
    # scale_color_manual(values = c("Overall" = "#1e9bae" )) +
    labs(y = "Retest Correlation", x = "Retest Interval (Years)", color = "", linetype = "", fill = "",
         title = "Propensity") +
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
          strip.text = element_text(family = "Source Sans 3", face = "bold", color = "grey20",
                                    margin = margin(b = 5)),
          plot.title = element_blank(),
          panel.grid = element_blank(),
          plot.title.position = "plot",
          plot.margin = margin(b = 5, r = 5, l = 5),
          panel.background = element_rect(color = "grey75", fill = NA, size = .4)) +
    guides(color = guide_legend(override.aes = list(size = .75)),
           fill =  guide_legend(override.aes = list(size = .75)),
           size = "none", linetype = guide_legend(override.aes = list(size = .75))) +
    coord_cartesian(ylim = c(0, 1), xlim = c(0,20))+
    scale_y_continuous(breaks = seq(0,1,0.25), expand = c(0,0)) +
    scale_x_continuous(breaks = c(0,20), expand = c(0,0))
  
  
  
  
  p_age <-  ggplot(filter(epred_draws_df_age, domain_name == curr_domain)) +
    geom_point(data = filter(data_w_age, domain_name == curr_domain), aes(x =  40+(10*age_dec_c), y = wcor),
               shape = 21,
               color = "#e07f00",
               fill = "#e07f00",
               size = .75,
               alpha = .25,
               stroke = .5) +
    stat_lineribbon(alpha = 1/4, aes(x =  40+(10*age_dec_c), y = .epred), size = .5,
                    color = "#e07f00", fill = "#e07f00") + 
    facet_grid(.~reorder(time_diff_dec_lbl,time_diff_dec), switch =  "y") +
    theme_minimal() +
    
    labs(y = "Retest Correlation", x = "Age (Years)", color = "", linetype = "", fill = "",
         title = "Propensity") +
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
          strip.text = element_text(family = "Source Sans 3", face = "bold", color = "grey20",
                                    margin = margin(b = 5)),
          strip.text.y.left = element_blank(),
          plot.title = element_blank(),
          panel.grid = element_blank(),
          plot.title.position = "plot",
          plot.margin = margin(b = 0, r = 5, l = 5, t = 5),
          panel.background = element_rect(color = "grey75", fill = NA, size = .4)) +
    guides(color = guide_legend(override.aes = list(size = .75)),
           fill =  guide_legend(override.aes = list(size = .75)),
           size = "none", linetype = guide_legend(override.aes = list(size = .75))) +
    coord_cartesian(ylim = c(0, 1), xlim = c(10,70)) +
    scale_y_continuous(breaks = seq(0,1,0.25), expand = c(0,0)) +
    scale_x_continuous(breaks = c(10,70), expand = c(0,0))
  
  
  
  
  plot_title <- rename_domain(curr_domain)
  
  pred_df_beh <- pred_df %>% filter(measure == "Behaviour" & domain_name == plot_title)
  
  pred_df_beh$categ <- factor(pred_df_beh$categ, levels = c("age", "gender", "all"))
  
  pred_df_beh$nlpar <- factor(pred_df_beh$nlpar, levels = c("Reliability", "Change", "Stab. Change"))
  
  
  pred_df_beh$x <- factor(pred_df_beh$x, levels = c("70+"        
                                                    ,"60-69"     
                                                    ,"50-59"      
                                                    ,"40-49"     
                                                    ,"30-39"      
                                                    ,"20-29"     
                                                    ,"10-19"      
                                                    ,"Female"    
                                                    , "Male"      
                                                    , "**Overall**"))   
  
  
  
  
  
  
  # scale_x_continuous(breaks = seq(0,20,5), expand = c(0,0)
  p_nlpar <- filter(pred_df_beh, domain_name == plot_title) %>% ggplot() +
    geom_crossbar(aes(xmin = .lower_0.95, x = .epred, 
                      xmax = .upper_0.95, y = x),
                  fill = "white", color = "NA",
                  linewidth = .15,width = 0.25, alpha =  1) +
    geom_crossbar(aes(xmin = .lower_0.95, x = .epred, 
                      xmax = .upper_0.95, y = x),
                  fill = "#e07f00", color = "NA",
                  linewidth = .15,width = 0.25, alpha =  .3) +
    geom_crossbar(aes(xmin = .lower_0.8, x = .epred,
                      xmax = .upper_0.8, y = x),
                  fill = "white", color = "NA",
                  linewidth = .15,width = 0.25, alpha =  1) +
    geom_crossbar(aes(xmin = .lower_0.8, x = .epred,
                      xmax = .upper_0.8, y = x),
                  fill = "#e07f00",color = "NA",
                  linewidth = .15,width = 0.25, alpha =  .6) +
    geom_crossbar(aes(xmin = .lower_0.5, x = .epred, 
                      xmax = .upper_0.5, y = x),
                  fill = "white",color = "NA",
                  linewidth = .15,width = 0.25, alpha =  1) +
    geom_crossbar(aes(xmin = .lower_0.5, x = .epred, 
                      xmax = .upper_0.5, y = x),
                  fill = "#e07f00",color = "NA",
                  linewidth = .15,width = 0.25, alpha =  .9) +
    geom_point(aes(x = .epred, y =x),
               fill = "white", color = "grey20",
               shape = 21, 
               stroke = .25, 
               size = 1.1) +
    facet_grid(categ~nlpar , scales = "free_y", space = "free") + # switch = "y"
    # scale_x_continuous(limits = c(0,1), expand = c(0,0)) +
    scale_x_continuous(limits = c(0,1), expand = c(0,0), breaks = c(0,.5,1), labels = c("0","0.5" ,"1")) +
    scale_y_discrete(position = "left") +
    theme_minimal() +
    geom_rect(data = subset(pred_df_beh, x %in% c("**Overall**")), 
              fill = NA, colour = "black", size = .75, xmin = -Inf,xmax = Inf,
              ymin = -Inf,ymax = Inf) +
    theme(panel.grid = element_blank(),
          legend.position = "none",
          plot.title.position = "plot",
          strip.placement = "outside",
          strip.text.y = element_blank(),
          axis.title.x = element_text(family = "Source Sans 3", size = 9, color = "grey20"),
          plot.margin = margin(b = 10, t = 10, r = 15, l = 0),
          panel.spacing.x = unit(.3, "cm"),
          # panel.grid.major.x = element_line(linewidth = .2, color = "grey80", linetype = "solid"),
          panel.background = element_rect(linewidth = .25, color = "grey50", fill = "NA"),
          # plot.background = element_rect(linewidth = .25, color = "grey40", fill = "NA"),
          strip.text =  element_text(family = "Source Sans 3", face = "bold"),
          plot.title = element_text(family = "Source Sans 3", face = "bold", hjust = 0,
                                    size = 11, colour = "grey20", margin = margin(b = 7)),
          # plot.title = element_blank(),
          # title = element_text(family = "Source Sans 3", face = "bold", size = 9),
          axis.text.x =  element_markdown(family = "Source Sans 3", color = "black", size = 8, hjust=c(0,.5, 1)),
          axis.text.y.left =  element_markdown(family = "Source Sans 3", angle = 0, hjust = 1, color = "grey20", size = 8), # hjust = c(0,.5,.5,.5,1)
          text = element_text(family = "Source Sans 3")) +
    labs(y = "", x = "Parameter Estimate", title = plot_title) 
  
  
  
  
  p_pred <- p_time/p_age
  p <- p_nlpar + p_pred + plot_layout(ncol = 2, widths = c(.5,1))
  
  
  
  p_list[[curr_domain]] <- p
  
  
}

p <- wrap_plots(p_list,  ncol = 1)

ggsave(plot = p, filename =paste0(output_path,"masc_pred_beh_fig.png"), dpi = 300, width = 25, height = 40, units = "cm") 


