
# DESCRIPTION -------------------------------------------------------------

# Script to plot predictions of MASC for different operationalisations of risk and domains.
# Creates separate plots (as a function of time and age as well as model parameters) and then combines them

# Author(s): Alexandra Bagaini, Centre for Cognitive and Decision Sciences, Faculty of Psychology, University of Basel.




# PACKAGES ---------------------------------------------------------------

library(tidyverse)
library(brms)
library(patchwork)
library(tidybayes)
library(ggtext)
library(ggrepel)


# FUNCTIONS ---------------------------------------------------------------

source("helper_functions.R")
# DATA --------------------------------------------------------------------

model_path <- c("analysis/output/temp_stability/") # where is the  data stored
output_path <- c("plotting/output/temp_stability/") # where to store the output data

# list of fitted masc models
masc <- list(Propensity = read_rds(paste0(model_path, "masc_pro.rds")),
             Frequency = read_rds(paste0(model_path, "masc_fre.rds")),
             Behaviour  = read_rds(paste0(model_path, "masc_beh.rds")))


# NLPAR CALC --------------------------------------------------------------

### ACCROSS ALL DOMAINS FOR 40 YEAR OLDS  
pred_df_all <- NULL
for(curr_meas in c("Propensity", "Frequency", "Behaviour")) {
  
  fit_masc <- masc[[curr_meas]]
  
  
  for (curr_nlpar in c("stabch","rel","change")) {
    
    
    nd <-  crossing(domain_name = NA, #making predictions of the grand mean when using sum coding
                    female_prop_c = 0,
                    time_diff_dec = 0,
                    sei = 0.01,
                    age_dec_c = 0) %>% 
      mutate(age_dec_c2 = age_dec_c^2)
    
    
    fit_nlpar_all <- nd %>% 
      add_epred_draws(fit_masc, nlpar = curr_nlpar, re_formula = NA)
    
    
    if (curr_nlpar == "stabch") {
      fit_nlpar_all <- fit_nlpar_all %>%
        mutate(.epred = .epred^.1) 
    }
    
    
    
    fit_nlpar_all <- fit_nlpar_all %>%
      mean_hdci(.epred,.width = c(.95,.8,.5)) %>% 
      pivot_wider(names_from = .width, values_from = c(.lower,.upper)) %>% 
      mutate(nlpar = curr_nlpar,
             estimate = .epred,
             measure = curr_meas,
             x = curr_meas,
             categ = "all") %>% 
      select(categ, x, measure, nlpar, estimate, dplyr::contains("er_"))
    
    
    pred_df_all <- bind_rows(fit_nlpar_all, pred_df_all) 
    
  }
  
}



pred_df_all <- pred_df_all %>% 
  mutate(x =  case_when(x == "Propensity" ~ "**Propensity**",
                        x == "Frequency" ~ "**Frequency**",
                        x == "Behaviour" ~"**Behaviour**"),
         nlpar = case_when(nlpar == "rel" ~ "Reliability",
                           nlpar == "change" ~ "Change",
                           nlpar == "stabch" ~"Stab. Change"))




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
    
    
    
    fit_nlpar_dom <- nd %>% 
      add_epred_draws(fit_masc, nlpar = curr_nlpar, re_formula = NA)     
    
    if (curr_nlpar == "stabch") {
      fit_nlpar_dom <- fit_nlpar_dom %>%
        mutate(.epred = .epred^.1) 
    }
    
    
    
    fit_nlpar_dom <- fit_nlpar_dom %>%
      group_by(domain_name) %>%
      mean_hdci(.epred,.width = c(.95,.8,.5)) %>% 
      pivot_wider(names_from = .width, values_from = c(.lower,.upper)) %>% 
      mutate(nlpar = curr_nlpar,
             estimate = .epred,
             measure = curr_meas,
             x = domain_name,
             categ = "domain") %>% 
      select(categ, x, measure, nlpar, estimate, dplyr::contains("er_"))
    
    
    
    
    
    pred_df_dom <- bind_rows(fit_nlpar_dom, pred_df_dom) 
    
  }
  
}



pred_df_dom <-pred_df_dom %>% 
  rowwise() %>% 
  mutate(x = rename_domain(x),
         nlpar = case_when(nlpar == "rel" ~ "Reliability",
                           nlpar == "change" ~ "Change",
                           nlpar == "stabch" ~"Stab. Change")) %>% 
  ungroup()




### BY AGE GROUP ACCROSS ALL DOMAINS WITH A 50% FEMALE SAMPLE

pred_df_age <- NULL
for(curr_meas in c("Propensity", "Frequency", "Behaviour")) {
  
  fit_masc <- masc[[curr_meas]]
  
  for (curr_nlpar in c("stabch","rel","change")) {
    
    
    nd <-  crossing(domain_name = NA,
                    female_prop_c = 0,
                    time_diff_dec = 0,
                    sei = 0.01,
                    age_dec_c = (c(15,25,35,45,55,65,75)-40)/10) %>% 
      mutate(age_dec_c2 = age_dec_c^2) 
    
    
    
    fit_nlpar_age <- nd %>% 
      add_epred_draws(fit_masc, nlpar = curr_nlpar, re_formula = NA)    
    
    
    if (curr_nlpar == "stabch") {
      fit_nlpar_age <- fit_nlpar_age %>%
        mutate(.epred = .epred^.1) 
    }
    
    
    
    fit_nlpar_age <- fit_nlpar_age %>%
      group_by(age_dec_c) %>% 
      mean_hdci(.epred,.width = c(.95,.8,.5)) %>% 
      pivot_wider(names_from = .width, values_from = c(.lower,.upper)) %>% 
      mutate(nlpar = curr_nlpar,
             estimate = .epred,
             measure = curr_meas,
             categ = "age",
             x = case_when(age_dec_c == -2.5 ~ "10-19",
                           age_dec_c == -1.5 ~ "20-29",
                           age_dec_c == -.5 ~ "30-39",
                           age_dec_c == 0.5  ~ "40-49",
                           age_dec_c == 1.5 ~ "50-59",
                           age_dec_c == 2.5 ~ "60-69",
                           age_dec_c == 3.5 ~ "70+"))%>% 
      select(categ, x, measure, nlpar, estimate, dplyr::contains("er_"))
    
    
    pred_df <- fit_nlpar_age
    
    
    pred_df_age <- bind_rows(pred_df, pred_df_age) 
  }
  
}





pred_df_age <- pred_df_age %>% 
  mutate(nlpar = case_when(nlpar == "rel" ~ "Reliability",
                           nlpar == "change" ~ "Change",
                           nlpar == "stabch" ~"Stab. Change"))





# BY GENDER COLLAPSED ACROSS ALL DOMAINS FOR 40 YEAR OLDS


pred_df_gend <- NULL
for(curr_meas in c("Propensity", "Frequency", "Behaviour")) {
  
  fit_masc <- masc[[curr_meas]]
  
  
  for (curr_nlpar in c("stabch","rel","change")) {
    
    
    nd <-  crossing(domain_name = NA,
                    female_prop_c = c(-.5,.5),
                    time_diff_dec = 0,
                    sei = 0.01,
                    age_dec_c = 0) %>% 
      mutate(age_dec_c2 = age_dec_c^2) 
    
    
    fit_nlpar_gend <- nd %>% 
      add_epred_draws(fit_masc, nlpar = curr_nlpar, re_formula = NA) 
    
    
    if (curr_nlpar == "stabch") {
      fit_nlpar_gend <- fit_nlpar_gend %>%
        mutate(.epred = .epred^.1) 
    }
    
    
    
    fit_nlpar_gend <- fit_nlpar_gend %>%
      group_by(female_prop_c) %>% 
      mean_hdci(.epred,.width = c(.95,.8,.5)) %>% 
      pivot_wider(names_from = .width, values_from = c(.lower,.upper)) %>% 
      mutate(nlpar = curr_nlpar,
             estimate = .epred,
             measure = curr_meas,
             categ = "gender",
             x = case_when(female_prop_c == -.5 ~ "Male",
                           female_prop_c == .5 ~ "Female"))%>% 
      select(categ, x, measure, nlpar, estimate, dplyr::contains("er_"))
    
    
    
    pred_df <- fit_nlpar_gend
    
    
    pred_df_gend <- bind_rows(pred_df, pred_df_gend) 
  }
  
}


pred_df_gend <- pred_df_gend %>% 
  mutate(nlpar = case_when(nlpar == "rel" ~ "Reliability",
                           nlpar == "change" ~ "Change",
                           nlpar == "stabch" ~"Stab. Change"))




pred_df <- bind_rows(pred_df_gend, pred_df_all, pred_df_age, pred_df_dom)



# PROPENSITY TIME --------------------------------------------------------

fit_masc <- masc[["Propensity"]]

### Overall propensity
nd <- crossing(domain_name = NA,
               time_diff_dec = seq(0, 20, .25)/10,
               female_prop_c = 0,
               sei = 0.1,
               age_dec_c = (c(seq(15,65,10), 75)-40)/10)
nd <- nd %>% mutate(age_dec_c2 = age_dec_c^2)

epred_draws_df <- nd %>% 
  add_epred_draws(fit_masc, re_formula = NA)%>% 
  mutate(age_group = case_when(age_dec_c == -2.5 ~ "10-19",
                               age_dec_c == -1.5 ~ "20-29",
                               age_dec_c == -.5 ~ "30-39",
                               age_dec_c == 0.5  ~ "40-49",
                               age_dec_c == 1.5 ~ "50-59",
                               age_dec_c == 2.5 ~ "60-69",
                               age_dec_c == 3.5 ~ "70+"))


#summary line
epred_draws_agg <- nd %>% 
  add_epred_draws(fit_masc, re_formula = NA)  %>% 
  group_by(age_dec_c, time_diff_dec, domain_name) %>% 
  mean_hdci(.epred,.width = c(.95,.8,.5)) %>% 
  mutate(age_group = case_when(age_dec_c == -2.5 ~ "10-19",
                               age_dec_c == -1.5 ~ "20-29",
                               age_dec_c == -.5 ~ "30-39",
                               age_dec_c == 0.5  ~ "40-49",
                               age_dec_c == 1.5 ~ "50-59",
                               age_dec_c == 2.5 ~ "60-69",
                               age_dec_c == 3.5 ~ "70+")) %>% 
  pivot_wider(names_from = .width, values_from = c(.lower,.upper))

# by domain
nd_dom <- crossing(domain_name = c("hea_gen", "inv"),
                   time_diff_dec = seq(0, 20, .25)/10,
                   female_prop_c = 0,
                   sei = 0.1,
                   age_dec_c = (c(seq(15,65,10), 75)-40)/10)

nd_dom <- nd_dom %>% mutate(age_dec_c2 = age_dec_c^2)


epred_draws_dom <- nd_dom %>% 
  add_epred_draws(fit_masc, re_formula = NA)  %>% 
  group_by(age_dec_c, time_diff_dec, domain_name) %>% 
  mean_hdci(.epred,.width = c(.95,.8,.5)) %>% 
  mutate(age_group = case_when(age_dec_c == -2.5 ~ "10-19",
                               age_dec_c == -1.5 ~ "20-29",
                               age_dec_c == -.5 ~ "30-39",
                               age_dec_c == 0.5  ~ "40-49",
                               age_dec_c == 1.5 ~ "50-59",
                               age_dec_c == 2.5 ~ "60-69",
                               age_dec_c == 3.5 ~ "70+"),
         domain_name = case_when(domain_name == "hea_gen" ~ "Health",
                                 domain_name == "inv" ~ "Investment")) %>% 
  pivot_wider(names_from = .width, values_from = c(.lower,.upper))


lbl_dot_df <- tibble(age_group = "10-19",
                     .epred = c(epred_draws_dom$.epred[epred_draws_dom$domain_name == "Health" & 
                                                         epred_draws_dom$time_diff_dec == .4 &
                                                         epred_draws_dom$age_group == "10-19"],
                                epred_draws_dom$.epred[epred_draws_dom$domain_name == "Investment" & 
                                                         epred_draws_dom$time_diff_dec == .5 &
                                                         epred_draws_dom$age_group == "10-19"]),
                     time_diff_dec = c(.4,.5),
                     label = c("Health", "Investment"))



p_time <-  ggplot(epred_draws_df) +
  stat_lineribbon(alpha = 1/4, point_interval = "mean_hdci", aes(x = time_diff_dec*10, y = .epred, fill = "Overall"), color = "NA") + 
  geom_line(data = epred_draws_agg, 
            aes(x = time_diff_dec*10, y = .epred),
            color = "grey95",
            size = .5) +
  geom_line(data = epred_draws_dom, 
            aes(x = time_diff_dec*10, y = .epred, linetype = domain_name),
            color = "#502a7a",
            linewidth = .25) +
  geom_text_repel(data = lbl_dot_df, 
                  aes(x = time_diff_dec*10, y = .epred, label = label),
                  family = "Source Sans 3", size = 2.5,
                  min.segment.length = 0,
                  segment.color = "grey50",
                  segment.size = .25,
                  box.padding = 0.5,
                  nudge_x = .5,
                  nudge_y = c(.1, -.05)
  ) +
  facet_grid(.~age_group, switch =  "y") +
  theme_minimal() +
  scale_fill_manual(values = c("Overall" = "#502a7a" )) +
  labs(y = "Retest Correlation", x = "Retest Interval (Years)", color = "", linetype = "", fill = "", tag = "D",
       title = "Propensity") +
  theme(strip.placement = "outside",
        legend.position = "none", # c(0,0) bottom left, c(1,1) top-right.
        legend.margin = margin(-.5,0,0,0, unit="cm"),
        legend.spacing.y = unit(0.15, 'cm'),
        legend.key.width = unit(1, "cm"),
        legend.key.size = unit(.3, "cm"),
        legend.text = element_text(family = "Source Sans 3", size = 8.5, color = "grey20"),
        text = element_text(family = "Source Sans 3", size = 9, color = "grey40"),
        axis.text.y = element_text( vjust=seq(0,1, length.out = 5)),
        axis.text.x = element_text( hjust=c(0,1)),
        title = element_text(family = "Source Sans 3", size = 9, color = "grey20"),
        panel.spacing = unit(.5, "lines"),
        strip.text = element_text(family = "Source Sans 3", face = "bold", color = "grey20",
                                  margin = margin(b = 5)),
        plot.title = element_blank(),
        # plot.tag.position = c(0,.8),
        panel.grid = element_blank(),
        plot.title.position = "plot",
        plot.tag  = element_text(family = "Source Sans 3", size = 11, face = "bold", color = "grey20"),
        plot.margin = margin(b = 5, r = 5, l = 5),
        panel.background = element_rect(color = "grey75", fill = NA, size = .4)) +
  guides(color = guide_legend(override.aes = list(size = .75)),
         fill =  guide_legend(override.aes = list(size = .75)),
         size = "none", linetype = guide_legend(override.aes = list(size = .75))) +
  coord_cartesian(ylim = c(0, 1), xlim = c(0,20))+
  scale_y_continuous(breaks = seq(0,1,0.25), expand = c(0,0)) +
  scale_x_continuous(breaks = c(0,20), expand = c(0,0))


# p_time






# PROPENSITY AGE --------------------------------------------------------

fit_masc <- masc[["Propensity"]]



nd <- crossing(domain_name = NA,
               time_diff_dec = c(.5,1,2, 5,10,15, 20)/10,
               female_prop_c = 0,
               sei = 0.1,
               age_dec_c = (c(seq(10,70,1))-40)/10)
nd <- nd %>% mutate(age_dec_c2 = age_dec_c^2)

epred_draws_df <- nd %>% 
  add_epred_draws(fit_masc, re_formula = NA)%>% 
  mutate(time_diff_dec_lbl = case_when(time_diff_dec == 1/10 ~ paste0((10*time_diff_dec), " year"),
                                       time_diff_dec > 1/10 ~ paste0((10*time_diff_dec), " years"),
                                       time_diff_dec < 1/10 ~ paste0((120*time_diff_dec), " months")))




#summary line
epred_draws_agg <- nd %>% 
  add_epred_draws(fit_masc, re_formula = NA)  %>% 
  group_by(age_dec_c, time_diff_dec, domain_name) %>% 
  mean_hdci(.epred,.width = c(.95,.8,.5)) %>% 
  mutate(time_diff_dec_lbl = case_when(time_diff_dec == 1/10 ~ paste0((10*time_diff_dec), " year"),
                                       time_diff_dec > 1/10 ~ paste0((10*time_diff_dec), " years"),
                                       time_diff_dec < 1/10 ~ paste0((120*time_diff_dec), " months"))) %>% 
  pivot_wider(names_from = .width, values_from = c(.lower,.upper))


# by domain
nd_dom <- crossing(domain_name = c("hea_gen", "inv"),
                   time_diff_dec = c(.5,1,2, 5,10,15, 20)/10,
                   female_prop_c = 0,
                   sei = 0.1,
                   age_dec_c = (c(seq(10,70,1))-40)/10)
nd_dom <- nd_dom %>% mutate(age_dec_c2 = age_dec_c^2)

epred_draws_dom <- nd_dom %>% 
  add_epred_draws(fit_masc, re_formula = NA)  %>% 
  group_by(age_dec_c, time_diff_dec, domain_name) %>% 
  mean_hdci(.epred,.width = c(.95,.8,.5)) %>% 
  mutate(domain_name = case_when(domain_name == "hea_gen" ~ "Health",
                                 domain_name == "inv" ~ "Investment"),
         time_diff_dec_lbl = case_when(time_diff_dec == 1/10 ~ paste0((10*time_diff_dec), " year"),
                                       time_diff_dec > 1/10 ~ paste0((10*time_diff_dec), " years"),
                                       time_diff_dec < 1/10 ~ paste0((120*time_diff_dec), " months"))) %>% 
  pivot_wider(names_from = .width, values_from = c(.lower,.upper))


lbl_dot_df <- tibble(time_diff_dec_lbl = "6 months",
                     time_diff_dec = .05,
                     .epred = c(epred_draws_dom$.epred[epred_draws_dom$domain_name == "Health" & 
                                                         epred_draws_dom$time_diff_dec_lbl == "6 months" &
                                                         epred_draws_dom$age_dec_c == -2],
                                epred_draws_dom$.epred[epred_draws_dom$domain_name == "Investment" & 
                                                         epred_draws_dom$time_diff_dec_lbl == "6 months" &
                                                         epred_draws_dom$age_dec_c == -2.5]),
                     age_dec_c = c(-2,-2.5),
                     label = c("Health", "Investment"))




p_age <-  ggplot(epred_draws_df) +
  stat_lineribbon(alpha = 1/4, point_interval = "mean_hdci", aes(x = 40+(10*age_dec_c), y = .epred, fill = "Overall"), color = "NA") + 
  geom_line(data = epred_draws_agg, 
            aes(x = 40+(10*age_dec_c), y = .epred),
            color = "grey95",
            size = .5) +
  geom_line(data = epred_draws_dom, 
            aes(x = 40+(10*age_dec_c), y = .epred, linetype = domain_name),
            color = "#502a7a",
            size = .25) +
  geom_text_repel(data = lbl_dot_df, 
                  aes(x = 40+(10*age_dec_c), y = .epred, label = label),
                  family = "Source Sans 3", size = 2.5,
                  min.segment.length = 0,
                  segment.color = "grey50",
                  segment.size = .25,
                  box.padding = 0.5,
                  nudge_x = .1,
                  nudge_y = c(.15, -.15)
  ) +
  facet_grid(.~reorder(time_diff_dec_lbl,time_diff_dec), switch =  "y") +
  theme_minimal() +
  scale_fill_manual(values = c("Overall" = "#502a7a" )) +
  labs(y = "Retest Correlation", x = "Age (Years)", color = "", linetype = "", fill = "", tag = "E",
       title = "Propensity") +
  theme(strip.placement = "outside",
        legend.position = "none", # c(0,0) bottom left, c(1,1) top-right.
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
        panel.spacing = unit(.5, "lines"),
        strip.text = element_text(family = "Source Sans 3", face = "bold", color = "grey20",
                                  margin = margin(b = 5)),
        plot.title = element_blank(),
        panel.grid = element_blank(),
        plot.title.position = "plot",
        plot.tag  = element_text(family = "Source Sans 3", size = 11, face = "bold", color = "grey20"),
        plot.margin = margin(b = 0, r = 5, l = 5, t = 5),
        panel.background = element_rect(color = "grey75", fill = NA, size = .4)) +
  guides(color = guide_legend(override.aes = list(size = .75)),
         fill =  guide_legend(override.aes = list(size = .75)),
         size = "none", linetype = guide_legend(override.aes = list(size = .75))) +
  coord_cartesian(ylim = c(0, 1), xlim = c(10,70)) +
  scale_y_continuous(breaks = seq(0,1,0.25), expand = c(0,0)) +
  scale_x_continuous(breaks = c(10,70), expand = c(0,0))

# p_age

p_pred <- p_time/p_age



# PROPENSITY NLPAR  ---------------------------------------------------------------------

# get data
pred_df_pro <- pred_df %>% filter(measure == "Propensity")

pred_df_pro$categ <- factor(pred_df_pro$categ, levels = c("all", "domain", "age", "gender"))

pred_df_pro$nlpar <- factor(pred_df_pro$nlpar, levels = c("Reliability", "Change", "Stab. Change"))


domain_x_order <- pred_df_pro %>% filter(nlpar == "Reliability" & categ == "domain") %>% select(estimate, x) %>% arrange(estimate)
pred_df_pro$x <- factor(pred_df_pro$x, levels = c("**Propensity**"
                                                  ,domain_x_order$x
                                                  ,"70+"        
                                                  ,"60-69"     
                                                  ,"50-59"      
                                                  ,"40-49"     
                                                  ,"30-39"      
                                                  ,"20-29"     
                                                  ,"10-19"      
                                                  ,"Female"    
                                                  , "Male"))   


c_palette <-  c("Propensity" = "#502a7a",
                "Frequency" = "#1e9bae",
                "Behaviour" ="#e07f00")

p_nlpar <- pred_df_pro %>% ggplot() +
  geom_crossbar(aes(xmin = .lower_0.95, x = estimate, 
                    xmax = .upper_0.95, y = x),
                fill = "white", color = "NA",
                linewidth = .15,width = 0.25, alpha =  1) +
  geom_crossbar(aes(xmin = .lower_0.95, x = estimate, 
                    xmax = .upper_0.95, y = x),
                fill = "#502a7a", color = "NA",
                linewidth = .15,width = 0.25, alpha =  .3) +
  geom_crossbar(aes(xmin = .lower_0.8, x = estimate,
                    xmax = .upper_0.8, y = x),
                fill = "white", color = "NA",
                linewidth = .15,width = 0.25, alpha =  1) +
  geom_crossbar(aes(xmin = .lower_0.8, x = estimate,
                    xmax = .upper_0.8, y = x),
                fill = "#502a7a",color = "NA",
                linewidth = .15,width = 0.25, alpha =  .6) +
  geom_crossbar(aes(xmin = .lower_0.5, x = estimate, 
                    xmax = .upper_0.5, y = x),
                fill = "white",color = "NA",
                linewidth = .15,width = 0.25, alpha =  1) +
  geom_crossbar(aes(xmin = .lower_0.5, x = estimate, 
                    xmax = .upper_0.5, y = x),
                fill = "#502a7a",color = "NA",
                linewidth = .15,width = 0.25, alpha =  .9) +
  geom_point(aes(x = estimate, y =x),
             fill = "white", color = "grey20",
             shape = 21, 
             stroke = .25, 
             size = 1.1) +
  facet_grid(categ~nlpar, scales = "free_y", space = "free") + # switch = "y"
  # scale_x_continuous(limits = c(0,1), expand = c(0,0)) +
  scale_x_continuous(limits = c(0,1), expand = c(0,0), breaks = c(0,.5,1), labels = c("0","0.5" ,"1")) +
  scale_y_discrete(position = "left") +
  theme_minimal() +
  geom_rect(data = subset(pred_df_pro, x %in% c("**Propensity**")), 
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
        # plot.tag.position = c(0.025,.9),
        # panel.grid.major.x = element_line(linewidth = .2, color = "grey80", linetype = "solid"),
        panel.background = element_rect(linewidth = .25, color = "grey50", fill = "NA"),
        # plot.background = element_rect(linewidth = .25, color = "grey40", fill = "NA"),
        strip.text =  element_text(family = "Source Sans 3", face = "bold"),
        plot.tag  = element_text(family = "Source Sans 3", size = 11, face = "bold", color = "grey20"),
        plot.title = element_blank(),
        # title = element_text(family = "Source Sans 3", face = "bold", size = 9),
        axis.text.x =  element_markdown(family = "Source Sans 3", color = "black", size = 8, hjust=c(0,.5, 1)),
        axis.text.y.left =  element_markdown(family = "Source Sans 3", angle = 0, hjust = 1, color = "grey20", size = 8), # hjust = c(0,.5,.5,.5,1)
        text = element_text(family = "Source Sans 3")) +
  labs(y = "", x = "Parameter Estimate", title = "Propensity", tag = "A") 


# p_nlpar


p_pro <- p_nlpar + p_pred + plot_layout(widths = c(.5,1))
# p_pro
# FREQUENCY TIME ---------------------------------------------------------------

fit_masc <- masc[["Frequency"]]

### Overall frequency
nd <- crossing(domain_name = NA,
               time_diff_dec = seq(0, 20, .25)/10,
               female_prop_c = 0,
               sei = 0.1,
               age_dec_c = (c(seq(15,65,10), 75)-40)/10)
nd <- nd %>% mutate(age_dec_c2 = age_dec_c^2)

epred_draws_df <- nd %>% 
  add_epred_draws(fit_masc, re_formula = NA)%>% 
  mutate(age_group = case_when(age_dec_c == -2.5 ~ "10-19",
                               age_dec_c == -1.5 ~ "20-29",
                               age_dec_c == -.5 ~ "30-39",
                               age_dec_c == 0.5  ~ "40-49",
                               age_dec_c == 1.5 ~ "50-59",
                               age_dec_c == 2.5 ~ "60-69",
                               age_dec_c == 3.5 ~ "70+"))


#summary line
epred_draws_agg <- nd %>% 
  add_epred_draws(fit_masc, re_formula = NA)  %>% 
  group_by(age_dec_c, time_diff_dec, domain_name) %>% 
  mean_hdci(.epred,.width = c(.95,.8,.5)) %>% 
  mutate(age_group = case_when(age_dec_c == -2.5 ~ "10-19",
                               age_dec_c == -1.5 ~ "20-29",
                               age_dec_c == -.5 ~ "30-39",
                               age_dec_c == 0.5  ~ "40-49",
                               age_dec_c == 1.5 ~ "50-59",
                               age_dec_c == 2.5 ~ "60-69",
                               age_dec_c == 3.5 ~ "70+")) %>% 
  pivot_wider(names_from = .width, values_from = c(.lower,.upper))

# by domain
nd_dom <- crossing(domain_name = c("alc", "smo"),
                   time_diff_dec = seq(0, 20, .25)/10,
                   female_prop_c = 0,
                   sei = 0.1,
                   age_dec_c = (c(seq(15,65,10), 75)-40)/10)

nd_dom <- nd_dom %>% mutate(age_dec_c2 = age_dec_c^2)


epred_draws_dom <- nd_dom %>% 
  add_epred_draws(fit_masc, re_formula = NA)  %>% 
  group_by(age_dec_c, time_diff_dec, domain_name) %>% 
  mean_hdci(.epred,.width = c(.95,.8,.5)) %>% 
  mutate(age_group = case_when(age_dec_c == -2.5 ~ "10-19",
                               age_dec_c == -1.5 ~ "20-29",
                               age_dec_c == -.5 ~ "30-39",
                               age_dec_c == 0.5  ~ "40-49",
                               age_dec_c == 1.5 ~ "50-59",
                               age_dec_c == 2.5 ~ "60-69",
                               age_dec_c == 3.5 ~ "70+"),
         domain_name = case_when(domain_name == "alc" ~ "Alcohol",
                                 domain_name == "smo" ~ "Smoking")) %>% 
  pivot_wider(names_from = .width, values_from = c(.lower,.upper))



lbl_dot_df <- tibble(age_group = "10-19",
                     .epred = c(epred_draws_dom$.epred[epred_draws_dom$domain_name == "Alcohol" & 
                                                         epred_draws_dom$time_diff_dec == .4 &
                                                         epred_draws_dom$age_group == "10-19"],
                                epred_draws_dom$.epred[epred_draws_dom$domain_name == "Smoking" & 
                                                         epred_draws_dom$time_diff_dec == .5 &
                                                         epred_draws_dom$age_group == "10-19"]),
                     time_diff_dec = c(.4,.75),
                     label = c("Alcohol", "Smoking"))



p_time <- ggplot(epred_draws_df) +
  stat_lineribbon(alpha = 1/4, point_interval = "mean_hdci", aes(x = time_diff_dec*10, y = .epred, fill = "Overall"), color = "NA") + 
  geom_line(data = epred_draws_agg, 
            aes(x = time_diff_dec*10, y = .epred),
            color = "grey95",
            size = .5) +
  geom_line(data = epred_draws_dom, 
            aes(x = time_diff_dec*10, y = .epred, linetype = domain_name),
            color = "#1e9bae",
            linewidth = .25) +
  geom_text_repel(data = lbl_dot_df, 
                  aes(x = time_diff_dec*10, y = .epred, label = label),
                  family = "Source Sans 3", size = 2.5,
                  min.segment.length = 0,
                  segment.color = "grey50",
                  segment.size = .25,
                  box.padding = 0.5,
                  nudge_x = .5,
                  nudge_y = c(.1, .1)
  ) +
  facet_grid(.~age_group, switch =  "y") +
  theme_minimal() +
  scale_fill_manual(values = c("Overall" = "#1e9bae" )) +
  labs(y = "Retest Correlation", x = "Retest Interval (Years)", color = "", linetype = "", fill = "",tag = "F",
       title = "Frequency") +
  theme(strip.placement = "outside",
        legend.position = "none", # c(0,0) bottom left, c(1,1) top-right.
        legend.margin = margin(-.5,0,0,0, unit="cm"),
        legend.spacing.y = unit(0.15, 'cm'),
        legend.key.width = unit(1, "cm"),
        legend.key.size = unit(.3, "cm"),
        # plot.tag.position = c(0,.8),
        plot.tag  = element_text(family = "Source Sans 3", size = 11, face = "bold", color = "grey20"),
        legend.text = element_text(family = "Source Sans 3", size = 8.5, color = "grey20"),
        text = element_text(family = "Source Sans 3", size = 9, color = "grey40"),
        axis.text.y = element_text( vjust=seq(0,1, length.out = 5)),
        axis.text.x = element_text( hjust=c(0,1)),
        title = element_text(family = "Source Sans 3", size = 9, color = "grey20"),
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










# FREQUENCY AGE --------------------------------------------------------

fit_masc <- masc[["Frequency"]]

nd <- crossing(domain_name = NA,
               time_diff_dec = c(.5,1,2, 5,10,15, 20)/10,
               female_prop_c = 0,
               sei = 0.1,
               age_dec_c = (c(seq(10,70,1))-40)/10)
nd <- nd %>% mutate(age_dec_c2 = age_dec_c^2)

epred_draws_df <- nd %>% 
  add_epred_draws(fit_masc, re_formula = NA)%>% 
  mutate(time_diff_dec_lbl = case_when(time_diff_dec == 1/10 ~ paste0((10*time_diff_dec), " year"),
                                       time_diff_dec > 1/10 ~ paste0((10*time_diff_dec), " years"),
                                       time_diff_dec < 1/10 ~ paste0((120*time_diff_dec), " months")))




#summary line
epred_draws_agg <- nd %>% 
  add_epred_draws(fit_masc, re_formula = NA)  %>% 
  group_by(age_dec_c, time_diff_dec, domain_name) %>% 
  mean_hdci(.epred,.width = c(.95,.8,.5)) %>% 
  mutate(time_diff_dec_lbl = case_when(time_diff_dec == 1/10 ~ paste0((10*time_diff_dec), " year"),
                                       time_diff_dec > 1/10 ~ paste0((10*time_diff_dec), " years"),
                                       time_diff_dec < 1/10 ~ paste0((120*time_diff_dec), " months"))) %>% 
  pivot_wider(names_from = .width, values_from = c(.lower,.upper))




# by domain
nd_dom <- crossing(domain_name = c("alc", "smo"),
                   time_diff_dec = c(.5,1,2, 5,10,15, 20)/10,
                   female_prop_c = 0,
                   sei = 0.1,
                   age_dec_c = (c(seq(10,70,1))-40)/10)
nd_dom <- nd_dom %>% mutate(age_dec_c2 = age_dec_c^2)

epred_draws_dom <- nd_dom %>% 
  add_epred_draws(fit_masc, re_formula = NA)  %>% 
  group_by(age_dec_c, time_diff_dec, domain_name) %>% 
  mean_hdci(.epred,.width = c(.95,.8,.5)) %>% 
  mutate(domain_name = case_when(domain_name == "alc" ~ "Alcohol",
                                 domain_name == "smo" ~ "Smoking"),
         time_diff_dec_lbl = case_when(time_diff_dec == 1/10 ~ paste0((10*time_diff_dec), " year"),
                                       time_diff_dec > 1/10 ~ paste0((10*time_diff_dec), " years"),
                                       time_diff_dec < 1/10 ~ paste0((120*time_diff_dec), " months"))) %>% 
  pivot_wider(names_from = .width, values_from = c(.lower,.upper))


lbl_dot_df <- tibble(time_diff_dec_lbl = "6 months",
                     time_diff_dec = .05,
                     .epred = c(epred_draws_dom$.epred[epred_draws_dom$domain_name == "Alcohol" & 
                                                         epred_draws_dom$time_diff_dec_lbl == "6 months" &
                                                         epred_draws_dom$age_dec_c == 0],
                                epred_draws_dom$.epred[epred_draws_dom$domain_name == "Smoking" & 
                                                         epred_draws_dom$time_diff_dec_lbl == "6 months" &
                                                         epred_draws_dom$age_dec_c == -.5]),
                     age_dec_c = c(0,-.5),
                     label = c("Alcohol", "Smoking"))




p_age <-  ggplot(epred_draws_df) +
  stat_lineribbon(alpha = 1/4, point_interval = "mean_hdci", aes(x = 40+(10*age_dec_c), y = .epred, fill = "Overall"), color = "NA") + 
  geom_line(data = epred_draws_agg, 
            aes(x = 40+(10*age_dec_c), y = .epred),
            color = "grey95",
            size = .5) +
  geom_line(data = epred_draws_dom, 
            aes(x = 40+(10*age_dec_c), y = .epred, linetype = domain_name),
            color = "#1e9bae",
            size = .25) +
  geom_text_repel(data = lbl_dot_df, 
                  aes(x = 40+(10*age_dec_c), y = .epred, label = label),
                  family = "Source Sans 3", size = 2.5,
                  min.segment.length = 0,
                  segment.color = "grey50",
                  segment.size = .25,
                  box.padding = 0.5,
                  nudge_x = .1,
                  nudge_y = c(-.15, .15)
  ) +
  facet_grid(.~reorder(time_diff_dec_lbl,time_diff_dec), switch =  "y") +
  theme_minimal() +
  scale_fill_manual(values = c("Overall" = "#1e9bae" )) +
  labs(y = "Retest Correlation", x = "Age (Years)", color = "", linetype = "", fill = "", tag = "G",
       title = "Frequency") +
  theme(strip.placement = "outside",
        legend.position = "none", # c(0,0) bottom left, c(1,1) top-right.
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
        panel.spacing = unit(.5, "lines"),
        strip.text = element_text(family = "Source Sans 3", face = "bold", color = "grey20",
                                  margin = margin(b = 5)),
        plot.tag  = element_text(family = "Source Sans 3", size = 11, face = "bold", color = "grey20"),
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

p_pred <- p_time/p_age








# FREQUENCY: NLPAR  ---------------------------------------------------------------------


# get data
pred_df_fre <- pred_df %>% filter(measure == "Frequency")

pred_df_fre$categ <- factor(pred_df_fre$categ, levels = c("all", "domain", "age", "gender"))

pred_df_fre$nlpar <- factor(pred_df_fre$nlpar, levels = c("Reliability", "Change", "Stab. Change"))


domain_x_order <- pred_df_fre %>% filter(nlpar == "Reliability" & categ == "domain") %>% select(estimate, x) %>% arrange(estimate)
pred_df_fre$x <- factor(pred_df_fre$x, levels = c("**Frequency**"
                                                  ,domain_x_order$x
                                                  ,"70+"        
                                                  ,"60-69"     
                                                  ,"50-59"      
                                                  ,"40-49"     
                                                  ,"30-39"      
                                                  ,"20-29"     
                                                  ,"10-19"      
                                                  ,"Female"    
                                                  , "Male"))   

c_palette <-  c("Propensity" = "#502a7a",
                "Frequency" = "#1e9bae",
                "Behaviour" ="#e07f00")

p_nlpar <- pred_df_fre %>% ggplot() +
  geom_crossbar(aes(xmin = .lower_0.95, x = estimate, 
                    xmax = .upper_0.95, y = x),
                fill = "white", color = "NA",
                linewidth = .15,width = 0.25, alpha =  1) +
  geom_crossbar(aes(xmin = .lower_0.95, x = estimate, 
                    xmax = .upper_0.95, y = x),
                fill = "#1e9bae", color = "NA",
                linewidth = .15,width = 0.25, alpha =  .3) +
  geom_crossbar(aes(xmin = .lower_0.8, x = estimate,
                    xmax = .upper_0.8, y = x),
                fill = "white", color = "NA",
                linewidth = .15,width = 0.25, alpha =  1) +
  geom_crossbar(aes(xmin = .lower_0.8, x = estimate,
                    xmax = .upper_0.8, y = x),
                fill = "#1e9bae",color = "NA",
                linewidth = .15,width = 0.25, alpha =  .6) +
  geom_crossbar(aes(xmin = .lower_0.5, x = estimate, 
                    xmax = .upper_0.5, y = x),
                fill = "white",color = "NA",
                linewidth = .15,width = 0.25, alpha =  1) +
  geom_crossbar(aes(xmin = .lower_0.5, x = estimate, 
                    xmax = .upper_0.5, y = x),
                fill = "#1e9bae",color = "NA",
                linewidth = .15,width = 0.25, alpha =  .9) +
  geom_point(aes(x = estimate, y =x),
             fill = "white", color = "grey20",
             shape = 21, 
             stroke = .25, 
             size = 1.1) +
  facet_grid(categ~nlpar, scales = "free_y", space = "free") + # switch = "y"
  # scale_x_continuous(limits = c(0,1), expand = c(0,0)) +
  scale_x_continuous(limits = c(0,1), expand = c(0,0), breaks = c(0,.5,1), labels = c("0","0.5" ,"1")) +
  scale_y_discrete(position = "left") +
  theme_minimal() +
  geom_rect(data = subset(pred_df_fre, x %in% c("**Frequency**")), 
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
        # plot.tag.position = c(0.025,0.9),
        # panel.grid.major.x = element_line(linewidth = .2, color = "grey80", linetype = "solid"),
        panel.background = element_rect(linewidth = .25, color = "grey50", fill = "NA"),
        # plot.background = element_rect(linewidth = .25, color = "grey40", fill = "NA"),
        strip.text =  element_text(family = "Source Sans 3", face = "bold"),
        plot.tag  = element_text(family = "Source Sans 3", size = 11, face = "bold", color = "grey20"),
        plot.title = element_blank(),
        axis.text.x =  element_markdown(family = "Source Sans 3", color = "black", size = 8, hjust=c(0,.5, 1)),
        axis.text.y.left =  element_markdown(family = "Source Sans 3", angle = 0, hjust = 1, color = "grey20", size = 8), # hjust = c(0,.5,.5,.5,1)
        text = element_text(family = "Source Sans 3")) +
  labs(y = "", x = "Parameter Estimate", title = "Frequency", tag = "B") 



# p_nlpar

p_fre <- p_nlpar + p_pred + plot_layout(widths = c(.5,1))
# p_fre

# BEHAVIOUR: TIME ---------------------------------------------------------------


fit_masc <- masc[["Behaviour"]]

nd <- crossing(domain_name = NA,
               time_diff_dec = seq(0, 20, .25)/10,
               female_prop_c = 0,
               sei = 0.1,
               age_dec_c = (c(seq(15,65,10), 75)-40)/10)
nd <- nd %>% mutate(age_dec_c2 = age_dec_c^2)

epred_draws_df <- nd %>% 
  add_epred_draws(fit_masc, re_formula = NA)%>% 
  mutate(age_group = case_when(age_dec_c == -2.5 ~ "10-19",
                               age_dec_c == -1.5 ~ "20-29",
                               age_dec_c == -.5 ~ "30-39",
                               age_dec_c == 0.5  ~ "40-49",
                               age_dec_c == 1.5 ~ "50-59",
                               age_dec_c == 2.5 ~ "60-69",
                               age_dec_c == 3.5 ~ "70+"))


#summary line
epred_draws_agg <- nd %>% 
  add_epred_draws(fit_masc, re_formula = NA)  %>% 
  group_by(age_dec_c, time_diff_dec, domain_name) %>% 
  mean_hdci(.epred,.width = c(.95,.8,.5)) %>% 
  mutate(age_group = case_when(age_dec_c == -2.5 ~ "10-19",
                               age_dec_c == -1.5 ~ "20-29",
                               age_dec_c == -.5 ~ "30-39",
                               age_dec_c == 0.5  ~ "40-49",
                               age_dec_c == 1.5 ~ "50-59",
                               age_dec_c == 2.5 ~ "60-69",
                               age_dec_c == 3.5 ~ "70+")) %>% 
  pivot_wider(names_from = .width, values_from = c(.lower,.upper))

# by domain
nd_dom <- crossing(domain_name = c("gam", "inv"),
                   time_diff_dec = seq(0, 20, .25)/10,
                   female_prop_c = 0,
                   sei = 0.1,
                   age_dec_c = (c(seq(15,65,10), 75)-40)/10)

nd_dom <- nd_dom %>% mutate(age_dec_c2 = age_dec_c^2)


epred_draws_dom <- nd_dom %>% 
  add_epred_draws(fit_masc, re_formula = NA)  %>% 
  group_by(age_dec_c, time_diff_dec, domain_name) %>% 
  mean_hdci(.epred,.width = c(.95,.8,.5)) %>% 
  mutate(age_group = case_when(age_dec_c == -2.5 ~ "10-19",
                               age_dec_c == -1.5 ~ "20-29",
                               age_dec_c == -.5 ~ "30-39",
                               age_dec_c == 0.5  ~ "40-49",
                               age_dec_c == 1.5 ~ "50-59",
                               age_dec_c == 2.5 ~ "60-69",
                               age_dec_c == 3.5 ~ "70+"),
         domain_name = case_when(domain_name == "inv" ~ "Investment",
                                 domain_name == "gam" ~ "Gambling")) %>% 
  pivot_wider(names_from = .width, values_from = c(.lower,.upper))


lbl_dot_df <- tibble(age_group = "10-19",
                     .epred = c(epred_draws_dom$.epred[epred_draws_dom$domain_name == "Investment" & 
                                                         epred_draws_dom$time_diff_dec == 1.5 &
                                                         epred_draws_dom$age_group == "10-19"],
                                epred_draws_dom$.epred[epred_draws_dom$domain_name == "Gambling" & 
                                                         epred_draws_dom$time_diff_dec == .3 &
                                                         epred_draws_dom$age_group == "10-19"]),
                     time_diff_dec = c(1.5,.3),
                     label = c("Investment", "Gambling"))



p_time <-  ggplot(epred_draws_df) +
  stat_lineribbon(alpha = 1/4, point_interval = "mean_hdci", aes(x = time_diff_dec*10, y = .epred, fill = "Overall"), color = "NA") + 
  geom_line(data = epred_draws_agg, 
            aes(x = time_diff_dec*10, y = .epred),
            color = "grey95",
            size = .5) +
  geom_line(data = epred_draws_dom, 
            aes(x = time_diff_dec*10, y = .epred, linetype = domain_name),
            color = "#e07f00",
            linewidth = .25) +
  geom_text_repel(data = lbl_dot_df, 
                  aes(x = time_diff_dec*10, y = .epred, label = label),
                  family = "Source Sans 3", size = 2.5,
                  min.segment.length = 0,
                  segment.color = "grey50",
                  segment.size = .25,
                  box.padding = 0.5,
                  nudge_x = .5,
                  nudge_y = c(.1, -.05)
  ) +
  facet_grid(.~age_group, switch =  "y") +
  theme_minimal() +
  scale_fill_manual(values = c("Overall" = "#e07f00" )) +
  labs(y = "Retest Correlation", x = "Retest Interval (Years)", color = "", linetype = "", fill = "", tag = "H",
       title = "Behaviour") +
  theme(strip.placement = "outside",
        legend.position = "none", # c(0,0) bottom left, c(1,1) top-right.
        legend.margin = margin(-.5,0,0,0, unit="cm"),
        legend.spacing.y = unit(0.15, 'cm'),
        legend.key.width = unit(1, "cm"),
        legend.key.size = unit(.3, "cm"),
        # plot.tag.position = c(0,.8),
        legend.text = element_text(family = "Source Sans 3", size = 8.5, color = "grey20"),
        text = element_text(family = "Source Sans 3", size = 9, color = "grey40"),
        axis.text.y = element_text( vjust=seq(0,1, length.out = 5)),
        axis.text.x = element_text( hjust=c(0,1)),
        title = element_text(family = "Source Sans 3", size = 9, color = "grey20"),
        plot.tag  = element_text(family = "Source Sans 3", size = 11, face = "bold", color = "grey20"),
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










# BEHAVIOUR AGE --------------------------------------------------------

fit_masc <- masc[["Behaviour"]]

nd <- crossing(domain_name = NA,
               time_diff_dec = c(.5,1,2, 5,10,15, 20)/10,
               female_prop_c = 0,
               sei = 0.1,
               age_dec_c = (c(seq(10,70,1))-40)/10)
nd <- nd %>% mutate(age_dec_c2 = age_dec_c^2)

epred_draws_df <- nd %>% 
  add_epred_draws(fit_masc, re_formula = NA)%>% 
  mutate(time_diff_dec_lbl = case_when(time_diff_dec == 1/10 ~ paste0((10*time_diff_dec), " year"),
                                       time_diff_dec > 1/10 ~ paste0((10*time_diff_dec), " years"),
                                       time_diff_dec < 1/10 ~ paste0((120*time_diff_dec), " months")))




#summary line
epred_draws_agg <- nd %>% 
  add_epred_draws(fit_masc, re_formula = NA)  %>% 
  group_by(age_dec_c, time_diff_dec, domain_name) %>% 
  mean_hdci(.epred,.width = c(.95,.8,.5)) %>% 
  mutate(time_diff_dec_lbl = case_when(time_diff_dec == 1/10 ~ paste0((10*time_diff_dec), " year"),
                                       time_diff_dec > 1/10 ~ paste0((10*time_diff_dec), " years"),
                                       time_diff_dec < 1/10 ~ paste0((120*time_diff_dec), " months"))) %>% 
  pivot_wider(names_from = .width, values_from = c(.lower,.upper))




# by domain
nd_dom <- crossing(domain_name = c("gam", "inv"),
                   time_diff_dec = c(.5,1,2, 5,10,15, 20)/10,
                   female_prop_c = 0,
                   sei = 0.1,
                   age_dec_c = (c(seq(10,70,1))-40)/10)
nd_dom <- nd_dom %>% mutate(age_dec_c2 = age_dec_c^2)

epred_draws_dom <- nd_dom %>% 
  add_epred_draws(fit_masc, re_formula = NA)  %>% 
  group_by(age_dec_c, time_diff_dec, domain_name) %>% 
  mean_hdci(.epred,.width = c(.95,.8,.5)) %>% 
  mutate(domain_name = case_when(domain_name == "gam" ~ "Gambling",
                                 domain_name == "inv" ~ "Investment"),
         time_diff_dec_lbl = case_when(time_diff_dec == 1/10 ~ paste0((10*time_diff_dec), " year"),
                                       time_diff_dec > 1/10 ~ paste0((10*time_diff_dec), " years"),
                                       time_diff_dec < 1/10 ~ paste0((120*time_diff_dec), " months"))) %>% 
  pivot_wider(names_from = .width, values_from = c(.lower,.upper))


lbl_dot_df <- tibble(time_diff_dec_lbl = "6 months",
                     time_diff_dec = .05,
                     .epred = c(epred_draws_dom$.epred[epred_draws_dom$domain_name == "Gambling" & 
                                                         epred_draws_dom$time_diff_dec_lbl == "6 months" &
                                                         epred_draws_dom$age_dec_c == 0],
                                epred_draws_dom$.epred[epred_draws_dom$domain_name == "Investment" & 
                                                         epred_draws_dom$time_diff_dec_lbl == "6 months" &
                                                         epred_draws_dom$age_dec_c == -.5]),
                     age_dec_c = c(-1,-2),
                     label = c("Gambling", "Investment"))




p_age <-  ggplot(epred_draws_df) +
  stat_lineribbon(alpha = 1/4, point_interval = "mean_hdci", aes(x = 40+(10*age_dec_c), y = .epred, fill = "Overall"), color = "NA") + 
  geom_line(data = epred_draws_agg, 
            aes(x = 40+(10*age_dec_c), y = .epred),
            color = "grey95",
            size = .5) +
  geom_line(data = epred_draws_dom, 
            aes(x = 40+(10*age_dec_c), y = .epred, linetype = domain_name),
            color = "#e07f00",
            size = .25) +
  geom_text_repel(data = lbl_dot_df, 
                  aes(x = 40+(10*age_dec_c), y = .epred, label = label),
                  family = "Source Sans 3", size = 2.5,
                  min.segment.length = 0,
                  segment.color = "grey50",
                  segment.size = .25,
                  box.padding = 0.5,
                  nudge_x = .1,
                  nudge_y = c(-.15, .15)
  ) +
  facet_grid(.~reorder(time_diff_dec_lbl,time_diff_dec), switch =  "y") +
  theme_minimal() +
  scale_fill_manual(values = c("Overall" = "#e07f00" )) +
  labs(y = "Retest Correlation", x = "Age (Years)", color = "", linetype = "", fill = "", tag = "I",
       title = "Behaviour") +
  theme(strip.placement = "outside",
        legend.position = "none", # c(0,0) bottom left, c(1,1) top-right.
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
        panel.spacing = unit(.5, "lines"),
        plot.tag  = element_text(family = "Source Sans 3", size = 11, face = "bold", color = "grey20"),
        strip.text = element_text(family = "Source Sans 3", face = "bold", color = "grey20",
                                  margin = margin(b = 5)),
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

p_pred <- p_time/p_age


# BEHAVIOUR: NLPAR  ---------------------------------------------------------------------


# get data
pred_df_beh <- pred_df %>% filter(measure == "Behaviour")

pred_df_beh$categ <- factor(pred_df_beh$categ, levels =c("all", "domain", "age", "gender"))

pred_df_beh$nlpar <- factor(pred_df_beh$nlpar, levels = c("Reliability", "Change", "Stab. Change"))


domain_x_order <- pred_df_beh %>% filter(nlpar == "Reliability" & categ == "domain") %>% select(estimate, x) %>% arrange(estimate)
pred_df_beh$x <- factor(pred_df_beh$x, levels = c("**Behaviour**"
                                                  ,domain_x_order$x
                                                  ,"70+"        
                                                  ,"60-69"     
                                                  ,"50-59"      
                                                  ,"40-49"     
                                                  ,"30-39"      
                                                  ,"20-29"     
                                                  ,"10-19"      
                                                  ,"Female"    
                                                  , "Male"))     


c_palette <-  c("Propensity" = "#502a7a",
                "Frequency" = "#1e9bae",
                "Behaviour" ="#e07f00")

p_nlpar <- pred_df_beh %>% ggplot() +
  geom_crossbar(aes(xmin = .lower_0.95, x = estimate, 
                    xmax = .upper_0.95, y = x),
                fill = "white", color = "NA",
                linewidth = .15,width = 0.25, alpha =  1) +
  geom_crossbar(aes(xmin = .lower_0.95, x = estimate, 
                    xmax = .upper_0.95, y = x),
                fill = "#e07f00", color = "NA",
                linewidth = .15,width = 0.25, alpha =  .3) +
  geom_crossbar(aes(xmin = .lower_0.8, x = estimate,
                    xmax = .upper_0.8, y = x),
                fill = "white", color = "NA",
                linewidth = .15,width = 0.25, alpha =  1) +
  geom_crossbar(aes(xmin = .lower_0.8, x = estimate,
                    xmax = .upper_0.8, y = x),
                fill = "#e07f00",color = "NA",
                linewidth = .15,width = 0.25, alpha =  .6) +
  geom_crossbar(aes(xmin = .lower_0.5, x = estimate, 
                    xmax = .upper_0.5, y = x),
                fill = "white",color = "NA",
                linewidth = .15,width = 0.25, alpha =  1) +
  geom_crossbar(aes(xmin = .lower_0.5, x = estimate, 
                    xmax = .upper_0.5, y = x),
                fill = "#e07f00",color = "NA",
                linewidth = .15,width = 0.25, alpha =  .9) +
  geom_point(aes(x = estimate, y =x),
             fill = "white", color = "grey20",
             shape = 21, 
             stroke = .25, 
             size = 1.1) +
  facet_grid(categ~nlpar, scales = "free_y", space = "free") + # switch = "y"
  # scale_x_continuous(limits = c(0,1), expand = c(0,0)) +
  scale_x_continuous(limits = c(0,1), expand = c(0,0), breaks = c(0,.5,1), labels = c("0","0.5" ,"1")) +
  scale_y_discrete(position = "left") +
  theme_minimal() +
  geom_rect(data = subset(pred_df_beh, x %in% c("**Behaviour**")), 
            fill = NA, colour = "black", size = .75, xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf) +
  theme(panel.grid = element_blank(),
        legend.position = "none",
        plot.title.position = "plot",
        strip.placement = "outside",
        # plot.tag.position =  c(0.025,0.9),
        strip.text.y = element_blank(),
        axis.title.x = element_text(family = "Source Sans 3", size = 9, color = "grey20"),
        plot.margin = margin(b = 10, t = 10, r = 15, l = 0),
        panel.spacing.x = unit(.3, "cm"),
        # panel.grid.major.x = element_line(linewidth = .2, color = "grey80", linetype = "solid"),
        panel.background = element_rect(linewidth = .25, color = "grey50", fill = "NA"),
        # plot.background = element_rect(linewidth = .25, color = "grey40", fill = "NA"),
        strip.text =  element_text(family = "Source Sans 3", face = "bold"),
        plot.tag  = element_text(family = "Source Sans 3", size = 11, face = "bold", color = "grey20"),
        plot.title = element_blank(),
        # title = element_text(family = "Source Sans 3", face = "bold", size = 9),
        axis.text.x =  element_markdown(family = "Source Sans 3", color = "black", size = 8, hjust=c(0,.5, 1)),
        axis.text.y.left =  element_markdown(family = "Source Sans 3", angle = 0, hjust = 1, color = "grey20", size = 8), # hjust = c(0,.5,.5,.5,1)
        text = element_text(family = "Source Sans 3")) +
  labs(y = "", x = "Parameter Estimate", title = "Behaviour", tag = "C") 



# p_nlpar



p_beh <- p_nlpar + p_pred + plot_layout(widths = c(.5,1))
# p_beh





# COMBINE --------------------------------------------------------------


p <- p_pro/p_fre/p_beh + theme(plot.background = element_rect(color = NA, fill = "white"))



# p
ggsave(plot = p,
       filename = paste0(output_path, "masc_pred_figv4.png"), 
       dpi = 300, width = 28, height = 30, units = "cm") 

