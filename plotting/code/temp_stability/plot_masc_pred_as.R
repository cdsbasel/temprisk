
# DESCRIPTION -------------------------------------------------------------

# Script to plot predictions of MASC for the different constructs included in the Anusic & Schimmack (2016) study 

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
data_path <- c("processing/output/temp_stability/") # where is the  data stored
model_path <- c("analysis/output/temp_stability/") # where is the  data stored
output_path <- c("plotting/output/temp_stability/") # where to store the output data

# list of fitted masc model
fit_masc <-read_rds(paste0(model_path, "masc_as.rds"))


data_file <- "anusic_schimmack_2015.csv" # name of retest data 


col_specs <- cols(
  author = col_character(),
  sample = col_double(),
  n = col_double(),
  prop_female = col_double(),
  age = col_double(),
  items = col_double(),
  time_diff = col_double(),
  retest = col_double(),
  construct = col_character()
)


data_as <-read_csv(paste0(data_path,data_file), col_types = col_specs)

data_as <- data_as %>% 
  fill(author, .direction = "down") %>% 
  mutate(age_group = age_binning(age, age_bin = 10))

data_as <- data_as[complete.cases(data_as),] # 4 rows removed because of missing sample size

# NLPAR CALC --------------------------------------------------------------

### BY CONSTRUCT & FOR 40 YEAR OLDS  

pred_df_construct <- NULL


for (curr_nlpar in c("stabch","rel","change")) {
  
  
  nd <-  crossing(construct = unique(fit_masc$data$construct),
                  female_prop_c = 0,
                  time_diff_dec = 0,
                  se = 0.01,
                  age_dec_c = 0) %>% 
    mutate(age_dec_c2 = age_dec_c^2)
  

  
  pred_df <- nd %>%  
    add_epred_draws(fit_masc, nlpar = curr_nlpar, re_formula = NA)   
 
  
   if (curr_nlpar == "stabch") {
    pred_df <- pred_df %>%
      mutate(.epred = .epred^.1) 
  }
  
  
  
  pred_df <- pred_df %>%
    group_by(construct) %>%
    mean_hdci(.epred,.width = c(.95,.8,.5)) %>% 
    pivot_wider(names_from = .width, values_from = c(.lower,.upper)) %>% 
    mutate(nlpar = curr_nlpar,
           x = "all",
           categ = "all") %>% 
    select(categ, x, construct, nlpar, .epred, dplyr::contains("er_"))
  

  
  
  pred_df_construct <- bind_rows(pred_df, pred_df_construct) 
  
}




pred_df_construct <- pred_df_construct %>% 
  mutate(x = case_when(x == "all" ~  "**Overall**"),
         construct = case_when(construct == "pers"  ~ "Personality",
                               construct == "affe" ~ "Affect",
                               construct == "life" ~ "Life-Satisfaction",
                               construct == "self" ~ "Self-Esteem"),
         nlpar = case_when(nlpar == "rel" ~ "Reliability",
                           nlpar == "change" ~ "Change",
                           nlpar == "stabch" ~"Stab. Change"))




### BY CONSTRUCT & BY AGE GROUP  

pred_df_age <- NULL

for (curr_nlpar in c("stabch","rel","change")) {
  
  
  nd <-  crossing(construct = unique(fit_masc$data$construct),
                  female_prop_c = 0,
                  time_diff_dec = 0,
                  se = 0.01,
                  age_dec_c = (c(15,25,35,45,55,65,75)-40)/10) %>% 
    mutate(age_dec_c2 = age_dec_c^2) %>% 
    mutate(row_id = paste0("V", 1:n()))
  
 
  fit_nlpar_age <- nd %>%  add_epred_draws(fit_masc, nlpar = curr_nlpar, re_formula = NA)   
 
  
   if (curr_nlpar == "stabch") {
    fit_nlpar_age <- fit_nlpar_age %>%
      mutate(.epred = .epred^.1) 
  }
  
  
  
  fit_nlpar_age <- fit_nlpar_age %>%
    group_by(construct, age_dec_c) %>%
    mean_hdci(.epred,.width = c(.95,.8,.5)) %>% 
    pivot_wider(names_from = .width, values_from = c(.lower,.upper)) %>% 
    mutate(nlpar = curr_nlpar,
           categ = "age",
           x = case_when(age_dec_c == -2.5 ~ "10-19",
                         age_dec_c == -1.5 ~ "20-29",
                         age_dec_c == -.5 ~ "30-39",
                         age_dec_c == 0.5  ~ "40-49",
                         age_dec_c == 1.5 ~ "50-59",
                         age_dec_c == 2.5 ~ "60-69",
                         age_dec_c == 3.5 ~ "70+"))%>% 
    select(categ, x,construct, nlpar, .epred, dplyr::contains("er_"))
  
  
  pred_df <- fit_nlpar_age
  
  
  pred_df_age <- bind_rows(pred_df, pred_df_age) 
}







pred_df_age <- pred_df_age %>% 
  mutate(construct = case_when(construct == "pers"  ~ "Personality",
                               construct == "affe" ~ "Affect",
                               construct == "life" ~ "Life-Satisfaction",
                               construct == "self" ~ "Self-Esteem"),
         nlpar = case_when(nlpar == "rel" ~ "Reliability",
                           nlpar == "change" ~ "Change",
                           nlpar == "stabch" ~"Stab. Change"))




# BY CONSTRUCT & BY GENDER


pred_df_gend <- NULL

for (curr_nlpar in c("stabch","rel","change")) {
  
  
  nd <-  crossing(construct = unique(fit_masc$data$construct),
                  female_prop_c = c(-.5,.5),
                  time_diff_dec = 0,
                  se = 0.01,
                  age_dec_c = 0) %>% 
    mutate(age_dec_c2 = age_dec_c^2) %>% 
    mutate(row_id = paste0("V", 1:n()))

  
  fit_nlpar_gend <- nd %>%  add_epred_draws(fit_masc, nlpar = curr_nlpar, re_formula = NA)   
  
  if (curr_nlpar == "stabch") {
    fit_nlpar_gend <- fit_nlpar_gend %>%
      mutate(.epred = .epred^.1) 
  }
  
  
  
  fit_nlpar_gend <- fit_nlpar_gend %>%
    group_by(construct, female_prop_c) %>%
    mean_hdci(.epred,.width = c(.95,.8,.5)) %>% 
    pivot_wider(names_from = .width, values_from = c(.lower,.upper)) %>% 
    mutate(nlpar = curr_nlpar,
           categ = "gender",
           x = case_when(female_prop_c == .5 ~ "Female",
                         female_prop_c == -.5 ~ "Male"))%>% 
    select(categ, x,construct, nlpar, .epred, dplyr::contains("er_"))
  
  
  
  pred_df <- fit_nlpar_gend
  

  pred_df_gend <- bind_rows(pred_df, pred_df_gend) 
}



pred_df_gend <- pred_df_gend %>% 
  mutate(construct = case_when(construct == "pers"  ~ "Personality",
                               construct == "affe" ~ "Affect",
                               construct == "life" ~ "Life-Satisfaction",
                               construct == "self" ~ "Self-Esteem"),
         nlpar = case_when(nlpar == "rel" ~ "Reliability",
                           nlpar == "change" ~ "Change",
                           nlpar == "stabch" ~"Stab. Change"))




pred_df <- bind_rows(pred_df_gend, pred_df_construct, pred_df_age)




# AS TIME --------------------------------------------------------

nd <- crossing(construct = unique(fit_masc$data$construct),
               time_diff_dec = seq(0, 20, .25)/10,
               female_prop_c = 0,
               se = 0.1,
               age_dec_c = (c(seq(15,65,10), 75)-40)/10)
nd <- nd %>% mutate(age_dec_c2 = age_dec_c^2)

epred_draws_df_time <- nd %>% 
  add_epred_draws(fit_masc, re_formula = NA)%>% 
  mutate(age_group = case_when(age_dec_c == -2.5 ~ "10-19",
                               age_dec_c == -1.5 ~ "20-29",
                               age_dec_c == -.5 ~ "30-39",
                               age_dec_c == 0.5  ~ "40-49",
                               age_dec_c == 1.5 ~ "50-59",
                               age_dec_c == 2.5 ~ "60-69",
                               age_dec_c == 3.5 ~ "70+"))



data_w_time <- data_as %>% 
  mutate(time_diff_bin =  time_binning(x = time_diff, year_bin =  3/12),
         time_diff_dec = time_diff_bin/10,
         age_rnd = round(age),
         age_group = case_when(age_rnd %in% c(10:19) ~ "10-19",
                               age_rnd %in% c(20:29) ~ "20-29",
                               age_rnd %in% c(30:39) ~ "30-39",
                               age_rnd %in% c(40:49) ~ "40-49",
                               age_rnd %in% c(50:59) ~ "50-59",
                               age_rnd %in% c(60:69) ~ "60-69",
                               age_rnd >= 70 ~ "70+")) 




# AS AGE --------------------------------------------------------


nd <- crossing(construct = unique(fit_masc$data$construct),
               time_diff_dec = c(.5,1,2, 5,10,12, 15)/10,
               female_prop_c = 0,
               se = 0.1,
               age_dec_c = (c(seq(10,70,1))-40)/10)
nd <- nd %>% mutate(age_dec_c2 = age_dec_c^2)

epred_draws_df_age <- nd %>% 
  add_epred_draws(fit_masc, re_formula = NA)%>% 
  mutate(time_diff_dec_lbl = case_when(time_diff_dec == 1/10 ~ paste0((10*time_diff_dec), " year"),
                                       time_diff_dec > 1/10 ~ paste0((10*time_diff_dec), " years"),
                                       time_diff_dec < 1/10 ~ paste0((120*time_diff_dec), " months")))



data_w_age <- data_as %>%  
  mutate(time_diff_bin =  time_binning(x = time_diff, year_bin =  3/12),
         time_diff_dec = time_diff_bin/10,
         age_rnd = round(age),
         age_dec_c = (age-40)/10) %>% 
  filter(time_diff_dec %in% nd$time_diff_dec)%>%
  mutate(time_diff_dec_lbl = case_when(time_diff_dec == 1/10 ~ paste0((10*time_diff_dec), " year"),
                                       time_diff_dec > 1/10 ~ paste0((10*time_diff_dec), " years"),
                                       time_diff_dec < 1/10 ~ paste0((120*time_diff_dec), " months")))


# AS COMBINE ------------------------------------------------------

p_list <- list()
dom_vec<- c("pers",
            "affe",
            "life",
            "self")


for (curr_domain in dom_vec) {
  
  
  # plotting predicted cors as a function of retest interval
  p_time <-  ggplot(filter(epred_draws_df_time, construct == curr_domain)) +
    geom_point(data = filter(data_w_time, construct == curr_domain), aes(x = time_diff_dec*10, y = retest, size = 1/se),
               shape = 21,
               color = "#003d5b",
               fill = "#003d5b",
               size = .75,
               stroke = .2,
               alpha = .5) +
    stat_lineribbon(alpha = 1/4, aes(x = time_diff_dec*10, y = .epred, fill = "Overall", color = "Overall")) + 
    facet_grid(.~age_group, switch =  "y") +
    theme_minimal() +
    scale_color_manual(values = c("Overall" = "#003d5b" )) +
    scale_fill_manual(values = c("Overall" = "#003d5b" )) +
    labs(y = "Retest Correlation", x = "Retest Interval (Years)", color = "", linetype = "", fill = "",
         title = "Propensity") +
    theme(strip.placement = "outside",
          legend.position = "none", # c(0,0) bottom left, c(1,1) top-right.
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
    coord_cartesian(ylim = c(0, 1), xlim = c(0,15))+
    scale_y_continuous(breaks = seq(0,1,0.25), expand = c(0,0)) +
    scale_x_continuous(breaks = c(0,15), expand = c(0,0))
  
  
  # p_time
  
  # plotting predicted cors as a function of age
  p_age <-  ggplot(filter(epred_draws_df_age, construct == curr_domain)) +
    geom_point(data = filter(data_w_age, construct == curr_domain), aes(x =  40+(10*age_dec_c), y = retest, size = 1/se),
               shape = 21,
               color = "#003d5b",
               fill = "#003d5b",
               size = .75,
               stroke = .2,
               alpha = .5) +
    stat_lineribbon(alpha = 1/4, aes(x =  40+(10*age_dec_c), y = .epred), size = .5,
                    color = "#003d5b", fill = "#003d5b") + 
    facet_grid(.~reorder(time_diff_dec_lbl,time_diff_dec), switch =  "y") +
    theme_minimal() +
    labs(y = "Retest Correlation", x = "Age (Years)", color = "", linetype = "", fill = "",
         title = "Propensity") +
    theme(strip.placement = "outside",
          legend.position = "none",
          text = element_text(family = "Source Sans 3", size = 9, color = "grey40"),
          axis.text.y = element_text( vjust=seq(0,1, length.out = 5)),
          axis.text.x = element_text( hjust=c(0,1)),
          title = element_text(family = "Source Sans 3", size = 9, color = "grey20"),
          panel.spacing = unit(.5, "lines"),
          strip.text = element_text(family = "Source Sans 3", face = "bold", color = "grey20",
                                    margin = margin(b = 5)),
          strip.text.y.left = element_blank(),
          plot.title = element_blank(),
          panel.grid = element_blank(),
          plot.title.position = "plot",
          plot.margin = margin(b = 0, r = 5, l = 5, t = 5),
          panel.background = element_rect(color = "grey75", fill = NA, size = .4)) +
    coord_cartesian(ylim = c(0, 1), xlim = c(10,70)) +
    scale_y_continuous(breaks = seq(0,1,0.25), expand = c(0,0)) +
    scale_x_continuous(breaks = c(10,70), expand = c(0,0))
  
  # p_age
  
  
  # nlpar predictions
  plot_title <- case_when(curr_domain == "pers"  ~ "Personality",
                          curr_domain == "affe" ~ "Affect",
                          curr_domain == "life" ~ "Life-Satisfaction",
                          curr_domain == "self" ~ "Self-Esteem")
  
  
  pred_df_pro <- pred_df %>% filter(construct == plot_title)
  
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
  
  
  
  p_nlpar <- filter(pred_df_pro,  construct == plot_title) %>% ggplot() +
    geom_crossbar(aes(xmin = .lower_0.95, x = .epred, 
                      xmax = .upper_0.95, y = x),
                  fill = "white", color = "NA",
                  linewidth = .15,width = 0.25, alpha =  1) +
    geom_crossbar(aes(xmin = .lower_0.95, x = .epred, 
                      xmax = .upper_0.95, y = x),
                  fill = "#003d5b", color = "NA",
                  linewidth = .15,width = 0.25, alpha =  .3) +
    geom_crossbar(aes(xmin = .lower_0.8, x = .epred,
                      xmax = .upper_0.8, y = x),
                  fill = "white", color = "NA",
                  linewidth = .15,width = 0.25, alpha =  1) +
    geom_crossbar(aes(xmin = .lower_0.8, x = .epred,
                      xmax = .upper_0.8, y = x),
                  fill = "#003d5b",color = "NA",
                  linewidth = .15,width = 0.25, alpha =  .6) +
    geom_crossbar(aes(xmin = .lower_0.5, x = .epred, 
                      xmax = .upper_0.5, y = x),
                  fill = "white",color = "NA",
                  linewidth = .15,width = 0.25, alpha =  1) +
    geom_crossbar(aes(xmin = .lower_0.5, x = .epred, 
                      xmax = .upper_0.5, y = x),
                  fill = "#003d5b",color = "NA",
                  linewidth = .15,width = 0.25, alpha =  .9) +
    geom_point(aes(x = .epred, y =x),
               fill = "white", color = "grey20",
               shape = 21, 
               stroke = .25, 
               size = 1.1) +
    facet_grid(categ~nlpar, scales = "free_y", space = "free") + # switch = "y"
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
          panel.background = element_rect(linewidth = .25, color = "grey50", fill = "NA"),
          strip.text =  element_text(family = "Source Sans 3", face = "bold"),
          plot.title = element_text(family = "Source Sans 3", face = "bold", hjust = 0,
                                    size = 11, colour = "grey20", margin = margin(b = 7)),
          axis.text.x =  element_markdown(family = "Source Sans 3", color = "black", size = 8, hjust=c(0,.5, 1)),
          axis.text.y.left =  element_markdown(family = "Source Sans 3", angle = 0, hjust = 1, color = "grey20", size = 8), # hjust = c(0,.5,.5,.5,1)
          text = element_text(family = "Source Sans 3")) +
    labs(y = "", x = "Parameter .epred", title = plot_title) 
  
  
  
  
  p_masc_pred <- p_time/p_age
  p <- p_nlpar + p_masc_pred + plot_layout(ncol = 2, widths = c(.5,1))
  
  
  p_list[[curr_domain]] <- p
  
  
}



pA <- wrap_plots(p_list,  ncol = 1)

ggsave(plot = pA, filename = paste0(output_path, "masc_pred_as_fig.png"),
       dpi = 300, width = 25, height = 37, units = "cm") 

