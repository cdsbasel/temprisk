

# DESCRIPTION -------------------------------------------------------------

# Script that plots the results of the multiverse results for MASC for the different risk preference measures
# Code adapted from plotting function of the specr package
# https://github.com/masurp/specr/tree/master
# Masur, Philipp K. & Scharkow, M. (2020). specr: Conducting and Visualizing Specification Curve Analyses. Available from https://CRAN.R-project.org/package=specr.


# Author(s): Alexandra Bagaini, Centre for Cognitive and Decision Sciences, Faculty of Psychology, University of Basel.



# PACKAGES ----------------------------------------------------------------


library(tidyverse)
library(data.table)
library(cowplot)
library(patchwork)
library(ggh4x)
source("helper_functions.R")

# FILES ------------------------------------------------------------------


data_path <- c("analysis/output/temp_stability/")
output_path <- c("docs/images/")

masc_list <- list(
  fre = read_rds(paste0(data_path, "multiverse_fit_masc_fre.rds")),
  pro = read_rds(paste0(data_path, "multiverse_fit_masc_pro.rds")),
  beh = read_rds(paste0(data_path, "multiverse_fit_masc_beh.rds")))

c_palette <-  c("Propensity" = "#502a7a",
                "Frequency" = "#1e9bae",
                "Behaviour" ="#ff8800")


# PROPENSITY ----------------------------------------------------------------

color_name <- as.character(c_palette["Propensity"])

# check diagnostics
rhat_num <- (sapply(masc_list$pro$rhat,function(x) x[1:4]))
sum(rhat_num > 1.05, na.rm = T) # 98  param estimates with rhat > 1.05
table(masc_list$pro$div_trans) # 1 model with 1 divergent transition



df <- masc_list$pro$results


df_plot <- df %>%
  group_by(domain_name, nlpar) %>%
  arrange(.epred) %>%
  mutate(spec_num = 1:n()) %>% ungroup()  %>%  mutate(rho_val_01 = if_else(rho_val == 0.1, 1,0),
                                                      rho_val_05 = if_else(rho_val == 0.5, 1,0),
                                                      rho_val_09 = if_else(rho_val == 0.9, 1,0),
                                                      time_bin_3mo = if_else(time_bin == 3, 1,0),
                                                      time_bin_6mo = if_else(time_bin == 6, 1,0),
                                                      time_bin_12mo = if_else(time_bin == 12, 1,0),
                                                      pearson_cor = if_else(grepl("pearson", cor_metric), 1,0),
                                                      spearman_cor = if_else(grepl("spearman", cor_metric), 1,0),
                                                      icc_cor = if_else(grepl("icc", cor_metric), 1,0),
                                                      year_age_bin_5 = if_else(year_age_bin == 5, 1,0),
                                                      year_age_bin_10 = if_else(year_age_bin == 10, 1,0),
                                                      year_age_bin_20 = if_else(year_age_bin == 20, 1,0),
                                                      none_transf = if_else(dt_transform == "none", 1,0),
                                                      log_transf = if_else(dt_transform == "log", 1,0),
                                                      min_n_30 = if_else(min_n == 30, 1,0), 
                                                      min_n_100 = if_else(min_n == 100, 1,0),                                                    
                                                      min_n_250 = if_else(min_n == 250, 1,0),  
                                                      student = if_else(family == "student", 1,0),  
                                                      gaussian = if_else(family == "gaussian", 1,0),  
                                                      vague = if_else(priors == "vague", 1,0),  
                                                      weakly_informative = if_else(priors == "weakly_informative", 1,0))



df_plot <- df_plot  %>%  pivot_longer(rho_val_01:weakly_informative, names_to = "vars", values_to = "incl") 


df_plot <- df_plot %>% mutate(key = case_when(vars %like% "rho"  ~ "Sampling\nerrors corr.",
                                              vars %like% "_cor"  ~ "Corr.\nmetric",
                                              vars %like% "_transf"  ~ "Data\ntransf.",
                                              vars %like% "_age_"  ~ "Age\nbinning",
                                              vars %like% "time_bin"  ~ "Time\nbinning",
                                              vars %like% "min_"  ~ "Sample\nsize (min.)",
                                              vars %like% "stud"  ~ "Likelih.",
                                              vars %like% "gauss"  ~ "Likelih.",
                                              vars %like% "vague"  ~ "Priors",
                                              vars %like% "weak"  ~ "Priors"),
                              # domain_name = case_when(grepl("ins", domain_name)  ~ "Insurance",
                              #                         grepl("gam", domain_name)  ~ "Gambling",
                              #                         grepl("occ", domain_name)  ~ "Occupational",
                              #                         grepl("inv", domain_name)  ~ "Investment"),
                              nlpar = case_when(grepl("rel", nlpar)  ~ "Reliability",
                                                grepl("stabch", nlpar)  ~ "Stab. of Change",
                                                grepl("change", nlpar)  ~ "Change"),
                              nester = case_when(grepl("metric|Sample|Age|transf.", key)  ~ "Data",
                                                 grepl("Time|Sampling", key)  ~ "Aggregation",
                                                 grepl("Priors|Likelih.", key)  ~ "Analysis"),
                              fill = case_when(.lower_0.95 < 0 & .upper_0.95 > 0  ~ "white",
                                               .lower_0.95 > 0 &  .upper_0.95 > 0 ~"white",
                                               .lower_0.95 < 0 &  .upper_0.95 < 0 ~"white"),
                              color = case_when(.lower_0.95 < 0 & .upper_0.95 > 0  ~ color_name,
                                                .lower_0.95 > 0 &  .upper_0.95 > 0 ~ color_name,
                                                .lower_0.95 < 0 &  .upper_0.95 < 0 ~ color_name))


# rename controls
replace_names <- data.frame(old_name =  c("rho_val_01"   
                                          ,"rho_val_05"
                                          ,"rho_val_09"
                                          ,"time_bin_3mo" 
                                          ,"time_bin_6mo" 
                                          ,"time_bin_12mo"
                                          ,"pearson_cor"       
                                          ,"spearman_cor"  
                                          , "icc_cor"
                                          ,"year_age_bin_5"    
                                          ,"year_age_bin_10"   
                                          ,"year_age_bin_20"   
                                          ,"min_n_30"          
                                          ,"min_n_100"         
                                          ,"min_n_250"         
                                          , "student"           
                                          , "gaussian"          
                                          , "vague"             
                                          , "weakly_informative"
                                          ,"none_transf"
                                          ,"log_transf"
),
new_name = c("rho = 0.1" 
             ,"rho = 0.5" 
             ,"rho = 0.9"
             ,"3-month bin"
             ,"6-month bin"
             ,"12-month bin"
             ,"Pearson's r"       
             ,"Spearman's rho" 
             , "ICC"
             ,"5-year age bin"    
             ,"10-year age bin"    
             ,"20-year age bin"    
             ,"30"          
             ,"100"         
             ,"250"         
             , "Student-t"           
             , "Gaussian"          
             , "Vague"             
             , "Weakly Informative"
             , "None"
             , "Log. transf."))




# replacing var codes
names_found <- match(df_plot$vars,replace_names$old_name , nomatch = 0)
df_plot$vars[df_plot$vars %in% replace_names$old_name] <- replace_names$new_name[names_found]

max_specs <- max(df_plot$spec_num)

df_plot$vars <- factor(df_plot$vars, levels = c("rho = 0.9" 
                                                ,"rho = 0.5" 
                                                ,"rho = 0.1" 
                                                ,"3-month bin"
                                                ,"6-month bin"
                                                ,"12-month bin"
                                                ,"ICC"
                                                ,"Pearson's r"       
                                                ,"Spearman's rho" 
                                                ,"20-year age bin"    
                                                ,"10-year age bin"    
                                                ,"5-year age bin"    
                                                ,"250"         
                                                ,"100"         
                                                ,"30"          
                                                , "Student-t"           
                                                , "Gaussian"          
                                                , "Vague"             
                                                , "Weakly Informative"
                                                , "None"
                                                , "Log. transf."))





curr_domains <- unique(df_plot$domain_name)

p_list <- NULL

for (i in 1:length(curr_domains)) {
  
  dom_name <- curr_domains[i]
  
  full_dom_name = rename_domain(dom_name)
  
  p3 <- ggplot(data = df_plot %>%
                 filter(domain_name %in% curr_domains[[i]]) %>% 
                 distinct(spec_num, color, n_obs, nlpar),
               aes(x = spec_num, color = color, fill = color,
                   y = n_obs)) +
    geom_bar(stat = "identity", alpha = .5, size = .1)+ 
    theme_minimal() +
    facet_grid(.~nlpar)+
    scale_color_identity() +
    scale_fill_identity() +
    scale_y_continuous(labels = scales::comma) +
    theme(text = element_text(family = "Source Sans 3"),
          title = element_text(family = "Source Sans 3", face = "bold", size = 8, margin = margin(r = 5)),
          plot.title = element_text(face = "bold", size = 16),
          strip.text = element_blank(),
          panel.grid = element_line(linetype = "dotted"),
          axis.text.x = element_blank(),
          panel.spacing.x = unit(.75, "cm"),
          plot.margin = margin(b = 5, r = 10),
          axis.line = element_line("black", size = .5),
          legend.position = "none",
          axis.text = element_text(colour = "black", size = 6)) +
    labs(x = "", y = "N. obs") +
    scale_x_continuous( breaks = c(1,seq(10,round(max_specs),10), max_specs))+
    coord_cartesian(xlim = c(1,max_specs), clip = "off")
  
  p3
  
  
  p2 <- ggplot(data = df_plot %>% 
                 filter(incl == 1 & domain_name %in% curr_domains[[i]]), 
               aes(x = spec_num,
                   y = vars)) +
    geom_point(shape = "|",
               size = 5,
               aes(color = color)) +
    theme_minimal() +
    scale_color_identity() +
    scale_x_continuous( breaks = c(1,seq(10,round(max_specs),10), max_specs))+
    coord_cartesian(xlim = c(1,max_specs), clip = "off") +
    theme( text = element_text(family = "Source Sans 3", size = 8),
           title = element_text(family = "Source Sans 3", face = "bold", size = 8),
           axis.line = element_line("black", size = .5),
           legend.position = "none",
           panel.spacing.x = unit(.75, "cm"),
           plot.margin = margin(r = 10, b = 15),
           panel.grid = element_line(linetype = "dotted"),
           panel.spacing.y = unit(.75, "lines"),
           strip.placement = "outside",
           axis.text = element_text(colour = "black"),
           strip.text.y =  element_text(face = "bold", size = 7),
           strip.text.x = element_blank()
    ) +
    labs(x = "Dataset/Aggregation/Analysis Combination", y = "") 
  
  p2 <- p2 +
    facet_nested(nester + key~nlpar, scales = "free_y", space = "free_y", switch = "y",
                 nest_line = element_line(linetype = 2, linewidth = .25, color = ("grey30")))
  p2
  
  p1 <- df_plot %>% 
    filter(domain_name %in% curr_domains[[i]]) %>% 
    ggplot(aes(x = spec_num,
               y =  .epred,
               ymin = .lower_0.95,
               ymax = .upper_0.95,
               fill = fill,
               color = color)) +
    geom_errorbar(width=0, size = .25) +
    geom_point(size = .25, color = "white")+ 
    theme_minimal() +
    facet_grid(.~nlpar) +
    scale_color_identity() +
    scale_fill_identity() +
    theme(text = element_text(family = "Source Sans 3", size = 8),
          title = element_text(family = "Source Sans 3", face = "bold", size = 8, margin = margin(r = 5)),
          plot.title = element_text(face = "bold", size = 11),
          panel.background = element_rect(fill = "NA", color = "grey80", linewidth = .1),
          strip.text.x =  element_text(face = "bold", size = 9),
          panel.spacing.x = unit(.75, "cm"),
          panel.grid = element_line(linetype = "dotted"),
          plot.margin = margin(r = 10, b = 10),
          axis.line = element_line("black", size = .5),
          legend.position = "none",
          axis.text = element_text(colour = "black")) +
    labs(x = "", y = "Paramater Estimate", title = full_dom_name) +
    scale_x_continuous( breaks = c(1,seq(10,round(max_specs),10), max_specs))+
    coord_cartesian(xlim = c(1,max_specs), clip = "off") +
    scale_y_continuous(breaks = seq(0,1, .1),
                       limits = c(0, 1))
  
  
  p1
  
  p <- plot_grid(p1,
                 p3,
                 p2,
                 align = "v",
                 ncol = 1,
                 axis = "rbl",
                 rel_heights = c(.5,.15,.75)) 
  
  
  
  
  ggsave(plot = p, paste0(output_path, "masc_pro_",dom_name, "_multiverse.png"), 
         height = 25,
         width = 40, units = "cm", dpi = 300)
  
}



# FREQUENCY ----------------------------------------------------------------

color_name <- as.character(c_palette["Frequency"])

# check diagnostics
rhat_num <- (sapply(masc_list$fre$rhat,function(x) x[1:4]))
sum(rhat_num > 1.05, na.rm = T) # 122  param estimates with rhat > 1.05
table(masc_list$fre$div_trans) # 0 models



df <- masc_list$fre$results


df_plot <- df %>%
  group_by(domain_name, nlpar) %>%
  arrange(.epred) %>%
  mutate(spec_num = 1:n()) %>% ungroup()  %>%  mutate(rho_val_01 = if_else(rho_val == 0.1, 1,0),
                                                      rho_val_05 = if_else(rho_val == 0.5, 1,0),
                                                      rho_val_09 = if_else(rho_val == 0.9, 1,0),
                                                      time_bin_3mo = if_else(time_bin == 3, 1,0),
                                                      time_bin_6mo = if_else(time_bin == 6, 1,0),
                                                      time_bin_12mo = if_else(time_bin == 12, 1,0),
                                                      pearson_cor = if_else(grepl("pearson", cor_metric), 1,0),
                                                      spearman_cor = if_else(grepl("spearman", cor_metric), 1,0),
                                                      icc_cor = if_else(grepl("icc", cor_metric), 1,0),
                                                      year_age_bin_5 = if_else(year_age_bin == 5, 1,0),
                                                      year_age_bin_10 = if_else(year_age_bin == 10, 1,0),
                                                      year_age_bin_20 = if_else(year_age_bin == 20, 1,0),
                                                      none_transf = if_else(dt_transform == "none", 1,0),
                                                      log_transf = if_else(dt_transform == "log", 1,0),
                                                      min_n_30 = if_else(min_n == 30, 1,0), 
                                                      min_n_100 = if_else(min_n == 100, 1,0),                                                    
                                                      min_n_250 = if_else(min_n == 250, 1,0),  
                                                      student = if_else(family == "student", 1,0),  
                                                      gaussian = if_else(family == "gaussian", 1,0),  
                                                      vague = if_else(priors == "vague", 1,0),  
                                                      weakly_informative = if_else(priors == "weakly_informative", 1,0))



df_plot <- df_plot  %>%  pivot_longer(rho_val_01:weakly_informative, names_to = "vars", values_to = "incl") 


df_plot <- df_plot %>% mutate(key = case_when(vars %like% "rho"  ~ "Sampling\nerrors corr.",
                                              vars %like% "_cor"  ~ "Corr.\nmetric",
                                              vars %like% "_transf"  ~ "Data\ntransf.",
                                              vars %like% "_age_"  ~ "Age\nbinning",
                                              vars %like% "time_bin"  ~ "Time\nbinning",
                                              vars %like% "min_"  ~ "Sample\nsize (min.)",
                                              vars %like% "stud"  ~ "Likelih.",
                                              vars %like% "gauss"  ~ "Likelih.",
                                              vars %like% "vague"  ~ "Priors",
                                              vars %like% "weak"  ~ "Priors"),
                              # domain_name = case_when(grepl("ins", domain_name)  ~ "Insurance",
                              #                         grepl("gam", domain_name)  ~ "Gambling",
                              #                         grepl("occ", domain_name)  ~ "Occupational",
                              #                         grepl("inv", domain_name)  ~ "Investment"),
                              nlpar = case_when(grepl("rel", nlpar)  ~ "Reliability",
                                                grepl("stabch", nlpar)  ~ "Stab. of Change",
                                                grepl("change", nlpar)  ~ "Change"),
                              nester = case_when(grepl("metric|Sample|Age|transf.", key)  ~ "Data",
                                                 grepl("Time|Sampling", key)  ~ "Aggregation",
                                                 grepl("Priors|Likelih.", key)  ~ "Analysis"),
                              fill = case_when(.lower_0.95 < 0 & .upper_0.95 > 0  ~ "white",
                                               .lower_0.95 > 0 &  .upper_0.95 > 0 ~"white",
                                               .lower_0.95 < 0 &  .upper_0.95 < 0 ~"white"),
                              color = case_when(.lower_0.95 < 0 & .upper_0.95 > 0  ~ color_name,
                                                .lower_0.95 > 0 &  .upper_0.95 > 0 ~ color_name,
                                                .lower_0.95 < 0 &  .upper_0.95 < 0 ~ color_name))


# rename controls
replace_names <- data.frame(old_name =  c("rho_val_01"   
                                          ,"rho_val_05"
                                          ,"rho_val_09"
                                          ,"time_bin_3mo" 
                                          ,"time_bin_6mo" 
                                          ,"time_bin_12mo"
                                          ,"pearson_cor"       
                                          ,"spearman_cor"  
                                          , "icc_cor"
                                          ,"year_age_bin_5"    
                                          ,"year_age_bin_10"   
                                          ,"year_age_bin_20"   
                                          ,"min_n_30"          
                                          ,"min_n_100"         
                                          ,"min_n_250"         
                                          , "student"           
                                          , "gaussian"          
                                          , "vague"             
                                          , "weakly_informative"
                                          ,"none_transf"
                                          ,"log_transf"
),
new_name = c("rho = 0.1" 
             ,"rho = 0.5" 
             ,"rho = 0.9"
             ,"3-month bin"
             ,"6-month bin"
             ,"12-month bin"
             ,"Pearson's r"       
             ,"Spearman's rho" 
             , "ICC"
             ,"5-year age bin"    
             ,"10-year age bin"    
             ,"20-year age bin"    
             ,"30"          
             ,"100"         
             ,"250"         
             , "Student-t"           
             , "Gaussian"          
             , "Vague"             
             , "Weakly Informative"
             , "None"
             , "Log. transf."))




# replacing var codes
names_found <- match(df_plot$vars,replace_names$old_name , nomatch = 0)
df_plot$vars[df_plot$vars %in% replace_names$old_name] <- replace_names$new_name[names_found]

max_specs <- max(df_plot$spec_num)

df_plot$vars <- factor(df_plot$vars, levels = c("rho = 0.9" 
                                                ,"rho = 0.5" 
                                                ,"rho = 0.1" 
                                                ,"3-month bin"
                                                ,"6-month bin"
                                                ,"12-month bin"
                                                ,"ICC"
                                                ,"Pearson's r"       
                                                ,"Spearman's rho" 
                                                ,"20-year age bin"    
                                                ,"10-year age bin"    
                                                ,"5-year age bin"    
                                                ,"250"         
                                                ,"100"         
                                                ,"30"          
                                                , "Student-t"           
                                                , "Gaussian"          
                                                , "Vague"             
                                                , "Weakly Informative"
                                                , "None"
                                                , "Log. transf."))





curr_domains <- unique(df_plot$domain_name)

p_list <- NULL

for (i in 1:length(curr_domains)) {
  
  dom_name <- curr_domains[i]
  
  full_dom_name = rename_domain(dom_name)
  
  p3 <- ggplot(data = df_plot %>%
                 filter(domain_name %in% curr_domains[[i]]) %>% 
                 distinct(spec_num, color, n_obs, nlpar),
               aes(x = spec_num, color = color, fill = color,
                   y = n_obs)) +
    geom_bar(stat = "identity", alpha = .5, size = .1)+ 
    theme_minimal() +
    facet_grid(.~nlpar)+
    scale_color_identity() +
    scale_fill_identity() +
    scale_y_continuous(labels = scales::comma) +
    theme(text = element_text(family = "Source Sans 3"),
          title = element_text(family = "Source Sans 3", face = "bold", size = 8, margin = margin(r = 5)),
          plot.title = element_text(face = "bold", size = 16),
          strip.text = element_blank(),
          panel.grid = element_line(linetype = "dotted"),
          axis.text.x = element_blank(),
          panel.spacing.x = unit(.75, "cm"),
          plot.margin = margin(b = 5, r = 10),
          axis.line = element_line("black", size = .5),
          legend.position = "none",
          axis.text = element_text(colour = "black", size = 6)) +
    labs(x = "", y = "N. obs") +
    scale_x_continuous( breaks = c(1,seq(10,round(max_specs),10), max_specs))+
    coord_cartesian(xlim = c(1,max_specs), clip = "off")
  
  p3
  
  
  p2 <- ggplot(data = df_plot %>% 
                 filter(incl == 1 & domain_name %in% curr_domains[[i]]), 
               aes(x = spec_num,
                   y = vars)) +
    geom_point(shape = "|",
               size = 5,
               aes(color = color)) +
    theme_minimal() +
    scale_color_identity() +
    scale_x_continuous( breaks = c(1,seq(10,round(max_specs),10), max_specs))+
    coord_cartesian(xlim = c(1,max_specs), clip = "off") +
    theme( text = element_text(family = "Source Sans 3", size = 8),
           title = element_text(family = "Source Sans 3", face = "bold", size = 8),
           axis.line = element_line("black", size = .5),
           legend.position = "none",
           panel.spacing.x = unit(.75, "cm"),
           plot.margin = margin(r = 10, b = 15),
           panel.grid = element_line(linetype = "dotted"),
           panel.spacing.y = unit(.75, "lines"),
           strip.placement = "outside",
           axis.text = element_text(colour = "black"),
           strip.text.y =  element_text(face = "bold", size = 7),
           strip.text.x = element_blank()
    ) +
    labs(x = "Dataset/Aggregation/Analysis Combination", y = "") 
  
  p2 <- p2 +
    facet_nested(nester + key~nlpar, scales = "free_y", space = "free_y", switch = "y",
                 nest_line = element_line(linetype = 2, linewidth = .25, color = ("grey30")))
  p2
  
  p1 <- df_plot %>% 
    filter(domain_name %in% curr_domains[[i]]) %>% 
    ggplot(aes(x = spec_num,
               y =  .epred,
               ymin = .lower_0.95,
               ymax = .upper_0.95,
               fill = fill,
               color = color)) +
    geom_errorbar(width=0, size = .25) +
    geom_point(size = .25, color = "white")+ 
    theme_minimal() +
    facet_grid(.~nlpar) +
    scale_color_identity() +
    scale_fill_identity() +
    theme(text = element_text(family = "Source Sans 3", size = 8),
          title = element_text(family = "Source Sans 3", face = "bold", size = 8, margin = margin(r = 5)),
          plot.title = element_text(face = "bold", size = 11),
          panel.background = element_rect(fill = "NA", color = "grey80", linewidth = .1),
          strip.text.x =  element_text(face = "bold", size = 9),
          panel.spacing.x = unit(.75, "cm"),
          panel.grid = element_line(linetype = "dotted"),
          plot.margin = margin(r = 10, b = 10),
          axis.line = element_line("black", size = .5),
          legend.position = "none",
          axis.text = element_text(colour = "black")) +
    labs(x = "", y = "Paramater Estimate", title = full_dom_name) +
    scale_x_continuous( breaks = c(1,seq(10,round(max_specs),10), max_specs))+
    coord_cartesian(xlim = c(1,max_specs), clip = "off") +
    scale_y_continuous(breaks = seq(0,1, .1),
                       limits = c(0, 1))
  
  
  p1
  
  p <- plot_grid(p1,
                 p3,
                 p2,
                 align = "v",
                 ncol = 1,
                 axis = "rbl",
                 rel_heights = c(.5,.15,.75)) 
  
  
  
  
  ggsave(plot = p, paste0(output_path, "masc_fre_",dom_name, "_multiverse.png"), 
         height = 25,
         width = 40, units = "cm", dpi = 300)
  
}



# BEHAVIOUR ----------------------------------------------------------------

color_name <- as.character(c_palette["Behaviour"])

# check diagnostics
rhat_num <- (sapply(masc_list$beh$rhat,function(x) x[1:4]))
sum(rhat_num > 1.05, na.rm = T) # 33  param estimates with rhat > 1.05
table(masc_list$beh$div_trans) # 32 models with 1-4 divergent transitions



df <- masc_list$beh$results


df_plot <- df %>%
  group_by(domain_name, nlpar) %>%
  arrange(.epred) %>%
  mutate(spec_num = 1:n()) %>% ungroup()  %>%  mutate(rho_val_01 = if_else(rho_val == 0.1, 1,0),
                                                      rho_val_05 = if_else(rho_val == 0.5, 1,0),
                                                      rho_val_09 = if_else(rho_val == 0.9, 1,0),
                                                      time_bin_3mo = if_else(time_bin == 3, 1,0),
                                                      time_bin_6mo = if_else(time_bin == 6, 1,0),
                                                      time_bin_12mo = if_else(time_bin == 12, 1,0),
                                                      pearson_cor = if_else(grepl("pearson", cor_metric), 1,0),
                                                      spearman_cor = if_else(grepl("spearman", cor_metric), 1,0),
                                                      icc_cor = if_else(grepl("icc", cor_metric), 1,0),
                                                      year_age_bin_5 = if_else(year_age_bin == 5, 1,0),
                                                      year_age_bin_10 = if_else(year_age_bin == 10, 1,0),
                                                      year_age_bin_20 = if_else(year_age_bin == 20, 1,0),
                                                      none_transf = if_else(dt_transform == "none", 1,0),
                                                      log_transf = if_else(dt_transform == "log", 1,0),
                                                      min_n_30 = if_else(min_n == 30, 1,0), 
                                                      min_n_100 = if_else(min_n == 100, 1,0),                                                    
                                                      min_n_250 = if_else(min_n == 250, 1,0),  
                                                      student = if_else(family == "student", 1,0),  
                                                      gaussian = if_else(family == "gaussian", 1,0),  
                                                      vague = if_else(priors == "vague", 1,0),  
                                                      weakly_informative = if_else(priors == "weakly_informative", 1,0))



df_plot <- df_plot  %>%  pivot_longer(rho_val_01:weakly_informative, names_to = "vars", values_to = "incl") 


df_plot <- df_plot %>% mutate(key = case_when(vars %like% "rho"  ~ "Sampling\nerrors corr.",
                                              vars %like% "_cor"  ~ "Corr.\nmetric",
                                              vars %like% "_transf"  ~ "Data\ntransf.",
                                              vars %like% "_age_"  ~ "Age\nbinning",
                                              vars %like% "time_bin"  ~ "Time\nbinning",
                                              vars %like% "min_"  ~ "Sample\nsize (min.)",
                                              vars %like% "stud"  ~ "Likelih.",
                                              vars %like% "gauss"  ~ "Likelih.",
                                              vars %like% "vague"  ~ "Priors",
                                              vars %like% "weak"  ~ "Priors"),
                              # domain_name = case_when(grepl("ins", domain_name)  ~ "Insurance",
                              #                         grepl("gam", domain_name)  ~ "Gambling",
                              #                         grepl("occ", domain_name)  ~ "Occupational",
                              #                         grepl("inv", domain_name)  ~ "Investment"),
                              nlpar = case_when(grepl("rel", nlpar)  ~ "Reliability",
                                                grepl("stabch", nlpar)  ~ "Stab. of Change",
                                                grepl("change", nlpar)  ~ "Change"),
                              nester = case_when(grepl("metric|Sample|Age|transf.", key)  ~ "Data",
                                                 grepl("Time|Sampling", key)  ~ "Aggregation",
                                                 grepl("Priors|Likelih.", key)  ~ "Analysis"),
                              fill = case_when(.lower_0.95 < 0 & .upper_0.95 > 0  ~ "white",
                                               .lower_0.95 > 0 &  .upper_0.95 > 0 ~"white",
                                               .lower_0.95 < 0 &  .upper_0.95 < 0 ~"white"),
                              color = case_when(.lower_0.95 < 0 & .upper_0.95 > 0  ~ color_name,
                                                .lower_0.95 > 0 &  .upper_0.95 > 0 ~ color_name,
                                                .lower_0.95 < 0 &  .upper_0.95 < 0 ~ color_name))


# rename controls
replace_names <- data.frame(old_name =  c("rho_val_01"   
                                          ,"rho_val_05"
                                          ,"rho_val_09"
                                          ,"time_bin_3mo" 
                                          ,"time_bin_6mo" 
                                          ,"time_bin_12mo"
                                          ,"pearson_cor"       
                                          ,"spearman_cor"  
                                          , "icc_cor"
                                          ,"year_age_bin_5"    
                                          ,"year_age_bin_10"   
                                          ,"year_age_bin_20"   
                                          ,"min_n_30"          
                                          ,"min_n_100"         
                                          ,"min_n_250"         
                                          , "student"           
                                          , "gaussian"          
                                          , "vague"             
                                          , "weakly_informative"
                                          ,"none_transf"
                                          ,"log_transf"
),
new_name = c("rho = 0.1" 
             ,"rho = 0.5" 
             ,"rho = 0.9"
             ,"3-month bin"
             ,"6-month bin"
             ,"12-month bin"
             ,"Pearson's r"       
             ,"Spearman's rho" 
             , "ICC"
             ,"5-year age bin"    
             ,"10-year age bin"    
             ,"20-year age bin"    
             ,"30"          
             ,"100"         
             ,"250"         
             , "Student-t"           
             , "Gaussian"          
             , "Vague"             
             , "Weakly Informative"
             , "None"
             , "Log. transf."))




# replacing var codes
names_found <- match(df_plot$vars,replace_names$old_name , nomatch = 0)
df_plot$vars[df_plot$vars %in% replace_names$old_name] <- replace_names$new_name[names_found]

max_specs <- max(df_plot$spec_num)

df_plot$vars <- factor(df_plot$vars, levels = c("rho = 0.9" 
                                                ,"rho = 0.5" 
                                                ,"rho = 0.1" 
                                                ,"3-month bin"
                                                ,"6-month bin"
                                                ,"12-month bin"
                                                ,"ICC"
                                                ,"Pearson's r"       
                                                ,"Spearman's rho" 
                                                ,"20-year age bin"    
                                                ,"10-year age bin"    
                                                ,"5-year age bin"    
                                                ,"250"         
                                                ,"100"         
                                                ,"30"          
                                                , "Student-t"           
                                                , "Gaussian"          
                                                , "Vague"             
                                                , "Weakly Informative"
                                                , "None"
                                                , "Log. transf."))





curr_domains <- unique(df_plot$domain_name)

p_list <- NULL

for (i in 1:length(curr_domains)) {
  
  dom_name <- curr_domains[i]
  
  full_dom_name = case_when(grepl("ins", dom_name)  ~ "Insurance",
                    grepl("gam", dom_name)  ~ "Gambling",
                    grepl("occ", dom_name)  ~ "Occupational",
                    grepl("inv", dom_name)  ~ "Investment")
  
  p3 <- ggplot(data = df_plot %>%
                 filter(domain_name %in% curr_domains[[i]]) %>% 
                 distinct(spec_num, color, n_obs, nlpar),
               aes(x = spec_num, color = color, fill = color,
                   y = n_obs)) +
    geom_bar(stat = "identity", alpha = .5, size = .1)+ 
    theme_minimal() +
    facet_grid(.~nlpar)+
    scale_color_identity() +
    scale_fill_identity() +
    scale_y_continuous(labels = scales::comma) +
    theme(text = element_text(family = "Source Sans 3"),
          title = element_text(family = "Source Sans 3", face = "bold", size = 8, margin = margin(r = 5)),
          plot.title = element_text(face = "bold", size = 16),
          strip.text = element_blank(),
          panel.grid = element_line(linetype = "dotted"),
          axis.text.x = element_blank(),
          panel.spacing.x = unit(.75, "cm"),
          plot.margin = margin(b = 5, r = 10),
          axis.line = element_line("black", size = .5),
          legend.position = "none",
          axis.text = element_text(colour = "black", size = 6)) +
    labs(x = "", y = "N. obs") +
    scale_x_continuous( breaks = c(1,seq(100,round(max_specs,-2),100), max_specs))+
    coord_cartesian(xlim = c(1,max_specs), clip = "off")
  
  p3
  
  
  p2 <- ggplot(data = df_plot %>% 
                 filter(incl == 1 & domain_name %in% curr_domains[[i]]), 
               aes(x = spec_num,
                   y = vars)) +
    geom_point(shape = "|",
               size = 5,
               aes(color = color)) +
    theme_minimal() +
    scale_color_identity() +
    scale_x_continuous( breaks = c(1,seq(100,round(max_specs,-2),100), max_specs))+
    coord_cartesian(xlim = c(1,max_specs), clip = "off")+
    theme( text = element_text(family = "Source Sans 3", size = 8),
           title = element_text(family = "Source Sans 3", face = "bold", size = 8),
           axis.line = element_line("black", size = .5),
           legend.position = "none",
           panel.spacing.x = unit(.75, "cm"),
           plot.margin = margin(r = 10, b = 15),
           panel.grid = element_line(linetype = "dotted"),
           panel.spacing.y = unit(.75, "lines"),
           strip.placement = "outside",
           axis.text = element_text(colour = "black"),
           strip.text.y =  element_text(face = "bold", size = 7),
           strip.text.x = element_blank()
    ) +
    labs(x = "Dataset/Aggregation/Analysis Combination", y = "") 
  
  p2 <- p2 +
    facet_nested(nester + key~nlpar, scales = "free_y", space = "free_y", switch = "y",
                 nest_line = element_line(linetype = 2, linewidth = .25, color = ("grey30")))
  p2
  
  p1 <- df_plot %>% 
    filter(domain_name %in% curr_domains[[i]]) %>% 
    ggplot(aes(x = spec_num,
               y =  .epred,
               ymin = .lower_0.95,
               ymax = .upper_0.95,
               fill = fill,
               color = color)) +
    geom_errorbar(width=0, size = .25) +
    geom_point(size = .25, color = "white")+ 
    theme_minimal() +
    facet_grid(.~nlpar) +
    scale_color_identity() +
    scale_fill_identity() +
    theme(text = element_text(family = "Source Sans 3", size = 8),
          title = element_text(family = "Source Sans 3", face = "bold", size = 8, margin = margin(r = 5)),
          plot.title = element_text(face = "bold", size = 11),
          panel.background = element_rect(fill = "NA", color = "grey80", linewidth = .1),
          strip.text.x =  element_text(face = "bold", size = 9),
          panel.spacing.x = unit(.75, "cm"),
          panel.grid = element_line(linetype = "dotted"),
          plot.margin = margin(r = 10, b = 10),
          axis.line = element_line("black", size = .5),
          legend.position = "none",
          axis.text = element_text(colour = "black")) +
    labs(x = "", y = "Paramater Estimate", title = full_dom_name) +
    scale_x_continuous( breaks = c(1,seq(100,round(max_specs,-2),100), max_specs))+
    scale_y_continuous(breaks = seq(0,1, .1),
                       limits = c(0, 1)) +
    coord_cartesian(xlim = c(1,max_specs), clip = "off")
  
  
  p1
  
  p <- plot_grid(p1,
                 p3,
                 p2,
                 align = "v",
                 ncol = 1,
                 axis = "rbl",
                 rel_heights = c(.5,.15,.75)) 
  
  
  
  
  ggsave(plot = p, paste0(output_path, "masc_beh_",dom_name, "_multiverse.png"), 
         height = 25,
         width = 40, units = "cm", dpi = 300)
  
}
