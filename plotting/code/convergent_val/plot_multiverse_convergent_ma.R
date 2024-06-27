

# DESCRIPTION -------------------------------------------------------------

# Script that plots the results of the multiverse meta-analysis of inter-correlation (overall and measure-pair estimates)
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


# FILES ------------------------------------------------------------------


data_path <- c("analysis/output/convergent_val/")
output_path <- c("docs/images/")

ma_list <- list(
  overall = read_rds(paste0(data_path, "multiverse_fit_convergent_ma_overall.rds")),
  measures = read_rds(paste0(data_path, "multiverse_fit_convergent_ma_measure.rds")))




# OVERALL -----------------------------------------------------------------

# check diagnostics
rhat_num <- (sapply(ma_list$overall$rhat,"[[",1))
hist(rhat_num)
sum(rhat_num > 1.05) 
table(ma_list$overall$div_trans) 
neff_num <- (sapply(ma_list$overall$neffRatio,"[[",1))
summary(neff_num)


df <- ma_list$overall$results


df_plot <- df %>%
  arrange(Intercept) %>%
  mutate(spec_num = 1:n()) %>% ungroup()  %>%  mutate(rho_val_01 = if_else(rho_val == 0.1, 1,0),
                                                      rho_val_05 = if_else(rho_val == 0.5, 1,0),
                                                      rho_val_09 = if_else(rho_val == 0.9, 1,0),
                                                      pearson_cor = if_else(grepl("pearson", cor_metric), 1,0),
                                                      spearman_cor = if_else(grepl("spearman", cor_metric), 1,0),
                                                      icc_cor = if_else(grepl("icc", cor_metric), 1,0),
                                                      year_age_bin_5 = if_else(year_age_bin == 5, 1,0),
                                                      year_age_bin_10 = if_else(year_age_bin == 10, 1,0),
                                                      year_age_bin_20 = if_else(year_age_bin == 20, 1,0),
                                                      none_transf = if_else(data_transform == "none", 1,0),
                                                      log_transf = if_else(data_transform == "log", 1,0),
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
                                              vars %like% "min_"  ~ "Sample\nsize (min.)",
                                              vars %like% "stud"  ~ "Likelih.",
                                              vars %like% "gauss"  ~ "Likelih.",
                                              vars %like% "vague"  ~ "Priors",
                                              vars %like% "weak"  ~ "Priors"),
                              nester = case_when(grepl("metric|Sample|binning|transf.", key)  ~ "Data",
                                                 grepl("Monthly|Sampling", key)  ~ "Aggregation",
                                                 grepl("Priors|Likelih.", key)  ~ "Analysis"),
                              fill = case_when(.lower_0.95 < 0 & .upper_0.95 > 0  ~ "white",
                                               .lower_0.95 > 0 &  .upper_0.95 > 0 ~"white",
                                               .lower_0.95 < 0 &  .upper_0.95 < 0 ~"white"),
                              color = case_when(.lower_0.95 < 0 & .upper_0.95 > 0  ~ "#51A3A3",
                                                .lower_0.95 > 0 &  .upper_0.95 > 0 ~ "#51A3A3",
                                                .lower_0.95 < 0 &  .upper_0.95 < 0 ~ "#51A3A3"))


# rename controls
replace_names <- data.frame(old_name =  c("rho_val_01"   
                                          ,"rho_val_05"
                                          ,"rho_val_09"
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



p3 <- ggplot(data = df_plot %>% 
               distinct(spec_num, color, n_obs),
             aes(x = spec_num, color = color, fill = color,
                 y = n_obs)) +
  geom_bar(stat = "identity", alpha = .25, size = .15)+ 
  theme_minimal() +
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
               filter(incl == 1), 
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
  facet_nested(nester + key~., scales = "free_y", space = "free_y", switch = "y",
               nest_line = element_line(linetype = 2, linewidth = .25, color = ("grey30")))
p2

p1 <- df_plot %>% 
  ggplot(aes(x = spec_num,
             y = Intercept,
             ymin = .lower_0.95,
             ymax = .upper_0.95,
             fill = fill,
             color = color)) +
  geom_errorbar(width=0, size = .5) +
  geom_point(size = .25, color = "white")+ 
  theme_minimal() +
  scale_color_identity() +
  scale_fill_identity() +
  theme(text = element_text(family = "Source Sans 3", size = 8),
        title = element_text(family = "Source Sans 3", face = "bold", size = 8, margin = margin(r = 5)),
        plot.title = element_text(face = "bold", size = 16),
        panel.background = element_rect(fill = "NA", color = "grey80", linewidth = .1),
        strip.text.x =  element_text(face = "bold", size = 10),
        panel.spacing.x = unit(.75, "cm"),
        panel.grid = element_line(linetype = "dotted"),
        plot.margin = margin(r = 10, b = 10),
        axis.line = element_line("black", size = .5),
        legend.position = "none",
        axis.text = element_text(colour = "black")) +
  labs(x = "", y = "Pooled Estimate") +
  scale_x_continuous( breaks = c(1,seq(100,round(max_specs,-2),100), max_specs))+
  scale_y_continuous(breaks = seq(0,.4, .1),
                     limits = c(0, .4)) +
  coord_cartesian(xlim = c(1,max_specs), clip = "off")


p1

p <- plot_grid(p1,
               p3,
               p2,
               align = "v",
               ncol = 1,
               axis = "rbl",
               rel_heights = c(.5,.2,.75)) 

p



ggsave(plot = p,  paste0(output_path, "convergent_ma_overall_multiverse.png"), height = 20,
       width = 20, units = "cm", dpi = 300)






# MEASURES ----------------------------------------------------------------



# check diagnostics
rhat_num <- (sapply(ma_list$measures$rhat,function(x) x[1:6]))
sum(rhat_num > 1.05, na.rm = T) # 1 model
table(ma_list$measures$div_trans) # 5 divergent transition in one model
neff_num <- (sapply(ma_list$overall$neffRatio,"[[",1))
summary(neff_num)


df <- ma_list$measures$results


df_plot <- df %>%
  group_by(meas_pair_id) %>%
  arrange(estimate) %>%
  mutate(spec_num = 1:n()) %>% ungroup()  %>%  mutate(rho_val_01 = if_else(rho_val == 0.1, 1,0),
                                                      rho_val_05 = if_else(rho_val == 0.5, 1,0),
                                                      rho_val_09 = if_else(rho_val == 0.9, 1,0),
                                                      pearson_cor = if_else(grepl("pearson", cor_metric), 1,0),
                                                      spearman_cor = if_else(grepl("spearman", cor_metric), 1,0),
                                                      icc_cor = if_else(grepl("icc", cor_metric), 1,0),
                                                      year_age_bin_5 = if_else(year_age_bin == 5, 1,0),
                                                      year_age_bin_10 = if_else(year_age_bin == 10, 1,0),
                                                      year_age_bin_20 = if_else(year_age_bin == 20, 1,0),
                                                      none_transf = if_else(data_transform == "none", 1,0),
                                                      log_transf = if_else(data_transform == "log", 1,0),
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
                                              vars %like% "min_"  ~ "Sample\nsize (min.)",
                                              vars %like% "stud"  ~ "Likelih.",
                                              vars %like% "gauss"  ~ "Likelih.",
                                              vars %like% "vague"  ~ "Priors",
                                              vars %like% "weak"  ~ "Priors"),
                              nester = case_when(grepl("metric|Sample|binning|transf.", key)  ~ "Data",
                                                 grepl("Monthly|Sampling", key)  ~ "Aggregation",
                                                 grepl("Priors|Likelih.", key)  ~ "Analysis"),
                              fill = case_when(.lower_0.95 < 0 & .upper_0.95 > 0  ~ "white",
                                               .lower_0.95 > 0 &  .upper_0.95 > 0 ~"white",
                                               .lower_0.95 < 0 &  .upper_0.95 < 0 ~"white"),
                              color = case_when(.lower_0.95 < 0 & .upper_0.95 > 0  ~ "#51A3A3",
                                                .lower_0.95 > 0 &  .upper_0.95 > 0 ~ "#51A3A3",
                                                .lower_0.95 < 0 &  .upper_0.95 < 0 ~ "#51A3A3"))


# rename controls
replace_names <- data.frame(old_name =  c("rho_val_01"   
                                          ,"rho_val_05"
                                          ,"rho_val_09"
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





curr_pairs <- unique(df_plot$meas_pair_id)
meas_pairs <- distinct(df, meas_pair_id, meas_pair_lbl)

p_list <- NULL

for (i in 1:length(curr_pairs)) {

 pair_name <- gsub("_", "-", meas_pairs$meas_pair_lbl[meas_pairs$meas_pair_id == curr_pairs[[i]]]) 
  
  
p3 <- ggplot(data = df_plot %>%
               filter(meas_pair_id %in% curr_pairs[[i]]) %>% 
               distinct(spec_num, color, n_obs),
             aes(x = spec_num, color = color, fill = color,
                 y = n_obs)) +
  geom_bar(stat = "identity", alpha = .25, size = .15)+ 
  theme_minimal() +
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
               filter(incl == 1 & meas_pair_id %in% curr_pairs[[i]]), 
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
  facet_nested(nester + key~., scales = "free_y", space = "free_y", switch = "y",
               nest_line = element_line(linetype = 2, linewidth = .25, color = ("grey30")))
p2

p1 <- df_plot %>% 
  filter(meas_pair_id %in% curr_pairs[[i]]) %>% 
  ggplot(aes(x = spec_num,
             y = estimate,
             ymin = .lower_0.95,
             ymax = .upper_0.95,
             fill = fill,
             color = color)) +
  geom_errorbar(width=0, size = .5) +
  geom_point(size = .25, color = "white")+ 
  theme_minimal() +
  scale_color_identity() +
  scale_fill_identity() +
  theme(text = element_text(family = "Source Sans 3", size = 8),
        title = element_text(family = "Source Sans 3", face = "bold", size = 8, margin = margin(r = 5)),
        plot.title = element_text(face = "bold", size = 10),
        panel.background = element_rect(fill = "NA", color = "grey80", linewidth = .1),
        strip.text.x =  element_text(face = "bold", size = 10),
        panel.spacing.x = unit(.75, "cm"),
        panel.grid = element_line(linetype = "dotted"),
        plot.margin = margin(r = 10, b = 10),
        axis.line = element_line("black", size = .5),
        legend.position = "none",
        axis.text = element_text(colour = "black")) +
  labs(x = "", y = "Pooled Estimate", title = pair_name) +
  scale_x_continuous( breaks = c(1,seq(100,round(max_specs,-2),100), max_specs))+
  scale_y_continuous(breaks = seq(0,.6, .1),
                     limits = c(0, .6)) +
  coord_cartesian(xlim = c(1,max_specs), clip = "off")


p1

p <- plot_grid(p1,
               p3,
               p2,
               align = "v",
               ncol = 1,
               axis = "rbl",
               rel_heights = c(.5,.2,.75)) 




ggsave(plot = p, paste0(output_path, "convergent_ma_measure_",pair_name, "_multiverse.png"), 
       height = 20,
       width = 20, units = "cm", dpi = 300)

}
