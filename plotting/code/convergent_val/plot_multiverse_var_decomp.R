

# DESCRIPTION -------------------------------------------------------------

# Script that plots the results of the mulriverse Variance Decomposition analysis
# Code adapted from plotting function of the specr package
# https://github.com/masurp/specr/tree/master
# Masur, Philipp K. & Scharkow, M. (2020). specr: Conducting and Visualizing Specification Curve Analyses. Available from https://CRAN.R-project.org/package=specr.


# Author(s): Alexandra Bagaini, Centre for Cognitive and Decision Sciences, Faculty of Psychology, University of Basel.



# PACKAGES ----------------------------------------------------------------




library(tidyverse)
library(data.table)
library(cowplot)
library(patchwork)


# FILES ------------------------------------------------------------------


data_path <- c("analysis/output/convergent_val/")
output_path <- c("docs/images/")

shapley_list <- list(
  ominbus = read_csv(paste0(data_path, "shapley_values_multiv_overall.csv")))
# OMNIBUS -----------------------------------------------------------------


df <- shapley_list[["ominbus"]]


df_plot <- df %>%
  group_by(x) %>%  arrange(m) %>%
  mutate(spec_num = 1:n()) %>% ungroup()  %>%  mutate(pearson_cor = if_else(grepl("pearson", corr_metric), 1,0),
                                                      spearman_cor = if_else(grepl("spearman", corr_metric), 1,0),
                                                      icc_cor = if_else(grepl("icc", corr_metric), 1,0),
                                                      year_age_bin_5 = if_else(age_bin == 5, 1,0),
                                                      year_age_bin_10 = if_else(age_bin == 10, 1,0),
                                                      year_age_bin_20 = if_else(age_bin == 20, 1,0),
                                                      log_dat = if_else(data_transform == "log", 1,0), 
                                                      none_dat = if_else(data_transform == "none", 1,0), 
                                                      min_n_30 = if_else(min_n == 30, 1,0), 
                                                      min_n_100 = if_else(min_n == 100, 1,0),                                                    
                                                      min_n_250 = if_else(min_n == 250, 1,0)) 



df_plot <- df_plot  %>%  pivot_longer(pearson_cor:min_n_250, names_to = "vars", values_to = "incl") 

df_plot <- df_plot %>% mutate(key = case_when(vars %like% "_cor"  ~ "Corr.\nmetric",
                                              vars %like% "_age_"  ~ "Age\nbinning",
                                              vars %like% "min_"  ~ "Sample\nsize (min.)",
                                              vars %like% "_dat"  ~ "Data\ntransf."),
                              fill = "white",
                              color =  "#23583A")


# rename controls
replace_names <- data.frame(old_name =  c("pearson_cor"       
                                          ,"spearman_cor"  
                                          , "icc_cor"
                                          , "log_dat"
                                          , "none_dat"
                                          ,"year_age_bin_5"    
                                          ,"year_age_bin_10"   
                                          ,"year_age_bin_20"   
                                          ,"min_n_30"          
                                          ,"min_n_100"         
                                          ,"min_n_250"         
                                          , "months_3"           
                                          , "months_6"          
                                          , "months_12"),
                            new_name = c("Pearson's r"       
                                         ,"Spearman's rho"    
                                         , "ICC"
                                         , "Log. transf."
                                         , "None"
                                         ,"5-year age bin"    
                                         ,"10-year age bin"    
                                         ,"20-year age bin"    
                                         ,"30"          
                                         ,"100"         
                                         ,"250"         
                                         , "3 months"           
                                         , "6 months"          
                                         , "12 months"))




# replacing var codes
names_found <- match(df_plot$vars,replace_names$old_name , nomatch = 0)
df_plot$vars[df_plot$vars %in% replace_names$old_name] <- replace_names$new_name[names_found]

max_specs <- max(df_plot$spec_num)

df_plot$vars <- factor(df_plot$vars, levels = c("ICC"
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

df_plot <- df_plot %>% mutate(x = case_when(x == "gender_group" ~ "Gender",
                                            x == "age_group" ~ "Age",
                                            x == "sample_size" ~ "n",
                                            x == "measure_category_pair_same" ~ "Matching Category",
                                            x == "scale_type_pair_same" ~ "Matching Scale Type",
                                            x == "domain_name_pair_same" ~ "Matching Domain",
                                            x == "mean_rel" ~ "Reliability",
                                            x == "panel" ~ "Panel"))

curr_preds <- list(unique(df_plot$x)[1:3],
                   unique(df_plot$x)[4:6],
                   unique(df_plot$x)[7:8])
p_list <- NULL

for (i in 1:length(curr_preds)) {
  p3 <- ggplot(data = df_plot %>% 
                 filter(x %in% curr_preds[[i]]) %>% 
                 distinct(spec_num, color, n_obs, x),
               aes(x = spec_num, color = color, fill = color,
                   y = n_obs)) +
    geom_bar(stat = "identity", alpha = .25, size = .15)+ 
    facet_wrap(x~., scales = "fixed", nrow = 1) +
    # geom_hline(yintercept = 0, color = "red", size = 0.75, alpha = .5) +
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
    # scale_y_continuous(expand = c(0,0))+
    scale_x_continuous( breaks = c(1,seq(10,round(max_specs,-1),10), max_specs))+
    coord_cartesian(xlim = c(1,max_specs), clip = "off")
  
  
  
  
  p2 <- ggplot(data = df_plot %>% 
                 filter(incl == 1 & x %in% curr_preds[[i]]), 
               aes(x = spec_num,
                   y = vars)) +
    geom_point(shape = "|",
               size = 5,
               aes(color = color)) +
    # geom_vline(xintercept = max(max_specs + 5)) +
    theme_minimal() +
    scale_color_identity() +
    scale_x_continuous( breaks = c(1,seq(10,round(max_specs,-1),10), max_specs))+
    coord_cartesian(xlim = c(1,max_specs), clip = "off")+
    theme( text = element_text(family = "Source Sans 3", size = 8),
           title = element_text(family = "Source Sans 3", face = "bold", size = 8),
           axis.line = element_line("black", size = .5),
           # strip.background = element_rect(fill = NA, color = "black", size = 1),
           legend.position = "none",
           panel.spacing.x = unit(.75, "cm"),
           plot.margin = margin(r = 10, b = 15),
           # strip.placement = "outside",
           panel.grid = element_line(linetype = "dotted"),
           panel.spacing.y = unit(.75, "lines"),
           strip.placement = "outside",
           axis.text = element_text(colour = "black"),
           strip.text.y =  element_text(face = "bold", size = 7),
           strip.text.x = element_blank()
    ) +
    labs(x = "Dataset/Aggregation/Analysis Combination", y = "") 
  
  p3
  p2 <- p2 +
    facet_grid(key~x, scales = "free_y", space = "free_y", switch = "y")
  
  
  p2
  
  p1 <- df_plot %>% 
    filter(x %in% curr_preds[[i]]) %>% 
    distinct(m, spec_num, color, fill,x) %>% 
    ggplot(aes(x = spec_num,
               y = m,
               fill = fill,
               color = color)) +
    # geom_ribbon(data = df_null, aes(x = spec_num,
    #                                 ymin= conf.low,
    #                                 y = m_estimate,
    #                                 ymax = conf.high,
    #                                 fill = fill,
    #                                 color = color),
    #             alpha = .25, size = .25) +
    # geom_errorbar(width=0, size = .5) +
    geom_point(size = 1, alpha = .75)+ 
    facet_wrap(x~., scales = "fixed", nrow = 1)+  # geom_hline(yintercept = 0, color = "red", size = 0.75, alpha = .5) +
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
    labs(x = "", y = "Shapley Value") +
    scale_x_continuous( breaks = c(1,seq(10,round(max_specs,-1),10), max_specs))+
    scale_y_continuous(breaks = seq(0,.45, .1),
                       limits = c(0, .45)) +
    coord_cartesian(xlim = c(1,max_specs), clip = "off")
  
  
  p1
  
  p <- plot_grid(p1,
                 p3,
                 p2,
                 align = "v",
                 ncol = 1,
                 axis = "rbl",
                 rel_heights = c(.5,.2,.75)) 
  # theme(plot.background = element_rect(color = "grey70", fill = NA, size = .2))
  
  p
  
  
  p_list[[i]] <- p
  
}



layout <- "
AAA
BBB
CC#
"


p <- p_list[[1]] + p_list[[2]] + (p_list[[3]] + theme(plot.margin = margin(r = 10))) +
  plot_layout(design = layout)



ggsave(plot = p,  paste0(output_path, "shapley_intercor_multiverse.png"), height = 35,
       width = 25, units = "cm", dpi = 300)


