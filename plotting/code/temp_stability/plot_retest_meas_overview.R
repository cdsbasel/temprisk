# DESCRIPTION -------------------------------------------------------------

# Plotting the number of measures (split by category) across different retest intervals. The distribution of test-retest correlations
# split by measure category across different retest intervals

# Author(s): Alexandra Bagaini, Centre for Cognitive and Decision Sciences, Faculty of Psychology, University of Basel.

# PACKAGES ---------------------------------------------------------------

library(tidyverse)
library(modi)
library(spatstat)
library(ggdist)
library(cowplot)

# FILES  ---------------------------------------------------

data_path <- c("processing/output/temp_stability/") # where is the input file stored
retest_file <- "complete_retest.csv" # name of merged retest data 
output_path <- c("plotting/output/temp_stability/") # where to store the output 


# READ DATA ---------------------------------------------------------------
col_spec <-cols(
  panel = col_character(),
  wave_id_t1 = col_character(),
  wave_year_t1 = col_double(),
  wave_id_t2 = col_character(),
  wave_year_t2 = col_double(),
  time_diff_mean = col_double(),
  time_diff_median = col_double(),
  time_diff_min = col_double(),
  time_diff_max = col_double(),
  time_diff_sd = col_double(),
  year_age_group = col_double(),
  age_group = col_character(),
  age_mean = col_double(),
  age_median = col_double(),
  age_min = col_double(),
  age_max = col_double(),
  age_sd = col_double(),
  gender_group = col_character(),
  prop_female = col_double(),
  n = col_double(),
  attrition_rate = col_double(),
  varcode = col_character(),
  cor_pearson = col_double(),
  cor_spearman = col_double(),
  icc2_1 = col_double(),
  cor_pearson_log = col_double(),
  cor_spearman_log = col_double(),
  icc2_1_log = col_double(),
  coeff_var_t1 = col_double(),
  coeff_var_t2 = col_double(),
  skewness_t1 = col_double(),
  skewness_t2 = col_double(),
  measure_category = col_character(),
  general_domain = col_character(),
  domain_name = col_character(),
  subdomain_name = col_character(),
  scale_type = col_character(),
  scale_length = col_double(),
  time_frame = col_double(),
  behav_type = col_character(),
  behav_paid = col_character(),
  continent = col_character(),
  country = col_character(),
  language = col_character(),
  data_collect_mode = col_character(),
  sample_type = col_character()
)

dat <- read_csv(paste0(data_path,retest_file), col_types = col_spec)


# PREP. DATA ---------------------------------------------------------------


dat_fltr <-  dat %>%
  filter(age_group != "10-90" &
           gender_group != "all" & 
           year_age_group == 10 & 
           n >= 30)


dataA <- dat_fltr %>% 
  mutate(measure_category = case_when(measure_category == "fre"  ~ "Frequency",
                                      measure_category == "beh" ~ "Behaviour",
                                      measure_category == "pro" ~ "Propensity"
  ))  %>% 
  mutate(varcode_panel = paste0(panel, "_", varcode)) %>% 
  mutate(time_diff_bin = round(time_diff_mean,0)) %>%  # 1 year bins
  mutate(time_diff_bin_fct = as.factor(time_diff_bin)) %>%  # 1 year bins
  group_by(time_diff_bin_fct, time_diff_bin, measure_category) %>% 
  summarise(num_meas_year = length(unique(varcode_panel))) %>% 
  ungroup()


dataA$measure_category <-  factor(dataA$measure_category, levels=c('Propensity','Frequency','Behaviour'))


dataB <- dat_fltr %>% 
  mutate(measure_category = case_when(measure_category == "fre"  ~ "Frequency",
                                      measure_category == "beh" ~ "Behaviour",
                                      measure_category == "pro" ~ "Propensity"),
         time_diff_bin = round(time_diff_mean,0),
         time_diff_bin_fct =  as.factor(time_diff_bin)) # 1 year bins


dataB$measure_category <-  factor(dataB$measure_category, levels=c('Propensity','Frequency','Behaviour'))


# CORRELATION OVERVIEW --------------------------------------------------------


c_palette <-  c("Propensity" = "#502a7a",
                "Frequency" = "#1e9bae",
                "Behaviour" ="#e07f00")
  
  num_labels <- dataB %>% 
    group_by(time_diff_bin_fct, measure_category) %>% 
    summarise(num_cor = as.character(n()),
              min_cor = weighted.quantile(cor_pearson, w = n, prob = 0.925)+.01) %>%
    ungroup() %>% 
    mutate(label = paste0("(", num_cor, ")"),
           y = -.05)
  
  
  num_labels$measure_category <-  factor(num_labels$measure_category, levels=c('Propensity','Frequency','Behaviour'))
  
  dataB_agg <-  dataB %>% 
    group_by(time_diff_bin_fct, time_diff_bin, measure_category) %>% 
    summarise(m_cor = weighted.mean(cor_pearson, w = n),
              med_cor = weighted.median(cor_pearson, w = n),
              ci25_cor = weighted.quantile(cor_pearson, w = n, prob = 0.25),
              ci75_cor = weighted.quantile(cor_pearson, w = n, prob = 0.75),
              ci05_cor = weighted.quantile(cor_pearson, w = n, prob = 0.025),
              ci95_cor = weighted.quantile(cor_pearson, w = n, prob = 0.975),
              ci10_cor = weighted.quantile(cor_pearson, w = n, prob = 0.1),
              ci90_cor = weighted.quantile(cor_pearson, w = n, prob = 0.9)) %>%
    ungroup() 
  

  dataB_agg$measure_category <-  factor(dataB_agg$measure_category, levels=c('Propensity','Frequency','Behaviour'))
  
  
  A <- dataB %>% 
    ggplot(aes(x = cor_pearson, y = time_diff_bin_fct, fill = measure_category, color = measure_category)) +
    geom_vline(xintercept = 0, linetype = "solid", color = "grey80", size = .3) +
    stat_halfeye(color = "NA", alpha = .3, size = .3, normalize = "panels") +
    geom_crossbar(data = dataB_agg, aes(xmin = ci05_cor, x = med_cor, color = measure_category,
                                        xmax = ci95_cor, y =time_diff_bin_fct),
                  fill = "white",
                  linewidth = .15, position = position_nudge(y=-0.0),
                  width = 0.175, alpha =  1) +
    geom_crossbar(data = dataB_agg, aes(xmin = ci05_cor, x = med_cor,  xmax = ci95_cor, y =time_diff_bin_fct),
                  color = "NA",
                  linewidth = .2, position = position_nudge(y=-0.0),
                  width = 0.175, alpha =  .4) +
    geom_crossbar(data = dataB_agg, aes(xmin = ci10_cor, x = med_cor,  xmax = ci90_cor, y =time_diff_bin_fct),
                  color = "NA", fill = "white",
                  linewidth = .2, position = position_nudge(y=-0.0),
                  width = 0.175, alpha =  1) +
    geom_crossbar(data = dataB_agg, aes(xmin = ci10_cor, x = med_cor,  xmax = ci90_cor, y =time_diff_bin_fct),
                  color = "NA",
                  linewidth = .2, position = position_nudge(y=-0.0),
                  width = 0.175, alpha =  .7) +
    geom_crossbar(data = dataB_agg, aes(xmin = ci25_cor, x = med_cor,  xmax = ci75_cor, y =time_diff_bin_fct),
                  color = "NA", fill = "white",
                  linewidth = .2, position = position_nudge(y=-0.0),
                  width = 0.175, alpha =  1) +
    geom_crossbar(data = dataB_agg, aes(xmin = ci25_cor, x = med_cor,  xmax = ci75_cor, y =time_diff_bin_fct),
                  color = "NA",
                  linewidth = .2, position = position_nudge(y=-0.0),
                  width = 0.175, alpha =  1) +
    geom_point(data = dataB_agg, aes(x = med_cor, y =time_diff_bin_fct),
               fill = "white", color = "grey20",
               position = position_nudge(y=-0.0),
               shape = 21, 
               stroke = .5, 
               size = 1.5) +
    geom_text(data = num_labels, aes(label = label, y = time_diff_bin_fct, x = y),
              family = "Source Sans 3",  position = position_nudge(y = 0),
              size = 2.25, hjust = 0, vjust = .5, angle = 0, color = "grey50",
              show.legend = F)+
    scale_y_discrete(limits = (levels(unique(dataB$time_diff_bin_fct))),
                     expand = expansion(add = .1),
                     labels = (c("<1","1",paste0(as.character(c(2:20)))))) +
    scale_x_continuous(breaks = seq(0,1,.25), expand = c(0,0), limits = c(-.5,1.00)) +
   coord_cartesian(xlim = c(-.5,1.00)) +
    theme_minimal() +
    scale_fill_manual(values = c_palette) +
    scale_color_manual(values = c_palette) +
    labs(y = "Retest Interval (Years)",
         x = "Retest Correlation",
         tag = "B") + #Pearson correlation coefficient
    facet_wrap(.~measure_category, ncol = 1, scales= "fixed") +
    theme(panel.grid = element_blank(),
          plot.margin = margin(r = 10, l = 0, t = 0, b = 10),
          strip.text.x  = element_text(family = "Source Sans 3", face = "bold", size = 11, hjust = 0),
          legend.position = "none",
          plot.tag = element_text(family = "Source Sans 3", face = "bold", size = 12, hjust = .5),
          axis.text.x = element_text(family = "Source Sans 3", face = "bold", size = 10),
          axis.title.x = element_text(family = "Source Sans 3", face = "bold", size = 9,
                                      margin = margin(t = 5), color = "grey30"),
          axis.title.y =  element_text(family = "Source Sans 3",face = "bold",
                                       angle = 90,
                                       color = "grey30",
                                       size = 9,margin = margin(r = 5, l = 5)),
          axis.text.y = element_text(family = "Source Sans 3 Light", size = 10,
                                     vjust = c(rep(0.5,4), 1)),
          text =  element_text(family = "Source Sans 3"),
          title = element_text(family = "Source Sans 3"),
          panel.background = element_rect(color = "grey85", fill = NA, size = .4),
          panel.grid.major.x = element_line(color = "grey80", size = .4, linetype = "dotted")) +
    coord_flip() 
  
  
  
A






B <- dataA %>%
  mutate(dum = "") %>% 
  ggplot(aes(y = num_meas_year, x =time_diff_bin_fct, fill = measure_category)) +
  geom_col(color = "white",
           size = .25,
           position = "stack",
           alpha= 1,
           width = .9) +
  scale_y_continuous(expand = c(0,0), limits = c(0,225), breaks = seq(50,200,50)) +

  scale_x_discrete(labels = (c("<1",as.character(c(1:20)))),
                   expand = expansion(add = 0.1),
                   limits = (levels(unique(dataA$time_diff_bin_fct)))) +
  theme_minimal()+
  facet_grid(.~dum)+
  labs(y = "Number of Measures", x = "Retest Interval (Years)", fill = "", tag = "A") +
  scale_fill_manual(values = c_palette) +
  theme(plot.margin = margin(r = 10, l = 0, t = 0, b = 15),
        panel.grid = element_blank(),
        plot.tag = element_text(family = "Source Sans 3", face = "bold", size = 12, hjust = .5),
        axis.text.x = element_text(family = "Source Sans 3", face = "bold", size = 10, hjust = .5),
        axis.title.x = element_text(family = "Source Sans 3", face = "bold", size = 9,
                                    margin = margin(t = 5), color = "grey30"),
        axis.title.y =  element_text(family = "Source Sans 3",face = "bold", 
                                     angle = 90,
                                     color = "grey30",
                                     size = 9,margin = margin(r = 5, l = 5)),
        axis.text.y = element_text(family = "Source Sans 3 Light", size = 10),
        legend.text = element_text(family = "Source Sans 3",face = "bold", size = 10, hjust = .5),
        legend.position = c(.5,.95),
        legend.direction='horizontal',
        legend.spacing.x = unit(1, 'cm'),
        legend.spacing.y = unit(.1, 'cm'),
        legend.key.height =  unit(0.25,"cm"),
        legend.key.width =  unit(1,"cm"),
        panel.background = element_rect(color = "grey85", fill = NA, size = .4),
        panel.grid.major.y = element_line(color = "grey70", size = .1, linetype = "dashed")) +
  guides(fill = guide_legend(title.position = "top", label.position = "bottom", nrow = 1,
                             override.aes = list(alpha= 1)))

B


p <- plot_grid(B,A, nrow = 2, rel_heights = c(.5,1))

p

ggsave(plot = p, filename =paste0(output_path, "retest_meas_overview_fig.png"),
       dpi = 300, width = 22, height = 26, units = "cm") 

