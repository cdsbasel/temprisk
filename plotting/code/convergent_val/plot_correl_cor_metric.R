
# DESCRIPTION -------------------------------------------------------------

#Plotting association between different correlation coefficients (suppl. material)


# Author(s): Alexandra Bagaini, Centre for Cognitive and Decision Sciences, Faculty of Psychology, University of Basel.


# PACKAGES ----------------------------------------------------------------



library(tidyverse)
library(patchwork)
library(cowplot)



# FILES  ---------------------------------------------------

data_path <- c("processing/output/convergent_val/") # where is the input file stored
intercor_file <- "complete_intercor.csv" # name of merged data 
output_path <- c("plotting/output/convergent_val/") # where to store the output 

# READ DATA ---------------------------------------------------------------
col_spec <- cols(
  panel = col_character(),
  sample = col_character(),
  wave_id = col_character(),
  wave_year = col_double(),
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
  varcode_a = col_character(),
  varcode_b = col_character(),
  cor_pearson = col_double(),
  cor_spearman = col_double(),
  icc2_1 = col_double(),
  cor_pearson_log = col_double(),
  cor_spearman_log = col_double(),
  icc2_1_log = col_double(),
  coeff_var_a = col_double(),
  coeff_var_b = col_double(),
  skewness_a = col_double(),
  skewness_b = col_double(),
  measure_category_a = col_character(),
  general_domain_a = col_character(),
  domain_name_a = col_character(),
  scale_type_a = col_character(),
  scale_length_a = col_double(),
  time_frame_a = col_double(),
  behav_type_a = col_character(),
  behav_paid_a = col_character(),
  measure_category_b = col_character(),
  general_domain_b = col_character(),
  domain_name_b = col_character(),
  scale_type_b = col_character(),
  scale_length_b = col_double(),
  time_frame_b = col_double(),
  behav_type_b = col_character(),
  behav_paid_b = col_character(),
  continent = col_character(),
  country = col_character(),
  language = col_character(),
  data_collect_mode = col_character(),
  sample_type = col_character()
)

dat <-read_csv(paste0(data_path,intercor_file), col_types = col_spec)



# PREP. DATA ---------------------------------------------------------------


dat_fltr <-  dat %>%
  filter(age_group != "10-90" &
           gender_group != "all" & 
           year_age_group == 10 & 
           n >= 30)

col_c = "#38a3a5"



dat <- dat_fltr 


p_labels <-  dat %>% 
  summarise(labs = paste0("r = ", as.character(round(cor(cor_pearson, cor_spearman),3))),
            n = format(n(), big.mark = "'")) %>% 
  mutate(x = -.8,
         y = .8,
         y_n = -.9,
         x_n = .9,
         label_n = paste0("k: ", n),
         labs = str_remove(labs, "0+"))



p1 <- dat %>% 
  ggplot(aes(x = cor_pearson, y = cor_spearman)) +
  geom_point(alpha = .25, shape = 1, size = .5, color = col_c) +
  geom_text(data = p_labels, aes(x = x_n, y = y_n, label = label_n), color = "grey30", family = "Source Sans 3 Medium", fontface = "italic", size = 3, hjust = 1) +
  geom_text(data = p_labels, aes(x = x, y = y, label = labs), color = "grey30", family = "Source Sans 3", size = 4, hjust = 0) +
  geom_abline(intercept = 0, slope = 1, color = "grey30") +
  theme_minimal() +
  labs(x = "Pearson",
       y = "Spearman") +
  theme(text = element_text(family = "Source Sans 3", size = 8),
        panel.grid = element_blank(),
        title = element_text(family = "Source Sans 3", face = "bold", size = 8),
        strip.text.x  = element_text(family = "Source Sans 3", face = "bold",size = 9),
        legend.position = "none",
        panel.background = element_rect(fill = NA, color = "grey30")) +
  scale_x_continuous(limits = c(-1,1), breaks = seq(-1,1, .5)) +
  scale_y_continuous(limits = c(-1,1), breaks = seq(-1,1, .5)) 



p_labels <-  dat %>%
  summarise(labs = paste0("r = ", as.character(round(cor(cor_pearson, icc2_1),3)))) %>% 
  mutate(x = -.8,
         y = .8,
         labs = str_remove(labs, "0+"))


# warning about 30 "missing" ICC2,1 values, but there is no missing values in the data +
#  range is adequate (message disappears if we do not specify x and y ranges)
p2 <- dat %>% 
  ggplot(aes(x = cor_pearson, y = icc2_1)) +
  geom_point(alpha = .25, shape = 1, size = .5, color = col_c) +
  geom_text(data = p_labels, aes(x = x, y = y, label = labs), color = "grey30", family = "Source Sans 3", size = 4, hjust = 0) +
  geom_abline(intercept = 0, slope = 1, color = "grey30") +
  theme_minimal() +
  labs(x = "Pearson",
       y = "ICC(2,1)") +
  theme(text = element_text(family = "Source Sans 3", size = 8),
        panel.grid = element_blank(),
        title = element_text(family = "Source Sans 3", face = "bold", size = 8),
        strip.text.x = element_blank(),
        legend.position = "none",
        plot.margin = margin(r = 20,l = 20),
        panel.background = element_rect(fill = NA, color = "grey30")) +
  scale_x_continuous(limits = c(-1,1), breaks = seq(-1,1, .5)) +
  scale_y_continuous(limits = c(-1,1), breaks = seq(-1,1, .5)) 





p_labels <-  dat %>% 
  summarise(labs = paste0("r = ", as.character(round(cor(cor_spearman, icc2_1),3)))) %>% 
  mutate(x = -.8,
         y = .8,
         labs = str_remove(labs, "0+"))


# warning about 30 "missing" ICC2,1 values, but there is no missing values in the data +
#  range is adequate (message disappears if we do not specify x and y ranges)
p3 <- dat %>% 
  ggplot(aes(x = cor_spearman, y = icc2_1)) +
  geom_point(alpha = .25, shape = 1, size = .5, color = col_c) +
  geom_text(data = p_labels, aes(x = x, y = y, label = labs), color = "grey30", family = "Source Sans 3", size = 4, hjust = 0) +
  geom_abline(intercept = 0, slope = 1, color = "grey30") +
  theme_minimal() +
  labs(x = "Spearman",
       y = "ICC(2,1)") +
  theme(text = element_text(family = "Source Sans 3", size = 8),
        panel.grid = element_blank(),
        title = element_text(family = "Source Sans 3", face = "bold", size = 8),
        strip.text.x = element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill = NA, color = "grey30")) +
  scale_x_continuous(limits = c(-1,1), breaks = seq(-1,1, .5)) +
  scale_y_continuous(limits = c(-1,1), breaks = seq(-1,1, .5)) 




p <- p1+p2+p3 



p



# SAVE -----------------------------------------------------------------




ggsave(filename = paste0(output_path,"correl_metric_intercor_plot.png"), plot = p, height = 10, width = 30, units = "cm", dpi = 200)

