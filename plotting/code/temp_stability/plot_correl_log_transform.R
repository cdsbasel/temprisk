


# DESCRIPTION -------------------------------------------------------------

#Plotting association between different correlations computed using log-transformed and non-transformed data.
# Included in the suppl. material


# Author(s): Alexandra Bagaini, Centre for Cognitive and Decision Sciences, Faculty of Psychology, University of Basel.


# PACKAGES ----------------------------------------------------------------



library(tidyverse)
library(patchwork)
library(cowplot)

# FILES  ---------------------------------------------------

data_path <- c("processing/output/temp_stability/") # where is the input file stored
retest_file <- "complete_retest.csv" # name of merged data 
output_path <- c("plotting/output/temp_stability/") # where to store the output 
source("helper_functions.R")

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
  scale_type = col_character(),
  scale_length = col_double(),
  time_frame = col_double(),
  behav_type = col_character(),
  behav_paid = col_character(),
  continent = col_character(),
  country = col_character(),
  language = col_character(),
  data_collect_mode = col_character(),
  sample_type = col_character(),
  item_num = col_double()
)

dat <- read_csv(paste0(data_path,retest_file), col_types = col_spec)


# PREP. DATA ---------------------------------------------------------------


dat_fltr <-  dat %>%
  filter(age_group != "10-90" &
           gender_group != "all" & 
           year_age_group == 10 & 
           n >= 30)

c_palette <-  c("Propensity" = "#502a7a",
                "Frequency" = "#1e9bae",
                "Behaviour" ="#e07f00")




# PROPENSITY --------------------------------------------------------------
meas <- "pro"

dat <- dat_fltr %>% filter(measure_category == meas) %>% 
  rowwise() %>% 
  mutate(domain_name = rename_domain(domain_name)) %>% 
  ungroup()



col_c <-  case_when(meas == "pro" ~ "#502a7a",
                    meas =="fre" ~ "#1e9bae",
                    meas == "beh" ~"#e07f00")




p_labels <-  dat %>% group_by(domain_name) %>%  
  summarise(labs = paste0("r = ", as.character(round(cor(cor_pearson, cor_pearson_log),3))),
            n = format(n(), big.mark = "'")) %>% 
  mutate(x = -.8,
         y = .8,
         y_n = -.9,
         x_n = .9,
         label_n = paste0("k: ", n),
         labs = str_remove(labs, "0+"))



p1 <- dat %>% 
  ggplot(aes(x = cor_pearson, y = cor_pearson_log)) +
  geom_point(alpha = .25, shape = 1, size = .5, color = col_c) +
  geom_text(data = p_labels, aes(x = x_n, y = y_n, label = label_n), color = "grey30", family = "Source Sans 3 Medium", fontface = "italic", size = 3, hjust = 1) +
  geom_text(data = p_labels, aes(x = x, y = y, label = labs), color = "grey30", family = "Source Sans 3", size = 3, hjust = 0) +
  geom_abline(intercept = 0, slope = 1, color = "grey30") +
  theme_minimal() +
  labs(x = "Pearson",
       y = "Pearson (log-transf. data)") +
  scale_color_manual(values = c_palette) +
  theme(text = element_text(family = "Source Sans 3", size = 8),
        panel.grid = element_blank(),
        title = element_text(family = "Source Sans 3", face = "bold", size = 8),
        strip.text.x  = element_text(family = "Source Sans 3", face = "bold",size = 9),
        legend.position = "none",
        plot.margin = margin(0),
        panel.background = element_rect(fill = NA, color = "grey30")) +
  scale_x_continuous(limits = c(-1,1), breaks = seq(-1,1, .5)) +
  scale_y_continuous(limits = c(-1,1), breaks = seq(-1,1, .5)) +
  facet_wrap(.~ domain_name, nrow = 1)



p_labels <-  dat %>% group_by(domain_name) %>%  
  summarise(labs = paste0("r = ", as.character(round(cor(icc2_1_log, icc2_1),3)))) %>% 
  mutate(x = -.8,
         y = .8,
         labs = str_remove(labs, "0+"))



p2 <- dat %>% 
  ggplot(aes(x = icc2_1, y = icc2_1_log)) +
  geom_point(alpha = .25, shape = 1, size = .5, color = col_c) +
  geom_text(data = p_labels, aes(x = x, y = y, label = labs), color = "grey30", family = "Source Sans 3", size = 3, hjust = 0) +
  geom_abline(intercept = 0, slope = 1, color = "grey30") +
  theme_minimal() +
  labs(x = "ICC(2,1)",
       y = "ICC(2,1) (log-transf. data)") +
  scale_color_manual(values = c_palette) +
  theme(text = element_text(family = "Source Sans 3", size = 8),
        panel.grid = element_blank(),
        title = element_text(family = "Source Sans 3", face = "bold", size = 8),
        strip.text.x = element_blank(),
        legend.position = "none",
        plot.margin = margin(t = 20),
        panel.background = element_rect(fill = NA, color = "grey30")) +
  scale_x_continuous(limits = c(-1,1), breaks = seq(-1,1, .5)) +
  scale_y_continuous(limits = c(-1,1), breaks = seq(-1,1, .5)) +
  facet_wrap(.~ domain_name, nrow = 1)





p_labels <-  dat %>% group_by(domain_name) %>%  
  summarise(labs = paste0("r = ", as.character(round(cor(cor_spearman, cor_spearman_log),3)))) %>% 
  mutate(x = -.8,
         y = .8,
         labs = str_remove(labs, "0+"))



p3 <- dat %>% 
  ggplot(aes(x = cor_spearman, y = cor_spearman_log)) +
  geom_point(alpha = .25, shape = 1, size = .5, color = col_c) +
  geom_text(data = p_labels, aes(x = x, y = y, label = labs), color = "grey30", family = "Source Sans 3", size = 3, hjust = 0) +
  geom_abline(intercept = 0, slope = 1, color = "grey30") +
  theme_minimal() +
  labs(x = "Spearman",
       y = "Spearman (log-transf. data)") +
  scale_color_manual(values = c_palette) +
  theme(text = element_text(family = "Source Sans 3", size = 8),
        panel.grid = element_blank(),
        title = element_text(family = "Source Sans 3", face = "bold", size = 8),
        strip.text.x = element_blank(),
        legend.position = "none",
        plot.margin = margin(t = 20),
        panel.background = element_rect(fill = NA, color = "grey30")) +
  scale_x_continuous(limits = c(-1,1), breaks = seq(-1,1, .5)) +
  scale_y_continuous(limits = c(-1,1), breaks = seq(-1,1, .5)) +
  facet_wrap(.~ domain_name, nrow = 1)




pp <- p1/p2/p3 + plot_annotation(title = 'Propensity',
                                 theme = theme(plot.title = element_text(size = 13, family = "Source Sans 3",
                                                                         face = "bold")))



pp





# FREQUENCY --------------------------------------------------------------
meas <- "fre"

dat <- dat_fltr %>% filter(measure_category == meas)%>% 
  rowwise() %>% 
  mutate(domain_name = rename_domain(domain_name)) %>% 
  ungroup()



col_c <-  case_when(meas == "pro" ~ "#502a7a",
                    meas =="fre" ~ "#1e9bae",
                    meas == "beh" ~"#e07f00")






p_labels <-  dat %>% group_by(domain_name) %>%  
  summarise(labs = paste0("r = ", as.character(round(cor(cor_pearson, cor_pearson_log),3))),
            n = format(n(), big.mark = "'")) %>% 
  mutate(x = -.8,
         y = .8,
         y_n = -.9,
         x_n = .9,
         label_n = paste0("k: ", n),
         labs = str_remove(labs, "0+"))



p1 <- dat %>% 
  ggplot(aes(x = cor_pearson, y = cor_pearson_log)) +
  geom_point(alpha = .25, shape = 1, size = .5, color = col_c) +
  geom_text(data = p_labels, aes(x = x_n, y = y_n, label = label_n), color = "grey30", family = "Source Sans 3 Medium", fontface = "italic", size = 3, hjust = 1) +
  geom_text(data = p_labels, aes(x = x, y = y, label = labs), color = "grey30", family = "Source Sans 3", size = 3, hjust = 0) +
  geom_abline(intercept = 0, slope = 1, color = "grey30") +
  theme_minimal() +
  labs(x = "Pearson",
       y = "Pearson (log-transf. data)") +
  scale_color_manual(values = c_palette) +
  theme(text = element_text(family = "Source Sans 3", size = 8),
        panel.grid = element_blank(),
        title = element_text(family = "Source Sans 3", face = "bold", size = 8),
        strip.text.x  = element_text(family = "Source Sans 3", face = "bold",size = 9),
        legend.position = "none",
        plot.margin = margin(l = 20, r = 0, b = 0, t = 0),
        panel.background = element_rect(fill = NA, color = "grey30")) +
  scale_x_continuous(limits = c(-1,1), breaks = seq(-1,1, .5)) +
  scale_y_continuous(limits = c(-1,1), breaks = seq(-1,1, .5)) +
  facet_wrap(.~ domain_name, nrow = 1)



p_labels <-  dat %>% group_by(domain_name) %>%  
  summarise(labs = paste0("r = ", as.character(round(cor(icc2_1, icc2_1_log),3)))) %>% 
  mutate(x = -.8,
         y = .8,
         labs = str_remove(labs, "0+"))



p2 <- dat %>% 
  ggplot(aes(x = icc2_1, y = icc2_1_log)) +
  geom_point(alpha = .25, shape = 1, size = .5, color = col_c) +
  geom_text(data = p_labels, aes(x = x, y = y, label = labs), color = "grey30", family = "Source Sans 3", size = 3, hjust = 0) +
  geom_abline(intercept = 0, slope = 1, color = "grey30") +
  theme_minimal() +
  labs(x = "ICC(2,1)",
       y = "ICC(2,1) (log-transf. data)") +
  scale_color_manual(values = c_palette) +
  theme(text = element_text(family = "Source Sans 3", size = 8),
        panel.grid = element_blank(),
        title = element_text(family = "Source Sans 3", face = "bold", size = 8),
        strip.text.x = element_blank(),
        legend.position = "none",
        plot.margin = margin(l = 20, r = 0, b = 0, t = 20),
        panel.background = element_rect(fill = NA, color = "grey30")) +
  scale_x_continuous(limits = c(-1,1), breaks = seq(-1,1, .5)) +
  scale_y_continuous(limits = c(-1,1), breaks = seq(-1,1, .5)) +
  facet_wrap(.~ domain_name, nrow = 1)





p_labels <-  dat %>% group_by(domain_name) %>%  
  summarise(labs = paste0("r = ", as.character(round(cor(cor_spearman, cor_spearman_log),3)))) %>% 
  mutate(x = -.8,
         y = .8,
         labs = str_remove(labs, "0+"))



p3 <- dat %>% 
  ggplot(aes(x = cor_spearman, y = cor_spearman_log)) +
  geom_point(alpha = .25, shape = 1, size = .5, color = col_c) +
  geom_text(data = p_labels, aes(x = x, y = y, label = labs), color = "grey30", family = "Source Sans 3", size = 3, hjust = 0) +
  geom_abline(intercept = 0, slope = 1, color = "grey30") +
  theme_minimal() +
  labs(x = "Spearman",
       y = "Spearman (log-transf. data)") +
  scale_color_manual(values = c_palette) +
  theme(text = element_text(family = "Source Sans 3", size = 8),
        panel.grid = element_blank(),
        title = element_text(family = "Source Sans 3", face = "bold", size = 8),
        strip.text.x = element_blank(),
        legend.position = "none",
        plot.margin = margin(l = 20, r = 0, b = 0, t = 20),
        panel.background = element_rect(fill = NA, color = "grey30")) +
  scale_x_continuous(limits = c(-1,1), breaks = seq(-1,1, .5)) +
  scale_y_continuous(limits = c(-1,1), breaks = seq(-1,1, .5)) +
  facet_wrap(.~ domain_name, nrow = 1)



pf <- p1/p2/p3 + plot_annotation(title = 'Frequency',
                                 theme = theme(plot.title = element_text(size = 13, family = "Source Sans 3",
                                                                         face = "bold")))


# warning about  108 "missing" ICC2,1 values, but there is no missing values in the data +
#  range is adequate (message disappears if we do not specify x and y ranges)
pf






# BEHAVIOUR --------------------------------------------------------------
meas <- "beh"

dat <- dat_fltr %>% filter(measure_category == meas) %>% 
  rowwise() %>% 
  mutate(domain_name = rename_domain(domain_name)) %>% 
  ungroup()


col_c <-  case_when(meas == "pro" ~ "#502a7a",
                    meas =="fre" ~ "#1e9bae",
                    meas == "beh" ~"#e07f00")



p_labels <-  dat %>% group_by(domain_name) %>%  
  summarise(labs = paste0("r = ", as.character(round(cor(cor_pearson, cor_pearson_log),3))),
            n = format(n(), big.mark = "'")) %>% 
  mutate(x = -.8,
         y = .8,
         y_n = -.9,
         x_n = .9,
         label_n = paste0("k: ", n),
         labs = str_remove(labs, "0+"))



p1 <- dat %>% 
  ggplot(aes(x = cor_pearson, y = cor_pearson_log)) +
  geom_point(alpha = .25, shape = 1, size = .5, color = col_c) +
  geom_text(data = p_labels, aes(x = x_n, y = y_n, label = label_n), color = "grey30", family = "Source Sans 3 Medium", fontface = "italic", size = 3, hjust = 1) +
  geom_text(data = p_labels, aes(x = x, y = y, label = labs), color = "grey30", family = "Source Sans 3", size = 3, hjust = 0) +
  geom_abline(intercept = 0, slope = 1, color = "grey30") +
  theme_minimal() +
  labs(x = "Pearson",
       y = "Pearson (log-transf. data)") +
  scale_color_manual(values = c_palette) +
  theme(text = element_text(family = "Source Sans 3", size = 8),
        panel.grid = element_blank(),
        title = element_text(family = "Source Sans 3", face = "bold", size = 8),
        strip.text.x  = element_text(family = "Source Sans 3", face = "bold",size = 9),
        legend.position = "none",
        plot.margin = margin(l = 20, r = 0, b = 0, t = 0),
        panel.background = element_rect(fill = NA, color = "grey30")) +
  scale_x_continuous(limits = c(-1,1), breaks = seq(-1,1, .5)) +
  scale_y_continuous(limits = c(-1,1), breaks = seq(-1,1, .5)) +
  facet_wrap(.~ domain_name, nrow = 1)



p_labels <-  dat %>% group_by(domain_name) %>%  
  summarise(labs = paste0("r = ", as.character(round(cor(icc2_1, icc2_1_log),3)))) %>% 
  mutate(x = -.8,
         y = .8,
         labs = str_remove(labs, "0+"))



p2 <- dat %>% 
  ggplot(aes(x = icc2_1, y = icc2_1_log)) +
  geom_point(alpha = .25, shape = 1, size = .5, color = col_c) +
  geom_text(data = p_labels, aes(x = x, y = y, label = labs), color = "grey30", family = "Source Sans 3", size = 3, hjust = 0) +
  geom_abline(intercept = 0, slope = 1, color = "grey30") +
  theme_minimal() +
  labs(x = "ICC(2,1)",
       y = "ICC(2,1) (log-transf. data)") +
  scale_color_manual(values = c_palette) +
  theme(text = element_text(family = "Source Sans 3", size = 8),
        panel.grid = element_blank(),
        title = element_text(family = "Source Sans 3", face = "bold", size = 8),
        strip.text.x = element_blank(),
        legend.position = "none",
        plot.margin = margin(l = 20, r = 0, b = 0, t = 20),
        panel.background = element_rect(fill = NA, color = "grey30")) +
  scale_x_continuous(limits = c(-1,1), breaks = seq(-1,1, .5)) +
  scale_y_continuous(limits = c(-1,1), breaks = seq(-1,1, .5)) +
  facet_wrap(.~ domain_name, nrow = 1)





p_labels <-  dat %>% group_by(domain_name) %>%  
  summarise(labs = paste0("r = ", as.character(round(cor(cor_spearman, cor_spearman_log),3)))) %>% 
  mutate(x = -.8,
         y = .8,
         labs = str_remove(labs, "0+"))



p3 <- dat %>% 
  ggplot(aes(x = cor_spearman, y = cor_spearman_log)) +
  geom_point(alpha = .25, shape = 1, size = .5, color = col_c) +
  geom_text(data = p_labels, aes(x = x, y = y, label = labs), color = "grey30", family = "Source Sans 3", size = 3, hjust = 0) +
  geom_abline(intercept = 0, slope = 1, color = "grey30") +
  theme_minimal() +
  labs(x = "Spearman",
       y = "Spearman (log-transf. data)") +
  scale_color_manual(values = c_palette) +
  theme(text = element_text(family = "Source Sans 3", size = 8),
        panel.grid = element_blank(),
        title = element_text(family = "Source Sans 3", face = "bold", size = 8),
        strip.text.x = element_blank(),
        legend.position = "none",
        plot.margin = margin(l = 20, r = 0, b = 0, t = 20),
        panel.background = element_rect(fill = NA, color = "grey30")) +
  scale_x_continuous(limits = c(-1,1), breaks = seq(-1,1, .5)) +
  scale_y_continuous(limits = c(-1,1), breaks = seq(-1,1, .5)) +
  facet_wrap(.~ domain_name, nrow = 1)




pb <- p1/p2/p3 + plot_annotation(title = 'Behaviour',
                                 theme = theme(plot.title = element_text(size = 13, family = "Source Sans 3",
                                                                         face = "bold")))



pb



# p <- pp/pf/pb

# COMBINE -----------------------------------------------------------------


p <- plot_grid(pp, pf, pb, nrow = 1, rel_widths = c(9,8,4))



ggsave(filename = paste0(output_path,"log_metric_plot.png"), plot = p, height = 15, width = 60, units = "cm", dpi = 200)

