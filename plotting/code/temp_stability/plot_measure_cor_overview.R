# DESCRIPTION -------------------------------------------------------------

# Plotting overview of the dataset
# A) Retest correlations as a function of retest interval
# B) Inter-correlation distribution
# C) Number of measures split by ctageory and domain


# Author(s): Alexandra Bagaini, Centre for Cognitive and Decision Sciences, Faculty of Psychology, University of Basel.



# PACKAGES ---------------------------------------------------------------

library(tidyverse)
library(patchwork)
library(ggh4x)
library(ggtext)

# FILES  ---------------------------------------------------

data_path <- c("processing/output/temp_stability/") # where is the input file stored
retest_file <- "complete_retest.csv" # name of merged retest data 
output_path <- c("plotting/output/temp_stability/") # where to store the output 

data_path_inter <- c("processing/output/convergent_val/") # where is the input file stored
intercor_file <- "complete_intercor.csv" # name of merged retest data 



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
  sample_type = col_character()
)

dat <- read_csv(paste0(data_path,retest_file), col_types = col_spec)


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

dat_inter <-read_csv(paste0(data_path_inter,intercor_file), col_types = col_spec)


# PREP. DATA ---------------------------------------------------------------


dtC <-  dat %>%
  filter(age_group != "10-90" &
           gender_group != "all" & 
           year_age_group == 10 & 
           n >= 30) %>% 
  mutate(measure_category = case_when(measure_category == "fre"  ~ "Frequency",
                                      measure_category == "beh" ~ "Behaviour",
                                      measure_category == "pro" ~ "Propensity"
  ))  %>% 
  distinct( varcode, domain_name, measure_category) %>% 
  group_by(domain_name, measure_category) %>% 
  summarise(num_meas_dom = n()) %>% 
  rowwise() %>% 
  mutate(domain_name = rename_domain(domain_name)) %>% 
  mutate(domain_meas = paste0(measure_category, "-",domain_name)) %>% 
  ungroup() %>% 
  mutate(col = case_when(grepl("Frequency", domain_meas) ~ "#1e9bae",
                         grepl("Propensity", domain_meas)  ~ "#502a7a",
                         grepl("Behaviour", domain_meas) ~"#e07f00"),
         meas_categ = case_when(grepl("Frequency", domain_meas) ~ "Frequency",
                                grepl("Propensity", domain_meas)  ~ "Propensity",
                                grepl("Behaviour", domain_meas) ~"Behaviour"),
         meas_categ_ord = case_when(grepl("Frequency", domain_meas) ~ 2,
                                    grepl("Propensity", domain_meas)  ~ 1,
                                    grepl("Behaviour", domain_meas) ~3),
         # label = ifelse(domain_meas == "Propensity-Financial", "Number of Measures", NA_character_)
         ) %>% 
  rowwise() %>% 
  mutate(dom_name = unlist(strsplit(domain_meas, "-"))[2])

dtC$dom_name <- paste0("<span style=\"color: ", dtC$col, "\">", dtC$dom_name, "</span>")
dtC$meas_categ <- paste0("<span style=\"color: ", dtC$col, "\">", dtC$meas_categ, "</span>")

backgrounds <- list(element_rect(fill = "dodgerblue"), element_rect(fill = "dodgerblue"))
strip <- strip_themed(background_x = elem_list_rect(fill = rainbow(7)))

dfrt  <-  tibble(x = 1,
                 y = 5,
                 label = "Number of Unique Measures")




dtA <-  dat %>%
  mutate(cor_pearson = if_else(cor_pearson < 0, 0, cor_pearson)) %>% # set negative retest correlations to 0 (see also Enkavi et al., 2019, PNAS)
  filter(age_group != "10-90" &
           gender_group != "all" & 
           year_age_group == 10 & 
           n >= 30) 


dtB <- dat_inter %>%
  filter(age_group != "10-90" &
           gender_group != "all" & 
           year_age_group == 10 & 
           n >= 30)


# PLOTTING MEASURE COUNT --------------------------------------------------

pC <- dtC %>% 
  ggplot(aes(y = num_meas_dom, x = reorder(dom_name,-num_meas_dom),  fill = col, color = col)) +
  geom_bar(stat = "identity", alpha = .1, linewidth = .25) +
  geom_text(aes(label = as.character(num_meas_dom, color = col)), 
            fontface = "bold", size = 3.25,
            family = "Source Sans 3", vjust = -0.5) +
  geom_segment(aes(x =reorder(dom_name,-num_meas_dom) , y = num_meas_dom + 5, xend = reorder(dom_name,-num_meas_dom), yend = Inf, colour = col), 
               alpha = .5, size = .1) +
  theme_minimal()+
  scale_color_identity() +
  scale_fill_identity() +
  scale_x_discrete(position = "top") +
  # scale_y_continuous(expand = c(0,0), limits = c(0,52))+
  facet_grid2(.~reorder(meas_categ,meas_categ_ord),scales = "free_x", space = "free", switch = "y",
              strip = strip_themed(background_x = elem_list_rect(color = c("#502a7a",  "#1e9bae","#e07f00"),
                                                                 fill = alpha( c("#502a7a",  "#1e9bae","#e07f00"),.1)))) +
  theme(title = element_text(family = "Source Sans 3"),
        strip.text = element_markdown(family = "Source Sans 3", face = "bold", size = 11.5),
        strip.placement = "outside",
        plot.tag = element_text(family = "Source Sans 3", face = "bold", size = 11),
        legend.position = "none",
        axis.title =  element_text(family = "Source Sans 3 Light", size = 10, hjust = 0.1, color = "grey20"),
        axis.text.y  = element_blank(),
        plot.margin = margin(r = 20, t = 5, l = 5, b = 15),
        axis.text.x.top = element_markdown(angle = 45, hjust = 0,
                                           margin = margin(t = 5),
                                           vjust = 0, family = "Source Sans 3",
                                           face = "bold", size = 9.5),
        legend.text = element_text(size = 10, face = "bold"),
        legend.title.align=0.5,
        panel.background = element_rect(fill = NA, color = "NA", linewidth = .5),
        legend.margin = margin(0),
        panel.grid = element_blank(),
        panel.spacing.x= unit(0.75, 'cm'),
        legend.key.size = unit(0.3,"line")
  ) +
  coord_cartesian(clip = "off") +
  labs(
    tag = "C",
    y = "Number of Unique Measures",
    x = "",
    fill = "",
  ) 


pC



# PLOTTING RETEST DENSITY ---------------------------------------------------------------

pA <- dtA %>% 
  ggplot(aes(x = time_diff_mean, y = cor_pearson))+
  stat_density_2d(geom = "point", aes(size =after_stat(density)), n = 100, contour = FALSE, 
                  color = "grey70", fill = "grey40",   ##11693F
                  shape = 21, stroke = .25, alpha = .2) +
  theme_minimal() +
  scale_x_continuous(expand = c(0.0,0.0)) +
  scale_y_continuous(expand = c(0.0,0.0)) +
  theme(legend.position = "none",
        axis.line.x = element_line(size = .35, color = "grey20"),
        axis.line.y = element_line(size = .35, color = "grey20"),
        # panel.background = element_rect(color = "grey50", size = .25),
        text = element_text(family = "Source Sans 3", size = 9, color = "grey40"),
        axis.text.y = element_text( vjust=seq(0,1, length.out = 5)),
        axis.text.x = element_text( hjust=c(0,1)),
        plot.margin = margin(r = 30, b = 30),
        plot.tag = element_text(family = "Source Sans 3", face = "bold", size = 11),
        title = element_text(family = "Source Sans 3 Light", size = 9, color = "grey20"),
        panel.grid = element_blank()) +
  scale_size(range = c(.00,10)) +
  scale_alpha(range = c(0.1,.8)) +
  labs(y = "Retest Correlation", x = "Retest Interval (Years)", tag = "A")


pA



# PLOTTING INTERCOR DISTRIB -----------------------------------------------


pB <-  ggplot() +
  geom_vline(xintercept = 0, linetype = "solid", color = "grey40", size = .3) +
  geom_density(data = dtB, aes(x = cor_spearman),
               color = "grey40", fill = "grey40",##0F4C5C
               alpha = .2, size = .25) +
  theme_minimal()+
  scale_x_continuous(limits = c(-1,1), breaks = seq(-1,1,.25)) +
  labs(x = "Spearman's rho", tag = "B")+
  theme(panel.spacing.x = unit(1, "cm"),
        plot.tag = element_text(family = "Source Sans 3", face = "bold", size = 11),
        strip.text.x  = element_blank(),
        # strip.text.x  = element_text(family = "Source Sans 3", face = "bold", size = 11, color = "white"),
        axis.text.x = element_text(family = "Source Sans 3", size = 9),
        axis.title.x = element_text(family = "Source Sans 3 Light",  size = 9),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = margin(l = 30, b = 30),
        text =  element_text(family = "Source Sans 3"),
        title = element_text(family = "Source Sans 3"),
        panel.background = element_rect(color = "NA", fill = NA, size = .4),
        panel.grid = element_blank())

pB




# COMBINE & SAVE ----------------------------------------------------------




p <- (pA + pB)/pC + plot_layout(heights = c(1,1))




p



ggsave(plot = p,
       filename = paste0(output_path, "meas_count.png"), 
       dpi = 300, width = 30, height = 22, units = "cm") 
