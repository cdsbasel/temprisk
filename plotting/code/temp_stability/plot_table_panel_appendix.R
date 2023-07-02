
# DESCRIPTION -------------------------------------------------------------


# Overview of the avaialbe data (measures and domains) for each sample included in the analysis

# Author(s): Alexandra Bagaini, Centre for Cognitive and Decision Sciences, Faculty of Psychology, University of Basel.



# PACKAGES ----------------------------------------------------------------


library(tidyverse)
library(ggtext)
library(ggh4x)
library(patchwork)

# FILES  ---------------------------------------------------

data_path <- c("processing/output/temp_stability/") # where is the input file stored
retest_file <- "complete_retest.csv" # name of merged retest data 
output_path <- c("companion_site/images/") # where to store the output 
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


# PREP. DATA ---------------------------------------------------------------


dt <-  dat %>%
  filter(age_group != "10-90" &
           gender_group != "all" & 
           year_age_group == 10 & 
           n >= 30)

dt_sum <- dt %>% 
  mutate(domain_name = rename_domain(domain_name),
         measure_category = case_when(measure_category == "fre"  ~"Frequency",
                                      measure_category == "beh" ~ "Behaviour",
                                      measure_category == "pro" ~ "Propensity"),
         domain_meas = paste0(measure_category, "-",domain_name)) %>%
  group_by(sample) %>% 
  summarise(item_list = list(c(unique(domain_meas)))) %>% 
  ungroup() 


dt_w <- dt_sum %>% unnest(cols = c(item_list)) %>% mutate(incl = 1) %>% pivot_wider(names_from = "item_list", values_from = "incl", values_fill = NA)  
dt_l <- dt_w %>% pivot_longer(-sample, names_to = "item_list", values_to = "incl") %>% 
  mutate(col = case_when(grepl("Frequency", item_list) ~ "#1e9bae",
                         grepl("Propensity", item_list)  ~ "#502a7a",
                         grepl("Behaviour", item_list) ~"#e07f00"),
         meas_categ = case_when(grepl("Frequency", item_list) ~ "Frequency",
                                grepl("Propensity", item_list)  ~ "Propensity",
                                grepl("Behaviour", item_list) ~"Behaviour"),
         meas_categ_ord = case_when(grepl("Frequency", item_list) ~ 2,
                                    grepl("Propensity", item_list)  ~ 1,
                                    grepl("Behaviour", item_list) ~3)) %>% 
  rowwise() %>% 
  mutate(dom_name = unlist(strsplit(item_list, "-"))[2],
         sample = gsub("_", "-", sample)) %>% 
  rename(domain_meas = item_list)

dt_l$dom_name <- paste0("<span style=\"color: ", dt_l$col, "\">", dt_l$dom_name, "</span>")
dt_l$meas_categ <- paste0("<span style=\"color: ", dt_l$col, "\">", dt_l$meas_categ, "</span>")

p <- dt_l %>% ggplot() + geom_tile(aes(y = reorder(sample, desc(sample)),
                                        x = (dom_name),  color = col, fill = col),  alpha = .1, size = .25) +
  geom_point(aes(y = sample, x = dom_name, size = (incl), color = col)) + 
  scale_size_identity() + 
  scale_fill_identity() +
  scale_color_identity() +
  scale_x_discrete(position = "top", expand = c(0,0)) +
  coord_cartesian(clip = "off") +
  theme_minimal()+
  facet_grid2(.~reorder(meas_categ,meas_categ_ord),scales = "free_x", space = "free", switch = "y",
              strip = strip_themed(background_x = elem_list_rect(color = c("#502a7a",  "#1e9bae","#e07f00"),
                                                                 fill = alpha( c("#502a7a",  "#1e9bae","#e07f00"),.1)))) +
  # facet_grid(.~reorder(meas_categ,meas_categ_ord),scales = "free_x", space = "free", switch = "x") +
  theme(axis.text.y  = element_text(family = "Source Sans 3", color = "black", face = "bold"),
        axis.text.x.top  = element_markdown(family = "Source Sans 3", color = "black", face = "bold", angle = 45, hjust = 0),
        plot.tag = element_text(family = "Source Sans 3", face = "bold", size = 12, hjust = .5),
        title = element_text(family = "Source Sans 3"),
        strip.text = element_markdown(family = "Source Sans 3", face = "bold", size = 11.5),
        strip.placement = "outside",
        plot.margin = margin(r = 15, t = 15, l = 0, b = 0),
        panel.grid = element_blank(),
        axis.title = element_blank())


p


ggsave(plot = p, filename = paste0(output_path, "panel_domain_table.png"), dpi = 300, width = 25, height = 27, units = "cm") 


