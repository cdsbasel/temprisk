

# DESCRIPTION -------------------------------------------------------------

# Script to plot the distribution of the number of items for the measures in each cattegory

# Author(s): Alexandra Bagaini, Centre for Cognitive and Decision Sciences, Faculty of Psychology, University of Basel.




# PACKAGES ----------------------------------------------------------------




library(tidyverse)
library(readxl)
library(tidyverse)
library(ggh4x)
library(ggtext)


# FILES & DATA ---------------------------------------------------

data_path <- c("processing/output/temp_stability/") # where is the input file stored
retest_file <- "complete_retest.csv" # name of merged retest data 
output_path <- c("plotting/output/temp_stability/") # where to store the output 


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


dat_fltr <-  dat %>%
  filter(age_group != "10-90" &
           gender_group != "all" & 
           year_age_group == 10 & 
           n >= 30) 

dat_fltr <- dat_fltr %>% distinct(varcode,item_num, measure_category) %>% 
  group_by(item_num, measure_category) %>% 
  summarise(num_meas = n()) %>% 
  mutate(measure_category = case_when(measure_category == "pro" ~"Propensity",
         measure_category == "fre" ~"Frequency",
         measure_category == "beh" ~"Behaviour"),
         num_item_chr = case_when(item_num > 1 ~ paste0(as.character(item_num), " items"),
                                  item_num == 1 ~ paste0(as.character(item_num), " item")))

dat_fltr$measure_category <- factor(dat_fltr$measure_category, levels = c("Propensity", "Frequency", "Behaviour"))


# PLOTTING ----------------------------------------------------------------



p <- dat_fltr %>% ggplot(aes(x = reorder(num_item_chr, item_num), fill = measure_category, color = measure_category, y = num_meas)) + 
  geom_bar(stat = "identity",alpha = .1, linewidth = .25) +
  geom_text(aes(label = as.character(num_meas, color = col)), 
            fontface = "bold", size = 3.25,
            family = "Source Sans 3", vjust = -0.5) +
  labs(x = "Num. of items per measure") +
  facet_grid2(.~measure_category,scales = "free_x", space = "free", switch = "y",
              strip = strip_themed(background_x = elem_list_rect(color = c("#502a7a",  "#1e9bae","#e07f00"),
                                                                 fill = alpha( c("#502a7a",  "#1e9bae","#e07f00"),.1)))) +
  # facet_grid(.~measure_category, scales = "free_x", space = "free") + 
  theme_minimal() +
  theme(legend.position = "none") +  scale_fill_manual("", 
                                                       breaks = c("Propensity", "Frequency", "Behaviour"),
                                                       values = c("#502a7a", "#1e9bae","#e07f00"), 
                                                       labels = c("Propensity", "Frequency", "Behaviour")) +
  scale_color_manual("", 
                     breaks = c("Propensity", "Frequency", "Behaviour"),
                     values = c("#502a7a", "#1e9bae","#e07f00"), 
                     labels = c("Propensity", "Frequency", "Behaviour")) +
  scale_y_continuous(expand = c(0,0,0.1,0.25)) + theme(panel.spacing.y = unit(2,"cm")) +
  theme(title = element_text(family = "Source Sans 3"),
        axis.text.x =  element_text(family = "Source Sans 3",  size = 9),
        strip.text = element_markdown(family = "Source Sans 3", face = "bold", size = 11.5),
        strip.placement = "outside",
        plot.tag = element_text(family = "Source Sans 3", face = "bold", size = 11),
        legend.position = "none",
        axis.title =  element_text(family = "Source Sans 3", size = 10, hjust = 0.1, color = "black"),
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
    y = "Number of Unique Measures",
    x = "",
    fill = "",
  ) 


p



# SAVE --------------------------------------------------------------------



ggsave(plot = p,
       filename = paste0(output_path, "item_count_fig.png"), 
       dpi = 300, width = 35, height = 12, units = "cm") 
