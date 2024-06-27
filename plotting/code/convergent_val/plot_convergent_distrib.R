# DESCRIPTION -------------------------------------------------------------

#Plotting distributiuons of inter-correlations, split by category-domains and measure categories  (suppl. material)


# Author(s): Alexandra Bagaini, Centre for Cognitive and Decision Sciences, Faculty of Psychology, University of Basel.


# LIBRARIES ---------------------------------------------------------------

library(tidyverse)
library(modi)
library(matrixStats)
library(ggtext)
library(glue)
library(ggdist)
library(tidybayes)
library(patchwork)

source("helper_functions.R") 

# FILES  ---------------------------------------------------

data_path <- c("processing/output/convergent_val/") # where is the input file stored
intercor_file <- "complete_intercor.csv" # name of merged retest data 
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
dt <- dat %>%
  filter(age_group != "10-90" &
           gender_group != "all" & 
           year_age_group == 10 & 
           n >= 30) %>% 
  # creating labels
  mutate(meas_pair_lbl = measure_pair_lbl(m1 = measure_category_a, m2 = measure_category_b),
         name_a_lbl = name_lbl(measure = measure_category_a, domain = domain_name_a),
         name_b_lbl = name_lbl(measure = measure_category_b, domain = domain_name_b),
         name_pair_lbl = paste0(name_a_lbl, "_", name_b_lbl))



# renaming & harmonizing some pairs to be consistent with prev. analyses
labels_harmo_name <- dt %>% 
  mutate(temp_lbl_name = str_arrange(name_pair_lbl)) %>% 
  distinct(temp_lbl_name,name_pair_lbl) %>% 
  group_by(temp_lbl_name) %>% 
  slice(1) %>% 
  rename(harmonized_pair_name = name_pair_lbl)

labels_harmo_meas <- dt %>% 
  mutate(temp_lbl_meas = str_arrange(meas_pair_lbl)) %>% 
  distinct(temp_lbl_meas,meas_pair_lbl) %>% 
  group_by(temp_lbl_meas) %>% 
  slice(1) %>% 
  rename(harmonized_pair_meas = meas_pair_lbl)



dt <- dt %>% 
  mutate(temp_lbl_name = str_arrange(name_pair_lbl)) %>% 
  mutate(temp_lbl_meas = str_arrange(meas_pair_lbl)) %>% 
  left_join(labels_harmo_name, by = "temp_lbl_name") %>% 
  left_join(labels_harmo_meas, by = "temp_lbl_meas") %>% 
  select(-c(temp_lbl_name, temp_lbl_meas)) %>%
  group_by(harmonized_pair_name) %>% 
  mutate(name_pair_id = as.character(cur_group_id())) %>% 
  ungroup()  %>% 
  group_by(harmonized_pair_meas) %>% 
  mutate(meas_pair_id = as.character(cur_group_id())) %>% 
  ungroup()   
sort(unique(dt$harmonized_pair_meas))
sort(unique(dt$harmonized_pair_name))




# GROUP BY DOMAIN DATA ---------------------------------------------------

summary_point <-  dt %>% 
  group_by(harmonized_pair_name) %>%
  median_qi(cor_spearman, .width = c(0.95,0.5,0.8)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = .width, values_from = c(.lower,.upper))

q1_thrsh <- quantile(summary_point$cor_spearman, .33)
q3_thrsh <- quantile(summary_point$cor_spearman, .66)

summary_point <- summary_point %>% 
  rowwise() %>% 
  mutate(domain_name_a = unlist(str_split(harmonized_pair_name,"_"))[1], 
         domain_name_b = unlist(str_split(harmonized_pair_name,"_"))[2]) %>% 
  ungroup() %>% 
  arrange(desc(cor_spearman)) %>% 
  mutate(up_low_med = as.numeric(cut_number(cor_spearman,3)),
         color_a = case_when(grepl("Freq", domain_name_a)  ~ "#1e9bae",
                             grepl("Pro", domain_name_a)  ~ "#502a7a",
                             grepl("Beh", domain_name_a)  ~ "#e07f00") ,
         color_b = case_when(grepl("Freq", domain_name_b)  ~ "#1e9bae",
                             grepl("Pro", domain_name_b)  ~ "#502a7a",
                             grepl("Beh", domain_name_b)  ~ "#e07f00"),
         pair_name = glue("<i style='color:{color_a}'>{domain_name_a}</i><br><i style='color:{color_b}'>{domain_name_b}</i>")) %>% 
  arrange(cor_spearman) %>% mutate(rank_order = 1:n())




summary_lbl <-  dt %>% 
  group_by(harmonized_pair_name) %>%
  summarise(m_cor = median(cor_spearman),
            ci05_cor = quantile(cor_spearman,prob = 0.025),
            q2 = quantile(cor_spearman, .5),
            ci95_cor = quantile(cor_spearman,prob = 0.975),
            num_cor = n()) %>%
  mutate(label_k = paste0("k: ", format(num_cor, big.mark = "'" )),
         label_qi = paste0("**", round(m_cor,2),"**", "  [", round(ci05_cor,2), ", ", round(ci95_cor,2), "]"),
         y_k = 1.1,
         y_qi = 1.2) %>% 
  rowwise() %>% 
  mutate(domain_name_a = unlist(str_split(harmonized_pair_name,"_"))[1], 
         domain_name_b = unlist(str_split(harmonized_pair_name,"_"))[2]) %>% 
  ungroup() %>% 
  arrange(desc(m_cor)) %>% 
  mutate(up_low_med = as.numeric(cut_number(m_cor,3)),
         color_a = case_when(grepl("Freq", domain_name_a)  ~ "#1e9bae",
                             grepl("Pro", domain_name_a)  ~ "#502a7a",
                             grepl("Beh", domain_name_a)  ~ "#e07f00") ,
         color_b = case_when(grepl("Freq", domain_name_b)  ~ "#1e9bae",
                             grepl("Pro", domain_name_b)  ~ "#502a7a",
                             grepl("Beh", domain_name_b)  ~ "#e07f00"),
         pair_name = glue("<i style='color:{color_a}'>{domain_name_a}</i><br><i style='color:{color_b}'>{domain_name_b}</i>")) %>% 
  arrange(m_cor) %>% mutate(rank_order = 1:n())




A <-  ggplot() +
  geom_vline(xintercept = 0, linetype = "solid", color = "grey80", size = .3) +
  geom_crossbar(data =  summary_point, 
                aes(xmin = .lower_0.95, x = cor_spearman,  xmax = .upper_0.95, y =reorder(pair_name, rank_order, decreasing = TRUE)),
                fill = "white",
                linewidth = .15, position = position_nudge(y=0.5),
                width = 0.25,alpha =  1) +
  geom_crossbar(data =  summary_point, 
                aes(xmin = .lower_0.95, x = cor_spearman,  xmax = .upper_0.95, y =reorder(pair_name, rank_order, decreasing = TRUE)),
                color = "NA",fill = "#0F4C5C",
                linewidth = .2, position = position_nudge(y=0.5),
                width = 0.25,alpha =  .4) +
  geom_crossbar(data =  summary_point,
                aes(xmin = .lower_0.8, x = cor_spearman,  xmax = .upper_0.8, y =reorder(pair_name, rank_order, decreasing = TRUE)),
                color = "NA", fill = "white",
                linewidth = .2, position = position_nudge(y=0.5),
                width = 0.25,alpha =  1) +
  geom_crossbar(data =  summary_point,
                aes(xmin =.lower_0.8, x = cor_spearman,  xmax = .upper_0.8, y =reorder(pair_name, rank_order, decreasing = TRUE)),
                color = "NA",fill = "#0F4C5C",
                linewidth = .2, position = position_nudge(y=0.5),
                width = 0.25,alpha =  .7) +
  geom_crossbar(data =  summary_point,
                aes(xmin = .lower_0.5, x = cor_spearman,  xmax = .upper_0.5, y =reorder(pair_name, rank_order, decreasing = TRUE)),
                color = "NA", fill = "white",
                linewidth = .2, position = position_nudge(y=0.5),
                width = 0.25,alpha =  1) +
  geom_crossbar(data =  summary_point,
                aes(xmin = .lower_0.5, x = cor_spearman,  xmax = .upper_0.5, y =reorder(pair_name, rank_order, decreasing = TRUE)),
                color = "NA",fill = "#0F4C5C",
                linewidth = .2, position = position_nudge(y=0.5),
                width = 0.25,alpha =  1) +
  geom_point(data =  summary_point,
             aes(x = cor_spearman, y =reorder(pair_name, rank_order, decreasing = TRUE)),
             fill = "white", color = "grey20",
             position = position_nudge(y=0.5),
             shape = 21,
             stroke = .5,
             size = 1.5) +
  geom_text(data = summary_lbl, aes(label = label_k,y =reorder(pair_name, rank_order, decreasing = TRUE), x = y_k),
            color = "grey50",
            family = "Source Sans 3", position = position_nudge(y = .5),
            size = 2, hjust = 0, vjust = .5, angle = 0,
            show.legend = F, color = "grey20")+
  # geom_richtext(data = summary_lbl, aes(label = label_qi, y =reorder(pair_name, rank_order), x = y_qi),
  #               color = "grey50",
  #               fill = NA, label.color = NA, # remove background and outline
  #               # label.padding = grid::unit(rep(0, 4), "pt"),
  #               family = "Source Sans 3", position = position_nudge(y = .4),
  #               size = 2, hjust = 0, vjust = .5, angle = 0,
  #               show.legend = F, color = "grey20")+
  scale_fill_identity() +
  scale_color_identity() +
  scale_y_discrete(position = "left", expand = c(0.025,.01)) +
  scale_x_continuous(breaks = seq(0,1,.25), expand = c(0,0)) +
  coord_cartesian(xlim = c(-.25,1.5)) +
  facet_wrap(.~up_low_med, nrow = 1, scales = "free")+
  theme_minimal() +
  labs(y = "",
       tag = "A",
       x = "Spearman rank correlations") + #Pearson correlation coefficient
  # facet_grid(.~measure_category) +
  theme(panel.grid = element_blank(),
        plot.margin = margin(b = 10),
        panel.spacing.x = unit(1, "cm"),
        plot.tag = element_text(family = "Source Sans 3", face = "bold", size = 11),
        strip.text.x  = element_blank(),
        # strip.text.x  = element_text(family = "Source Sans 3", face = "bold", size = 11, color = "white"),
        axis.text.x = element_text(family = "Source Sans 3", size = 8),
        axis.title.x = element_text(family = "Source Sans 3", face = "bold", size = 8),
        axis.title.y = element_text(family = "Source Sans 3",face = "bold", size = 8, margin = margin(r = 10)),
        axis.text.y = element_markdown(family = "Source Sans 3 Medium",  size = 8, vjust = 0),
        text =  element_text(family = "Source Sans 3"),
        title = element_text(family = "Source Sans 3"),
        panel.background = element_rect(color = "grey85", fill = NA, size = .4),
        panel.grid.major.y = element_line(color = "grey80", size = .4, linetype = "dotted"))

# GROUP BY MEASURE --------------------------------------------------------


ma_dat <-   dt %>%  
  group_by(harmonized_pair_meas) %>%
  mutate(m_cor = mean(cor_spearman)) %>%
  ungroup() %>% 
  rowwise() %>% 
  mutate(domain_name_a = unlist(str_split(harmonized_pair_meas,"_"))[1], 
         domain_name_b = unlist(str_split(harmonized_pair_meas,"_"))[2]) %>% 
  ungroup() %>% 
  mutate(color_a = case_when(grepl("Freq", domain_name_a)  ~ "#1e9bae",
                             grepl("Pro", domain_name_a)  ~ "#502a7a",
                             grepl("Beh", domain_name_a)  ~ "#e07f00") ,
         color_b = case_when(grepl("Freq", domain_name_b)  ~ "#1e9bae",
                             grepl("Pro", domain_name_b)  ~ "#502a7a",
                             grepl("Beh", domain_name_b)  ~ "#e07f00"),
         pair_name = glue("<i style='color:{color_a}'>{domain_name_a}</i><br><i style='color:{color_b}'>{domain_name_b}</i>")) %>% 
  arrange(m_cor) %>% mutate(rank_order = 1:n())



summary_point <-  dt %>% 
  group_by(harmonized_pair_meas) %>%
  median_qi(cor_spearman, .width = c(0.95,0.5,0.8)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = .width, values_from = c(.lower,.upper))

q2_thrsh <- quantile(summary_point$cor_spearman, .5)


summary_point <- summary_point %>% 
  rowwise() %>% 
  mutate(domain_name_a = unlist(str_split(harmonized_pair_meas,"_"))[1], 
         domain_name_b = unlist(str_split(harmonized_pair_meas,"_"))[2]) %>% 
  ungroup() %>% 
  mutate(up_low_med = if_else(cor_spearman >= q2_thrsh,"Above Median", "Below Median"),
         color_a = case_when(grepl("Freq", domain_name_a)  ~ "#1e9bae",
                             grepl("Pro", domain_name_a)  ~ "#502a7a",
                             grepl("Beh", domain_name_a)  ~ "#e07f00") ,
         color_b = case_when(grepl("Freq", domain_name_b)  ~ "#1e9bae",
                             grepl("Pro", domain_name_b)  ~ "#502a7a",
                             grepl("Beh", domain_name_b)  ~ "#e07f00"),
         pair_name = glue("<i style='color:{color_a}'>{domain_name_a}</i><br><i style='color:{color_b}'>{domain_name_b}</i>")) %>% 
  arrange(cor_spearman) %>% mutate(rank_order = 1:n())




summary_lbl <-  dt %>% 
  group_by(harmonized_pair_meas) %>%
  summarise(m_cor = median(cor_spearman),
            ci05_cor = quantile(cor_spearman,prob = 0.025),
            q2 = quantile(cor_spearman, .5),
            ci95_cor = quantile(cor_spearman,prob = 0.975),
            num_cor = n()) %>%
  mutate(label_k = paste0("k: ", format(num_cor, big.mark = "'" )),
         label_qi = paste0("**", round(m_cor,2),"**", "  [", round(ci05_cor,2), ", ", round(ci95_cor,2), "]"),
         y_k = .8,
         y_qi = 1) %>% 
  rowwise() %>% 
  mutate(domain_name_a = unlist(str_split(harmonized_pair_meas,"_"))[1], 
         domain_name_b = unlist(str_split(harmonized_pair_meas,"_"))[2]) %>% 
  ungroup() %>% 
  mutate(color_a = case_when(grepl("Freq", domain_name_a)  ~ "#1e9bae",
                             grepl("Pro", domain_name_a)  ~ "#502a7a",
                             grepl("Beh", domain_name_a)  ~ "#e07f00") ,
         color_b = case_when(grepl("Freq", domain_name_b)  ~ "#1e9bae",
                             grepl("Pro", domain_name_b)  ~ "#502a7a",
                             grepl("Beh", domain_name_b)  ~ "#e07f00"),
         pair_name = glue("<i style='color:{color_a}'>{domain_name_a}</i><br><i style='color:{color_b}'>{domain_name_b}</i>")) %>% 
  arrange(m_cor) %>% mutate(rank_order = 1:n())





B <-  ggplot() +
  geom_vline(xintercept = 0, linetype = "solid", color = "grey70", size = .3) +
  stat_halfeye(data = ma_dat, aes(x = cor_spearman, y = reorder(pair_name, cor_spearman)),
               color = "NA", fill = "#0F4C5C",
               position = position_nudge(y= .15),
               alpha = .3, size = .3) +
  geom_crossbar(data =  summary_point,
                aes(xmin = .lower_0.95, x = cor_spearman,  xmax = .upper_0.95, y =reorder(pair_name, cor_spearman)),
                fill = "white",
                linewidth = .15, position = position_nudge(y=0.1),
                width = 0.15,alpha =  1) +
  geom_crossbar(data =  summary_point, aes(xmin = .lower_0.95, x = cor_spearman,  xmax = .upper_0.95, y =reorder(pair_name, cor_spearman)),
                color = "NA",fill = "#0F4C5C",
                linewidth = .2, position = position_nudge(y=0.1),
                width = 0.15,alpha =  .4) +
  geom_crossbar(data =  summary_point, aes(xmin = .lower_0.8, x = cor_spearman,  xmax = .upper_0.8, y =reorder(pair_name, cor_spearman)),
                color = "NA", fill = "white",
                linewidth = .2, position = position_nudge(y=0.1),
                width = 0.15,alpha =  1) +
  geom_crossbar(data =  summary_point, aes(xmin = .lower_0.8, x = cor_spearman,  xmax = .upper_0.8, y =reorder(pair_name, cor_spearman)),
                color = "NA",fill = "#0F4C5C",
                linewidth = .2, position = position_nudge(y=0.1),
                width = 0.15,alpha =  .7) +
  geom_crossbar(data =  summary_point, aes(xmin = .lower_0.5, x = cor_spearman,  xmax = .upper_0.5,  y =reorder(pair_name, cor_spearman)),
                color = "NA", fill = "white",
                linewidth = .2, position = position_nudge(y=0.1),
                width = 0.15,alpha =  1) +
  geom_crossbar(data =  summary_point, aes(xmin = .lower_0.5, x = cor_spearman,  xmax = .upper_0.5, y =reorder(pair_name, cor_spearman)),
                color = "NA",fill = "#0F4C5C",
                linewidth = .2, position = position_nudge(y=0.1),
                width = 0.15,alpha =  1) +
  geom_point(data =  summary_point, aes(x = cor_spearman, y =reorder(pair_name, cor_spearman)),
             fill = "white", color = "grey20",
             position = position_nudge(y=0.1),
             shape = 21,
             stroke = .5,
             size = 1.5) +
  scale_fill_identity() +
  scale_color_identity() +
  geom_text(data = summary_lbl, aes(label = label_k,y =reorder(pair_name, rank_order), x = y_k),
            color = "grey50",
            family = "Source Sans 3", position = position_nudge(y = .4),
            size = 2, hjust = 0, vjust = .5, angle = 0,
            show.legend = F, color = "grey20")+
  # geom_richtext(data = summary_lbl, aes(label = label_qi, y =reorder(pair_name, rank_order), x = y_qi),
  #               color = "grey50",
  #               fill = NA, label.color = NA, # remove background and outline
  #               # label.padding = grid::unit(rep(0, 4), "pt"),
  #               family = "Source Sans 3", position = position_nudge(y = .4),
  #               size = 2, hjust = 0, vjust = .5, angle = 0,
  #               show.legend = F, color = "grey20")+
  scale_y_discrete(position = "left") +
  scale_x_continuous(breaks = seq(0,1,.25), expand = c(0,0)) +
  coord_cartesian(xlim = c(-.25,1.4)) +
  # facet_wrap(.~up_low_med, nrow = 1, scales = "free_y")+
  theme_minimal() +
  labs(y = "",
       tag = "B",
       x = "Spearman rank correlation") + #Pearson correlation coefficient
  # facet_grid(.~measure_category) +
  theme(panel.grid = element_blank(),
        plot.margin = margin(l = 15, r = 10),
        panel.spacing.x = unit(1, "cm"),
        plot.tag = element_text(family = "Source Sans 3", face = "bold", size = 11),
        strip.text.x  = element_blank(),
        # strip.text.x  = element_text(family = "Source Sans 3", face = "bold", size = 11, color = "white"),
        axis.text.x = element_text(family = "Source Sans 3", size = 9,  
                                   hjust=c(rep(.5,4),1)),
        axis.title.x = element_text(family = "Source Sans 3", face = "bold", size = 9),
        axis.title.y = element_text(family = "Source Sans 3",face = "bold", size = 9, margin = margin(r = 10)),
        axis.text.y = element_markdown(family = "Source Sans 3 Medium",  size = 9, vjust = -.2),
        text =  element_text(family = "Source Sans 3"),
        title = element_text(family = "Source Sans 3"),
        panel.background = element_rect(color = "grey85", fill = NA, size = .4),
        panel.grid.major.y = element_line(color = "grey80", size = .4, linetype = "dotted"))

# OVERALL -----------------------------------------------------------------


p <- A +B + plot_layout(widths = c(3,1.25), nrow = 1)
p


ggsave(paste0(output_path, "convergent_distribution_appendix.png"), plot = p, height = 32, 
       width = 50, units = "cm", dpi = 300)
