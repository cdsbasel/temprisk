

# DESCRIPTION -------------------------------------------------------------

# Script to plot additional figures showing intercorrelation distributions for the
# companion website


# PACKAGES ---------------------------------------------------------------

library(tidyverse)
library(brms)
library(metafor)
library(data.table)
library(tidybayes)
library(ggtext)
library(patchwork)
library(ggdist)

# FUNCTION ----------------------------------------------------------------

source("helper_functions.R")


# PATHS ---------------------------------------------------

output_path <- c("docs/images/") # where to store the output 

# CONVERGENCE: VAR DECOMP OVERVIEW  ---------------------------------------

data_path <- c("processing/output/convergent_val/") # where is the data
masc_dat_file <- "analysis/output/convergent_val/masc_nlpar_pred.csv"
data_file <- "complete_intercor.csv"
dat <-read_csv(paste0(data_path,data_file))
dat_masc <- read_csv(masc_dat_file)
dt <-  dat %>%
  filter(age_group != "10-90" &
           gender_group != "all" & 
           year_age_group == 10 & 
           n >= 30) %>%
  mutate(sample_size = n,
         item_num_a = if_else(item_num_a == 1, "single", "multi"),
         item_num_b = if_else(item_num_b == 1, "single", "multi"),
         item_num_type = case_when(item_num_b == "single" & item_num_a == "single"  ~ "both_single",
                                   item_num_b != "single" & item_num_a != "single"  ~ "both_multi",
                                   TRUE ~"mixed"),
         scale_type_pair_same = ifelse(scale_type_a == scale_type_b, 1,0),
         measure_category_pair_same = ifelse(measure_category_a == measure_category_b, 1,0),
         domain_name_pair_same = ifelse(domain_name_a == domain_name_b, 1,0)) 


#### add the reliability parameter information
# select relevant estimates
dat_masc <- dat_masc %>% 
  mutate(measure = case_when(measure == "Propensity" ~"pro",
                             measure == "Frequency" ~"fre",
                             measure == "Behaviour"  ~ "beh")) %>% 
  filter(nlpar == "Reliability")  %>% 
  select(measure,x, .epred, age_group, gender_group, sample) 

dat_masc_a <- dat_masc  %>% rename(estimate_a = .epred, measure_category_a = measure,
                                   domain_name_a = x)
dat_masc_b <- dat_masc  %>% rename(estimate_b = .epred, measure_category_b = measure,
                                   domain_name_b = x)

dt <- dt %>% 
  left_join(dat_masc_a, by = c("domain_name_a","measure_category_a", "age_group", "gender_group", "sample" )) %>% 
  left_join(dat_masc_b, by = c("domain_name_b","measure_category_b", "age_group", "gender_group", "sample" )) %>% 
  rowwise() %>% 
  mutate(mean_rel = mean(c(estimate_a, estimate_b))) %>% 
  ungroup()



dt_lbl <- dt %>% 
  mutate(domain_name_pair_same = if_else(domain_name_pair_same == 1, "Matching", "Not Matching")) %>% 
  group_by(domain_name_pair_same) %>% 
  summarise(lbl = paste0("k:", format(n(), big.mark = "'")),
            median_cor = median(cor_spearman),
            mean_cor = mean(cor_spearman))

p_dom <- dt %>%
  mutate(domain_name_pair_same = if_else(domain_name_pair_same == 1, "Matching", "Not Matching")) %>% 
  group_by(domain_name_pair_same) %>% 
  ggplot(aes(x = cor_spearman)) +
  # geom_vline(data = dt_lbl, aes(xintercept = mean_cor), linetype = "solid", linewidth = .25, color = "#003049")+
  # geom_vline(data = dt_lbl, aes(xintercept = median_cor), linetype = "solid", linewidth = .25, color = "#8093f1")+
  geom_vline(xintercept = 0, linetype = "dotted", linewidth = .25, color = "grey40")+
  geom_point(data = dt_lbl, aes(x = median_cor, y = .1),fill = "#0b2545", size = 3, shape = 21, color = "white", stroke = .5)+
  geom_point(data = dt_lbl, aes(x = mean_cor, y = .1),fill = "#6096ba", size = 3, shape = 21, color = "white", stroke = .5)+
  geom_density(fill = "#2e4f58", alpha = .3, color = "#2e4f58", size = .25) + 
  ggtext::geom_richtext(data = dt_lbl,
                        fill = NA, label.color = NA, # remove background and outline
                        label.padding = grid::unit(rep(0, 4), "pt"), # remove padding
                        size = 3.5, hjust = 1, vjust = 2, family = "Source Sans 3",
                        aes(label = lbl, y = 3.25, x = 0.75)) +
  facet_wrap(.~domain_name_pair_same, nrow = 1) + theme_minimal() +
  scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) +
  labs(x = "Inter-Correlation", y = "density") +
  theme(strip.placement = "outside",
        strip.text.x = element_text(family = "Source Sans 3", size = 9, color = "grey20", face = "bold"),
        legend.position = "none", # c(0,0) bottom left, c(1,1) top-right.
        # legend.justification = c(0.5,0.5),
        legend.margin = margin(-.5,0,0,0, unit="cm"),
        panel.spacing.x = unit(.5, "cm"),
        legend.spacing.y = unit(0.15, 'cm'),
        legend.key.width = unit(.25, "cm"),
        legend.key.size = unit(.3, "cm"),
        legend.text = element_text(family = "Source Sans 3", size = 8.5, color = "grey20"),
        # legend.background = theme_rect(fill = "white", colour = NA),
        text = element_text(family = "Source Sans 3", size = 9, color = "grey40"),
        title = element_text(family = "Source Sans 3", size = 9, color = "grey20", face = "bold"),
        # axis.title.x = element_text(margin = margin(t = 5)),
        # title = element_text(family = "Source Sans 3", size = 9, face = "bold", color = "grey20"),
        panel.spacing = unit(.5, "lines"),
        strip.text = element_text(family = "Source Sans 3", face = "bold", color = "grey20",
                                  margin = margin(b = 5)),
        plot.title = element_blank(),
        panel.grid = element_line(linetype = "dotted", color = "grey85", size = .1),
        plot.title.position = "plot",
        plot.margin = margin(b = 5, r = 5, l = 5),
        panel.background = element_rect(color = "grey75", fill = NA, size = .4))

p_dom


dt_lbl <- dt %>% 
  mutate(scale_type_pair_same = if_else(scale_type_pair_same == 1, "Matching", "Not Matching")) %>% 
  group_by(scale_type_pair_same) %>% 
  summarise(lbl = paste0("k:", format(n(), big.mark = "'")),
            median_cor = median(cor_spearman),
            mean_cor = mean(cor_spearman))

p_scale <- dt %>%
  mutate(scale_type_pair_same = if_else(scale_type_pair_same == 1, "Matching", "Not Matching")) %>% 
  group_by(scale_type_pair_same) %>% 
  ggplot(aes(x = cor_spearman)) +
  # geom_vline(data = dt_lbl, aes(xintercept = mean_cor), linetype = "solid", linewidth = .25, color = "#003049")+
  # geom_vline(data = dt_lbl, aes(xintercept = median_cor), linetype = "solid", linewidth = .25, color = "#8093f1")+
  geom_vline(xintercept = 0, linetype = "dotted", linewidth = .25, color = "grey40")+
  geom_point(data = dt_lbl, aes(x = median_cor, y = .1),fill = "#0b2545", size = 3, shape = 21, color = "white", stroke = .5)+
  geom_point(data = dt_lbl, aes(x = mean_cor, y = .1),fill = "#6096ba", size = 3, shape = 21, color = "white", stroke = .5)+
  geom_density(fill = "#2e4f58", alpha = .3, color = "#2e4f58", size = .25) + 
  ggtext::geom_richtext(data = dt_lbl,
                        fill = NA, label.color = NA, # remove background and outline
                        label.padding = grid::unit(rep(0, 4), "pt"), # remove padding
                        size = 3.5, hjust = 1, vjust = 2, family = "Source Sans 3",
                        aes(label = lbl, y = 3.25, x = 0.75)) +
  facet_wrap(.~scale_type_pair_same, nrow = 1) + theme_minimal() +
  scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) +
  labs(x = "Inter-Correlation", y = "density") +
  theme(strip.placement = "outside",
        strip.text.x = element_text(family = "Source Sans 3", size = 9, color = "grey20", face = "bold"),
        legend.position = "none", # c(0,0) bottom left, c(1,1) top-right.
        # legend.justification = c(0.5,0.5),
        legend.margin = margin(-.5,0,0,0, unit="cm"),
        panel.spacing.x = unit(.5, "cm"),
        legend.spacing.y = unit(0.15, 'cm'),
        legend.key.width = unit(.25, "cm"),
        legend.key.size = unit(.3, "cm"),
        legend.text = element_text(family = "Source Sans 3", size = 8.5, color = "grey20"),
        # legend.background = theme_rect(fill = "white", colour = NA),
        text = element_text(family = "Source Sans 3", size = 9, color = "grey40"),
        title = element_text(family = "Source Sans 3", size = 9, color = "grey20", face = "bold"),
        # axis.title.x = element_text(margin = margin(t = 5)),
        # title = element_text(family = "Source Sans 3", size = 9, face = "bold", color = "grey20"),
        panel.spacing = unit(.5, "lines"),
        strip.text = element_text(family = "Source Sans 3", face = "bold", color = "grey20",
                                  margin = margin(b = 5)),
        plot.title = element_blank(),
        panel.grid = element_line(linetype = "dotted", color = "grey85", size = .1),
        plot.title.position = "plot",
        plot.margin = margin(b = 5, r = 5, l = 5),
        panel.background = element_rect(color = "grey75", fill = NA, size = .4))

p_scale



dt_lbl <- dt %>% 
  mutate(measure_category_pair_same = if_else(measure_category_pair_same == 1, "Matching", "Not Matching")) %>% 
  group_by(measure_category_pair_same) %>% 
  summarise(lbl = paste0("k:", format(n(), big.mark = "'")),
            median_cor = median(cor_spearman),
            mean_cor = mean(cor_spearman))

p_categ <- dt %>%
  mutate(measure_category_pair_same = if_else(measure_category_pair_same == 1, "Matching", "Not Matching")) %>% 
  group_by(measure_category_pair_same) %>% 
  ggplot(aes(x = cor_spearman)) +
  # geom_vline(data = dt_lbl, aes(xintercept = mean_cor), linetype = "solid", linewidth = .25, color = "#003049")+
  # geom_vline(data = dt_lbl, aes(xintercept = median_cor), linetype = "solid", linewidth = .25, color = "#8093f1")+
  geom_vline(xintercept = 0, linetype = "dotted", linewidth = .25, color = "grey40")+
  geom_point(data = dt_lbl, aes(x = median_cor, y = .1),fill = "#0b2545", size = 3, shape = 21, color = "white", stroke = .5)+
  geom_point(data = dt_lbl, aes(x = mean_cor, y = .1),fill = "#6096ba", size = 3, shape = 21, color = "white", stroke = .5)+
  geom_density(fill = "#2e4f58", alpha = .3, color = "#2e4f58", size = .25) + 
  ggtext::geom_richtext(data = dt_lbl,
                        fill = NA, label.color = NA, # remove background and outline
                        label.padding = grid::unit(rep(0, 4), "pt"), # remove padding
                        size = 3.5, hjust = 1, vjust = 2, family = "Source Sans 3",
                        aes(label = lbl, y = 3.25, x = 0.75)) +
  facet_wrap(.~measure_category_pair_same, nrow = 1) + theme_minimal() +
  scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) +
  labs(x = "Inter-Correlation", y = "density") +
  theme(strip.placement = "outside",
        strip.text.x = element_text(family = "Source Sans 3", size = 9, color = "grey20", face = "bold"),
        legend.position = "none", # c(0,0) bottom left, c(1,1) top-right.
        # legend.justification = c(0.5,0.5),
        legend.margin = margin(-.5,0,0,0, unit="cm"),
        panel.spacing.x = unit(.5, "cm"),
        legend.spacing.y = unit(0.15, 'cm'),
        legend.key.width = unit(.25, "cm"),
        legend.key.size = unit(.3, "cm"),
        legend.text = element_text(family = "Source Sans 3", size = 8.5, color = "grey20"),
        # legend.background = theme_rect(fill = "white", colour = NA),
        text = element_text(family = "Source Sans 3", size = 9, color = "grey40"),
        title = element_text(family = "Source Sans 3", size = 9, color = "grey20", face = "bold"),
        # axis.title.x = element_text(margin = margin(t = 5)),
        # title = element_text(family = "Source Sans 3", size = 9, face = "bold", color = "grey20"),
        panel.spacing = unit(.5, "lines"),
        strip.text = element_text(family = "Source Sans 3", face = "bold", color = "grey20",
                                  margin = margin(b = 5)),
        plot.title = element_blank(),
        panel.grid = element_line(linetype = "dotted", color = "grey85", size = .1),
        plot.title.position = "plot",
        plot.margin = margin(b = 5, r = 5, l = 5),
        panel.background = element_rect(color = "grey75", fill = NA, size = .4))

p_categ




dt_lbl <- dt %>% 
  group_by(age_group) %>% 
  summarise(lbl = paste0("k:", format(n(), big.mark = "'")),
            median_cor = median(cor_spearman),
            mean_cor = mean(cor_spearman))

p_age <- dt %>%
  ggplot(aes(x = cor_spearman)) +
  # geom_vline(data = dt_lbl, aes(xintercept = mean_cor), linetype = "solid", linewidth = .25, color = "#003049")+
  # geom_vline(data = dt_lbl, aes(xintercept = median_cor), linetype = "solid", linewidth = .25, color = "#8093f1")+
  geom_vline(xintercept = 0, linetype = "dotted", linewidth = .25, color = "grey40")+
  geom_point(data = dt_lbl, aes(x = median_cor, y = .1),fill = "#0b2545", size = 3, shape = 21, color = "white", stroke = .5)+
  geom_point(data = dt_lbl, aes(x = mean_cor, y = .1),fill = "#6096ba", size = 3, shape = 21, color = "white", stroke = .5)+
  geom_density(fill = "#2e4f58", alpha = .3, color = "#2e4f58", size = .25) + 
  ggtext::geom_richtext(data = dt_lbl,
                        fill = NA, label.color = NA, # remove background and outline
                        label.padding = grid::unit(rep(0, 4), "pt"), # remove padding
                        size = 3.5, hjust = 1, vjust = 2, family = "Source Sans 3",
                        aes(label = lbl, y = 3.25, x = 0.75)) +
  facet_wrap(.~age_group, nrow = 2) + theme_minimal() +
  scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) +
  labs(x = "Inter-Correlation", y = "density") +
  theme(strip.placement = "outside",
        strip.text.x = element_text(family = "Source Sans 3", size = 9, color = "grey20", face = "bold"),
        legend.position = "none", # c(0,0) bottom left, c(1,1) top-right.
        # legend.justification = c(0.5,0.5),
        legend.margin = margin(-.5,0,0,0, unit="cm"),
        panel.spacing.x = unit(.5, "cm"),
        legend.spacing.y = unit(0.15, 'cm'),
        legend.key.width = unit(.25, "cm"),
        legend.key.size = unit(.3, "cm"),
        legend.text = element_text(family = "Source Sans 3", size = 8.5, color = "grey20"),
        # legend.background = theme_rect(fill = "white", colour = NA),
        text = element_text(family = "Source Sans 3", size = 9, color = "grey40"),
        title = element_text(family = "Source Sans 3", size = 9, color = "grey20", face = "bold"),
        # axis.title.x = element_text(margin = margin(t = 5)),
        # title = element_text(family = "Source Sans 3", size = 9, face = "bold", color = "grey20"),
        panel.spacing = unit(.5, "lines"),
        strip.text = element_text(family = "Source Sans 3", face = "bold", color = "grey20",
                                  margin = margin(b = 5)),
        plot.title = element_blank(),
        panel.grid = element_line(linetype = "dotted", color = "grey85", size = .1),
        plot.title.position = "plot",
        plot.margin = margin(b = 5, r = 5, l = 5),
        panel.background = element_rect(color = "grey75", fill = NA, size = .4))

p_age



dt_lbl <- dt %>% 
  group_by(gender_group) %>% 
  summarise(lbl = paste0("k:", format(n(), big.mark = "'")),
            median_cor = median(cor_spearman),
            mean_cor = mean(cor_spearman))

p_gender <- dt %>%
  ggplot(aes(x = cor_spearman)) +
  # geom_vline(data = dt_lbl, aes(xintercept = mean_cor), linetype = "solid", linewidth = .25, color = "#003049")+
  # geom_vline(data = dt_lbl, aes(xintercept = median_cor), linetype = "solid", linewidth = .25, color = "#8093f1")+
  geom_vline(xintercept = 0, linetype = "dotted", linewidth = .25, color = "grey40")+
  geom_point(data = dt_lbl, aes(x = median_cor, y = .1),fill = "#0b2545", size = 3, shape = 21, color = "white", stroke = .5)+
  geom_point(data = dt_lbl, aes(x = mean_cor, y = .1),fill = "#6096ba", size = 3, shape = 21, color = "white", stroke = .5)+
  geom_density(fill = "#2e4f58", alpha = .3, color = "#2e4f58", size = .25) + 
  ggtext::geom_richtext(data = dt_lbl,
                        fill = NA, label.color = NA, # remove background and outline
                        label.padding = grid::unit(rep(0, 4), "pt"), # remove padding
                        size = 3.5, hjust = 1, vjust = 2, family = "Source Sans 3",
                        aes(label = lbl, y = 3.25, x = 0.75)) +
  facet_wrap(.~gender_group, nrow = 1) + theme_minimal() +
  scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) +
  labs(x = "Inter-Correlation", y = "density") +
  theme(strip.placement = "outside",
        strip.text.x = element_text(family = "Source Sans 3", size = 9, color = "grey20", face = "bold"),
        legend.position = "none", # c(0,0) bottom left, c(1,1) top-right.
        # legend.justification = c(0.5,0.5),
        legend.margin = margin(-.5,0,0,0, unit="cm"),
        panel.spacing.x = unit(.5, "cm"),
        legend.spacing.y = unit(0.15, 'cm'),
        legend.key.width = unit(.25, "cm"),
        legend.key.size = unit(.3, "cm"),
        legend.text = element_text(family = "Source Sans 3", size = 8.5, color = "grey20"),
        # legend.background = theme_rect(fill = "white", colour = NA),
        text = element_text(family = "Source Sans 3", size = 9, color = "grey40"),
        title = element_text(family = "Source Sans 3", size = 9, color = "grey20", face = "bold"),
        # axis.title.x = element_text(margin = margin(t = 5)),
        # title = element_text(family = "Source Sans 3", size = 9, face = "bold", color = "grey20"),
        panel.spacing = unit(.5, "lines"),
        strip.text = element_text(family = "Source Sans 3", face = "bold", color = "grey20",
                                  margin = margin(b = 5)),
        plot.title = element_blank(),
        panel.grid = element_line(linetype = "dotted", color = "grey85", size = .1),
        plot.title.position = "plot",
        plot.margin = margin(b = 5, r = 5, l = 5),
        panel.background = element_rect(color = "grey75", fill = NA, size = .4))

p_gender



dt_lbl <- dt %>% 
  group_by(panel) %>% 
  summarise(lbl = paste0("k:", format(n(), big.mark = "'")),
            median_cor = median(cor_spearman),
            mean_cor = mean(cor_spearman))

p_panel <- dt %>%
  ggplot(aes(x = cor_spearman)) +
  # geom_vline(data = dt_lbl, aes(xintercept = mean_cor), linetype = "solid", linewidth = .25, color = "#003049")+
  # geom_vline(data = dt_lbl, aes(xintercept = median_cor), linetype = "solid", linewidth = .25, color = "#8093f1")+
  geom_vline(xintercept = 0, linetype = "dotted", linewidth = .25, color = "grey40")+
  geom_point(data = dt_lbl, aes(x = median_cor, y = .1),fill = "#0b2545", size = 3, shape = 21, color = "white", stroke = .5)+
  geom_point(data = dt_lbl, aes(x = mean_cor, y = .1),fill = "#6096ba", size = 3, shape = 21, color = "white", stroke = .5)+
  geom_density(fill = "#2e4f58", alpha = .3, color = "#2e4f58", size = .25) + 
  ggtext::geom_richtext(data = dt_lbl,
                        fill = NA, label.color = NA, # remove background and outline
                        label.padding = grid::unit(rep(0, 4), "pt"), # remove padding
                        size = 3.5, hjust = 1, vjust = 2, family = "Source Sans 3",
                        aes(label = lbl, y = 3.25, x = 0.75)) +
  facet_wrap(.~panel, nrow = 4, scales = "free") + theme_minimal() +
  scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) +
  labs(x = "Inter-Correlation", y = "density") +
  theme(strip.placement = "outside",
        strip.text.x = element_text(family = "Source Sans 3", size = 9, color = "grey20", face = "bold"),
        legend.position = "none", # c(0,0) bottom left, c(1,1) top-right.
        # legend.justification = c(0.5,0.5),
        legend.margin = margin(-.5,0,0,0, unit="cm"),
        panel.spacing.x = unit(.5, "cm"),
        legend.spacing.y = unit(0.15, 'cm'),
        legend.key.width = unit(.25, "cm"),
        legend.key.size = unit(.3, "cm"),
        legend.text = element_text(family = "Source Sans 3", size = 8.5, color = "grey20"),
        # legend.background = theme_rect(fill = "white", colour = NA),
        text = element_text(family = "Source Sans 3", size = 9, color = "grey40"),
        title = element_text(family = "Source Sans 3", size = 9, color = "grey20", face = "bold"),
        # axis.title.x = element_text(margin = margin(t = 5)),
        # title = element_text(family = "Source Sans 3", size = 9, face = "bold", color = "grey20"),
        panel.spacing = unit(.5, "lines"),
        strip.text = element_text(family = "Source Sans 3", face = "bold", color = "grey20",
                                  margin = margin(b = 5)),
        plot.title = element_blank(),
        panel.grid = element_line(linetype = "dotted", color = "grey85", size = .1),
        plot.title.position = "plot",
        plot.margin = margin(b = 5, r = 5, l = 5),
        panel.background = element_rect(color = "grey75", fill = NA, size = .4))

p_panel


p_rel <- dt %>%
  ggplot(aes(x = mean_rel, y =cor_spearman)) + 
  stat_density_2d(geom = "point", aes(size =after_stat(density)), n = 100, contour = FALSE, 
                  fill = "#2e4f58", color = "grey95",   ##11693F
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
        plot.tag = element_text(family = "Source Sans 3", face = "bold", size = 11),
        title = element_text(family = "Source Sans 3 Light", size = 9, color = "grey20"),
        panel.grid = element_blank()) +
  scale_size(range = c(.00,10)) +
  scale_alpha(range = c(0.1,.8)) +
  labs(y = "Inter-Correlation", x = "Reliability")


p_rel



p_n <- dt %>% 
  ggplot(aes(x = n, y = cor_spearman))+
  stat_density_2d(geom = "point", aes(size =after_stat(density)), n = 100, contour = FALSE, 
                  fill = "#2e4f58", color = "grey95",   ##11693F
                  shape = 21, stroke = .25, alpha = .2) +
  theme_minimal() +
  scale_x_log10(expand = c(0.0,0.0)) +
  scale_y_continuous(expand = c(0.0,0.0)) +
  theme(legend.position = "none",
        axis.line.x = element_line(size = .35, color = "grey20"),
        axis.line.y = element_line(size = .35, color = "grey20"),
        # panel.background = element_rect(color = "grey50", size = .25),
        text = element_text(family = "Source Sans 3", size = 9, color = "grey40"),
        axis.text.y = element_text( vjust=seq(0,1, length.out = 5)),
        axis.text.x = element_text( hjust=c(0,1)),
        plot.tag = element_text(family = "Source Sans 3", face = "bold", size = 11),
        title = element_text(family = "Source Sans 3 Light", size = 9, color = "grey20"),
        panel.grid = element_blank()) +
  scale_size(range = c(.00,10)) +
  scale_alpha(range = c(0.1,.8)) +
  labs(y = "Inter-Correlation", x = "Sample Size (log10)")


p_n


dt_lbl <- dt %>% 
  group_by(item_num_type) %>% 
  summarise(lbl = paste0("k:", format(n(), big.mark = "'")),
            median_cor = median(cor_spearman),
            mean_cor = mean(cor_spearman))

p_item <- dt %>%
  group_by(item_num_type) %>% 
  ggplot(aes(x = cor_spearman)) +
  geom_vline(xintercept = 0, linetype = "dotted", linewidth = .25, color = "grey40")+
  geom_point(data = dt_lbl, aes(x = median_cor, y = .1),fill = "#0b2545", size = 3, shape = 21, color = "white", stroke = .5)+
  geom_point(data = dt_lbl, aes(x = mean_cor, y = .1),fill = "#6096ba", size = 3, shape = 21, color = "white", stroke = .5)+
  geom_density(fill = "#2e4f58", alpha = .3, color = "#2e4f58", size = .25) + 
  ggtext::geom_richtext(data = dt_lbl,
                        fill = NA, label.color = NA, # remove background and outline
                        label.padding = grid::unit(rep(0, 4), "pt"), # remove padding
                        size = 3.5, hjust = 1, vjust = 2, family = "Source Sans 3",
                        aes(label = lbl, y = 3.25, x = 0.75)) +
  facet_wrap(.~item_num_type, nrow = 1) + theme_minimal() +
  scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) +
  labs(x = "Inter-Correlation", y = "density") +
  theme(strip.placement = "outside",
        strip.text.x = element_text(family = "Source Sans 3", size = 9, color = "grey20", face = "bold"),
        legend.position = "none", # c(0,0) bottom left, c(1,1) top-right.
        # legend.justification = c(0.5,0.5),
        legend.margin = margin(-.5,0,0,0, unit="cm"),
        panel.spacing.x = unit(.5, "cm"),
        legend.spacing.y = unit(0.15, 'cm'),
        legend.key.width = unit(.25, "cm"),
        legend.key.size = unit(.3, "cm"),
        legend.text = element_text(family = "Source Sans 3", size = 8.5, color = "grey20"),
        # legend.background = theme_rect(fill = "white", colour = NA),
        text = element_text(family = "Source Sans 3", size = 9, color = "grey40"),
        title = element_text(family = "Source Sans 3", size = 9, color = "grey20", face = "bold"),
        # axis.title.x = element_text(margin = margin(t = 5)),
        # title = element_text(family = "Source Sans 3", size = 9, face = "bold", color = "grey20"),
        panel.spacing = unit(.5, "lines"),
        strip.text = element_text(family = "Source Sans 3", face = "bold", color = "grey20",
                                  margin = margin(b = 5)),
        plot.title = element_blank(),
        panel.grid = element_line(linetype = "dotted", color = "grey85", size = .1),
        plot.title.position = "plot",
        plot.margin = margin(b = 5, r = 5, l = 5),
        panel.background = element_rect(color = "grey75", fill = NA, size = .4))

p_item






ggsave(plot = p_dom, filename = paste0(output_path, "domain_overview_intercor.png"),
       dpi = 300, width = 20, height = 7, units = "cm")
ggsave(plot = p_age, filename = paste0(output_path, "age_overview_intercor.png"),
       dpi = 300, width = 30, height = 10, units = "cm") 
ggsave(plot = p_categ, filename = paste0(output_path, "categ_overview_intercor.png"),
       dpi = 300, width = 20, height = 7, units = "cm") 
ggsave(plot = p_scale, filename = paste0(output_path, "scale_overview_intercor.png"),
       dpi = 300, width = 20, height = 7, units = "cm") 
ggsave(plot = p_gender, filename = paste0(output_path, "gender_overview_intercor.png"),
       dpi = 300, width = 20, height = 7, units = "cm")
ggsave(plot = p_panel, filename = paste0(output_path, "panel_overview_intercor.png"),
       dpi = 300, width = 30, height = 25, units = "cm") 
ggsave(plot = p_n, filename = paste0(output_path, "n_overview_intercor.png"),
       dpi = 300, width = 20, height = 10, units = "cm")  
ggsave(plot = p_rel, filename = paste0(output_path, "rel_overview_intercor.png"),
       dpi = 300, width = 20, height = 10, units = "cm") 
ggsave(plot = p_item, filename = paste0(output_path, "item_overview_intercor.png"),
       dpi = 300, width = 30, height = 10, units = "cm") 


# CONVERGENCE: ROBUSTNESS CHECK -------------------------------------------

data_path <- c("analysis/output/convergent_val/") # where is the  data stored

ma <- list(overall = read_csv(paste0(data_path, "cor_mat_convergent_overall_dat.csv")),
           measure = read_csv(paste0(data_path, "cor_mat_convergent_measure_dat.csv")),
           domain = read_csv(paste0(data_path, "cor_mat_convergent_domain_dat.csv")))


ma_agg <- list(overall = read_csv(paste0(data_path, "cor_mat_convergent_overall_dat_beh_agg.csv")),
               measure = read_csv(paste0(data_path, "cor_mat_convergent_measure_dat_beh_agg.csv")),
               domain = read_csv(paste0(data_path, "cor_mat_convergent_domain_dat_beh_agg.csv")))



ma_meas <- ma$domain %>% mutate(type = "FOUR domains")
ma_agg_meas <- ma_agg$domain %>% mutate(type = "ONE domain")
dt_meas <- bind_rows(ma_meas, ma_agg_meas) %>% 
  filter(domain_pair_lbl %like% "Beh")


dt_meas_lbl <- dt_meas %>% group_by(type) %>% summarise(cor_n = paste0("num of pairs = ", n()))


pA <- ggplot(dt_meas, aes(x = estimate, y = type)) + 
  ggdist::stat_halfeye(
    fill = "grey70",
    # adjust = .5, 
    # width = .6, 
    alpha = .9,
    .width = 0, 
    point_colour = NA
  ) + 
  geom_boxplot(
    color = "black",
    size = 0.3,
    width = .1, 
    fill = "white",
    outlier.shape = NA
  ) +
  geom_jitter(
    width = 0,
    height = .05,
    shape = 1,
    size = 1,
    stroke = .5,
    alpha = .95,
    color = "grey30"
  ) +
  geom_text(data = dt_meas_lbl, aes(y = type, label = cor_n, x = .3), vjust = -2, family = "Source Sans 3", size = 3.25, fontface = "italic") +
  scale_fill_manual(values =  c("FOUR domains" = "grey50", "ONE domain" = "#003d5b")) +
  scale_color_manual(values =  c("FOUR domains" = "#003d5b", "ONE domain" = "grey50")) +
  scale_x_continuous(limits = c(-.25, .5), breaks = seq(-0.25,.5,.25))+
  scale_y_discrete(expand = c(0.25,0)) +
  theme_minimal()+ 
  labs(y = "", x = "Intercorrelation") +
  theme(axis.text.x = element_markdown(angle = 0, family = "Source Sans 3",
                                       color = "black", size = 11),
        axis.text.y = element_markdown(angle = 0, family = "Source Sans 3",  face = "bold", hjust = 0,
                                       color = "black", size = 10),
        axis.title.y = element_markdown(family = "Source Sans 3", margin = margin(r = 10),
                                        color = "black", size = 11),
        text = element_text(family = "Source Sans 3", size = 10),
        panel.grid.major.y = element_blank(),
        # legend.position = c(.95,.95),
        legend.justification=c(1,0.75), 
        # legend.position=c(1,0.5),
        panel.background = element_rect(fill = NA, color = "grey40", size = .4),
        plot.tag = element_text(family = "Source Sans 3", face = "bold", size = 14),
        legend.text = element_text(family = "Source Sans 3", size = 8, vjust = .5),
        legend.title = element_text(family = "Source Sans 3",
                                    face = "bold", size = 9.5, margin = margin(b = 5)),
        legend.key.width = unit(.5, "cm"))


pA

ma_meas <- ma$measure %>% mutate(type = "FOUR domains")
ma_agg_meas <- ma_agg$measure %>% mutate(type = "ONE domain") %>% 
  select(meas_pair_lbl, estimate) %>% rename(estimate_one = estimate)

dt_meas <- ma_meas %>% left_join(ma_agg_meas, by = "meas_pair_lbl") %>% 
  mutate(meas_pair_lbl = str_replace(meas_pair_lbl, "_", "-")) %>% 
  filter(meas_pair_lbl %like% "Beh")


pB <- dt_meas %>%
  ggplot() +
  geom_segment( aes(x = estimate, y = reorder(meas_pair_lbl,desc((estimate))), 
                    yend =reorder(meas_pair_lbl,desc(estimate)), xend = estimate_one), linewidth = 3,color = "grey70") +
  geom_point(aes(x = estimate, y = reorder(meas_pair_lbl,desc(estimate)), fill = "FOUR Domains"), shape = 21, color = "grey20", size = 3, stroke = .75) +
  geom_point(aes(x = estimate_one, y = reorder(meas_pair_lbl,desc(estimate)), fill = "ONE Domain"), shape = 21, color = "grey20", size = 3, stroke = .75) +
  theme_minimal() +
  scale_fill_manual(values = c("ONE Domain" = "white", "FOUR Domains" = "black","Corrected Indv." = "black" )) +
  theme(plot.tag = element_text(family = "Source Sans 3", face = "bold", size = 14),
        legend.title = element_blank(),
        plot.subtitle = element_text(family = "Source Sans 3", size = 13),
        legend.text = element_text(family = "Source Sans 3", size = 9),
        axis.text.x = element_markdown(angle = 0, family = "Source Sans 3",
                                       color = "black", size = 11),
        axis.text.y = element_markdown(angle = 0, family = "Source Sans 3",  face = "bold", hjust = 0,
                                       color = "black", size = 10),
        axis.title.y = element_markdown(family = "Source Sans 3", margin = margin(r = 10),
                                        color = "black", size = 11),
        text = element_text(family = "Source Sans 3", size = 10),
        # legend.background = element_rect(fill = "white", color = NA),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(size = 18, face = "bold"),
        plot.caption = element_text(size = 5, hjust = .5),
        panel.background = element_rect(fill = NA, color = "grey40", size = .4),
        legend.position=c(.75,0.85) 
  ) +
  labs(x = "Intercorrelation", y = NULL, 
       fill = ""
  ) +
  guides(fill = guide_legend(override.aes = list(size = 2, stroke = .5,color = "grey20")))








p <- pA + theme(plot.margin = margin(r = 50)) +pB + plot_layout(widths = c(1,.4)) + plot_annotation(tag_levels = 'A')

p

ggsave(p, filename =  paste0(output_path,"converge_comp_categorization.png"),
       width = 30, height = 8, units = "cm")


# CONVERGENCE: ATTENUATION ------------------------------------------------
nlpar_dat <- read_csv("analysis/output/temp_stability/masc_nlpar_pred.csv") # mean_age = 0; prop_female_c = 0
melted_cormat <- read_csv("analysis/output/convergent_val/cor_mat_convergent_domain_dat.csv")


nlpar_dat <- nlpar_dat %>% 
  filter(nlpar == "Reliability" & categ == "domain") %>% 
  select(x,.epred, measure)%>% 
  mutate(measure = case_when(measure == "Propensity" ~"pro",
                             measure == "Frequency" ~"fre",
                             measure == "Behaviour"  ~ "beh")) 

# rename labels
dat_a <- nlpar_dat  %>% rename(rel_a = .epred, measure_category_a = measure,
                                    domain_name_a = x) %>% 
  mutate(x = name_lbl(measure = measure_category_a, domain = domain_name_a)) 
dat_b <- nlpar_dat  %>% rename(rel_b = .epred, measure_category_b = measure,
                                    domain_name_b = x) %>% 
  mutate(y = name_lbl(measure = measure_category_b, domain = domain_name_b))

# join new data
melted_cormat <- melted_cormat %>% 
  mutate(x = gsub("\\**", "", x),
         x = gsub("<br>", " - ", x),
         y = gsub("\\**", "", y),
         y = gsub("<br>", " - ", y)) %>% 
  left_join(dat_a, by = c("x")) %>% 
  left_join(dat_b, by = c("y")) %>% 
  rowwise() %>% 
  mutate(mean_rel = mean(c(rel_a, rel_b)),
         estimate_corrected = estimate/(sqrt(rel_a*rel_b))) %>% 
  ungroup() %>% 
  mutate(lbl_id = as.character(1:n())) %>% 
  mutate(got_beh = domain_pair_lbl %like% "Beh") %>% 
  mutate(got_beh_txt = if_else(got_beh == TRUE, "*", "")) %>% 
  mutate(estimate_corrected2 = .5*(estimate +estimate_corrected)) %>% 
  mutate(measure_category_a = abbrev_rev_lbl(measure_category_a),
         measure_category_b = abbrev_rev_lbl(measure_category_b)) %>% 
  mutate(meas_pair = paste0(measure_category_a, " - ",measure_category_b)) %>% 
  group_by(meas_pair) %>% 
  mutate(cn = n()) %>% 
  ungroup() %>% 
  mutate(meas_pair = paste0(meas_pair, "  (*",  cn, "*)"))


p_dumbell <- melted_cormat %>%
  mutate(domain_pair_id = as.character(lbl_id)) %>%
  ggplot() +
  geom_segment( aes(x = estimate_corrected, y = reorder(lbl_id,desc((estimate_corrected))),
                    yend =reorder(lbl_id,desc(estimate_corrected)), xend = estimate, color = mean_rel), linewidth = 1.5) +
  geom_point(aes(x = estimate, y = reorder(lbl_id,desc(estimate_corrected)), fill = "Uncorrected"), shape = 21, color = "grey20", size = 2.1, stroke = .75) +
  geom_point(aes(x = estimate_corrected, y = reorder(lbl_id,desc(estimate_corrected)), fill = "Corrected"), shape = 21, color = "grey80", size = 2.1, stroke = .25) +
  facet_wrap(.~meas_pair, scales = "free") +
  scale_shape_manual(values =c(10,1)) +
  scale_color_gradient2(low = "#0F4C5C", high = "white", mid = "grey85", 
                        na.value = 'grey70',
                        midpoint = 0.5,  limit = c(.1,.9), space = "Lab", 
                        name="Mean reliability") + 
  geom_vline(aes(xintercept = 0), color = "grey30", linetype = "dotted", linewidth =.5) + 
  geom_vline(aes(xintercept = 1), color = "grey30", linetype = "solid", linewidth = .5) +
  geom_vline(aes(xintercept = -1), color = "grey30", linetype = "solid", linewidth = .5) +
  theme_minimal() +
  scale_fill_manual(values = c("Uncorrected" = "white", "Corrected" = "black","Corrected Indv." = "black" )) +
  theme(text = element_text(family = "Source Sans 3", size = 13),
        title = element_text(family = "Source Sans 3", size = 15),
        legend.title = element_text(margin = margin(b = 5), face = "bold", size = 11),
        plot.subtitle = element_text(family = "Source Sans 3", size = 13),
        legend.text = element_text(family = "Source Sans 3", size = 11),
        strip.text = ggtext::element_markdown(size = 13, face = "bold"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 18, face = "bold"),
        plot.caption = element_text(size = 5, hjust = .5),
        panel.background = element_rect(fill = NA, color = "grey40", size = .4),
  ) +
  labs(x = "Intercorrelation", y = NULL, 
       fill = "Type of correlation",
       title = "Corrected vs. Uncorrected Intercorrelations for Attenuation due to Measurement Error",
  ) +
  scale_x_continuous(limits = c(-1.1,2.05), breaks = seq(-1,2,.5)) + # sec.axis = dup_axis(), 
  guides(fill = guide_legend(override.aes = list(size = 4, stroke = 1,color = "grey20"))) +
  guides(color = guide_coloursteps(show.limits = TRUE,  frame.colour = "grey50"))



p_dumbell


ggsave(p_dumbell, filename = paste0(output_path,"converge_comp_attenuation.png"),
       width = 40, height = 17, units = "cm")



# CONVERGENCE: VAR DECOMP RESULTS ITEM ------------------------------------

data_path <- c("analysis/output/convergent_val/") # where is the input file stored



t <- read_csv(paste0(data_path,"summary_shapley_values_intercor_item.csv")) %>% 
  filter(measure_category == "Omnibus")

boot_t <-  read_csv(paste0(data_path,"summary_shapley_values_intercor_item_boot.csv")) %>% 
  filter(measure_category == "Omnibus")


boot_t$categ_lbl <-  factor(boot_t$categ_lbl, levels= rev(c('Panel','Respondent','Measure')))
t$categ_lbl <-  factor(t$categ_lbl, levels= rev(c('Panel','Respondent','Measure')))

p_omni <- ggplot(boot_t, aes(x=reorder(x_lbl,m),y = m )) +
  geom_crossbar(aes(ymin = .lower_0.95, ymax = .upper_0.95), color = "NA", fill = "white", linewidth = .15,
                width = 0.3, alpha =  1) +
  geom_crossbar(aes(ymin = .lower_0.95, ymax = .upper_0.95), color = "NA", fill = "#184e77", linewidth = .15,
                width = 0.3, alpha =  .4) +
  geom_crossbar(aes(ymin = .lower_0.8, ymax = .upper_0.8), color = "NA", fill = "white",linewidth = .15,
                width = 0.3, alpha =  1) +
  geom_crossbar(aes(ymin = .lower_0.8, ymax = .upper_0.8), color = "NA", fill = "#184e77",linewidth = .15,
                width = 0.3, alpha =  .7) +
  geom_crossbar(aes(ymin =.lower_0.5, ymax = .upper_0.5), color = "NA", fill = "white",linewidth = .15,
                width = 0.3, alpha =  1) +
  geom_crossbar(aes(ymin =.lower_0.5, ymax = .upper_0.5), color = "NA", fill = "#184e77",linewidth = .15,
                width = 0.3, alpha =  1) +
  geom_point(data = t,
             aes(x = reorder(x_lbl,m), y = m),
             color = "grey30", alpha = 1,fill = "grey95",
             size = 1.5, shape = 21, stroke = .5) +
  labs(y = "Proportion of variance explained",
       fill = "", color = "", 
       title = "") +
  coord_flip() +
  theme_void() +
  facet_grid(categ_lbl~., space = "free", scales = "free", switch = "y") +
  # geom_segment(aes(y = 0, yend = .2, x = -Inf, xend = -Inf), color = "grey80", size = 0.15, linetype = "solid")+
  # scale_x_discrete(expand = c(0.0,0.0)) +
  scale_y_continuous(limits = c(0,.15), breaks = seq(0,.15,.05),expand = c(0.0,0.0),
                     labels = c("0", format(seq(.05,.15,.05), nsmall = 2))) +
  theme(axis.text.x  = element_text(family = "Source Sans 3", size = 10, colour = "grey20",
                                    margin = margin(t = 5, b = 5),
                                    hjust=seq(0,1, length.out = 5)),
        axis.text.y = element_text(family = "Source Sans 3", size = 9.5, colour = "grey20",
                                   hjust = 1 ,margin = margin(l = 5, r = 10)),
        axis.title.x = element_text(family = "Source Sans 3 Light", size = 9, colour = "grey20",margin = margin(b = 5)),
        # panel.grid.major.x  = element_line(color = "grey80", size = 0.25, linetype = "solid"),
        # panel.grid.minor.x  = element_line(color = "grey80", size = 0.25, linetype = "solid"),
        plot.tag =  element_blank(),
        
        panel.grid = element_blank(),
        panel.grid.major.x = element_line(linewidth = .25, color = "grey80", linetype = "dotted"),
        # panel.grid.major.y = element_line(linewidth = .25, color = "grey80"),
        panel.background = element_rect(color = "grey50", size = .25),
        strip.text = element_text(family = "Source Sans 3 Light", size = 9, angle = 90,colour = "grey20"),
        strip.placement = "outside",
        plot.margin = margin(l = 10, r = 10, t = 10),
        plot.title.position = "plot",
        plot.title = element_blank(),
        plot.caption = element_text(family = "Source Sans 3", size = 9, lineheight = .75),
        axis.ticks.length = unit(0, "cm"),
        legend.title = element_blank(),
        legend.text = element_text(family = "Source Sans 3", face= "bold",size = 12, colour = "grey20"),
        legend.title.align=0.5,
        legend.margin = margin(0),
        legend.position = "none",
        legend.direction='horizontal',
        legend.spacing.y = unit(0, 'cm'),
        legend.spacing.x = unit(1.0, 'cm'),
        legend.key.size = unit(0.75,"line")) 


p_omni
ggsave(plot = p_omni, filename = paste0(output_path, "shapley_decomp_convergent_item.png"),
       dpi = 300, width = 25, height = 10, units = "cm") 





