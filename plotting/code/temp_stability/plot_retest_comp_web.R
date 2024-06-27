
# DESCRIPTION -------------------------------------------------------------

# Script to plot additional figures showing test-retest correlation distributions for the
# companion website


# PACKAGES ---------------------------------------------------------------

library(tidyverse)
library(brms)
library(patchwork)
library(tidybayes)
library(geomtextpath)
library(ggtext)



# FUNCTIONS ---------------------------------------------------------------

source("helper_functions.R")


# FILES -------------------------------------------------------------------



output_path <- c("docs/images/") # where to store the output 
data_path <- c("processing/output/temp_stability/") # where is the data

data_file <- "complete_retest.csv"

# PLOTTING SKEWNESS -------------------------------------------------------


dat <-read_csv(paste0(data_path,data_file))

dat <- dat %>%  mutate(domain_name = rename_domain(domain_name))

dt <-  dat %>%
  filter(age_group != "10-90" &
           gender_group != "all" & 
           year_age_group == 10 & 
           n >= 30) %>% 
  rowwise() %>% 
  mutate(mean_sk = mean(c(skewness_t1, skewness_t2)),
         mean_coef_var = mean(c(coeff_var_t1, coeff_var_t2))) %>% 
  ungroup()


dt_lbl <- dt %>% group_by(measure_category, domain_name) %>% 
  summarise(o_median_sk = round(median(abs(mean_sk)),2),
            n_cor = format(n(), big.mark = "'"),
            u95 = round(quantile(abs(mean_sk), .975),2),
            l95 = round(quantile(abs(mean_sk), .025),2),
            x = max(abs(mean_sk)) - .5,
            lbl = paste0("*k*: ", n_cor, "<br>**Mdn:** ",o_median_sk, "[", l95, ", ", u95, "]"))



p_b <- dt %>% filter(measure_category == "beh") %>% 
  ggplot(aes(x = abs(mean_sk))) +
  geom_vline(xintercept = 0, linetype = "solid", linewidth = .25, color = "grey10")+
  geom_density(fill = "#e07f00", alpha = .3, color = "#e07f00", size = .25) + 
  ggtext::geom_richtext(data = dt_lbl%>% filter(measure_category == "beh") ,
                        fill = NA, label.color = NA, # remove background and outline
                        label.padding = grid::unit(rep(0, 4), "pt"), # remove padding
                        size = 3.5, hjust = 1, vjust = 2, family = "Source Sans 3",
                        aes(label = lbl, y = .65, x = 3.5)) +
  facet_wrap(.~domain_name, nrow = 1) + theme_minimal() +
  scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) +
  labs(x = "|Average skewness|", y = "density") +
  theme(strip.placement = "outside",
        strip.text.x = element_text(family = "Source Sans 3", size = 11.5, color = "grey20", face = "bold"),
        legend.position = "none", # c(0,0) bottom left, c(1,1) top-right.
        # legend.justification = c(0.5,0.5),
        legend.margin = margin(-.5,0,0,0, unit="cm"),
        panel.spacing.x = unit(.5, "cm"),
        legend.spacing.y = unit(0.15, 'cm'),
        legend.key.width = unit(.25, "cm"),
        legend.key.size = unit(.3, "cm"),
        legend.text = element_text(family = "Source Sans 3", size = 8.5, color = "grey20"),
        # legend.background = theme_rect(fill = "white", colour = NA),
        text = element_text(family = "Source Sans 3", size = 10, color = "grey40"),
        title = element_text(family = "Source Sans 3", size = 10, color = "grey20", face = "bold"),
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
p_b

p_f <- dt %>% filter(measure_category == "fre") %>% 
  ggplot(aes(x = abs(mean_sk)))+
  geom_vline(xintercept = 0, linetype = "solid", linewidth = .25, color = "grey10")+
  geom_density(fill = "#1e9bae", alpha = .3, color = "#1e9bae", size = .25) + 
  ggtext::geom_richtext(data = dt_lbl%>% filter(measure_category == "fre") ,
                        fill = NA, label.color = NA, # remove background and outline
                        label.padding = grid::unit(rep(0, 4), "pt"), # remove padding
                        size = 3.5, hjust = 1, vjust = 2, family = "Source Sans 3",
                        aes(label = lbl, y = 1.15, x = x)) +
  facet_wrap(.~domain_name, nrow = 2, scales = "free") + theme_minimal() +
  scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) +
  labs(x = "|Average skewness|", y = "density") +
  theme(strip.placement = "outside",
        strip.text.x = element_text(family = "Source Sans 3", size = 11.5, color = "grey20", face = "bold"),
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
        title = element_text(family = "Source Sans 3", size = 9, color = "grey20"),
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
p_f



p_p <- dt %>% filter(measure_category == "pro") %>% 
  ggplot(aes(x = abs(mean_sk))) +
  geom_vline(xintercept = 0, linetype = "solid", linewidth = .25, color = "grey10")+
  geom_density(fill = "#502a7a", alpha = .3, color = "#502a7a", size = .25) + 
  ggtext::geom_richtext(data = dt_lbl%>% filter(measure_category == "pro") ,
                        fill = NA, label.color = NA, # remove background and outline
                        label.padding = grid::unit(rep(0, 4), "pt"), # remove padding
                        size = 3.5, hjust = 1, vjust = 2, family = "Source Sans 3",
                        aes(label = lbl, y = 3, x = x)) +
  facet_wrap(.~domain_name, nrow = 3, scales = "free") + theme_minimal() +
  scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) +
  labs(x = "|Average skewness|", y = "density") +
  theme(strip.placement = "outside",
        strip.text.x = element_text(family = "Source Sans 3", size = 11.5, color = "grey20", face = "bold"),
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
        title = element_text(family = "Source Sans 3", size = 9, color = "grey20"),
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
p_p



ggsave(plot = p_p, filename = paste0(output_path, "skewness_resp_pro.png"),
       dpi = 300, width = 30, height = 20, units = "cm") 
ggsave(plot = p_b, filename = paste0(output_path, "skewness_resp_beh.png"),
       dpi = 300, width = 25, height = 7, units = "cm") 
ggsave(plot = p_f, filename = paste0(output_path, "skewness_resp_fre.png"),
       dpi = 300, width = 30, height = 15, units = "cm") 





# PLOTTING VAR DECOMPT CATEG LEVELS: OMNBIUS ---------------------------------------

dat <-read_csv(paste0(data_path,data_file))

dat <- dat %>%
  mutate(domain_name = rename_domain(domain_name),
         measure_category = case_when(measure_category == "pro"  ~ "Propensity",
                                      measure_category == "fre" ~ "Frequency",
                                      measure_category == "beh" ~ "Behaviour"),
         scale_type = if_else(!scale_type %in% c("dis","ord"), "oe-comp", "ord-dis"),
         scale_type = case_when(scale_type == "oe-comp"  ~ "Open-ended/Composite",
                                scale_type == "ord-dis" ~ "Ordinal/Discrete"))  %>%
  filter(age_group != "10-90" &
           gender_group != "all" & 
           year_age_group == 10 & 
           n >= 30) %>% 
  mutate(cor_pearson = if_else(cor_pearson < 0, 0, cor_pearson))





dt <-  dat

dt_lbl <- dt %>% 
  group_by(domain_name) %>% 
  summarise(lbl = paste0("k:", format(n(), big.mark = "'")),
            median_cor = median(cor_pearson),
            mean_cor = mean(cor_pearson))

p_dom <- dt %>%
  ggplot(aes(x = cor_pearson)) +
  # geom_vline(data = dt_lbl, aes(xintercept = mean_cor), linetype = "solid", linewidth = .25, color = "#003049")+
  # geom_vline(data = dt_lbl, aes(xintercept = median_cor), linetype = "solid", linewidth = .25, color = "#8093f1")+
  # geom_vline(xintercept = 0, linetype = "dotted", linewidth = .25, color = "grey40")+
  geom_point(data = dt_lbl, aes(x = median_cor, y = .1),fill = "#364958", size = 3, shape = 21, color = "white", stroke = .5)+
  geom_point(data = dt_lbl, aes(x = mean_cor, y = .1),fill = "#718355", size = 3, shape = 21, color = "white", stroke = .5)+
  geom_density(fill = "#23583A", alpha = .3, color = "#23583A", size = .25) + 
  ggtext::geom_richtext(data = dt_lbl,
                        fill = NA, label.color = NA, # remove background and outline
                        label.padding = grid::unit(rep(0, 4), "pt"), # remove padding
                        size = 3.5, hjust = 1, vjust = 2, family = "Source Sans 3",
                        aes(label = lbl, y = 3.25, x = 0.95)) +
  facet_wrap(.~domain_name, nrow = 2, scales = "free_x") + theme_minimal() +
  scale_x_continuous(expand = c(0,0), limits = c(0,1)) + scale_y_continuous(expand = c(0,0)) +
  labs(x = "Retest Correlation", y = "density") +
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
        plot.title.position = "plot",
        panel.grid = element_blank(),
        plot.margin = margin(b = 5, r = 5, l = 5),
        panel.background = element_rect(color = "grey75", fill = NA, size = .4))

p_dom
dt_lbl <- dt %>% 
  group_by(measure_category) %>% 
  summarise(lbl = paste0("k:", format(n(), big.mark = "'")),
            median_cor = median(cor_pearson),
            mean_cor = mean(cor_pearson))

p_categ <- dt %>%
  ggplot(aes(x = cor_pearson)) +
  # geom_vline(data = dt_lbl, aes(xintercept = mean_cor), linetype = "solid", linewidth = .25, color = "#003049")+
  # geom_vline(data = dt_lbl, aes(xintercept = median_cor), linetype = "solid", linewidth = .25, color = "#8093f1")+
  geom_vline(xintercept = 0, linetype = "dotted", linewidth = .25, color = "grey40")+
  geom_point(data = dt_lbl, aes(x = median_cor, y = .1),fill = "#364958", size = 3, shape = 21, color = "white", stroke = .5)+
  geom_point(data = dt_lbl, aes(x = mean_cor, y = .1),fill = "#718355", size = 3, shape = 21, color = "white", stroke = .5)+
  geom_density(fill = "#23583A", alpha = .3, color = "#23583A", size = .25) + 
  ggtext::geom_richtext(data = dt_lbl,
                        fill = NA, label.color = NA, # remove background and outline
                        label.padding = grid::unit(rep(0, 4), "pt"), # remove padding
                        size = 3.5, hjust = 1, vjust = 2, family = "Source Sans 3",
                        aes(label = lbl, y = 3.25, x = 0.95)) +
  facet_wrap(.~measure_category, nrow = 1, scales = "free_x") + theme_minimal() +
  scale_x_continuous(expand = c(0,0), limits = c(0,1)) + scale_y_continuous(expand = c(0,0)) +
  labs(x = "Retest Correlation", y = "density") +
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
  group_by(scale_type) %>% 
  summarise(lbl = paste0("k:", format(n(), big.mark = "'")),
            median_cor = median(cor_pearson),
            mean_cor = mean(cor_pearson))

p_scale <- dt %>%
  ggplot(aes(x = cor_pearson)) +
  # geom_vline(data = dt_lbl, aes(xintercept = mean_cor), linetype = "solid", linewidth = .25, color = "#003049")+
  # geom_vline(data = dt_lbl, aes(xintercept = median_cor), linetype = "solid", linewidth = .25, color = "#8093f1")+
  geom_vline(xintercept = 0, linetype = "dotted", linewidth = .25, color = "grey40")+
  geom_point(data = dt_lbl, aes(x = median_cor, y = .1),fill = "#364958", size = 3, shape = 21, color = "white", stroke = .5)+
  geom_point(data = dt_lbl, aes(x = mean_cor, y = .1),fill = "#718355", size = 3, shape = 21, color = "white", stroke = .5)+
  geom_density(fill = "#23583A", alpha = .3, color = "#23583A", size = .25) + 
  ggtext::geom_richtext(data = dt_lbl,
                        fill = NA, label.color = NA, # remove background and outline
                        label.padding = grid::unit(rep(0, 4), "pt"), # remove padding
                        size = 3.5, hjust = 1, vjust = 2, family = "Source Sans 3",
                        aes(label = lbl, y = 3.25, x = 0.95)) +
  facet_wrap(.~scale_type, nrow = 1, scales = "free_x") + theme_minimal() +
  scale_x_continuous(expand = c(0,0), limits = c(0,1)) + scale_y_continuous(expand = c(0,0)) +
  labs(x = "Retest Correlation", y = "density") +
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
  group_by(gender_group) %>% 
  summarise(lbl = paste0("k:", format(n(), big.mark = "'")),
            median_cor = median(cor_pearson),
            mean_cor = mean(cor_pearson))

p_gender <- dt %>%
  ggplot(aes(x = cor_pearson)) +
  # geom_vline(data = dt_lbl, aes(xintercept = mean_cor), linetype = "solid", linewidth = .25, color = "#003049")+
  # geom_vline(data = dt_lbl, aes(xintercept = median_cor), linetype = "solid", linewidth = .25, color = "#8093f1")+
  geom_vline(xintercept = 0, linetype = "dotted", linewidth = .25, color = "grey40")+
  geom_point(data = dt_lbl, aes(x = median_cor, y = .1),fill = "#364958", size = 3, shape = 21, color = "white", stroke = .5)+
  geom_point(data = dt_lbl, aes(x = mean_cor, y = .1),fill = "#718355", size = 3, shape = 21, color = "white", stroke = .5)+
  geom_density(fill = "#23583A", alpha = .3, color = "#23583A", size = .25) + 
  ggtext::geom_richtext(data = dt_lbl,
                        fill = NA, label.color = NA, # remove background and outline
                        label.padding = grid::unit(rep(0, 4), "pt"), # remove padding
                        size = 3.5, hjust = 1, vjust = 2, family = "Source Sans 3",
                        aes(label = lbl, y = 3.25, x = 0.95)) +
  facet_wrap(.~gender_group, nrow = 1, scales = "free_x") + theme_minimal() +
  scale_x_continuous(expand = c(0,0), limits = c(0,1)) + scale_y_continuous(expand = c(0,0)) +
  labs(x = "Retest Correlation", y = "density") +
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
  group_by(age_group) %>% 
  summarise(lbl = paste0("k:", format(n(), big.mark = "'")),
            median_cor = median(cor_pearson),
            mean_cor = mean(cor_pearson))

p_age <- dt %>%
  ggplot(aes(x = cor_pearson)) +
  # geom_vline(data = dt_lbl, aes(xintercept = mean_cor), linetype = "solid", linewidth = .25, color = "#003049")+
  # geom_vline(data = dt_lbl, aes(xintercept = median_cor), linetype = "solid", linewidth = .25, color = "#8093f1")+
  geom_vline(xintercept = 0, linetype = "dotted", linewidth = .25, color = "grey40")+
  geom_point(data = dt_lbl, aes(x = median_cor, y = .1),fill = "#364958", size = 3, shape = 21, color = "white", stroke = .5)+
  geom_point(data = dt_lbl, aes(x = mean_cor, y = .1),fill = "#718355", size = 3, shape = 21, color = "white", stroke = .5)+
  geom_density(fill = "#23583A", alpha = .3, color = "#23583A", size = .25) + 
  ggtext::geom_richtext(data = dt_lbl,
                        fill = NA, label.color = NA, # remove background and outline
                        label.padding = grid::unit(rep(0, 4), "pt"), # remove padding
                        size = 3.5, hjust = 1, vjust = 2, family = "Source Sans 3",
                        aes(label = lbl, y = 3.25, x = 0.95)) +
  facet_wrap(.~age_group, nrow = 2, scales = "free_x") + theme_minimal() +
  scale_x_continuous(expand = c(0,0), limits = c(0,1)) + scale_y_continuous(expand = c(0,0)) +
  labs(x = "Retest Correlation", y = "density") +
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
        panel.grid = element_line(linetype = "dotted", color = "grey85", size = .2),
        plot.title.position = "plot",
        plot.margin = margin(b = 5, r = 5, l = 5),
        panel.background = element_rect(color = "grey35", fill = NA, size = .25))
p_age


dt_lbl <- dt %>% 
  group_by(panel) %>% 
  summarise(lbl = paste0("k:", format(n(), big.mark = "'")),
            median_cor = median(cor_pearson),
            mean_cor = mean(cor_pearson))

p_panel <- dt %>%
  ggplot(aes(x = cor_pearson)) +
  # geom_vline(data = dt_lbl, aes(xintercept = mean_cor), linetype = "solid", linewidth = .25, color = "#003049")+
  # geom_vline(data = dt_lbl, aes(xintercept = median_cor), linetype = "solid", linewidth = .25, color = "#8093f1")+
  geom_vline(xintercept = 0, linetype = "dotted", linewidth = .25, color = "grey40")+
  geom_point(data = dt_lbl, aes(x = median_cor, y = .1),fill = "#364958", size = 3, shape = 21, color = "white", stroke = .5)+
  geom_point(data = dt_lbl, aes(x = mean_cor, y = .1),fill = "#718355", size = 3, shape = 21, color = "white", stroke = .5)+
  geom_density(fill = "#23583A", alpha = .3, color = "#23583A", size = .25) + 
  ggtext::geom_richtext(data = dt_lbl,
                        fill = NA, label.color = NA, # remove background and outline
                        label.padding = grid::unit(rep(0, 4), "pt"), # remove padding
                        size = 3.5, hjust = 1, vjust = 2, family = "Source Sans 3",
                        aes(label = lbl, y = 5.25, x = 0.95)) +
  facet_wrap(.~panel, nrow = 3, scales = "free_x") + theme_minimal() +
  scale_x_continuous(expand = c(0,0), limits = c(0,1)) + scale_y_continuous(expand = c(0,0)) +
  labs(x = "Retest Correlation", y = "density") +
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
        panel.grid = element_line(linetype = "dotted", color = "grey85", size = .2),
        plot.title.position = "plot",
        plot.margin = margin(b = 5, r = 5, l = 5),
        panel.background = element_rect(color = "grey35", fill = NA, size = .25))
p_panel




p_n <- dt %>% 
  ggplot(aes(x = n, y = cor_pearson))+
  stat_density_2d(geom = "point", aes(size =after_stat(density)), n = 100, contour = FALSE, 
                  color = "#cad2c5", fill = "#006466",   ##11693F
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
  labs(y = "Retest Correlation", x = "Sample Size (log10)")


p_n



p_time <-  dt %>% 
  ggplot(aes(x = time_diff_mean, y = cor_pearson))+
  stat_density_2d(geom = "point", aes(size =after_stat(density)), n = 100, contour = FALSE, 
                  color = "#cad2c5", fill = "#006466",   ##11693F
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
  labs(y = "Retest Correlation", x = "Retest Interval (Years)")

p_time



ggsave(plot = p_dom, filename = paste0(output_path, "domain_overview_omnibus.png"),
       dpi = 300, width = 30, height = 10, units = "cm") 
ggsave(plot = p_categ, filename = paste0(output_path, "categ_overview_omnibus.png"),
       dpi = 300, width = 25, height = 7, units = "cm") 
ggsave(plot = p_age, filename = paste0(output_path, "age_overview_omnibus.png"),
       dpi = 300, width = 30, height = 10, units = "cm") 
ggsave(plot = p_scale, filename = paste0(output_path, "scale_overview_omnibus.png"),
       dpi = 300, width = 25, height = 10, units = "cm") 
ggsave(plot = p_gender, filename = paste0(output_path, "gender_overview_omnibus.png"),
       dpi = 300, width = 25, height = 10, units = "cm")
ggsave(plot = p_panel, filename = paste0(output_path, "panel_overview_omnibus.png"),
       dpi = 300, width = 35, height = 30, units = "cm") 
ggsave(plot = p_n, filename = paste0(output_path, "n_overview_omnibus.png"),
       dpi = 300, width = 20, height = 10, units = "cm") 
ggsave(plot = p_time, filename = paste0(output_path, "time_overview_omnibus.png"),
       dpi = 300, width = 20, height = 10, units = "cm") 




# PLOTTING VAR DECOMPT CATEG LEVELS: PRO ---------------------------------------

dt <-  dat %>% filter(measure_category == "Propensity")

dt_lbl <- dt %>% 
  group_by(domain_name) %>% 
  summarise(lbl = paste0("k:", format(n(), big.mark = "'")),
            median_cor = median(cor_pearson),
            mean_cor = mean(cor_pearson))

p_dom <- dt %>%
  ggplot(aes(x = cor_pearson)) +
  # geom_vline(data = dt_lbl, aes(xintercept = mean_cor), linetype = "solid", linewidth = .25, color = "#003049")+
  # geom_vline(data = dt_lbl, aes(xintercept = median_cor), linetype = "solid", linewidth = .25, color = "#8093f1")+
  geom_vline(xintercept = 0, linetype = "dotted", linewidth = .25, color = "grey40")+
  geom_point(data = dt_lbl, aes(x = median_cor, y = .1),fill = "#502a7a", size = 3, shape = 21, color = "white", stroke = .5)+
  geom_point(data = dt_lbl, aes(x = mean_cor, y = .1),fill = "#A19AA9", size = 3, shape = 21, color = "white", stroke = .5)+
  geom_density(fill = "#502a7a", alpha = .3, color = "#502a7a", size = .25) + 
  ggtext::geom_richtext(data = dt_lbl,
                        fill = NA, label.color = NA, # remove background and outline
                        label.padding = grid::unit(rep(0, 4), "pt"), # remove padding
                        size = 3.5, hjust = 1, vjust = 2, family = "Source Sans 3",
                        aes(label = lbl, y = 3.25, x = 0.95)) +
  facet_wrap(.~domain_name, nrow = 3, scales = "free_x") + theme_minimal() +
  scale_x_continuous(expand = c(0,0), limits = c(0,1)) + scale_y_continuous(expand = c(0,0)) +
  labs(x = "Retest Correlation", y = "density") +
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
  group_by(scale_type) %>% 
  summarise(lbl = paste0("k:", format(n(), big.mark = "'")),
            median_cor = median(cor_pearson),
            mean_cor = mean(cor_pearson))

p_scale <- dt %>%
  ggplot(aes(x = cor_pearson)) +
  # geom_vline(data = dt_lbl, aes(xintercept = mean_cor), linetype = "solid", linewidth = .25, color = "#003049")+
  # geom_vline(data = dt_lbl, aes(xintercept = median_cor), linetype = "solid", linewidth = .25, color = "#8093f1")+
  geom_vline(xintercept = 0, linetype = "dotted", linewidth = .25, color = "grey40")+
  geom_point(data = dt_lbl, aes(x = median_cor, y = .1),fill = "#502a7a", size = 3, shape = 21, color = "white", stroke = .5)+
  geom_point(data = dt_lbl, aes(x = mean_cor, y = .1),fill = "#A19AA9", size = 3, shape = 21, color = "white", stroke = .5)+
  geom_density(fill = "#502a7a", alpha = .3, color = "#502a7a", size = .25) + 
  ggtext::geom_richtext(data = dt_lbl,
                        fill = NA, label.color = NA, # remove background and outline
                        label.padding = grid::unit(rep(0, 4), "pt"), # remove padding
                        size = 3.5, hjust = 1, vjust = 2, family = "Source Sans 3",
                        aes(label = lbl, y = 3.25, x = 0.95)) +
  facet_wrap(.~scale_type, nrow = 1, scales = "free_x") + theme_minimal() +
  scale_x_continuous(expand = c(0,0), limits = c(0,1)) + scale_y_continuous(expand = c(0,0)) +
  labs(x = "Retest Correlation", y = "density") +
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
  group_by(gender_group) %>% 
  summarise(lbl = paste0("k:", format(n(), big.mark = "'")),
            median_cor = median(cor_pearson),
            mean_cor = mean(cor_pearson))

p_gender <- dt %>%
  ggplot(aes(x = cor_pearson)) +
  # geom_vline(data = dt_lbl, aes(xintercept = mean_cor), linetype = "solid", linewidth = .25, color = "#003049")+
  # geom_vline(data = dt_lbl, aes(xintercept = median_cor), linetype = "solid", linewidth = .25, color = "#8093f1")+
  geom_vline(xintercept = 0, linetype = "dotted", linewidth = .25, color = "grey40")+
  geom_point(data = dt_lbl, aes(x = median_cor, y = .1),fill = "#502a7a", size = 3, shape = 21, color = "white", stroke = .5)+
  geom_point(data = dt_lbl, aes(x = mean_cor, y = .1),fill = "#A19AA9", size = 3, shape = 21, color = "white", stroke = .5)+
  geom_density(fill = "#502a7a", alpha = .3, color = "#502a7a", size = .25) + 
  ggtext::geom_richtext(data = dt_lbl,
                        fill = NA, label.color = NA, # remove background and outline
                        label.padding = grid::unit(rep(0, 4), "pt"), # remove padding
                        size = 3.5, hjust = 1, vjust = 2, family = "Source Sans 3",
                        aes(label = lbl, y = 3.25, x = 0.95)) +
  facet_wrap(.~gender_group, nrow = 1, scales = "free_x") + theme_minimal() +
  scale_x_continuous(expand = c(0,0), limits = c(0,1)) + scale_y_continuous(expand = c(0,0)) +
  labs(x = "Retest Correlation", y = "density") +
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
  group_by(age_group) %>% 
  summarise(lbl = paste0("k:", format(n(), big.mark = "'")),
            median_cor = median(cor_pearson),
            mean_cor = mean(cor_pearson))

p_age <- dt %>%
  ggplot(aes(x = cor_pearson)) +
  # geom_vline(data = dt_lbl, aes(xintercept = mean_cor), linetype = "solid", linewidth = .25, color = "#003049")+
  # geom_vline(data = dt_lbl, aes(xintercept = median_cor), linetype = "solid", linewidth = .25, color = "#8093f1")+
  geom_vline(xintercept = 0, linetype = "dotted", linewidth = .25, color = "grey40")+
  geom_point(data = dt_lbl, aes(x = median_cor, y = .1),fill = "#502a7a", size = 3, shape = 21, color = "white", stroke = .5)+
  geom_point(data = dt_lbl, aes(x = mean_cor, y = .1),fill = "#A19AA9", size = 3, shape = 21, color = "white", stroke = .5)+
  geom_density(fill = "#502a7a", alpha = .3, color = "#502a7a", size = .25) + 
  ggtext::geom_richtext(data = dt_lbl,
                        fill = NA, label.color = NA, # remove background and outline
                        label.padding = grid::unit(rep(0, 4), "pt"), # remove padding
                        size = 3.5, hjust = 1, vjust = 2, family = "Source Sans 3",
                        aes(label = lbl, y = 3.25, x = 0.95)) +
  facet_wrap(.~age_group, nrow = 2, scales = "free_x") + theme_minimal() +
  scale_x_continuous(expand = c(0,0), limits = c(0,1)) + scale_y_continuous(expand = c(0,0)) +
  labs(x = "Retest Correlation", y = "density") +
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
        panel.grid = element_line(linetype = "dotted", color = "grey85", size = .2),
        plot.title.position = "plot",
        plot.margin = margin(b = 5, r = 5, l = 5),
        panel.background = element_rect(color = "grey35", fill = NA, size = .25))
p_age


dt_lbl <- dt %>% 
  group_by(panel) %>% 
  summarise(lbl = paste0("k:", format(n(), big.mark = "'")),
            median_cor = median(cor_pearson),
            mean_cor = mean(cor_pearson))

p_panel <- dt %>%
  ggplot(aes(x = cor_pearson)) +
  # geom_vline(data = dt_lbl, aes(xintercept = mean_cor), linetype = "solid", linewidth = .25, color = "#003049")+
  # geom_vline(data = dt_lbl, aes(xintercept = median_cor), linetype = "solid", linewidth = .25, color = "#8093f1")+
  geom_vline(xintercept = 0, linetype = "dotted", linewidth = .25, color = "grey40")+
  geom_point(data = dt_lbl, aes(x = median_cor, y = .1),fill = "#502a7a", size = 3, shape = 21, color = "white", stroke = .5)+
  geom_point(data = dt_lbl, aes(x = mean_cor, y = .1),fill = "#A19AA9", size = 3, shape = 21, color = "white", stroke = .5)+
  geom_density(fill = "#502a7a", alpha = .3, color = "#502a7a", size = .25) + 
  ggtext::geom_richtext(data = dt_lbl,
                        fill = NA, label.color = NA, # remove background and outline
                        label.padding = grid::unit(rep(0, 4), "pt"), # remove padding
                        size = 3.5, hjust = 1, vjust = 2, family = "Source Sans 3",
                        aes(label = lbl, y = 5.25, x = 0.95)) +
  facet_wrap(.~panel, nrow = 4, scales = "free_x" ) + theme_minimal() +
  scale_x_continuous(expand = c(0,0), limits = c(0,1)) + scale_y_continuous(expand = c(0,0)) +
  labs(x = "Retest Correlation", y = "density") +
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
        panel.grid = element_line(linetype = "dotted", color = "grey85", size = .2),
        plot.title.position = "plot",
        plot.margin = margin(b = 5, r = 5, l = 5),
        panel.background = element_rect(color = "grey35", fill = NA, size = .25))

p_panel



p_n <- dt %>% 
  ggplot(aes(x = n, y = cor_pearson))+
  stat_density_2d(geom = "point", aes(size =after_stat(density)), n = 100, contour = FALSE, 
                  color = "#A19AA9", fill = "#502a7a",   ##11693F
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
  labs(y = "Retest Correlation", x = "Sample Size (log10)")


p_n



p_time <-  dt %>% 
  ggplot(aes(x = time_diff_mean, y = cor_pearson))+
  stat_density_2d(geom = "point", aes(size =after_stat(density)), n = 100, contour = FALSE, 
                  color = "#A19AA9", fill = "#502a7a",   ##11693F
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
  labs(y = "Retest Correlation", x = "Retest Interval (Years)")

p_time



ggsave(plot = p_dom, filename = paste0(output_path, "domain_overview_pro.png"),
       dpi = 300, width = 25, height = 15, units = "cm") 
ggsave(plot = p_age, filename = paste0(output_path, "age_overview_pro.png"),
       dpi = 300, width = 30, height = 15, units = "cm") 
ggsave(plot = p_scale, filename = paste0(output_path, "scale_overview_pro.png"),
       dpi = 300, width = 25, height = 10, units = "cm") 
ggsave(plot = p_gender, filename = paste0(output_path, "gender_overview_pro.png"),
       dpi = 300, width = 25, height = 10, units = "cm")
ggsave(plot = p_panel, filename = paste0(output_path, "panel_overview_pro.png"),
       dpi = 300, width = 35, height = 30, units = "cm") 
ggsave(plot = p_n, filename = paste0(output_path, "n_overview_pro.png"),
       dpi = 300, width = 20, height = 10, units = "cm") 
ggsave(plot = p_time, filename = paste0(output_path, "time_overview_pro.png"),
       dpi = 300, width = 20, height = 10, units = "cm") 

# PLOTTING VAR DECOMPT CATEG LEVELS: FRE ---------------------------------------

dt <-  dat %>% filter(measure_category == "Frequency")

dt_lbl <- dt %>% 
  group_by(domain_name) %>% 
  summarise(lbl = paste0("k:", format(n(), big.mark = "'")),
            median_cor = median(cor_pearson),
            mean_cor = mean(cor_pearson))

p_dom <- dt %>%
  ggplot(aes(x = cor_pearson)) +
  # geom_vline(data = dt_lbl, aes(xintercept = mean_cor), linetype = "solid", linewidth = .25, color = "#003049")+
  # geom_vline(data = dt_lbl, aes(xintercept = median_cor), linetype = "solid", linewidth = .25, color = "#8093f1")+
  geom_vline(xintercept = 0, linetype = "dotted", linewidth = .25, color = "grey40")+
  geom_point(data = dt_lbl, aes(x = median_cor, y = .1),fill = "#023047", size = 3, shape = 21, color = "white", stroke = .5)+
  geom_point(data = dt_lbl, aes(x = mean_cor, y = .1),fill = "#1e9bae", size = 3, shape = 21, color = "white", stroke = .5)+
  geom_density(fill = "#1e9bae", alpha = .3, color = "#1e9bae", size = .25) + 
  ggtext::geom_richtext(data = dt_lbl,
                        fill = NA, label.color = NA, # remove background and outline
                        label.padding = grid::unit(rep(0, 4), "pt"), # remove padding
                        size = 3.5, hjust = 1, vjust = 2, family = "Source Sans 3",
                        aes(label = lbl, y = 3.25, x = 0.95)) +
  facet_wrap(.~domain_name, nrow = 2) + theme_minimal() +
  scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) +
  labs(x = "Retest Correlation", y = "density") +
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
  group_by(scale_type) %>% 
  summarise(lbl = paste0("k:", format(n(), big.mark = "'")),
            median_cor = median(cor_pearson),
            mean_cor = mean(cor_pearson))

p_scale <- dt %>%
  ggplot(aes(x = cor_pearson)) +
  # geom_vline(data = dt_lbl, aes(xintercept = mean_cor), linetype = "solid", linewidth = .25, color = "#003049")+
  # geom_vline(data = dt_lbl, aes(xintercept = median_cor), linetype = "solid", linewidth = .25, color = "#8093f1")+
  geom_vline(xintercept = 0, linetype = "dotted", linewidth = .25, color = "grey40")+
  geom_point(data = dt_lbl, aes(x = median_cor, y = .1),fill = "#023047", size = 3, shape = 21, color = "white", stroke = .5)+
  geom_point(data = dt_lbl, aes(x = mean_cor, y = .1),fill = "#1e9bae", size = 3, shape = 21, color = "white", stroke = .5)+
  geom_density(fill = "#1e9bae", alpha = .3, color = "#1e9bae", size = .25) + 
  ggtext::geom_richtext(data = dt_lbl,
                        fill = NA, label.color = NA, # remove background and outline
                        label.padding = grid::unit(rep(0, 4), "pt"), # remove padding
                        size = 3.5, hjust = 1, vjust = 2, family = "Source Sans 3",
                        aes(label = lbl, y = 3.25, x = 0.95)) +
  facet_wrap(.~scale_type, nrow = 1) + theme_minimal() +
  scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) +
  labs(x = "Retest Correlation", y = "density") +
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
  group_by(gender_group) %>% 
  summarise(lbl = paste0("k:", format(n(), big.mark = "'")),
            median_cor = median(cor_pearson),
            mean_cor = mean(cor_pearson))

p_gender <- dt %>%
  ggplot(aes(x = cor_pearson)) +
  # geom_vline(data = dt_lbl, aes(xintercept = mean_cor), linetype = "solid", linewidth = .25, color = "#003049")+
  # geom_vline(data = dt_lbl, aes(xintercept = median_cor), linetype = "solid", linewidth = .25, color = "#8093f1")+
  geom_vline(xintercept = 0, linetype = "dotted", linewidth = .25, color = "grey40")+
  geom_point(data = dt_lbl, aes(x = median_cor, y = .1),fill = "#023047", size = 3, shape = 21, color = "white", stroke = .5)+
  geom_point(data = dt_lbl, aes(x = mean_cor, y = .1),fill = "#1e9bae", size = 3, shape = 21, color = "white", stroke = .5)+
  geom_density(fill = "#1e9bae", alpha = .3, color = "#1e9bae", size = .25) +  
  ggtext::geom_richtext(data = dt_lbl,
                        fill = NA, label.color = NA, # remove background and outline
                        label.padding = grid::unit(rep(0, 4), "pt"), # remove padding
                        size = 3.5, hjust = 1, vjust = 2, family = "Source Sans 3",
                        aes(label = lbl, y = 3.25, x = 0.95)) +
  facet_wrap(.~gender_group, nrow = 1) + theme_minimal() +
  scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) +
  labs(x = "Retest Correlation", y = "density") +
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
  group_by(age_group) %>% 
  summarise(lbl = paste0("k:", format(n(), big.mark = "'")),
            median_cor = median(cor_pearson),
            mean_cor = mean(cor_pearson))

p_age <- dt %>%
  ggplot(aes(x = cor_pearson)) +
  # geom_vline(data = dt_lbl, aes(xintercept = mean_cor), linetype = "solid", linewidth = .25, color = "#003049")+
  # geom_vline(data = dt_lbl, aes(xintercept = median_cor), linetype = "solid", linewidth = .25, color = "#8093f1")+
  geom_vline(xintercept = 0, linetype = "dotted", linewidth = .25, color = "grey40")+
  geom_point(data = dt_lbl, aes(x = median_cor, y = .1),fill = "#023047", size = 3, shape = 21, color = "white", stroke = .5)+
  geom_point(data = dt_lbl, aes(x = mean_cor, y = .1),fill = "#1e9bae", size = 3, shape = 21, color = "white", stroke = .5)+
  geom_density(fill = "#1e9bae", alpha = .3, color = "#1e9bae", size = .25) + 
  ggtext::geom_richtext(data = dt_lbl,
                        fill = NA, label.color = NA, # remove background and outline
                        label.padding = grid::unit(rep(0, 4), "pt"), # remove padding
                        size = 3.5, hjust = 1, vjust = 2, family = "Source Sans 3",
                        aes(label = lbl, y = 3.25, x = 0.95)) +
  facet_wrap(.~age_group, nrow = 2) + theme_minimal() +
  scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) +
  labs(x = "Retest Correlation", y = "density") +
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
        panel.grid = element_line(linetype = "dotted", color = "grey85", size = .2),
        plot.title.position = "plot",
        plot.margin = margin(b = 5, r = 5, l = 5),
        panel.background = element_rect(color = "grey35", fill = NA, size = .25))
p_age


dt_lbl <- dt %>% 
  group_by(panel) %>% 
  summarise(lbl = paste0("k:", format(n(), big.mark = "'")),
            median_cor = median(cor_pearson),
            mean_cor = mean(cor_pearson))

p_panel <- dt %>%
  ggplot(aes(x = cor_pearson)) +
  # geom_vline(data = dt_lbl, aes(xintercept = mean_cor), linetype = "solid", linewidth = .25, color = "#003049")+
  # geom_vline(data = dt_lbl, aes(xintercept = median_cor), linetype = "solid", linewidth = .25, color = "#8093f1")+
  geom_vline(xintercept = 0, linetype = "dotted", linewidth = .25, color = "grey40")+
  geom_point(data = dt_lbl, aes(x = median_cor, y = .1),fill = "#023047", size = 3, shape = 21, color = "white", stroke = .5)+
  geom_point(data = dt_lbl, aes(x = mean_cor, y = .1),fill = "#1e9bae", size = 3, shape = 21, color = "white", stroke = .5)+
  geom_density(fill = "#1e9bae", alpha = .3, color = "#1e9bae", size = .25) + 
  ggtext::geom_richtext(data = dt_lbl,
                        fill = NA, label.color = NA, # remove background and outline
                        label.padding = grid::unit(rep(0, 4), "pt"), # remove padding
                        size = 3.5, hjust = 1, vjust = 2, family = "Source Sans 3",
                        aes(label = lbl, y = 5.25, x = 0.95)) +
  facet_wrap(.~panel, nrow = ) + theme_minimal() +
  scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) +
  labs(x = "Retest Correlation", y = "density") +
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
        panel.grid = element_line(linetype = "dotted", color = "grey85", size = .2),
        plot.title.position = "plot",
        plot.margin = margin(b = 5, r = 5, l = 5),
        panel.background = element_rect(color = "grey35", fill = NA, size = .25))
p_panel




p_n <- dt %>% 
  ggplot(aes(x = n, y = cor_pearson))+
  stat_density_2d(geom = "point", aes(size =after_stat(density)), n = 100, contour = FALSE, 
                  fill = "#1e9bae", color = "#023047",   ##11693F
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
  labs(y = "Retest Correlation", x = "Sample Size (log10)")


p_n



p_time <-  dt %>% 
  ggplot(aes(x = time_diff_mean, y = cor_pearson))+
  stat_density_2d(geom = "point", aes(size =after_stat(density)), n = 100, contour = FALSE, 
                  fill = "#1e9bae", color = "#023047",   ##11693F
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
  labs(y = "Retest Correlation", x = "Retest Interval (Years)")

p_time






ggsave(plot = p_dom, filename = paste0(output_path, "domain_overview_fre.png"),
       dpi = 300, width = 25, height = 15, units = "cm") 
ggsave(plot = p_age, filename = paste0(output_path, "age_overview_fre.png"),
       dpi = 300, width = 30, height = 15, units = "cm") 
ggsave(plot = p_scale, filename = paste0(output_path, "scale_overview_fre.png"),
       dpi = 300, width = 25, height = 10, units = "cm") 
ggsave(plot = p_gender, filename = paste0(output_path, "gender_overview_fre.png"),
       dpi = 300, width = 25, height = 10, units = "cm")
ggsave(plot = p_panel, filename = paste0(output_path, "panel_overview_fre.png"),
       dpi = 300, width = 35, height = 30, units = "cm") 
ggsave(plot = p_n, filename = paste0(output_path, "n_overview_fre.png"),
       dpi = 300, width = 20, height = 10, units = "cm") 
ggsave(plot = p_time, filename = paste0(output_path, "time_overview_fre.png"),
       dpi = 300, width = 20, height = 10, units = "cm") 








# PLOTTING VAR DECOMPT CATEG LEVELS: BEH ---------------------------------------

dt <-  dat %>% filter(measure_category == "Behaviour")

dt_lbl <- dt %>% 
  group_by(domain_name) %>% 
  summarise(lbl = paste0("k:", format(n(), big.mark = "'")),
            median_cor = median(cor_pearson),
            mean_cor = mean(cor_pearson))

p_dom <- dt %>%
  ggplot(aes(x = cor_pearson)) +
  # geom_vline(data = dt_lbl, aes(xintercept = mean_cor), linetype = "solid", linewidth = .25, color = "#003049")+
  # geom_vline(data = dt_lbl, aes(xintercept = median_cor), linetype = "solid", linewidth = .25, color = "#8093f1")+
  geom_vline(xintercept = 0, linetype = "dotted", linewidth = .25, color = "grey40")+
  geom_point(data = dt_lbl, aes(x = median_cor, y = .1),fill = "#3D2300", size = 3, shape = 21, color = "white", stroke = .5)+
  geom_point(data = dt_lbl, aes(x = mean_cor, y = .1),fill = "#CC7400", size = 3, shape = 21, color = "white", stroke = .5)+
  geom_density(fill = "#e07f00", alpha = .3, color = "#e07f00", size = .25) + 
  ggtext::geom_richtext(data = dt_lbl,
                        fill = NA, label.color = NA, # remove background and outline
                        label.padding = grid::unit(rep(0, 4), "pt"), # remove padding
                        size = 3.5, hjust = 1, vjust = 2, family = "Source Sans 3",
                        aes(label = lbl, y = 3.25, x = 0.75)) +
  facet_wrap(.~domain_name, nrow = 1) + theme_minimal() +
  scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) +
  labs(x = "Retest Correlation", y = "density") +
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
  group_by(scale_type) %>% 
  summarise(lbl = paste0("k:", format(n(), big.mark = "'")),
            median_cor = median(cor_pearson),
            mean_cor = mean(cor_pearson))

p_scale <- dt %>%
  ggplot(aes(x = cor_pearson)) +
  # geom_vline(data = dt_lbl, aes(xintercept = mean_cor), linetype = "solid", linewidth = .25, color = "#003049")+
  # geom_vline(data = dt_lbl, aes(xintercept = median_cor), linetype = "solid", linewidth = .25, color = "#8093f1")+
  geom_vline(xintercept = 0, linetype = "dotted", linewidth = .25, color = "grey40")+
  geom_point(data = dt_lbl, aes(x = median_cor, y = .1),fill = "#3D2300", size = 3, shape = 21, color = "white", stroke = .5)+
  geom_point(data = dt_lbl, aes(x = mean_cor, y = .1),fill = "#CC7400", size = 3, shape = 21, color = "white", stroke = .5)+
  geom_density(fill = "#e07f00", alpha = .3, color = "#e07f00", size = .25) + 
  ggtext::geom_richtext(data = dt_lbl,
                        fill = NA, label.color = NA, # remove background and outline
                        label.padding = grid::unit(rep(0, 4), "pt"), # remove padding
                        size = 3.5, hjust = 1, vjust = 2, family = "Source Sans 3",
                        aes(label = lbl, y = 3.25, x = 0.75)) +
  facet_wrap(.~scale_type, nrow = 1) + theme_minimal() +
  scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) +
  labs(x = "Retest Correlation", y = "density") +
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
  group_by(gender_group) %>% 
  summarise(lbl = paste0("k:", format(n(), big.mark = "'")),
            median_cor = median(cor_pearson),
            mean_cor = mean(cor_pearson))

p_gender <- dt %>%
  ggplot(aes(x = cor_pearson)) +
  # geom_vline(data = dt_lbl, aes(xintercept = mean_cor), linetype = "solid", linewidth = .25, color = "#003049")+
  # geom_vline(data = dt_lbl, aes(xintercept = median_cor), linetype = "solid", linewidth = .25, color = "#8093f1")+
  geom_vline(xintercept = 0, linetype = "dotted", linewidth = .25, color = "grey40")+
  geom_point(data = dt_lbl, aes(x = median_cor, y = .1),fill = "#3D2300", size = 3, shape = 21, color = "white", stroke = .5)+
  geom_point(data = dt_lbl, aes(x = mean_cor, y = .1),fill = "#CC7400", size = 3, shape = 21, color = "white", stroke = .5)+
  geom_density(fill = "#e07f00", alpha = .3, color = "#e07f00", size = .25) + 
  ggtext::geom_richtext(data = dt_lbl,
                        fill = NA, label.color = NA, # remove background and outline
                        label.padding = grid::unit(rep(0, 4), "pt"), # remove padding
                        size = 3.5, hjust = 1, vjust = 2, family = "Source Sans 3",
                        aes(label = lbl, y = 3.25, x = 0.75)) +
  facet_wrap(.~gender_group, nrow = 1) + theme_minimal() +
  scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) +
  labs(x = "Retest Correlation", y = "density") +
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
  group_by(age_group) %>% 
  summarise(lbl = paste0("k:", format(n(), big.mark = "'")),
            median_cor = median(cor_pearson),
            mean_cor = mean(cor_pearson))

p_age <- dt %>%
  ggplot(aes(x = cor_pearson)) +
  # geom_vline(data = dt_lbl, aes(xintercept = mean_cor), linetype = "solid", linewidth = .25, color = "#003049")+
  # geom_vline(data = dt_lbl, aes(xintercept = median_cor), linetype = "solid", linewidth = .25, color = "#8093f1")+
  geom_vline(xintercept = 0, linetype = "dotted", linewidth = .25, color = "grey40")+
  geom_point(data = dt_lbl, aes(x = median_cor, y = .1),fill = "#3D2300", size = 3, shape = 21, color = "white", stroke = .5)+
  geom_point(data = dt_lbl, aes(x = mean_cor, y = .1),fill = "#CC7400", size = 3, shape = 21, color = "white", stroke = .5)+
  geom_density(fill = "#e07f00", alpha = .3, color = "#e07f00", size = .25) + 
  ggtext::geom_richtext(data = dt_lbl,
                        fill = NA, label.color = NA, # remove background and outline
                        label.padding = grid::unit(rep(0, 4), "pt"), # remove padding
                        size = 3.5, hjust = 1, vjust = 2, family = "Source Sans 3",
                        aes(label = lbl, y = 3.25, x = 0.75)) +
  facet_wrap(.~age_group, nrow = 2) + theme_minimal() +
  scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) +
  labs(x = "Retest Correlation", y = "density") +
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
        panel.grid = element_line(linetype = "dotted", color = "grey85", size = .2),
        plot.title.position = "plot",
        plot.margin = margin(b = 5, r = 5, l = 5),
        panel.background = element_rect(color = "grey35", fill = NA, size = .25))
p_age


dt_lbl <- dt %>% 
  group_by(panel) %>% 
  summarise(lbl = paste0("k:", format(n(), big.mark = "'")),
            median_cor = median(cor_pearson),
            mean_cor = mean(cor_pearson))

p_panel <- dt %>%
  ggplot(aes(x = cor_pearson)) +
  # geom_vline(data = dt_lbl, aes(xintercept = mean_cor), linetype = "solid", linewidth = .25, color = "#003049")+
  # geom_vline(data = dt_lbl, aes(xintercept = median_cor), linetype = "solid", linewidth = .25, color = "#8093f1")+
  geom_vline(xintercept = 0, linetype = "dotted", linewidth = .25, color = "grey40")+
  geom_point(data = dt_lbl, aes(x = median_cor, y = .1),fill = "#3D2300", size = 3, shape = 21, color = "white", stroke = .5)+
  geom_point(data = dt_lbl, aes(x = mean_cor, y = .1),fill = "#CC7400", size = 3, shape = 21, color = "white", stroke = .5)+
  geom_density(fill = "#e07f00", alpha = .3, color = "#e07f00", size = .25) + 
  ggtext::geom_richtext(data = dt_lbl,
                        fill = NA, label.color = NA, # remove background and outline
                        label.padding = grid::unit(rep(0, 4), "pt"), # remove padding
                        size = 3.5, hjust = 1, vjust = 2, family = "Source Sans 3",
                        aes(label = lbl, y = 5.25, x = 0.75)) +
  facet_wrap(.~panel, nrow = ) + theme_minimal() +
  scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) +
  labs(x = "Retest Correlation", y = "density") +
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
        panel.grid = element_line(linetype = "dotted", color = "grey85", size = .2),
        plot.title.position = "plot",
        plot.margin = margin(b = 5, r = 5, l = 5),
        panel.background = element_rect(color = "grey35", fill = NA, size = .25))
p_panel




p_n <- dt %>% 
  ggplot(aes(x = n, y = cor_pearson))+
  stat_density_2d(geom = "point", aes(size =after_stat(density)), n = 100, contour = FALSE, 
                  fill = "#e07f00", color = "grey95",   ##11693F
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
  labs(y = "Retest Correlation", x = "Sample Size (log10)")


p_n



p_time <-  dt %>% 
  ggplot(aes(x = time_diff_mean, y = cor_pearson))+
  stat_density_2d(geom = "point", aes(size =after_stat(density)), n = 100, contour = FALSE, 
                  fill = "#e07f00", color = "grey95",   ##11693F
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
  labs(y = "Retest Correlation", x = "Retest Interval (Years)")

p_time



ggsave(plot = p_dom, filename = paste0(output_path, "domain_overview_beh.png"),
       dpi = 300, width = 25, height = 10, units = "cm") 
ggsave(plot = p_age, filename = paste0(output_path, "age_overview_beh.png"),
       dpi = 300, width = 30, height = 15, units = "cm") 
ggsave(plot = p_scale, filename = paste0(output_path, "scale_overview_beh.png"),
       dpi = 300, width = 20, height = 7, units = "cm") 
ggsave(plot = p_gender, filename = paste0(output_path, "gender_overview_beh.png"),
       dpi = 300, width = 20, height = 7, units = "cm")
ggsave(plot = p_panel, filename = paste0(output_path, "panel_overview_beh.png"),
       dpi = 300, width = 35, height = 20, units = "cm") 
ggsave(plot = p_n, filename = paste0(output_path, "n_overview_beh.png"),
       dpi = 300, width = 20, height = 10, units = "cm")  
ggsave(plot = p_time, filename = paste0(output_path, "time_overview_beh.png"),
       dpi = 300, width = 20, height = 10, units = "cm") 


