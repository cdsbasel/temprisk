
# DESCRIPTION -------------------------------------------------------------

# In this script we plot the number of inter-correlation coefficients
# available based on different thresholds/data configurations. Plot added to the 
# companion website

# PACKAGES ---------------------------------------------------------------

library(tidyverse)
library(data.table)

# INTERCOR FILES  ---------------------------------------------------

data_path <- c("processing/output/convergent_val/") # where is the input file stored
output_path <- c("docs/images/") # where to store output
intercor_file <- "complete_intercor.csv" # name of merged retest data 


dat <- read_csv(paste0(data_path,intercor_file))
dat <-  dat %>%  filter(age_group != "10-90" & gender_group != "all") 

dat_agg <- NULL

for (curr_yr in c(5,10,20)) {
  
  dt <- fread(paste0(data_path,"complete_agg_intercor_yb", curr_yr, ".csv"))
  
  dt <- dt %>% 
    filter(age_group != "10-90" &
             gender_group != "all" & 
             data_transform == "none" &
             rho_val == .5 &
             cor_metric == "pearson") %>% 
    group_by(age_group, min_n, age_bin) %>% 
    summarise(n_cor = n()) %>% 
    mutate(year_age_group = paste0(age_bin,"-year bins"),
           min_n = paste0("Min n: ", min_n))
  
  
  dat_agg <- bind_rows(dt, dat_agg)
  
  
}



# ORIGINAL INTER-CORRELATIONS ------------------------------------------------------------


dat_thrshA <- dat %>% 
  group_by(age_group, year_age_group) %>% 
  summarise(n_thresh_30 = sum(n >= 30),
            n_thresh_100 = sum(n >= 100),
            n_thresh_250 = sum(n >= 250)) %>% 
  pivot_longer(n_thresh_30:n_thresh_250, names_to = "threshold", values_to = "n_cor") %>% 
  rowwise() %>% 
  mutate(year_age_group = paste0(year_age_group, "-year bins"),
         threshold = gsub("n_thresh_", "Min n: ", threshold)) %>% 
  ungroup()

dat_thrshB <- dat_thrshA %>% filter(grepl("30|250", threshold)) %>% 
  pivot_wider(names_from = "threshold", values_from = "n_cor")


dat_thrshA$year_age_group <- factor(dat_thrshA$year_age_group, levels = c("5-year bins",  "10-year bins", "20-year bins"))
dat_thrshA$threshold <- factor(dat_thrshA$threshold, levels = c("Min n: 30",  "Min n: 100", "Min n: 250"))
dat_thrshB$year_age_group <- factor(dat_thrshB$year_age_group, levels = c("5-year bins",  "10-year bins", "20-year bins"))


p <- dat_thrshA %>% 
  ggplot(aes(x = n_cor, y = age_group)) +
  geom_segment(data = dat_thrshB, aes(x = `Min n: 250`, y = age_group, xend = `Min n: 30`, yend = age_group),
               color = "white", alpha = 1, size = 2) +
  geom_segment(data = dat_thrshB, aes(x = `Min n: 250`, y = age_group, xend = `Min n: 30`, yend = age_group),
               color = "grey85", alpha = .8, size = 2) +
  geom_point(size = 3, shape = 21, stroke = .55, aes(fill = threshold), color = "black") +
  theme_minimal() +
  labs(x = "Number of correlations",
       y = "",
       fill = "") +
  facet_grid(year_age_group~., scales = "free_y",space = "free_y", switch = "y") +
  scale_x_continuous(limits = c(0,26000),
                     expand = c(0.01,0.01),
                     breaks = seq(0,26000,2000), 
                     labels = scales::label_comma(big.mark = "'")) +
  scale_fill_manual(values = rev(c("#0C4A7A", "#5EB496", "#f0f3bd"))) +
  theme(text = element_text(family = "Source Sans 3"),
        title = element_text(family = "Source Sans 3", face = "bold"),
        panel.grid.minor.x = element_blank(),
        strip.placement = "outside",
        legend.position = c(.9,.9),
        legend.title = element_blank(),
        legend.spacing.x = unit(0, units = "cm"),
        legend.margin = margin(r = 5, b = 5),
        legend.background = element_rect(fill = "white", color = "grey30", size = .25),
        panel.spacing.y = unit(.25, units = "cm"),
        panel.border = element_rect(fill = NA, color = "grey50"),
        plot.caption = element_text(family = "Source Sans 3 Medium", face = "italic",size = 7, margin = margin(t = 10)),
        plot.margin = margin(r = 15),
        strip.text = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 9),
        axis.title.x = element_text(size = 8)) +
  guides(fill = guide_legend(title.position = "top", label.position = "right", ncol = 1))

p

ggsave(filename = paste0(output_path,"threshold_intercor_plot.png"), plot = p, height = 15, width = 25, units = "cm", dpi = 600)








# AGGREGATED INTER-CORRELATIONS ---------------------------------------------------

dat_aggB <- dat_agg %>% filter(grepl("30|250", min_n)) %>% 
  pivot_wider(names_from = "min_n", values_from = "n_cor")


dat_agg$year_age_group <- factor(dat_agg$year_age_group, levels = c("5-year bins",  "10-year bins", "20-year bins"))
dat_agg$min_n <- factor(dat_agg$min_n, levels = c("Min n: 30",  "Min n: 100", "Min n: 250"))

dat_aggB$year_age_group <- factor(dat_aggB$year_age_group, levels = c("5-year bins",  "10-year bins", "20-year bins"))

p <- dat_agg %>% 
  ggplot(aes(x = n_cor, y = age_group)) +
  geom_segment(data = dat_aggB, aes(x = `Min n: 250`, y = age_group, xend = `Min n: 30`, yend = age_group),
               color = "white", alpha = 1, size = 2) +
  geom_segment(data = dat_aggB, aes(x = `Min n: 250`, y = age_group, xend = `Min n: 30`, yend = age_group),
               color = "grey85", alpha = .8, size = 2) +
  geom_point(size = 3, shape = 21, stroke = .55, aes(fill = min_n), color = "black") +
  theme_minimal() +
  labs(x = "Number of aggregated correlations",
       y = "",
       fill = "") +
  facet_grid(year_age_group~., scales = "free_y",space = "free_y", switch = "y") +
  scale_x_continuous(limits = c(0,1250),
                     expand = c(0.01,0.01),
                     breaks = seq(0,1250,250), 
                     labels = scales::label_comma(big.mark = "'")) +
  scale_fill_manual(values = rev(c("#0C4A7A", "#5EB496", "#f0f3bd"))) +
  theme(text = element_text(family = "Source Sans 3"),
        title = element_text(family = "Source Sans 3", face = "bold"),
        panel.grid.minor.x = element_blank(),
        strip.placement = "outside",
        legend.position = c(.95,.9),
        legend.title = element_blank(),
        legend.spacing.x = unit(0, units = "cm"),
        legend.margin = margin(r = 5, b = 5),
        legend.background = element_rect(fill = "white", color = "grey30", size = .25),
        panel.spacing.y = unit(.25, units = "cm"),
        panel.border = element_rect(fill = NA, color = "grey50"),
        plot.caption = element_text(family = "Source Sans 3 Medium", face = "italic",size = 7, margin = margin(t = 10)),
        plot.margin = margin(r = 15),
        strip.text = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 9),
        axis.title.x = element_text(size = 8)) +
  guides(fill = guide_legend(title.position = "top", label.position = "right", ncol = 1))

p



ggsave(filename = paste0(output_path,"threshold_intercor_agg_plot.png"), plot = p, height = 15, width = 25, units = "cm", dpi = 600)



