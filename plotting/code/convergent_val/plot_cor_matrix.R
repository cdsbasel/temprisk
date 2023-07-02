# DESCRIPTION -------------------------------------------------------------

#Script to plot the meta-analytic correlation matrix



# Author(s): Alexandra Bagaini, Centre for Cognitive and Decision Sciences, Faculty of Psychology, University of Basel.

# PACKAGES ----------------------------------------------------------------


library(tidyverse)
library(ggtext)
library(patchwork)

source("helper_functions.R")

# DATA --------------------------------------------------------------------
data_path <- c("analysis/output/convergent_val/") # where is the  data stored
output_path <- c("plotting/output/convergent_val/") # where to store the output data
ma <- list(overall = read_csv(paste0(data_path, "cor_mat_convergent_overall_dat.csv")),
           measure = read_csv(paste0(data_path, "cor_mat_convergent_measure_dat.csv")),
           domain = read_csv(paste0(data_path, "cor_mat_convergent_domain_dat.csv")))

# MA DOMAIN: PLOT ---------------------------------------------------------


melted_cormat <- ma[["domain"]]

#data not available for all pairs; filling in  with NAs
melted_cormatNA <- crossing(re_x = melted_cormat$re_x,
                            re_y = melted_cormat$re_y) %>% 
  mutate(d = rnorm(n()))
mat <- reshape2::dcast(data = melted_cormatNA,formula = re_x~re_y,fun.aggregate = sum,value.var = "d")
mat <- column_to_rownames(mat, var = "re_x")
cor_mat <- as.matrix(mat)
cor_mat[lower.tri(cor_mat, diag = FALSE)] <- NA
cor_mat[ cor_mat == 0 ] <- NA
melted_cormatNA <- reshape::melt(cor_mat)
melted_cormatNA <- reshape::melt(cor_mat)
melted_cormatNA <- melted_cormatNA %>% filter(!is.na(value )) %>% 
  mutate(lbls = str_arrange(paste0(X1, "_", X2))) %>% 
  rename(re_x = X1, re_y = X2) %>% select(-value)

melted_cormatB <- melted_cormat %>% full_join(melted_cormatNA, by = c("re_x", "re_y"))


p1 <- ggplot(data = melted_cormatB, aes(re_x, re_y, fill = (estimate)))+
  geom_tile(color = "grey50", linewidth = .2)+
  geom_richtext(lineheight = .75, family = "Source Sans 3", 
                fill = NA, label.color = NA, fontface = "bold",
                aes(color = lbl_color, label = pooled_est_lbl), 
                position = position_nudge(y =.3),
                size = 3) +
  geom_richtext(lineheight = .75, family = "Source Sans 3", ##Light" 
                fill = NA, label.color = NA,
                aes(color = lbl_color, label = cred_int_lbl), 
                position = position_nudge(y =0),
                size = 3) +
  geom_richtext(lineheight = .75, family = "Source Sans 3 Light", 
                fill = NA, label.color = NA,
                aes(color = lbl_color, label = k_lbl), 
                position = position_nudge(y = -.3),
                size = 3) +
  scale_color_identity() +
  scale_fill_gradient2(low = "white", high = "#0F4C5C", mid = "#9FCBD6", 
                       na.value = '#F5F5F5',
                       midpoint = 0.4,  limit = c(-0.25,1), space = "Lab", 
                       name="Pooled\nEstimate") + #  (Mean)
  theme_minimal()+ 
  scale_x_discrete(position = "top", expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  labs(tag = "A") +
  theme(axis.text.x.top = element_markdown(angle = 0, family = "Source Sans 3",
                                           color = "black", size = 9),
        axis.text.y = element_markdown(angle = 0, family = "Source Sans 3",
                                       color = "black", size = 9, hjust=1),
        text = element_text(family = "Source Sans 3"),
        axis.title = element_blank(),
        # legend.position = c(.95,.95),
        plot.margin = margin(r = 10),
        legend.justification=c(1,0.75), 
        # legend.position=c(1,0.5),
        plot.tag = element_text(family = "Source Sans 3", face = "bold", size = 14),
        legend.text = element_text(family = "Source Sans 3", size = 8, vjust = .5),
        legend.title = element_text(family = "Source Sans 3",
                                    face = "bold", size = 9.5, margin = margin(b = 5)),
        legend.key.width = unit(.5, "cm"),
        panel.grid = element_blank()) +
  guides(fill = guide_coloursteps(show.limits = TRUE,  frame.colour = "grey50"))

p1


# MA BY MEAS: PLOT --------------------------------------------------------


melted_cormat <- ma[["measure"]]

p2 <- ggplot(data = melted_cormat, aes(x = y, y = x, fill = estimate))+
  geom_tile(color = "grey50", linewidth = .2)+
  geom_richtext(lineheight = .75, family = "Source Sans 3", 
                fill = NA, label.color = NA, fontface = "bold",
                aes(color = lbl_color, label = pooled_est_lbl), 
                position = position_nudge(y =.3),
                size = 4) +
  geom_richtext(lineheight = .75, family = "Source Sans 3", ##Light" 
                fill = NA, label.color = NA,
                aes(color = lbl_color, label = cred_int_lbl), 
                position = position_nudge(y =0),
                size = 4) +
  geom_richtext(lineheight = .75, family = "Source Sans 3 Light", 
                fill = NA, label.color = NA,
                aes(color = lbl_color, label = k_lbl), 
                position = position_nudge(y = -.3),
                size = 4) +
  scale_color_identity() +
  scale_fill_gradient2(low = "white", high = "#0F4C5C", mid = "#9FCBD6", 
                       na.value = 'grey70',
                       midpoint = 0.4,  limit = c(-0.2,1), space = "Lab", 
                       name="Pooled\nEstimate (Mean)") + 
  theme_minimal()+ 
  labs(title = "", tag = "B") +
  scale_x_discrete(position = "top", expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0), position = "left") +
  theme(axis.text.x.top = element_markdown(angle = 0, family = "Source Sans 3", face = "bold",
                                           color = "black", size = 11),
        axis.text.y.left = element_markdown(angle = 0, family = "Source Sans 3", face = "bold",
                                            color = "black", size = 11, hjust=1),
        text = element_text(family = "Source Sans 3"),
        axis.title = element_blank(),
        plot.title = element_text(family = "Source Sans 3 Medium", face = "italic",
                                  size = 10, hjust = 1),
        plot.tag.position = c(.05,.95),
        plot.background = element_rect(color = "NA", linewidth = .25, fill = "NA"),
        plot.margin = margin(0),
        legend.position = c("none"),
        plot.tag = element_text(family = "Source Sans 3", face = "bold", size = 14),
        legend.text = element_text(family = "Source Sans 3", size = 8, vjust = .5),
        legend.title = element_text(family = "Source Sans 3",
                                    face = "bold", size = 8, margin = margin(b = 5)),
        legend.key.width = unit(.25, "cm"),
        panel.grid = element_blank())

p2





# COMBINE -----------------------------------------------------------------



p <- p1 + inset_element(p2, .55,0, .925, .425, align_to = 'full', clip = FALSE)
p



ggsave(paste0(output_path, "cor_matrix_fig.png"),
       plot = p,
       height = 30, 
       width = 45,
       units = "cm",
       dpi = 300)









