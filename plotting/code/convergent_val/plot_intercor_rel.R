# DESCRIPTION -------------------------------------------------------------

#Plotting association between meta-analytics estimates of convergence between measure and their reliability.

# Author(s): Alexandra Bagaini, Centre for Cognitive and Decision Sciences, Faculty of Psychology, University of Basel.


# LIBRARIES ---------------------------------------------------------------

library(tidyverse)
library(brms)
library(ggtext)
library(ggrepel)

# FILES OF INTEREST ---------------------------------------------------

nlpar_dat <- read_csv("analysis/output/temp_stability/masc_nlpar_pred.csv")
melted_cormat <- read_csv("analysis/output/convergent_val/cor_mat_convergent_domain_dat.csv")
output_path <- c("plotting/output/convergent_val/")

source("helper_functions.R")
# BIND DATA ------------------------------------------------------------


nlpar_dat <- nlpar_dat %>% 
  filter(nlpar == "Reliability" & categ == "domain") %>% 
  select(x,.epred, measure)%>% 
  mutate(measure = case_when(measure == "Propensity" ~"pro",
                             measure == "Frequency" ~"fre",
                             measure == "Behaviour"  ~ "beh")) 

# rename labels
dat_masc_a <- nlpar_dat  %>% rename(estimate_a = .epred, measure_category_a = measure,
                                   domain_name_a = x) %>% 
  mutate(x = name_lbl(measure = measure_category_a, domain = domain_name_a)) 
dat_masc_b <- nlpar_dat  %>% rename(estimate_b = .epred, measure_category_b = measure,
                                    domain_name_b = x) %>% 
  mutate(y = name_lbl(measure = measure_category_b, domain = domain_name_b))

# join new data
melted_cormat <- melted_cormat %>% 
  mutate(x = gsub("\\**", "", x),
         x = gsub("<br>", " - ", x),
         y = gsub("\\**", "", y),
         y = gsub("<br>", " - ", y)) %>% 
  left_join(dat_masc_a, by = c("x")) %>% 
  left_join(dat_masc_b, by = c("y")) %>% 
  rowwise() %>% 
  mutate(mean_rel = mean(c(estimate_a, estimate_b))) %>% 
  ungroup()




# REGRESSION --------------------------------------------------------------



family <- brmsfamily(
  family = "gaussian", 
  link = "identity"
)


formula <- bf(estimate ~ 1 + mean_rel)


fit <- brm(
  formula = formula,
  family = family,
  data = melted_cormat,
  cores = 2, 
  chains = 2,
  threads = threading(2), 
  iter = 2000, 
  warmup = 1000, 
  backend = "cmdstanr",
  stan_model_args = list(stanc_options = list("O1")), 
  sample_prior = TRUE,
  control = list(max_treedepth = 10, adapt_delta = 0.8), 
  init = "0",
  seed = 3726 
)



fit_sum <- summary(fit)
b_est = fit_sum$fixed$Estimate[2]
b_l95 = fit_sum$fixed$`l-95% CI`[2]
b_u95 = fit_sum$fixed$`u-95% CI`[2]


lbl_df = tibble(x = .7,
                y = .7,
                label = paste0("*b* = ", format(round(b_est, digits=2), nsmall = 2), "<br>",
                               "[", format(round(b_l95, digits=2), nsmall = 2), ", ",
                               format(round(b_u95, digits=2), nsmall = 2), "]"))

nd <- crossing(mean_rel = seq(0,1,.1))


epred_draws_df <- fit %>% 
  epred_draws(newdata = nd, re_formula = NA)

melted_cormat <-  melted_cormat %>% mutate(
  lbl = paste0(x,"\n",y))




# PLOTTING ----------------------------------------------------------------



p <- melted_cormat %>% 
  ggplot(aes(x = mean_rel,y = estimate)) + 
  geom_hline(yintercept = 0, linewidth = .5, color = "grey70")+
  geom_vline(xintercept = 0.001, linewidth = .75, color = "grey70")+
  geom_richtext(data = lbl_df, aes(label = label, x = x, y = y), size = 3,
                family = "Source Sans 3", fontface = "bold", color = "grey30",
                fill = NA, label.color = NA, # remove background and outline
                label.padding = grid::unit(rep(0, 4), "pt")) +
  stat_lineribbon(data = epred_draws_df, alpha = 1/3, aes(x = mean_rel, y = .epred), fill = "#2E5EAA", color = "grey95", size = .5) +
  geom_text_repel(aes(label = lbl),min.segment.length = 0, 
                  seed = 5, box.padding = 0.25, size = 1.25, lineheight = .75, 
                  max.overlaps = 8, segment.size = .25,
                  family = "Source Sans 3 Medium", color = "grey30") +
  geom_point(show.legend = FALSE, size = 2, shape = 21, stroke = .75, alpha = 1,
             fill = "NA"  , color ="#2E5EAA" ) + 
  theme_minimal() +
   scale_x_continuous(breaks = seq(0,1,.2), expand = c(0,0)) +
   scale_y_continuous(breaks = seq(-.2,1,.2), limits = c(-.25,1)) +
  coord_cartesian(xlim = c(0,1)) +
  labs(x = "Average Reliability Estimate", y = "Inter-Correlation")+
  theme(panel.background = element_rect(fill = "NA", color = "grey80", linewidth = .2),
        panel.grid = element_line(linetype = "dotted", linewidth = .25, color = "grey85"),
        plot.margin = margin(b = 5, r = 5),
        title = element_text(family = "Source Sans 3 Medium", size = 8),
        text = element_text(family = "Source Sans 3", size = 8))


p
ggsave(paste0(output_path,"intercor_rel.png"),
              plot = p, height = 13, 
       width = 20, units = "cm", dpi = 300)


