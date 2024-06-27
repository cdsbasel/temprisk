

# DESCRIPTION -------------------------------------------------------------

# Script obtains the predicted values of the MASC model parameters for different samples,
# measure categories, domains, age groups, and gender. To be used for the variance decomposition analysis.

# Author(s): Alexandra Bagaini(1) 
# (1)Centre for Cognitive and Decision Sciences, Faculty of Psychology, University of Basel.



# PACKAGES ----------------------------------------------------------------


library(brms)
library(tidybayes)
library(tidyverse)

# FUNCTIONS ---------------------------------------------------------------

source("helper_functions.R")


# DATA --------------------------------------------------------------------

model_path <- c("analysis/output/temp_stability/") # where is the  data stored
output_path <- c("analysis/output/convergent_val/") # where to store the output data

# list of fitted masc models
masc <- list(Propensity = read_rds(paste0(model_path, "masc_pro.rds")),
             Frequency = read_rds(paste0(model_path, "masc_fre.rds")),
             Behaviour  = read_rds(paste0(model_path, "masc_beh.rds")))

intercor_dat <- read_csv("processing/output/convergent_val/complete_intercor.csv")
intercor_sample <- unique(intercor_dat$sample) 
intercor_sample <- gsub(" ", "_",intercor_sample )
# NLPAR PREDS ----------------------------------------------------------------

inv_logit <-function(x){rstanarm::invlogit(x)}
pred_df<- NULL
for(curr_meas in c("Propensity", "Frequency", "Behaviour")) {
  
  fit_masc <- masc[[curr_meas]]
  
  
  panel_dom  <- fit_masc$data %>% as_tibble() %>% distinct(sample, domain_name)
  
  
  for (curr_nlpar in c("stabch","rel","change")) {
    
    nd <-  crossing(panel_dom,
                    female_prop_c = c(-.5, .5),
                    time_diff_dec = 0,
                    item_num_c = 0,
                    sei = 0.01,
                    age_dec_c = (c(15,25,35,45,55,65,75,85)-40)/10) %>% 
      mutate(age_dec_c2 = age_dec_c^2)
    
    fit_nlpar <- nd %>% 
      add_epred_draws(fit_masc, nlpar = curr_nlpar, re_formula = NULL)   
    
    if (curr_nlpar == "stabch") {
      fit_nlpar <- fit_nlpar %>%
        mutate(.epred = .epred^.1) 
    }
    
    
    
    fit_nlpar <- fit_nlpar %>%
      group_by(sample, domain_name, age_dec_c, female_prop_c) %>%
      mean_hdci(.epred,.width = c(.95,.8,.5)) %>% 
      pivot_wider(names_from = .width, values_from = c(.lower,.upper)) %>% 
      mutate(nlpar = curr_nlpar,
             measure = curr_meas,
             x = domain_name,
             categ = "domain") %>% 
      mutate(age_group = case_when(age_dec_c == -2.5 ~ "10-19",
                                   age_dec_c == -1.5 ~ "20-29",
                                   age_dec_c == -.5 ~ "30-39",
                                   age_dec_c == 0.5  ~ "40-49",
                                   age_dec_c == 1.5 ~ "50-59",
                                   age_dec_c == 2.5 ~ "60-69",
                                   age_dec_c == 3.5 ~ "70-79",
                                   age_dec_c == 4.5 ~ "80-90"),
             gender_group = case_when(female_prop_c == -.5 ~ "male",
                                      female_prop_c == .5 ~ "female")) %>% 
      select(sample, categ, x, measure,age_group,gender_group, nlpar, .epred, dplyr::contains("er_"))
    
    
    
    
    pred_df <- bind_rows(pred_df, fit_nlpar) 
    
  }
  
}


df <- pred_df %>% 
  mutate(sub_component = case_when(x == "all" ~  "**Overall**",
                                   TRUE ~ rename_domain(x)),
         nlpar = case_when(nlpar == "rel" ~ "Reliability",
                           nlpar == "change" ~ "Change",
                           nlpar == "stabch" ~"Stab. Change"))

# select estimates from relevant samples only
df  <- df  %>% 
  filter(sample %in% intercor_sample) 

# SAVE OPUTPUT ------------------------------------------------------------




write_csv(df, paste0(output_path, "masc_nlpar_pred.csv"))
