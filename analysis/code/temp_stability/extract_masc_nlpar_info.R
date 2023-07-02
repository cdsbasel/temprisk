

# DESCRIPTION -------------------------------------------------------------

# Script to extract the estimated values of the paramaters of the MASC model fitted
# using the risk pref data and the data used by Anusic and Schimmack. 
# Combines results into a dataframe. Used for plotting and other analyses

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
output_path <- c("analysis/output/temp_stability/") # where to store the output data

# list of fitted masc models
masc <- list(Propensity = read_rds(paste0(model_path, "masc_pro.rds")),
             Frequency = read_rds(paste0(model_path, "masc_fre.rds")),
             Behaviour  = read_rds(paste0(model_path, "masc_beh.rds")),
             AS2015  = read_rds(paste0(model_path, "masc_as.rds")) )


# BAGAINI ET AL. DATA ----------------------------------------------------------------


pred_df_bagaini <- NULL
for(curr_meas in c("Propensity", "Frequency", "Behaviour")) {
  
  fit_masc <- masc[[curr_meas]]
  
  for (curr_nlpar in c("stabch","rel","change")) {
    
    ### BY DOMAIN FOR 40 YEAR OLDS  
    nd <-  crossing(domain_name = unique(fit_masc$data$domain_name),
                    female_prop_c = 0,
                    time_diff_dec = 0,
                    sei = 0.01,
                    age_dec_c = 0) %>% 
      mutate(age_dec_c2 = age_dec_c^2)
    
    fit_nlpar_dom <- nd %>% 
      add_epred_draws(fit_masc, nlpar = curr_nlpar, re_formula = NA)     
    
    if (curr_nlpar == "stabch") {
      fit_nlpar_dom <- fit_nlpar_dom %>%
        mutate(.epred = .epred^.1) 
    }
    
    
    
    fit_nlpar_dom <- fit_nlpar_dom %>%
      group_by(domain_name) %>%
      mean_hdci(.epred,.width = c(.95,.8,.5)) %>% 
      pivot_wider(names_from = .width, values_from = c(.lower,.upper)) %>% 
      mutate(nlpar = curr_nlpar,
             measure = curr_meas,
             x = domain_name,
             categ = "domain") %>% 
      select(categ, x, measure, nlpar, .epred, dplyr::contains("er_"))
    
    
    ### ACCROSS ALL DOMAINS FOR 40 YEAR OLDS  
    
    nd <-  crossing(domain_name = NA, #making predictions of the grand mean when using sum coding
                    female_prop_c = 0,
                    time_diff_dec = 0,
                    sei = 0.01,
                    age_dec_c = 0) %>% 
      mutate(age_dec_c2 = age_dec_c^2)
    
    
    fit_nlpar_all <- nd %>% 
      add_epred_draws(fit_masc, nlpar = curr_nlpar, re_formula = NA) 
    
    
    if (curr_nlpar == "stabch") {
      fit_nlpar_all <- fit_nlpar_all %>%
        mutate(.epred = .epred^.1) 
    }
    
    
    
    fit_nlpar_all <- fit_nlpar_all %>%
      group_by(domain_name) %>%
      mean_hdci(.epred,.width = c(.95,.8,.5)) %>% 
      pivot_wider(names_from = .width, values_from = c(.lower,.upper)) %>% 
      mutate(nlpar = curr_nlpar,
             measure = curr_meas,
             x = "all",
             categ = "all") %>% 
      select(categ, x, measure, nlpar, .epred, dplyr::contains("er_"))
    
    
    pred_df <- bind_rows(fit_nlpar_dom, fit_nlpar_all) 
    
    
    
    pred_df_bagaini <- bind_rows(pred_df,pred_df_bagaini ) 
    
  }
  
}



pred_df_bagaini <- pred_df_bagaini %>% 
  rowwise() %>% 
  mutate(sub_component = case_when(x == "all" ~  "**Overall**",
                                   TRUE ~ rename_domain(x)),
         nlpar = case_when(nlpar == "rel" ~ "Reliability",
                           nlpar == "change" ~ "Change",
                           nlpar == "stabch" ~"Stab. Change")) %>% 
  ungroup()




# ANUSIC & SCHIMMACK DATA --------------------------------------------------------------


pred_df_as <- NULL

fit_masc <- masc[["AS2015"]]

for (curr_nlpar in c("stabch","rel","change")) {
  
  
  nd <-  crossing(construct = unique(fit_masc$data$construct),
                  female_prop_c = 0,
                  time_diff_dec = 0,
                  se = 0.01,
                  age_dec_c = 0) %>% 
    mutate(age_dec_c2 = age_dec_c^2)
  
  
  
  
  pred_df <- nd %>% 
    add_epred_draws(fit_masc, nlpar = curr_nlpar, re_formula = NA)  
  
  
  if (curr_nlpar == "stabch") {
    pred_df <- pred_df %>%
      mutate(.epred = .epred^.1) 
  }
  
  
  
  pred_df <- pred_df %>%
    group_by(construct) %>%
    mean_hdci(.epred,.width = c(.95,.8,.5)) %>% 
    pivot_wider(names_from = .width, values_from = c(.lower,.upper)) %>% 
    mutate(nlpar = curr_nlpar,
           x = construct,
           categ = "construct") %>% 
    select(categ, x, construct, nlpar, .epred, dplyr::contains("er_"))
  
  
  
  pred_df_as <- bind_rows(pred_df, pred_df_as) 
  
}




pred_df_as <- pred_df_as %>% 
  mutate(measure = "Construct", 
         sub_component = case_when(construct == "pers"  ~ "Personality",
                                   construct == "affe" ~ "Affect",
                                   construct == "life" ~ "Life-Satisfaction",
                                   construct == "self" ~ "Self-Esteem"),
         nlpar = case_when(nlpar == "rel" ~ "Reliability",
                           nlpar == "change" ~ "Change",
                           nlpar == "stabch" ~"Stab. Change")) %>% 
  select(-construct)







# SAVE OPUTPUT ------------------------------------------------------------



df <- bind_rows(pred_df_bagaini, pred_df_as)


write_csv(df, paste0(output_path, "masc_nlpar_pred.csv"))
