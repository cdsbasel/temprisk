# DESCRIPTION -------------------------------------------------------------

# In this script we run a multiverse analysis by fitting the MASC model (Anusic & Schimmack, 2016 JPSP). 
# to different formats of data. Focus on propensity measures.

# Author(s): Alexandra Bagaini(1)
# (1)Centre for Cognitive and Decision Sciences, Faculty of Psychology, University of Basel.


# PACKAGES ---------------------------------------------------------------





library(tidyverse)
library(brms)
library(cmdstanr)
library(loo)
library(bayesplot)
library(tidybayes)
library(posterior)
library(data.table)

# FUNCTIONS ----------------------------------------------------------------


source("helper_functions.R")

# PATHS ---------------------------------------------------


data_w_path <- c("processing/output/temp_stability/") # where is the data stored
output_data_path <- c("analysis/output/temp_stability/") # where to store the output 

# DATA PREP ---------------------------------------------------------------

# combine + relabel datasets prior to analysis

dat_list <- list()
for (curr_yr in c(5,10,20)) {
  
  dat_list[[paste0("yb", curr_yr)]] <- fread(paste0(data_w_path,"complete_agg_retest_yb", curr_yr, ".csv"))
  
  
}


# COMBINATIONS ------------------------------------------------------------

age_bin<- c(5, 10, 20) 
min_n <- c(30, 100, 250)
month <- c(3, 6, 12)
rho_val <- c(.1,.5,.9)
metric <- c("pearson", "spearman", "icc2_1")
fam <- c("student", "gaussian")
prior_type <- c("vague", "weakly_informative")
dt_transform <- c("none", "log")

comb_dt <- crossing(rho_val, metric, dt_transform,
                    prior_type, age_bin, min_n, fam,
                    month)


# DATA PREP. --------------------------------------------------------------

df_sensitivity <- NULL
div_trans <- NULL
max_treedepth_reached <- NULL
rhat <- NULL
neffRatio <- NULL
set.seed(1234)
comb_select <- sample(1:nrow(comb_dt), 500)
counter <- 1

for (i in comb_select) {
  print(counter)
  curr_age_bin <- comb_dt$age_bin[i]
  curr_rho <- comb_dt$rho_val[i]
  curr_month <- comb_dt$month[i]
  curr_fam <- comb_dt$fam[i]
  curr_dt_transform <- comb_dt$dt_transform[i]
  curr_prior_type <- comb_dt$prior_type[i]
  curr_metric <- comb_dt$metric[i]
  curr_min_n <- comb_dt$min_n[i]
  
  #________________SCALING VARS___________________#
  
  dt <- dat_list[[paste0("yb",curr_age_bin)]]
  
  data_w <- dt %>% 
    filter( age_group != "10-90" &
              gender_group != "all" & 
              data_transform == curr_dt_transform &
              rho_val == curr_rho &
              month_bin == curr_month &
              age_bin == curr_age_bin & 
              min_n == curr_min_n &
              cor_metric == curr_metric &
              measure_category == "pro") %>% 
    mutate(time_diff_dec = time_diff_bin/10,
           female_prop_c = case_when(gender_group == "male" ~ -0.5,
                                     gender_group == "female" ~ 0.5),
           item_num_c = case_when(item_num == "multi_item" ~ 0.5,
                                  TRUE ~ -0.5),
           age_dec_c = (mean_age - 40)/10,
           age_dec_c2 = age_dec_c^2,
           domain_name = factor(domain_name),
           domain_name = relevel(domain_name, ref = "rec"), 
           # sum contrast coding 
           domain_name = sum_coding(domain_name, lvls = levels(domain_name)))
  
  
  
  
  # MODEL FITTING -------------------------------------------------------------
  
  
  family <- brmsfamily(
    family = curr_fam, 
    link = "identity"
  )
  
  
  formula <- bf(
    wcor|resp_se(sei, sigma = TRUE) ~ rel * (change * ((stabch^time_diff_dec) - 1) + 1),
    nlf(rel ~ inv_logit(logitrel)),
    nlf(change ~ inv_logit(logitchange)),
    nlf(stabch ~ inv_logit(logitstabch)),
    logitrel ~ 1 + age_dec_c*domain_name + age_dec_c2*domain_name +  female_prop_c + item_num_c + (1 + age_dec_c + age_dec_c2 + female_prop_c | sample),
    logitchange ~ 1 + age_dec_c*domain_name + age_dec_c2*domain_name + female_prop_c, 
    logitstabch ~ 1  + age_dec_c*domain_name + age_dec_c2*domain_name + female_prop_c,
    nl = TRUE
  )
  
  
  
  if (curr_prior_type == "vague") {
    priors <-
      prior(normal(0,4), nlpar="logitrel", class = "b") +
      prior(normal(0,4), nlpar="logitchange", class = "b") +
      prior(normal(0,4), nlpar="logitstabch", class = "b") +
      prior(cauchy(0, 1), nlpar="logitrel", class = "sd")  +
      prior(lkj(1), group="sample", class = "L")   +
      prior(cauchy(0,2), class = "sigma")
    
  }
  
  
  if (curr_prior_type != "vague") {
    priors <-
      prior(normal(0,1), nlpar="logitrel", class = "b") +
      prior(normal(0,1), nlpar="logitchange", class = "b") +
      prior(normal(0,1), nlpar="logitstabch", class = "b") +
      prior(cauchy(0, 1), nlpar="logitrel", class = "sd")  +
      prior(lkj(1), group="sample", class = "L")  +
      prior(cauchy(0,1), class = "sigma")
    
  }
  
  # fit model
  fit_masc <- brm(
    formula = formula,
    prior = priors,
    family = family,
    data = data_w,
    cores = 2, 
    chains = 2,
    threads = threading(2), 
    iter = 4000,
    warmup = 1500, 
    backend = "cmdstanr",
    save_pars = save_pars(all = TRUE),
    stan_model_args = list(stanc_options = list("O1")), 
    sample_prior = TRUE,
    control = list(max_treedepth = 10, adapt_delta = 0.99), 
    init = "0",
    seed = 1299 + i
  )
  
  np <- nuts_params(fit_masc)
  div_trans[counter] <- sum(filter(np, Parameter == "divergent__")$Value)
  max_treedepth_reached[counter] <- sum(filter(np, Parameter == "treedepth__")$Value == control_params(fit_masc)[["max_treedepth"]])
  rhat[counter] <- list(brms::rhat(fit_masc))
  neffRatio[counter] <- list(neff_ratio(fit_masc))
  
  
  ### ESTIMATE PARAMS BY DOMAIN FOR 40 YEAR OLDS  
  pred_df_dom <- NULL
  
  
  for (curr_nlpar in c("stabch","rel","change")) {
    
    
    nd <-  crossing(domain_name = unique(fit_masc$data$domain_name),
                    female_prop_c = 0,
                    time_diff_dec = 0,
                    item_num_c = 0,
                    sei = 0.01,
                    age_dec_c = 0) %>% 
      mutate(age_dec_c2 = age_dec_c^2)
    
    
    
    pred_df <-  nd %>%  add_epred_draws(fit_masc, nlpar = curr_nlpar, re_formula = NA) 
    
    if (curr_nlpar == "stabch") {
      pred_df <- pred_df %>%
        mutate(.epred = .epred^.1) 
    }
    
    
    
    pred_df <- pred_df %>% 
      group_by(domain_name) %>%
      mean_hdci(.epred,.width = c(.95,.8,.5)) %>% 
      pivot_wider(names_from = .width, values_from = c(.lower,.upper)) %>% 
      mutate(nlpar = curr_nlpar) %>% 
      select(domain_name, nlpar, .epred, dplyr::contains("er_"))
    
    pred_df_dom <- bind_rows(pred_df, pred_df_dom) 
    
  }
  
  
  pred_df_dom <- pred_df_dom %>% 
    mutate(rho_val = curr_rho,
           comb_num = counter,
           comb_id = i,
           n_obs = nrow(fit_masc$data),
           cor_metric = curr_metric,
           year_age_bin = curr_age_bin,
           time_bin = curr_month,
           min_n = curr_min_n,
           family = curr_fam,
           dt_transform = curr_dt_transform,
           priors = curr_prior_type)
  
  
  df_sensitivity <- bind_rows(pred_df_dom, df_sensitivity)
  
  counter <- counter + 1
  
  
  
  list_multiv <- list(results = df_sensitivity,
                      div_trans = div_trans,
                      max_treedepth_reached = max_treedepth_reached,
                      rhat = rhat,
                      neffRatio = neffRatio)
  
  write_rds(list_multiv, paste0(output_data_path, "multiverse_fit_masc_pro.rds"))
  
  
  
  
}




