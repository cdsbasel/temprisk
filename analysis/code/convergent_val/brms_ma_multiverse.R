

#  DESCRIPTION -------------------------------------------------------------

# Script reads the aggregated set of  intercorrelations and conducts three Bayesian 
# meta-analyses/meta-regressions using various formats of data (multiverse): 
# Omnibus
# By-Measure
# By-Domain
# Saves meta-analytic estimates for plotting and model diagnostics




# PACKAGES ----------------------------------------------------------------


library(tidyverse)
library(brms)
library(data.table)
library(tidybayes)
library(metafor)


source("helper_functions.R")


# PATHS ---------------------------------------------------


data_w_path <- c("processing/output/convergent_val/") # where is the data stored
output_data_path <- c("analysis/output/convergent_val/") # where to store the output 



# DATA PREP ---------------------------------------------------------------

# combine + relabel datasets prior to analysis

dat_list <- list()
for (curr_yr in c(5,10,20)) {
  
  dat <- fread(paste0(data_w_path,"complete_agg_intercor_yb", curr_yr, ".csv"))
  
  
  dt <- dat %>% 
    group_by(domain_pair_lbl) %>% 
    mutate(domain_pair_id = as.character(cur_group_id())) %>% 
    ungroup()  %>% 
    group_by(meas_pair_lbl) %>% 
    mutate(meas_pair_id = as.character(cur_group_id())) %>% 
    ungroup()   
  
  
  
  dat_list[[paste0("yb", curr_yr)]] <- dt
  
  
}

# SUBSET LIST ---------------------------------------------------------------

# different possible versions of the dataset / model specs.
age_bin<- c(5, 10, 20) 
min_n <- c(30, 100, 250)
rho_val <- c(.1,.5,.9)
metric <- c("pearson", "spearman", "icc2_1")
likelihood <- c("student", "gaussian")
prior_type <- c("vague", "weakly_informative")
dt_transform <- c("none", "log")

comb_dt <- crossing(rho_val, metric, dt_transform,
                    prior_type, age_bin, min_n, likelihood)




# MA OVERALL --------------------------------------------------------
df_sensitivty <- NULL
div_trans <- NULL
max_treedepth_reached <- NULL
rhat <- NULL
neffRatio <- NULL

for (i in 1:nrow(comb_dt)) {
  
  print(i)
  curr_age_bin <- comb_dt$age_bin[i]
  curr_rho <- comb_dt$rho_val[i]
  curr_fam <- comb_dt$likelihood[i]
  curr_dt_transform <- comb_dt$dt_transform[i]
  curr_prior_type <- comb_dt$prior_type[i]
  curr_metric <- comb_dt$metric[i]
  curr_min_n <- comb_dt$min_n[i]
  
  
  dt <- dat_list[[paste0("yb",curr_age_bin)]]
  
  
  
  ### Select data
  
  dt_ma <- dt %>% 
    filter(gender_group != "all" &
             age_group != "10-90" &
             data_transform == curr_dt_transform &
             rho_val == curr_rho &
             age_bin == curr_age_bin & 
             min_n == curr_min_n &
             cor_metric == curr_metric)
  
  
  
  family <- brmsfamily(
    family = curr_fam, 
    link = "identity"
  )
  
  
  if (curr_prior_type != "vague") {
    
    priors <-  c(prior(normal(0, 1), class = "b", coef = "Intercept"),
                 prior(normal(0, 2), class = "b", dpar = "sigma", coef = "Intercept"),
                 prior(cauchy(0, 0.3), class = "sd"),
                 prior(cauchy(0, 0.3), class = "sd", dpar = "sigma"))
    
  }
  
  if (curr_prior_type == "vague") {
    
    priors <-  c(prior(normal(0, 10), class = "b", coef = "Intercept"),
                 prior(normal(0, 20), class = "b", dpar = "sigma", coef = "Intercept"),
                 prior(cauchy(0, 1), class = "sd"),
                 prior(cauchy(0, 1), class = "sd", dpar = "sigma"))
    
  }
  
  
  
  
  
  formula <-  bf(wcor_z|se(sei_z, sigma = TRUE) ~ 0 + Intercept + (1|sample),
                 sigma ~ 0 + Intercept + (1 | sample))
  
  
  ma_fit <- brm(
    formula = formula,
    data = dt_ma,
    family = family,
    prior = priors,
    cores = 2, 
    chains = 2,
    threads = threading(2), 
    iter = 3000, 
    warmup = 1500, 
    backend = "cmdstanr", 
    stan_model_args = list(stanc_options = list("O1")),
    sample_prior = TRUE,
    control = list(max_treedepth = 10, adapt_delta = 0.95), 
    seed = 458828 + i
  )
  
  np <- nuts_params(ma_fit)
  div_trans[i] <- sum(filter(np, Parameter == "divergent__")$Value)
  max_treedepth_reached[i] <- sum(filter(np, Parameter == "treedepth__")$Value == control_params(ma_fit)[["max_treedepth"]])
  rhat[i] <- list(brms::rhat(ma_fit))
  neffRatio[i] <- list(neff_ratio(ma_fit))
  
  fit_summary <- fixef(ma_fit, summary = FALSE) %>% 
    as_tibble() %>% 
    mutate(Intercept = transf.ztor(Intercept)) %>% 
    mean_hdci(Intercept, .width = c(.95, .8, .5)) %>% 
    pivot_wider(names_from = .width, values_from = c(.lower,.upper)) %>% 
    mutate(comb_num = i,
           param = "Intercept",
           rho_val = curr_rho,
           n_obs = nrow(ma_fit$data),
           data_transform = curr_dt_transform,
           cor_metric = curr_metric,
           year_age_bin = curr_age_bin,
           min_n = curr_min_n,
           family = curr_fam,
           priors = curr_prior_type)
  
  df_sensitivty <- bind_rows(fit_summary, df_sensitivty)
  
  
  list_multiv <- list(results = df_sensitivty,
                      div_trans = div_trans,
                      max_treedepth_reached = max_treedepth_reached,
                      rhat = rhat,
                      neffRatio = neffRatio)
  
  write_rds(list_multiv, paste0(output_data_path, "multiverse_fit_convergent_ma_overall.rds"))
  
  
}



# MA BY MEASURE --------------------------------------------------------


df_sensitivty <- NULL
div_trans <- NULL
max_treedepth_reached <- NULL
rhat <- NULL
neffRatio <- NULL


for (i in 1:nrow(comb_dt)) {
  
  print(i)
  curr_age_bin <- comb_dt$age_bin[i]
  curr_rho <- comb_dt$rho_val[i]
  curr_fam <- comb_dt$likelihood[i]
  curr_dt_transform <- comb_dt$dt_transform[i]
  curr_prior_type <- comb_dt$prior_type[i]
  curr_metric <- comb_dt$metric[i]
  curr_min_n <- comb_dt$min_n[i]
  
  
  dt <- dat_list[[paste0("yb",curr_age_bin)]]
  
  
  
  ### Select data
  dt_ma <- dt %>% 
    filter(gender_group != "all" &
             age_group != "10-90" &
             data_transform == curr_dt_transform &
             rho_val == curr_rho &
             age_bin == curr_age_bin & 
             min_n == curr_min_n &
             cor_metric == curr_metric)
  
  
  
  family <- brmsfamily(
    family = curr_fam, 
    link = "identity"
  )
  
  
  if (curr_prior_type != "vague") {
    
    priors <-  c(prior(normal(0, 1), class = "b"),
                 prior(normal(0, 2), class = "b", dpar = "sigma"),
                 prior(cauchy(0, 0.3), class = "sd"),
                 prior(cauchy(0, 0.3), class = "sd", dpar = "sigma"))  
    
  }
  
  if (curr_prior_type == "vague") {
    
    priors <-  c(prior(normal(0, 10), class = "b"),
                 prior(normal(0, 20), class = "b", dpar = "sigma"),
                 prior(cauchy(0, 1), class = "sd"),
                 prior(cauchy(0, 1), class = "sd", dpar = "sigma"))
    
  }
  
  
  
  formula <-  bf(wcor_z|se(sei_z, sigma = TRUE) ~ 0 + meas_pair_id + (1|sample),
                 sigma ~ 0 + meas_pair_id + (1|sample))
  
  
  ma_fit <- brm(
    formula = formula,
    data = dt_ma,
    family = family,
    prior = priors,
    cores = 2, 
    chains = 2,
    threads = threading(2), 
    iter = 3000, 
    warmup = 1500, 
    backend = "cmdstanr", 
    stan_model_args = list(stanc_options = list("O1")),
    sample_prior = TRUE,
    control = list(max_treedepth = 10, adapt_delta = 0.95), 
    seed = 458828 + i
  )
  
  np <- nuts_params(ma_fit)
  div_trans[i] <- sum(filter(np, Parameter == "divergent__")$Value)
  max_treedepth_reached[i] <- sum(filter(np, Parameter == "treedepth__")$Value == control_params(ma_fit)[["max_treedepth"]])
  rhat[i] <- list(brms::rhat(ma_fit))
  neffRatio[i] <- list(neff_ratio(ma_fit))
  
  
  fit_summary <- fixef(ma_fit, summary = FALSE) %>% 
    as_tibble() %>% 
    select(meas_pair_id1,meas_pair_id2,meas_pair_id3,meas_pair_id4,meas_pair_id5,meas_pair_id6) %>% 
    mutate_if(is.numeric, transf.ztor) %>% 
    pivot_longer(everything(), names_to = "meas_pair_id", values_to = "estimate") %>% 
    group_by(meas_pair_id) %>% 
    mean_hdci(.width = c(.5,.8,.95)) %>% 
    pivot_wider(names_from = .width, values_from = c(.lower,.upper)) %>% 
    mutate( comb_num = i,
            meas_pair_id = as.character(parse_number(meas_pair_id)),
            rho_val = curr_rho,
            n_obs = nrow(ma_fit$data),
            data_transform = curr_dt_transform,
            cor_metric = curr_metric,
            year_age_bin = curr_age_bin,
            min_n = curr_min_n,
            family = curr_fam,
            priors = curr_prior_type) %>% 
    left_join((dt_ma %>% distinct(meas_pair_lbl, meas_pair_id)), by = "meas_pair_id")
  
  dt_ma_nrow <- dt_ma %>% group_by(meas_pair_id) %>% 
    summarise(n_obs_meas = n())
  
  fit_summary <- fit_summary %>% 
  left_join(dt_ma_nrow, by = "meas_pair_id")
  
  df_sensitivty <- bind_rows(fit_summary, df_sensitivty)
  
  list_multiv <- list(results = df_sensitivty,
                      div_trans = div_trans,
                      max_treedepth_reached = max_treedepth_reached,
                      rhat = rhat,
                      neffRatio = neffRatio)
  
  write_rds(list_multiv, paste0(output_data_path, "multiverse_fit_convergent_ma_measure.rds"))
  
  
  
}




