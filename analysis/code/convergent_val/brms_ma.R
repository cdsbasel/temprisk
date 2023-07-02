

#  DESCRIPTION -------------------------------------------------------------

# Script reads the aggregated set of  intercorrelations and conducts three Bayesian 
# meta-analyses/meta-regressions: 
# Omnibus
# By-Measure
# By-Domain
# Saves meta-analytic estimates for plotting

# Author(s): Alexandra Bagaini(1) and Paul-Christian  Buerkner(2)  
# (1)Centre for Cognitive and Decision Sciences, Faculty of Psychology, University of Basel.
# (2)Department of Statistics, TU Dortmund University



# PACKAGES ----------------------------------------------------------------


library(tidyverse)
library(brms)
library(metafor)
library(data.table)
library(tidybayes)


# FUNCTION ----------------------------------------------------------------

source("helper_functions.R")


# PATHS ---------------------------------------------------


data_w_path <- c("processing/output/convergent_val/") # where is the data stored
output_data_path <- c("analysis/output/convergent_val/") # where to store the output 




#________________FILES___________________#

data_file <- "complete_agg_intercor_yb10.csv" # name of retest data 

dat <-read_csv(paste0(data_w_path,data_file))

# PREP DATA ---------------------------------------------------------------


dt_ma <- dat %>% 
  filter(gender_group != "all" &
           age_group != "10-90" &
           data_transform == "none" &
           rho_val == .5 &
           age_bin == 10 & 
           min_n == 30 &
           cor_metric == "spearman")

# renaming & harmonizing some pairs to be consistent with prev. analyses
labels_harmo_name <- dt_ma %>% 
  mutate(temp_lbl_name = str_arrange(name_pair_lbl)) %>% 
  distinct(temp_lbl_name,name_pair_lbl) %>% 
  group_by(temp_lbl_name) %>% 
  slice(1) %>% 
  rename(harmonized_pair_name = name_pair_lbl)

labels_harmo_meas <- dt_ma %>% 
  mutate(temp_lbl_meas = str_arrange(meas_pair_lbl)) %>% 
  distinct(temp_lbl_meas,meas_pair_lbl) %>% 
  group_by(temp_lbl_meas) %>% 
  slice(1) %>% 
  rename(harmonized_pair_meas = meas_pair_lbl)



dt_ma <- dt_ma %>% 
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
sort(unique(dt_ma$harmonized_pair_meas))
sort(unique(dt_ma$harmonized_pair_name))

# MA OVERALL --------------------------------------------------------

family <- brmsfamily(
  family = "student", 
  link = "identity"
)


formula <-  bf(wcor_z|se(sei_z, sigma = TRUE) ~ 0 + Intercept + (1|sample),
               sigma ~ 0 + Intercept + (1 | sample))

priors <-  c(prior(normal(0, 1), class = "b", coef = "Intercept"),
             prior(normal(0, 2), class = "b", dpar = "sigma", coef = "Intercept"),
             prior(cauchy(0, 0.3), class = "sd"),
             prior(cauchy(0, 0.3), class = "sd", dpar = "sigma"))

ma_fit <- brm(
  formula = formula,
  data = dt_ma,
  family = family,
  prior = priors,
  cores = 2, 
  chains = 2,
  threads = threading(2), 
  iter = 7000,
  warmup = 2000, 
  backend = "cmdstanr", 
  stan_model_args = list(stanc_options = list("O1")),
  sample_prior = TRUE,
  control = list(max_treedepth = 10, adapt_delta = 0.95), 
  seed = 458828 
)

write_rds(ma_fit, paste0(output_data_path, "fit_convergent_ma_overall", ".rds"))


# save output for plotting
ma_fit <- read_rds(paste0(output_data_path, "fit_convergent_ma_overall", ".rds"))

overall_es <- ma_fit %>% 
  spread_draws(b_Intercept) %>%
  mutate(b_Intercept = transf.ztor(b_Intercept)) %>% 
  mean_hdci(b_Intercept) %>% 
  mutate(x = "Overall",
         n_cor = sum(dt_ma$cor_num),
         n_wcor = nrow(ma_fit$data),
         y = "Overall")


dt <- overall_es %>% 
  mutate(pooled_est_lbl =paste0("",as.character(format(round(((b_Intercept)), digits=2), nsmall = 2)),""),
         cred_int_lbl =  paste0("<br>", "[", format(round((.lower), digits=2), nsmall = 2) ,
                                ", ", format(round((.upper), digits=2), nsmall = 2) ,"]",
                                "<br>"),
         k_lbl = paste0("*k: ", as.character(prettyNum(n_wcor, big.mark = "'")), "*"))

dt_simple <- dt %>% select(x,y,b_Intercept) 
mat <- reshape2::dcast(data = dt_simple,formula = x~y,fun.aggregate = sum,value.var = "b_Intercept")
mat <- column_to_rownames(mat, var = "x")
cor_mat <- as.matrix(mat)
cor_mat[lower.tri(cor_mat, diag = FALSE)] <- NA
cor_mat[ cor_mat == 0 ] <- NA


melted_cormat <- reshape::melt(cor_mat)
melted_cormat <- melted_cormat %>% filter(!is.na(value)) %>% 
  rename(x = X1,y = X2) %>% 
  left_join(dt, by = c("x", "y")) %>% 
  mutate(lbl_color = if_else(value >= .7, "grey90", "black"),
         x = gsub("^([[:alpha:]]+)", "**\\1**", x),
         x = gsub(" - ", "<br>", x),
         y = gsub("^([[:alpha:]]+)", "**\\1**", y),
         y = gsub(" - ", "<br>", y))

write_csv(melted_cormat, paste0(output_data_path, "cor_mat_convergent_overall_dat", ".csv"))

# MA BY MEASURE --------------------------------------------------------------


family <- brmsfamily(
  family = "student", 
  link = "identity"
)



formula <-  bf(wcor_z|se(sei_z, sigma = TRUE) ~ 0 + meas_pair_id + (1|sample),
               sigma ~ 0 + meas_pair_id + (1|sample))

priors <-  c(prior(normal(0, 1), class = "b"),
             prior(normal(0, 2), class = "b", dpar = "sigma"),
             prior(cauchy(0, 0.3), class = "sd"),
             prior(cauchy(0, 0.3), class = "sd", dpar = "sigma"))


ma_fit <-  brm(
  formula = formula,
  data = dt_ma,
  family = family,
  prior = priors,
  cores = 2, 
  chains = 2,
  threads = threading(2), # i guess this helps someho
  iter = 7000,
  warmup = 2000, 
  backend = "cmdstanr", #  works bett
  stan_model_args = list(stanc_options = list("O1")), # ptimizati
  sample_prior = TRUE,
  # refresh = 0,
  control = list(max_treedepth = 10, adapt_delta = 0.95), # keeping def
  seed = 4528 # 
)

write_rds(ma_fit, paste0(output_data_path, "fit_convergent_ma_measure", ".rds"))


# save output for plotting

ma_fit <- read_rds(paste0(output_data_path, "fit_convergent_ma_measure", ".rds"))



my_vars = str_subset(variables(ma_fit), "b_meas_pair*")
my_regex = paste0(my_vars, collapse="|")
t <- ma_fit %>%
  spread_draws(!!sym(my_regex), regex=TRUE) %>% 
  pivot_longer(-c(.chain:.draw), names_to = "param", values_to = "estimate") %>% 
  mutate(estimate = transf.ztor(estimate)) %>% 
  group_by(param) %>%
  mean_hdci(estimate) 




ma_dat_pair <- dt_ma %>% distinct(meas_pair_id,harmonized_pair_meas)
ma_dat_ncor <- dt_ma %>% group_by(meas_pair_id) %>% summarise(n_cor = sum(cor_num),
                                                              n_wcor = n())



dt <- t %>% 
  rowwise()  %>% 
  mutate(meas_pair_id = as.character(parse_number(param))) %>% 
  ungroup() %>% 
  left_join(ma_dat_pair, by = "meas_pair_id") %>% 
  rowwise()  %>% 
  mutate( x = unlist(str_split(harmonized_pair_meas, "_"))[1],
          y = unlist(str_split(harmonized_pair_meas, "_"))[2]) %>% 
  ungroup() %>% 
  left_join(ma_dat_ncor, by = "meas_pair_id") %>% 
  mutate(pooled_est_lbl =paste0("",as.character(format(round(((estimate)), digits=2), nsmall = 2)),""),
         cred_int_lbl =  paste0("<br>", "[", format(round((.lower), digits=2), nsmall = 2) ,
                                ", ", format(round((.upper), digits=2), nsmall = 2) ,"]",
                                "<br>"),
         k_lbl = paste0("*k: ", as.character(prettyNum(n_wcor, big.mark = "'")), "*")) %>% 
  mutate(lbl_color = if_else(estimate >= .7, "grey90", "black"),
         x = gsub("^([[:alpha:]]+)", "**\\1**", x),
         x = gsub(" - ", "<br>", x),
         y = gsub("^([[:alpha:]]+)", "**\\1**", y),
         y = gsub(" - ", "<br>", y))



write_csv(dt, paste0(output_data_path, "cor_mat_convergent_measure_dat", ".csv"))







# MA DOMAIN ---------------------------------------------------------


family <- brmsfamily(
  family = "student", 
  link = "identity"
)


formula <-  bf(wcor_z|se(sei_z, sigma = TRUE) ~ 0 + name_pair_id + (1|sample),
               sigma ~ 0 + name_pair_id + (1|sample))

priors <-  c(prior(normal(0, 1), class = "b"),
             prior(normal(0, 2), class = "b", dpar = "sigma"),
             prior(cauchy(0, 0.3), class = "sd"),
             prior(cauchy(0, 0.3), class = "sd", dpar = "sigma"))


ma_fit <-  brm(
  formula = formula,
  data = dt_ma,
  family = family,
  prior = priors,
  cores = 2, 
  chains = 2,
  threads = threading(2), 
  iter = 7000,
  warmup = 2000, 
  backend = "cmdstanr", #  works better
  stan_model_args = list(stanc_options = list("O1")), # 
  sample_prior = TRUE,
  # refresh = 0,
  control = list(max_treedepth = 10, adapt_delta = 0.95), 
  seed = 2946
)



write_rds(ma_fit, paste0(output_data_path, "fit_convergent_ma_domain", ".rds"))


# save output for plotting




ma_fit <- read_rds( paste0(output_data_path, "fit_convergent_ma_domain", ".rds"))
my_vars = str_subset(variables(ma_fit), "b_name_pair*")
my_regex = paste0(my_vars, collapse="|")
t <- ma_fit %>%
  spread_draws(!!sym(my_regex), regex=TRUE) %>% 
  pivot_longer(-c(.chain:.draw), names_to = "param", values_to = "estimate") %>% 
  mutate(estimate = transf.ztor(estimate)) %>% 
  group_by(param) %>%
  mean_hdci(estimate) 

agg_pair <- dt_ma %>% ungroup() %>%  select(harmonized_pair_name,name_pair_id) %>% distinct(harmonized_pair_name,name_pair_id)
agg_ncor <- dt_ma %>% group_by(name_pair_id) %>% summarise(n_cor = sum(cor_num),
                                                           n_wcor = n())





dt <- t %>% 
  rowwise()  %>% 
  mutate(name_pair_id = as.factor(parse_number(param))) %>% 
  ungroup() %>% 
  left_join(agg_pair, by = "name_pair_id") %>% 
  rowwise()  %>% 
  mutate( x = unlist(str_split(harmonized_pair_name, "_"))[1],
          y = unlist(str_split(harmonized_pair_name, "_"))[2]) %>% 
  ungroup() %>% 
  left_join(agg_ncor, by = "name_pair_id") %>% 
  mutate(pooled_est_lbl =paste0("",as.character(format(round(((estimate)), digits=2), nsmall = 2)),""),
         cred_int_lbl =  paste0("<br>", "[", format(round((.lower), digits=2), nsmall = 2) ,
                                ", ", format(round((.upper), digits=2), nsmall = 2) ,"]",
                                "<br>"),
         k_lbl = paste0("*k: ", as.character(prettyNum(n_wcor, big.mark = "'")), "*"))  %>%
 rowwise() %>% 
   mutate(lbl_color = if_else(estimate >= .7, "grey90", "black"),
         x = gsub("^([[:alpha:]]+)", "**\\1**", x),
         x = gsub(" - ", "<br>", x),
         y = gsub("^([[:alpha:]]+)", "**\\1**", y),
         y = gsub(" - ", "<br>", y))%>% 
  mutate(lbls = str_arrange(paste0(x, "_", y)))


# creating labels to plot upper triangle of matrix  
labs <- crossing(x = unique(dt$y),
                 y =  unique(dt$y)) %>% 
  mutate(d = rnorm(n()))

dt_simple <- labs %>% select(x,y,d)
mat <- reshape2::dcast(data = dt_simple,formula = x~y,fun.aggregate = sum,value.var = "d")
mat <- column_to_rownames(mat, var = "x")
cor_mat <- as.matrix(mat)
cor_mat[lower.tri(cor_mat, diag = FALSE)] <- NA
cor_mat[ cor_mat == 0 ] <- NA


melted_cormat <- reshape::melt(cor_mat)
melted_cormat <- melted_cormat %>% filter(!is.na(value )) %>% 
  mutate(lbls = str_arrange(paste0(X1, "_", X2))) %>% 
  rename(re_x = X1, re_y = X2) %>% select(-value)


dt <- dt %>% left_join(melted_cormat, by = c("lbls")) %>% 
  # because of matching domains across categories
  # some pairs of measures contain the same characters 
  # but are not equivalent,and rows get duplicated, thus adding 
  # estimates for pairs we do not have data for 
  # (e.g., Propensity-Gambling_Frequency-Occupational vs. 
  # Propensity-Occupational_Frequency-Gambling)
  filter((x == re_x & y == re_y) | 
        (x == re_y & y == re_x) )



write_csv(dt, paste0(output_data_path, "cor_mat_convergent_domain_dat.csv"))
    



