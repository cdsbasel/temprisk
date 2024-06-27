
#  DESCRIPTION -------------------------------------------------------------

# Script reads the output of the brms_ma.R scripts and corrects the 
# meta-analytic estimates for attenuation due to the measurement error using
# Spearman's formula. Saves the output as a .csv file to be used for plotting

# Author(s): Alexandra Bagaïni(1) 
# (1)Centre for Cognitive and Decision Sciences, Faculty of Psychology, University of Basel.


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


dt_ma <- dt_ma %>% 
  group_by(domain_pair_lbl) %>% 
  mutate(domain_pair_id = as.character(cur_group_id())) %>% 
  ungroup()  %>% 
  group_by(meas_pair_lbl) %>% 
  mutate(meas_pair_id = as.character(cur_group_id())) %>% 
  ungroup()  %>% 
  rowwise() %>% 
  mutate(lbl_a = unlist(str_split(domain_pair_lbl, "_", 2))[1],
         measure_category_a = unlist(str_split(lbl_a, " -", 2))[1],
         domain_name_a = unlist(str_split(lbl_a, "- ", 2))[2],
         lbl_b = unlist(str_split(domain_pair_lbl, "_", 2))[2],
         measure_category_b = unlist(str_split(lbl_b, " -", 2))[1],
         domain_name_b = unlist(str_split(lbl_b, "- ", 2))[2],
         measure_category_a = abbrev_lbl(measure_category_a),
         measure_category_b = abbrev_lbl(measure_category_b),
         domain_name_a = abbrev_lbl(domain_name_a),
         domain_name_b = abbrev_lbl(domain_name_b)) %>% 
  ungroup()
sort(unique(dt_ma$domain_pair_lbl))
sort(unique(dt_ma$meas_pair_lbl))



# DOMAIN: CORRECT CURRENT MATRIX --------------------------------------------------


# save output for plotting

ma_fit <- read_rds( paste0(output_data_path, "fit_convergent_ma_domain", ".rds"))
my_vars = str_subset(variables(ma_fit), "b_domain_pair*")
my_regex = paste0(my_vars, collapse="|")
t <- ma_fit %>%
  spread_draws(!!sym(my_regex), regex=TRUE) %>% 
  pivot_longer(-c(.chain:.draw), names_to = "param", values_to = "estimate") %>% 
  mutate(estimate = transf.ztor(estimate)) %>% 
  group_by(param) %>%
  mean_hdci(estimate) 

agg_pair <- dt_ma %>% ungroup() %>%  select(domain_pair_lbl,domain_pair_id) %>% distinct(domain_pair_lbl,domain_pair_id)
agg_ncor <- dt_ma %>% group_by(domain_pair_id) %>% summarise(n_cor = sum(cor_num),
                                                             n_wcor = n())





dt <- t %>% 
  rowwise()  %>% 
  mutate(domain_pair_id = as.factor(parse_number(param))) %>% 
  ungroup() %>% 
  left_join(agg_pair, by = "domain_pair_id") %>% 
  rowwise()  %>% 
  mutate( x = unlist(str_split(domain_pair_lbl, "_"))[1],
          y = unlist(str_split(domain_pair_lbl, "_"))[2]) %>% 
  ungroup() %>% 
  left_join(agg_ncor, by = "domain_pair_id") %>% 
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



nlpar_dat <- read_csv("analysis/output/temp_stability/masc_nlpar_pred.csv") # mean_age = 0; prop_female_c = 0
nlpar_dat <- nlpar_dat %>% 
  filter(nlpar == "Reliability" & categ == "domain") %>% 
  select(x,.epred, measure)%>% 
  mutate(measure = case_when(measure == "Propensity" ~"pro",
                             measure == "Frequency" ~"fre",
                             measure == "Behaviour"  ~ "beh")) 

# rename labels
dat_masc_a <- nlpar_dat  %>% rename(rel_a = .epred, measure_category_a = measure,
                                    domain_name_a = x) %>% 
  mutate(x_new = name_lbl(measure = measure_category_a, domain = domain_name_a)) 
dat_masc_b <- nlpar_dat  %>% rename(rel_b = .epred, measure_category_b = measure,
                                    domain_name_b = x) %>% 
  mutate(y_new = name_lbl(measure = measure_category_b, domain = domain_name_b))



# join new data
dt <- dt %>% 
  mutate(x_new = gsub("\\**", "", x),
         x_new = gsub("<br>", " - ", x_new),
         y_new = gsub("\\**", "", y),
         y_new = gsub("<br>", " - ", y_new)) %>% 
  left_join(dat_masc_a, by = c("x_new")) %>% 
  left_join(dat_masc_b, by = c("y_new")) %>% 
  rowwise() %>% 
  mutate(mean_rel = mean(c(rel_a, rel_b))) %>% 
  ungroup() %>% 
  mutate(estimate_corr = estimate/(sqrt(rel_a*rel_b)), # correct for attenuation and cap output
         estimate_corr = if_else(estimate_corr > 1, 1, estimate_corr),
         .lower_corr = .lower/(sqrt(rel_a*rel_b)), # correct for attenuation and cap output
         .lower_corr = if_else(.lower_corr > 1, 1, .lower_corr),
         .upper_corr = .upper/(sqrt(rel_a*rel_b)), # correct for attenuation and cap output
         .upper_corr = if_else(.upper_corr > 1, 1, .upper_corr)) %>% 
  mutate(pooled_est_lbl =paste0("",as.character(format(round(((estimate_corr)), digits=2), nsmall = 2)),""),
         cred_int_lbl =  paste0("<br>", "[", format(round((.lower_corr), digits=2), nsmall = 2) ,
                                ", ", format(round((.upper_corr), digits=2), nsmall = 2) ,"]",
                                "<br>"),
         lbl_color = if_else(estimate_corr >= .7, "grey90", "black"),
         k_lbl = paste0("*k: ", as.character(prettyNum(n_wcor, big.mark = "'")), "*"))


write_csv(dt, paste0(output_data_path, "cor_mat_convergent_domain_dat_corrected.csv"))





# MEASURE: CORRECT CURRENT MATRIX --------------------------------------------------


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




ma_dat_pair <- dt_ma %>% distinct(meas_pair_id,meas_pair_lbl)
ma_dat_ncor <- dt_ma %>% group_by(meas_pair_id) %>% summarise(n_cor = sum(cor_num),
                                                              n_wcor = n())



dt <- t %>% 
  rowwise()  %>% 
  mutate(meas_pair_id = as.character(parse_number(param))) %>% 
  ungroup() %>% 
  left_join(ma_dat_pair, by = "meas_pair_id") %>% 
  rowwise()  %>% 
  mutate( x = unlist(str_split(meas_pair_lbl, "_"))[1],
          y = unlist(str_split(meas_pair_lbl, "_"))[2]) %>% 
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


nlpar_dat <- read_csv("analysis/output/temp_stability/masc_nlpar_pred.csv") # mean_age = 0; prop_female_c = 0, item_num_c = 0
nlpar_dat <- nlpar_dat %>% 
  filter(nlpar == "Reliability" & categ == "all") %>% 
  select(x,.epred, measure)

# rename labels
dat_masc_a <- nlpar_dat  %>% rename(rel_a = .epred,  x_new = measure) %>% select(rel_a, x_new)
dat_masc_b <- nlpar_dat   %>% rename(rel_b = .epred,  y_new = measure)%>% select(rel_b, y_new)



# join new data
dt <- dt %>% 
  mutate(x_new = gsub("\\*\\*", "", x),
         y_new =gsub("\\*\\*", "", y)) %>% 
  left_join(dat_masc_a, by = c("x_new")) %>% 
  left_join(dat_masc_b, by = c("y_new")) %>% 
  rowwise() %>% 
  mutate(mean_rel = mean(c(rel_a, rel_b))) %>% 
  ungroup() %>% 
  mutate(estimate_corr = estimate/(sqrt(rel_a*rel_b)), # correct for attenuation and cap output
         estimate_corr = if_else(estimate_corr > 1, 1, estimate_corr), 
         .lower_corr = .lower/(sqrt(rel_a*rel_b)), # correct for attenuation and cap output
         .lower_corr = if_else(.lower_corr > 1, 1, .lower_corr),
         .upper_corr = .upper/(sqrt(rel_a*rel_b)), # correct for attenuation and cap output
         .upper_corr = if_else(.upper_corr > 1, 1, .upper_corr)) %>% 
  mutate(pooled_est_lbl =paste0("",as.character(format(round(((estimate_corr)), digits=2), nsmall = 2)),""),
         cred_int_lbl =  paste0("<br>", "[", format(round((.lower_corr), digits=2), nsmall = 2) ,
                                ", ", format(round((.upper_corr), digits=2), nsmall = 2) ,"]",
                                "<br>"),
         lbl_color = if_else(estimate_corr >= .7, "grey90", "black"),
         k_lbl = paste0("*k: ", as.character(prettyNum(n_wcor, big.mark = "'")), "*"))


write_csv(dt, paste0(output_data_path, "cor_mat_convergent_measure_dat_corrected.csv"))


