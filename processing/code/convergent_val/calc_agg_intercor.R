# DESCRIPTION -------------------------------------------------------------

#This script reads the complete set of rintercorrelations and computes aggregated correlation coefficients 
# Saves the resulting data frame as a csv file

# INPUT:
# complete_retest.csv  (i.e., file created from merge_retest.R. Need to specify location of file)
# OUTPUT:
# complete_agg_intercor.rds and .csv
# 
# # Author(s): Alexandra Bagaini, Centre for Cognitive and Decision Sciences, Faculty of Psychology, University of Basel.


# PACKAGES ---------------------------------------------------------------

library(tidyverse)
library(metafor)



# FUNCTIONS ---------------------------------------------------------------

source("helper_functions.R") # functions to aggregate effect sizes (adapted from:
#Pustejovsky, J.E. (2019). Sometimes, aggregating effect sizes is fine. https://www.jepusto.com/sometimes-aggregating-effect-sizes-is-fine/)


# FILES  ---------------------------------------------------

data_path <- c("processing/output/convergent_val/")
output_data_path <- c("processing/output/convergent_val/") # where will the plots be stored
retest_file <- "complete_intercor.csv" # name of retest data 



# LOAD DATA ---------------------------------------------------------------

col_spec <- cols(
  panel = col_character(),
  sample = col_character(),
  wave_id = col_character(),
  wave_year = col_double(),
  year_age_group = col_double(),
  age_group = col_character(),
  age_mean = col_double(),
  age_median = col_double(),
  age_min = col_double(),
  age_max = col_double(),
  age_sd = col_double(),
  gender_group = col_character(),
  prop_female = col_double(),
  n = col_double(),
  varcode_a = col_character(),
  varcode_b = col_character(),
  cor_pearson = col_double(),
  cor_spearman = col_double(),
  icc2_1 = col_double(),
  cor_pearson_log = col_double(),
  cor_spearman_log = col_double(),
  icc2_1_log = col_double(),
  coeff_var_a = col_double(),
  coeff_var_b = col_double(),
  skewness_a = col_double(),
  skewness_b = col_double(),
  measure_category_a = col_character(),
  general_domain_a = col_character(),
  domain_name_a = col_character(),
  scale_type_a = col_character(),
  scale_length_a = col_double(),
  time_frame_a = col_double(),
  behav_type_a = col_character(),
  behav_paid_a = col_character(),
  measure_category_b = col_character(),
  general_domain_b = col_character(),
  domain_name_b = col_character(),
  scale_type_b = col_character(),
  scale_length_b = col_double(),
  time_frame_b = col_double(),
  behav_type_b = col_character(),
  behav_paid_b = col_character(),
  continent = col_character(),
  country = col_character(),
  language = col_character(),
  data_collect_mode = col_character(),
  sample_type = col_character()
)




data <- read_csv(paste0(data_path,retest_file))


# DATA PREP. --------------------------------------------------------------------

dt <- data %>% 
  # avoiding Inf problem when transforming to Fisher z
  mutate(cor_pearson = case_when(cor_pearson > 0.99 ~ 0.99,
                                 TRUE ~ cor_pearson),
         cor_pearson_log = case_when(cor_pearson_log > 0.99 ~ 0.99, 
                                     TRUE ~ cor_pearson_log),
         cor_spearman = case_when(cor_spearman > 0.99 ~ 0.99, 
                                  TRUE ~ cor_spearman),
         cor_spearman_log = case_when(cor_spearman_log > 0.99 ~ 0.99, 
                                      TRUE ~ cor_spearman_log),
         icc2_1 = case_when(icc2_1 > 0.99 ~ 0.99, 
                            TRUE ~ icc2_1),
         icc2_1_log = case_when(icc2_1_log > 0.99 ~ 0.99, 
                                TRUE ~ icc2_1_log)) %>% 
  # creating labels
  mutate(meas_pair_lbl = measure_pair_lbl(m1 = measure_category_a, m2 = measure_category_b),
         name_a_lbl = name_lbl(measure = measure_category_a, domain = domain_name_a),
         name_b_lbl = name_lbl(measure = measure_category_b, domain = domain_name_b),
         name_pair_lbl = paste0(name_a_lbl, "_", name_b_lbl))


 


# CALCULATE AGGREGATE --------------------------------------------------------


age_bin<- c(5, 20, 10)
min_n <- c(30,100,250)
rho <- c(.1, .9,.5)
metric <- c("pearson", "spearman", "icc2_1")
dt_transform <- c("none", "log")



comb_dt <- crossing(age_bin, min_n,
                    rho, metric, dt_transform)


wcor_df <- NULL
for (i in 1:nrow(comb_dt)) {
  print(i)
  
  curr_rho_val <- comb_dt$rho[i]
  curr_metric <- comb_dt$metric[i]
  curr_transform <- comb_dt$dt_transform[i]
  curr_age_bin  <- comb_dt$age_bin[i]
  curr_min_n <- comb_dt$min_n[i]
  
  
  sub_dt <- dt %>%
    filter(year_age_group == curr_age_bin & n >= curr_min_n)
  
  
  
  if (curr_metric == "pearson" & curr_transform == "none") {
    ma_dat <- escalc(measure = "ZCOR", data = sub_dt, ri = cor_pearson, ni = n) 
  }
  
  if (curr_metric == "spearman"& curr_transform == "none") {
    ma_dat <- escalc(measure = "ZCOR", data = sub_dt, ri = cor_spearman, ni = n) 
  }
  
  
  if (curr_metric == "icc2_1"& curr_transform == "none") {
    ma_dat <- escalc(measure = "ZCOR", data = sub_dt, ri = icc2_1, ni = n) 
  }
  
  
  if (curr_metric == "pearson" & curr_transform == "log") {
    ma_dat <- escalc(measure = "ZCOR", data = sub_dt, ri = cor_pearson_log, ni = n) 
  }
  
  if (curr_metric == "spearman"& curr_transform == "log") {
    ma_dat <- escalc(measure = "ZCOR", data = sub_dt, ri = cor_spearman_log, ni = n) 
  }
  
  
  if (curr_metric == "icc2_1" & curr_transform == "log") {
    ma_dat <- escalc(measure = "ZCOR", data = sub_dt, ri = icc2_1_log, ni = n) 
  }
  
  
  # CALC WCOR ----------------------------------------------------------------
  
  agg_ma <- ma_dat %>% 
    group_by(panel, sample, continent, country, language, data_collect_mode, sample_type, # panel info
             age_group, gender_group, # sample info
             meas_pair_lbl, name_pair_lbl, name_a_lbl, name_b_lbl,
             measure_category_a, domain_name_a,
             measure_category_b, domain_name_b) %>% # measure info
    # aggregating
    summarise(n_mean = mean(n),
              n_sd = sd(n), # SD of the sample sizes of the ""raw" correlations included to calculate the effect size
              mean_age = mean(age_mean), # average of the average age of respondents
              sd_age = sd(age_mean),# sd of the average age of respondents
              cor_num = n(), # total of number "raw" correlations included to calculate the effect size
              wcor_z = agg_cor(yi = yi, vi = vi, r = curr_rho_val), 
              vi_z = agg_v(yi = yi, vi = vi, r = curr_rho_val),
              varcodes_a = list(unique(varcode_a)),
              varcodes_b = list(unique(varcode_b)), # names of varcodes included the calculate the effect size
              .groups = "drop") %>% 
    mutate(sei_z = sqrt(vi_z),
           ci_lb_z = wcor_z - 1.96*sei_z,
           ci_ub_z = wcor_z + 1.96*sei_z,
           # transform back to r
           wcor = transf.ztor(wcor_z),
           ci_lb =  transf.ztor(ci_lb_z),
           ci_ub = transf.ztor(ci_ub_z),
           sei = (ci_ub - ci_lb)/(2 * 1.96),
           vi = sei^2,
           es_id = 1:n(),
           age_bin = curr_age_bin,
           min_n = curr_min_n,
           rho_val = curr_rho_val,
           data_transform = curr_transform,
           cor_metric = curr_metric) 
  # %>% select(!ends_with("_z"))
  
  
  wcor_df <- bind_rows(agg_ma,wcor_df)
  
  
} # dt_comb


# write_rds(wcor_df, paste0(output_data_path,"complete_agg_intercor.rds"))
wcor_df_simple <- wcor_df %>% select(-c(varcodes_a,varcodes_b))
# write_csv(wcor_df_simple, paste0(output_data_path,"complete_agg_intercor.csv"))


# saving  sud-datasets for analysis
wcor_df_simple2 <- wcor_df_simple %>% filter(age_bin == 10)
write_csv(wcor_df_simple2, paste0(output_data_path,"complete_agg_intercor_yb10.csv"))
wcor_df_simple2 <- wcor_df_simple %>% filter(age_bin == 20)
write_csv(wcor_df_simple2, paste0(output_data_path,"complete_agg_intercor_yb20.csv"))
wcor_df_simple2 <- wcor_df_simple %>% filter(age_bin == 5)
write_csv(wcor_df_simple2, paste0(output_data_path,"complete_agg_intercor_yb5.csv"))
print("done!")


