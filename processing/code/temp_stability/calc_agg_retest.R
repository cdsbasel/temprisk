# DESCRIPTION -------------------------------------------------------------

#This script reads the complete set of retest correlations and computes aggregated correlation coefficients 
# Saves the resulting data frame as a csv file

# INPUT:
# complete_retest.csv  (i.e., file created from merge_retest.R. Need to specify location of file)
# OUTPUT:
# complete_agg_retest.csv
# 
# # Author(s): Alexandra Bagaini, Centre for Cognitive and Decision Sciences, Faculty of Psychology, University of Basel.


# PACKAGES ---------------------------------------------------------------

library(tidyverse)
library(metafor)

# FILES  ---------------------------------------------------

data_path <- c("processing/output/temp_stability/")
output_data_path <- c("processing/output/temp_stability/") # where will the plots be stored
retest_file <- "complete_retest.csv" # name of retest data 
source("helper_functions.R") # functions to aggregate effect sizes (adapted from:
#Pustejovsky, J.E. (2019). Sometimes, aggregating effect sizes is fine. https://www.jepusto.com/sometimes-aggregating-effect-sizes-is-fine/)



# LOAD DATA ---------------------------------------------------------------


col_spec <- cols(
  panel = col_character(),
  sample = col_character(),
  wave_id_t1 = col_character(),
  wave_year_t1 = col_double(),
  wave_id_t2 = col_character(),
  wave_year_t2 = col_double(),
  time_diff_mean = col_double(),
  time_diff_median = col_double(),
  time_diff_min = col_double(),
  time_diff_max = col_double(),
  time_diff_sd = col_double(),
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
  attrition_rate = col_double(),
  varcode = col_character(),
  cor_pearson = col_double(),
  cor_spearman = col_double(),
  icc2_1 = col_double(),
  cor_pearson_log = col_double(),
  cor_spearman_log = col_double(),
  icc2_1_log = col_double(),
  coeff_var_t1 = col_double(),
  coeff_var_t2 = col_double(),
  skewness_t1 = col_double(),
  skewness_t2 = col_double(),
  measure_category = col_character(),
  general_domain = col_character(),
  domain_name = col_character(),
  scale_type = col_character(),
  scale_length = col_double(),
  time_frame = col_double(),
  behav_type = col_character(),
  behav_paid = col_character(),
  continent = col_character(),
  country = col_character(),
  language = col_character(),
  data_collect_mode = col_character(),
  sample_type = col_character()
)




data <- read_csv(paste0(data_path,retest_file), col_types = col_spec) # or fread()


# DATA PREP. --------------------------------------------------------------------


dt <- data %>% 
  # avoiding Inf problem when transforming to Fisher z & set minimum values at 0
  mutate(cor_pearson = case_when(cor_pearson > 0.99 ~ 0.99,
                                 cor_pearson < 0 ~ 0,
                                 TRUE ~ cor_pearson),
         cor_pearson_log = case_when(cor_pearson_log > 0.99 ~ 0.99, 
                                     cor_pearson_log < 0 ~ 0,
                                     TRUE ~ cor_pearson_log),
         cor_spearman = case_when(cor_spearman > 0.99 ~ 0.99, 
                                  cor_spearman < 0 ~ 0,
                                  TRUE ~ cor_spearman),
         cor_spearman_log = case_when(cor_spearman_log > 0.99 ~ 0.99, 
                                      cor_spearman_log < 0 ~0,
                                      TRUE ~ cor_spearman_log),
         icc2_1 = case_when(icc2_1 > 0.99 ~ 0.99, 
                            icc2_1 < 0 ~ 0,
                            TRUE ~ icc2_1),
         icc2_1_log = case_when(icc2_1_log > 0.99 ~ 0.99, 
                                icc2_1_log < 0 ~ 0,
                                TRUE ~ icc2_1_log))



# CALCULATE AGGREGATE --------------------------------------------------------


age_bin<- c(5, 10, 20)
min_n <- c(30, 100, 250)
rho <- c(.1, .5, .9)
month_bin <- c(3,6,12)
metric <- c("pearson", "spearman", "icc2_1")
dt_transform <- c("none", "log")



comb_dt <- crossing(age_bin, min_n, month_bin,
                    rho, metric, dt_transform)


wcor_df <- NULL
for (i in 1:nrow(comb_dt)) {
  print(i)
  curr_rho_val <- comb_dt$rho[i]
  curr_metric <- comb_dt$metric[i]
  curr_transform <- comb_dt$dt_transform[i]
  curr_age_bin  <- comb_dt$age_bin[i]
  curr_min_n <- comb_dt$min_n[i]
  curr_mo <- comb_dt$month_bin[i]
  
  
  sub_dt <- dt %>%
    filter(year_age_group == curr_age_bin & n >= curr_min_n) %>% 
    mutate(time_diff_bin =  time_binning(x = time_diff_mean, year_bin =  curr_mo/12))
  
  
  
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
             time_diff_bin, # time info
             age_group, gender_group, # sample info
             measure_category, domain_name) %>% # measure info
    summarise(n_mean = mean(n),# average sample size of the ""raw" correlations included to calculate the effect size
              n_sd = sd(n), # SD of the sample sizes of the ""raw" correlations included to calculate the effect size
              mean_age = mean(age_mean), # average of the average age of respondents
              sd_age = sd(age_mean),# sd of the average age of respondents
              mean_attrition = mean(attrition_rate),
              sd_attrition = sd(attrition_rate),
              cor_num = n(), #  number "raw" correlations included to calculate the effect size
              wcor_z = agg_cor(yi = yi, vi = vi, r = curr_rho_val), # computing correlation aggregate
              vi_z = agg_v(yi = yi, vi = vi, r = curr_rho_val), # computing variance aggregate
              varcodes = list(unique(varcode)), # names of varcodes included the calculate the effect size
              .groups = "drop") %>% 
    mutate(sei_z = sqrt(vi_z),
           ci_lb_z = wcor_z - 1.96*sei_z,
           ci_ub_z = wcor_z + 1.96*sei_z,
           # transform back to r
           wcor = transf.ztor(wcor_z),
           ci_lb =  transf.ztor(ci_lb_z),
           ci_ub = transf.ztor(ci_ub_z),
           sei = (ci_ub - ci_lb)/(2 * 1.96), # compute standard error
           vi = sei^2,  # compute variance
           es_id = 1:n(),
           age_bin = curr_age_bin,
           min_n = curr_min_n,
           month_bin = curr_mo,
           rho_val = curr_rho_val,
           data_transform = curr_transform,
           cor_metric = curr_metric) 
  # %>% select(!ends_with("_z"))
  
  wcor_df <- bind_rows(agg_ma,wcor_df)
  
  
} # dt_comb

print("done!")


# write_rds(wcor_df, paste0(output_data_path,"complete_agg_retest.rds"))
wcor_df_simple <- wcor_df %>% select(-varcodes)
# write_csv(wcor_df_simple, paste0(output_data_path,"complete_agg_retest.csv"))

# saving  sud-datasets for analysis
wcor_df_simple2 <- wcor_df_simple %>% filter(age_bin == 10)
write_csv(wcor_df_simple2, paste0(output_data_path,"complete_agg_retest_yb10.csv"))
wcor_df_simple2 <- wcor_df_simple %>% filter(age_bin == 20)
write_csv(wcor_df_simple2, paste0(output_data_path,"complete_agg_retest_yb20.csv"))
wcor_df_simple2 <- wcor_df_simple %>% filter(age_bin == 5)
write_csv(wcor_df_simple2, paste0(output_data_path,"complete_agg_retest_yb5.csv"))
print("done!")
