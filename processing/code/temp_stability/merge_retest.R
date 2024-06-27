
# DESCRIPTION -------------------------------------------------------------

#This script reads and binds all the PANEL_retest.csv files together to create a single file to use for the analyses.
#Scripts also reads the PANEL_retest.rds to count the unique number of waves/responses/respondents and save info in a table
#Input: PANEL_retest.csv & .rds
#Output: complete_retest.csv & complete_retest_info.csv

# Author(s): Alexandra Bagaini, Centre for Cognitive and Decision Sciences, Faculty of Psychology, University of Basel.


# LIBRARIES ---------------------------------------------------------------

library(tidyverse)



# FUNCTIUON ---------------------------------------------------------------

source("helper_functions.R")

# FILES OF INTEREST ---------------------------------------------------

retest_data_path <- c("processing/output/temp_stability/") # where are the retest data files stored
retest_data_wid_path <-  c("~/Documents/TSRP/Data/", "/Volumes/usb_drive/") # where are the retest data files stored

# csv file list 
csv_file_list <-  list.files(path = retest_data_path, pattern = "retest_data.csv", full.names = TRUE) # list names of retest csv files

# rds file list 
rds_file_list <- list.files(path = retest_data_wid_path, pattern = "retest_data.rds",full.names = TRUE, recursive = TRUE) # list names of retest rds files


#_________________COMBINING FILES:  _________________________#


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
  item_num = col_double(),
  continent = col_character(),
  country = col_character(),
  language = col_character(),
  data_collect_mode = col_character(),
  sample_type = col_character()
)


csv_main <- do.call(bind_rows,
                    lapply(csv_file_list, function(i){read_csv(i,col_types = col_spec)}))



write_csv(csv_main,paste0(retest_data_path, "complete_retest.csv"))

print("complete_retest file created!")





# COMPUTE SUMMARY STATS -----------------------------------------------------------
# creating summnary table of included panels (Appendix/Suppl. Material)
dt_info <- NULL
for (curr_rds in rds_file_list) {
  print(curr_rds)
  dt <- read_rds(curr_rds)
  dt <- dt %>% filter(year_age_group == 10 & n >= 30 & age_group != "10-90" & gender_group != "all")
  
  dt_info <- bind_rows(tibble(panel = unique(dt$panel),
                              sample = unique(dt$sample),
                              continent = unique(dt$continent),
                              country = unique(dt$country),
                              sample_type = unique(dt$sample_type),
                              collect_mode = unique(dt$data_collect_mode),
                              measure_categ =  abbrev_categ(dt$measure_category),
                              domain_name = abbrev_domain(dt$domain_name),
                              unique_meas =  length(unique(dt$varcode)),
                              unique_waves = length(unique(c(dt$wave_id_t1, dt$wave_id_t2))),
                              retest_int_min = as.numeric(min(dt$time_diff_mean)),
                              retest_int_median = as.numeric(median(dt$time_diff_mean)),
                              retest_int_mean = as.numeric(mean(dt$time_diff_mean)),
                              retest_int_max = as.numeric(max(dt$time_diff_mean)),
                              cor_num = nrow(dt),
                              unique_id = length(unique(unlist(dt$id_list))),
                              unique_resp =  length(unique(unlist(dt$resp_id_list)))),
                       dt_info)
  
}

sum(dt_info$cor_num)
sum(dt_info$unique_id)
sum(dt_info$unique_resp)
sum(dt_info$unique_meas)
median(dt_info$unique_meas)
sum(dt_info$unique_waves)
mean(dt_info$retest_int_median)
mean(dt_info$retest_int_max)
min(dt_info$retest_int_min)

dt_info <- dt_info %>% mutate(cor_num = format(cor_num, big.mark = ","),
                              unique_id = format(unique_id, big.mark = ","),
                              unique_resp =  format(unique_resp, big.mark = ","),
                              retest_int_min = format(round(retest_int_min, 2), nsmall = 2),
                              retest_int_median = format(round(retest_int_median, 2), nsmall = 2),
                              retest_int_mean = format(round(retest_int_mean, 2), nsmall = 2),
                              retest_int_max = format(round(retest_int_max, 2), nsmall = 2),
                              collect_mode = case_when(collect_mode == "Interview" ~ "Int.",
                                                       collect_mode == "Online" ~ "Onl.",
                                                       collect_mode == "Self-administered" ~ "Self-adm.",
                                                       collect_mode == "Laboratory" ~ "Lab."))

write_csv(dt_info,paste0(retest_data_path, "complete_retest_info.csv"))

