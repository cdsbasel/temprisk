#### MERGE TEST INTERCORRELATIONS #####

# DESCRIPTION -------------------------------------------------------------

#This script reads and binds all the PANEL_intercor.csv files together to create a single file to use for the analyses.
#Scripts also reads the PANEL_intercor.rds to count the unique number of waves/responses/respondents and save info in a table
#Input: PANEL_intercor.csv & .rds
#Output: complete_intercor.csv & complete_intercor_info.csv

# Author(s): Alexandra Bagaini, Centre for Cognitive and Decision Sciences, Faculty of Psychology, University of Basel.


# LIBRARIES ---------------------------------------------------------------

library(tidyverse)

source("helper_functions.R")

# FILES OF INTEREST ---------------------------------------------------

intercor_data_path <- c("processing/output/convergent_val/") # where are the intercor data files stored
retest_data_wid_path <- c("InterCor/")

# csv file list 
csv_file_list <-  list.files(path = intercor_data_path, pattern = "_intercor_data.csv", full.names = TRUE) # list names of intercor csv files

# rds file list 
rds_file_list <- list.files(path = retest_data_wid_path, pattern = "_intercor_data.rds",full.names = TRUE) # list names of retest rds files


#_________________COMBINING FILES:  _________________________#


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

csv_main <- do.call(bind_rows,
                    lapply(csv_file_list, function(i){read_csv(i,col_types = col_spec)}))

write_csv(csv_main,paste0(intercor_data_path, "complete_intercor.csv"))

print("complete_intercor file created!")


# COMPUTE SUMMARY STATS -----------------------------------------------------------
# creating summnary table of included panels (Appendix/Suppl. Material)
dt_info <- NULL
for (curr_rds in rds_file_list) {
  print(curr_rds)
  dt <- read_rds(curr_rds)
  # fre_domains <- "alc|sex|soc|eth|dru|sex|smo"
  dt <- dt %>%
    filter(age_group != "10-90" &
             gender_group != "all" & 
             year_age_group == 10 & 
             n >= 30)
     
  
  
  
  dt_info <- bind_rows(tibble(panel = unique(dt$panel),
                              sample = unique(dt$sample),
                              continent = unique(dt$continent),
                              country = unique(dt$country),
                              collect_mode = unique(dt$data_collect_mode),
                              measure_categ =  abbrev_categ(unique(c(dt$measure_category_a, dt$measure_category_b))),
                              domain_name = abbrev_domain(unique(c(dt$domain_name_a, dt$domain_name_b))),
                              unique_meas =  length(unique(c(dt$varcode_a,dt$varcode_b))),
                              cor_num = nrow(dt),
                              unique_id = length(unique(unlist(dt$id_list))),
                              unique_resp =  length(unique(unlist(dt$resp_id_list)))),
                       dt_info)
  
}

sum(dt_info$cor_num)
sum(dt_info$unique_id)
sum(dt_info$unique_resp)
sum(dt_info$unique_meas)



dt_info <- dt_info %>% mutate(cor_num = format(cor_num, big.mark = ","),
                              unique_id = format(unique_id, big.mark = ","),
                              unique_resp =  format(unique_resp, big.mark = ","),
                              collect_mode = case_when(collect_mode == "Interview" ~ "Int.",
                                                       collect_mode == "Online" ~ "Onl.",
                                                       collect_mode == "Self-administered" ~ "Self-adm.",
                                                       collect_mode == "Laboratory" ~ "Lab."))

write_csv(dt_info,paste0(intercor_data_path, "complete_intercor_info.csv"))

