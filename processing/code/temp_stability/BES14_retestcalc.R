
##### CALCULATE TEST RETEST CORRELATIONS#####

# DESCRIPTION -------------------------------------------------------------

#This script reads the panel's file containing information on the risk measures identified in the panel
#and the processed panel data. Using the latter and the calc_retest function (see function script for more details) it calculates
# the retest correlation for every risk measure-time interval-age_group-gender_group combination. This is done for different categories of age bins
# The  dataframes with the retest information (one with and another without the subject ids) are saved as csv + RDS files 

# File input: (need to specify location of files)
# PANEL_proc_data.csv : clean/processed panel data with variables of interest in LONG format (output of processing script)
#  panel_risk_info : rds file with list of risk preference measures that will be used to calculate retest correlations (output of processing script)
# calc_retest_function.R : calc_retest() function 

# File output: (need to specify destination of files)
# PANEL_retest_data.rds: dataframe with correlations + respondent id (see calc_retest_function.R script for details)
# PANEL_retest_data.csv: dataframe with correlations  (see calc_retest_function.R script for details) 

# Author(s): Alexandra Bagaini, Centre for Cognitive and Decision Sciences, Faculty of Psychology, University of Basel.


# PACKAGES ---------------------------------------------------------------

library(tidyverse)


# PANEL INFORMATION -------------------------------------------------------

panel <- "BES14"
sample <- "BES14"

# PATH INFORMATION ---------------------------------------------------
preproc_data_path <- c("") # location of pre-processed panel data
retest_data_wid_path <- c("") # retest data with ids storage path
measure_info_path <- c("var_info/") # location of rds file containing info on the panel's risk measures to analyse
retest_data_path <- c("processing/output/temp_stability/") # retest data without ids storage path
age_range_data_path <- c("processing/") # age bins
func_path <- c("processing/code/temp_stability/") # location of function

# RETEST FUNCTION ---------------------------------------------------

# load function to calculate retest correlations
source(paste0(func_path,"calc_retest_function.R"))

# AGE GROUP INFORMATION ---------------------------------------------------

#_________________ READ age_group.csv _________________________#

col_specs_age_group_info <- cols(
  min = col_double(),
  max = col_double(),
  group = col_character(),
  age_bin_categ = col_double()
)

age_groups <- read_csv(paste0(age_range_data_path,"age_group.csv"), col_types = col_specs_age_group_info)

# VARIABLE INFORMATION ---------------------------------------------------

#_________________ READ RiskMeasures_Analyse.csv _________________________#

risk_info <- read_rds("var_info/panel_risk_info.rds") #  file that contains the basic item information to be used for the analyses

# keep rows that only have the variables that will be analyzed
risk_info <- risk_info[[panel]] %>% filter(var_include == 1) %>% select(-c(panel, var_consider, var_include))
# dim(risk_info)  2 rows x  10 cols


# READING CLEAN/PROCESSED PANEL DATA ---------------------------------------------------

#_________________ READ proc_data.csv _________________________#

col_spec_proc_data <- cols(
  panel = col_character(),
  id = col_character(),
  age = col_double(),
  gender = col_double(),
  wave_id = col_character(),
  wave_year = col_double(),
  date = col_date(format = ""),
  varcode = col_character(),
  response = col_double(),
  continent = col_character(),
  country = col_character(),
  language = col_character(),
  data_collect_mode = col_character(),
  sample_type = col_character()
)

proc_data <- read_csv(paste0(preproc_data_path, panel, "_proc_data.csv"), col_types = col_spec_proc_data)  
# dim(proc_data) 262444 rows x 14 cols

#_________________ GET PANEL INFO _________________________#

panel_info <- proc_data %>% select(c(continent:sample_type)) %>% distinct() %>%
  mutate(sample = sample) 
panel <- "BES"
# COMPUTING CORRELATIONS & SAVING OUTPUT --------------------------------------------

# How many unique SURVEY waves are in the data? SORT IN ORDER!
waves <- unique(proc_data$wave_id) # c("W1","W7", "W8","W20")

retest2_df <- NULL
retest_df <- NULL

#_________________AGE GROUP CATEGORIES _________________________#

for (CurrAgeBin in unique(age_groups$age_bin_categ)) {
  
  CurrAgeGroupSet <-  age_groups %>%  
    filter(age_bin_categ == CurrAgeBin)
  
  
  #_________________CORRELATIONS _________________________#
  
  results <- calc_retest(panel = panel,
                         data = proc_data,
                         varcodes = risk_info$varcode,
                         wave_ids = waves,
                         age_group = CurrAgeGroupSet)
  
  
  #_________________MERGING MEASURE & PANEL INFO _________________________#
  
  
  retest <- results[[1]] # without ids
  retest <- left_join(retest, risk_info, by = "varcode") %>% bind_cols(panel_info) %>%
    relocate(sample, .after = panel)
  
  retest2 <- results[[2]] # with ids
  retest2 <- left_join(retest2, risk_info, by = "varcode") %>% bind_cols(panel_info) %>%
    relocate(sample, .after = panel)
  
  #_________________SAVE OUTPUT _________________________#
  retest_df <- bind_rows(retest, retest_df)
  retest2_df <- bind_rows(retest2, retest2_df)
  
} # age bin category loop


write_csv(retest_df, file = paste0(retest_data_path,sample,"_retest_data.csv"))
write_rds(retest2_df, file = paste0(retest_data_wid_path,sample,"_retest_data.rds"))

print(paste0(panel, " retest correlation computation done!"))
