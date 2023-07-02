
##### CALCULATE INTERCORRELATIONS#####

# DESCRIPTION -------------------------------------------------------------

#This script reads the panel's file containing information on the risk measures identified in the panel
#and the processed panel data. Using the latter and the calc_intercor function (see function script for more details) it calculates
# the intercorrelation for every risk measure pair-time interval-age_group-gender_group combination. This is done for different categories of age bins
# The  dataframes with the intercorrelations  (one with and another without the subject ids) are saved as csv + RDS files 

# File input: (need to specify location of files)
# PANEL_proc_data.csv : clean/processed panel data with variables of interest in LONG format (output of processing script)
#  panel_risk_info : rds file with list of risk preference measures that will be used to calculate retest correlations (output of processing script)
# calc_intercor_function.R : calc_intercor() function 

# File output: (need to specify destination of files)
# PANEL_intercor_data.rds: dataframe with correlations + respondent id (see calc_intercor_function.R script for details)
# PANEL_intercor_data.csv: dataframe with correlations  (see calc_intercor_function.R script for details) 

# Author(s): Alexandra Bagaini, Centre for Cognitive and Decision Sciences, Faculty of Psychology, University of Basel.


# PACKAGES ---------------------------------------------------------------

library(tidyverse)


# PANEL INFORMATION -------------------------------------------------------

panel <- "HRS"
sample <- "HRS_Core"

# PATH INFORMATION ---------------------------------------------------
preproc_data_path <- c("") # location of pre-processed panel data
intercor_data_wid_path <- c("") # intercor data with ids storage path
measure_info_path <- c("var_info/") # location of rds file containing info on the panel's risk measures to analyse
intercor_data_path <- c("processing/output/convergent_val/") # intercor data without ids storage path
age_range_data_path <- c("processing/") # age bins
func_path <- c("processing/code/convergent_val/") # location of function


#  FUNCTION ---------------------------------------------------

# load function to calculate intercorrelations
source(paste0(func_path,"calc_intercor_function.R"))

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

#_________________ READ risk_info _________________________#

risk_info <- read_rds("var_info/panel_risk_info.rds") #  file that contains the basic item information to be used for the analyses

# keep rows that only have the variables that will be analyzed
risk_info <- risk_info[[panel]] %>% filter(var_include == 1) %>% select(-c(panel, var_consider, var_include))
# dim(risk_info)  15 rows x  10 cols

#_________________ Create list of Risk Measure Pairs _________________________#


varcodes <- crossing(var_a = risk_info$varcode,
                     var_b = risk_info$varcode) %>% 
  filter(var_a != var_b)

risk_info_a <- risk_info
colnames(risk_info_a) <- paste0(colnames(risk_info_a), "_a")
risk_info_b <- risk_info
colnames(risk_info_b) <- paste0(colnames(risk_info_b), "_b")
# dim(varcodes)  210 rows x  2 cols

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
# dim(proc_data) 3535845 rows x 14 cols

#_________________ GET PANEL INFO _________________________#

panel_info <- proc_data %>% 
  select(c(continent:sample_type)) %>% 
  distinct() %>% 
  mutate(sample = sample) 


# COMPUTING CORRELATIONS & SAVING OUTPUT --------------------------------------------

# How many unique SURVEY waves are in the data? SORT IN ORDER!
waves <- sort(unique(proc_data$wave_id))
intercor2_df <- NULL
intercor_df <- NULL

#_________________AGE GROUP CATEGORIES _________________________#

for (CurrAgeBin in unique(age_groups$age_bin_categ)) {
  
  CurrAgeGroupSet <-  age_groups %>%  
    filter(age_bin_categ == CurrAgeBin)
  
  
  #_________________CORRELATIONS _________________________#
  
  results <- calc_intercor(panel = panel,
                           data = proc_data,
                           varcodes = varcodes,
                           wave_ids = waves,
                           age_group = CurrAgeGroupSet)
  
  
  #_________________MERGING MEASURE INFO _________________________#
  
  intercor <- results[[1]] # without ids
  intercor <- left_join(intercor, risk_info_a, by = "varcode_a") 
  intercor <- left_join(intercor, risk_info_b, by = "varcode_b") 
  intercor <-  intercor %>%   bind_cols(panel_info) %>%
    relocate(sample, .after = panel)
  
  intercor2 <- results[[2]] # with ids
  intercor2 <- left_join(intercor2, risk_info_a, by = "varcode_a") 
  intercor2 <- left_join(intercor2, risk_info_b, by = "varcode_b")
  intercor2 <-  intercor2 %>%   bind_cols(panel_info) %>%
    relocate(sample, .after = panel)
  
  #_________________SAVE OUTPUT _________________________#
  intercor_df <- bind_rows(intercor, intercor_df)
  intercor2_df <- bind_rows(intercor2, intercor2_df)
  
} # age bin category loop


write_csv(intercor_df, file = paste0(intercor_data_path,panel,"_intercor_data.csv"))
write_rds(intercor2_df, file = paste0(intercor_data_wid_path,panel,"_intercor_data.rds"))



print(paste0(sample, " intercorrelation computation done!"))
