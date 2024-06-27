
##### PRE-PROCESSING: #####

#  DESCRIPTION -------------------------------------------------------------

#This script uses the file containing information on the risk measures & other variables of interest
# to select the relevant columns in the panel's original dataset. Where relevant, recoding/fixing of variables is done. 
# The distribution of risk preference measures is checked + other data quality checks are performed.
# A separate csv file of the risk preference measures to use to calculate retest & inter- correlations is saved. 
# The processed/clean data is saved as a csv file (long format). 

# File input: (need to specify location of files)
# panel_variable_info: rds file containing information of the variables to extract from each wave of the panel 
# PANEL data: files with the original panel data 

# File output:(need to specify location of storage for files) 
# PANEL_proc_data.csv : clean/processed panel data with variables of interest in LONG format
# PANEL_risk_var_info.csv : list of risk preference measures that will be used to calculate retest & inter- correlations
# PANEL_inconsistent_dems.csv : summary table of the % of (unique) respondents with year of birth or gender that is not consistently reported across waves 
# PANEL_resp_overview.html : summary statistics of the data in PANEL_proc_data.csv by wave id

# Author(s): Alexandra Bagaini and Rui Mata, Centre for Cognitive and Decision Sciences, Faculty of Psychology, University of Basel.

# PACKAGES ---------------------------------------------------------------

library(tidyverse) # data wrangling 
library(readxl) # reading excel file
library(lubridate) # dealing with dates
library(summarytools) # data check


# PATH/FILES  -----------------------------------------------------

var_book <- read_rds("var_info/panel_variable_info.rds")# panel risk measures
measure_info_path <- c("var_info/indv_panel_var_info/") 
panel_data_path <- c("~/Documents/TSRP/Data/GCOE/RawData/") # where the raw panel data is stored
preproc_data_path <- c("~/Documents/TSRP/Data/GCOE/ProcData/") # where to save processed panel data
panel_name <- "GCOE_JP"

# VARIABLE INFORMATION ----------------------------------------------------

panel_var <- var_book[[panel_name]]

# combining id & risk information
all_info <- bind_rows(panel_var$Measures, panel_var$ID, panel_var$Dems)
risk_info <- panel_var$Measures
newvars_info <- panel_var$NewVars
# READING DATA -----------------------------------------------------

#________________READ MAIN DATA FILES___________________# 

# list of unique waves
wave_list <- unique(na.omit(all_info$wave_id))

wave_num <- 1
data_list <- NULL

for (CurrWave in wave_list) {
  
  # select wave relevant info
  sub_all_info <- all_info %>% filter(wave_id == CurrWave) 
  
  
  vars_of_interest <- sub_all_info$origin_varcode

  file_of_interest_name <- unique(na.omit(sub_all_info$varfile)) # in which file are the relevant vars stored
  file_of_interest <- list.files(path = panel_data_path, pattern = file_of_interest_name, full.names = TRUE, recursive = TRUE) 
  
  # open the relevant file
  wave_data <- read.csv(file_of_interest) # opens the relevant file
  
  # select vars of interest
  wave_data <- wave_data %>% select(all_of(vars_of_interest)) %>%  mutate(wave_id = as.character(CurrWave))
  
  # replacing var codes
  colnames_found <- match(colnames(wave_data), vars_of_interest, nomatch = 0)
  colnames(wave_data)[colnames(wave_data) %in% vars_of_interest] <- sub_all_info$varcode[colnames_found]
  
  wave_data <- wave_data %>%  mutate(id = paste0(as.character(id_1), "_", as.character(id_2))) %>%  # create unique id
    select(-c(id_1, id_2))
  
  # place in list
  data_list[[wave_num]] <- wave_data
  wave_num <- wave_num + 1
}  # wave year loop


#________________BIND DATA FROM DIFFERENT WAVES TOGETHER___________________# 
main <- bind_rows(data_list)
main <- main %>%  select(sort(names(.))) # sort columns name order
main <- main %>% mutate_if(is.integer, ~ as.numeric(.x))
# dim(main) 50421 rows  x  71 cols

# FIX VARS  --------------------------------------------------------------------

#________________FIX VARS: REMOVE DUPLICATE IDS ___________________#

#avoid IDs that within a wave are not unique
main <- main %>% 
  group_by(id, wave_id) %>% 
  mutate(id_count = n()) %>%
  ungroup() 

main %>% group_by(wave_id) %>% summarise(sum(id_count > 1)) 
main <- main %>%  filter(id_count == 1) %>% select(-id_count)

#________________FIX VARS: REPLACING MISSING & NON READABLE RESPONSES___________________# 

main <- main %>% 
  mutate(RISK_GEN_ORD11B = case_when(RISK_GEN_ORD11B == 99 ~ NA_real_,
                            TRUE ~ RISK_GEN_ORD11B))


replace_dk <- function(x) (ifelse(x > 7, NA, x))  # for most vars, NA responses are coded 8 or 9
main <- main %>%
  mutate_at(vars(-c("id", "wave_id","yob", "gender", "RISK_GEN_ORD11B", "agegroup")), replace_dk) 



#________________FIX VARS: DATES ___________________#
# If month & date are missing replace with the mean of available dates.
# If date information is completely missing from the raw data AND cannot be estimated from panel documentation, use 15th of June 

#  No interview date data available. We use information from the SurveyOverview.pdf 
#  In general data collection took place between Jan and March each year 
main <- main %>% mutate(date = as.Date(paste(as.numeric(wave_id),"02","01",sep="-"), "%Y-%m-%d")) 

main <- main %>% group_by(wave_id) %>% mutate(wave_year = round(mean(year(date)))) %>% ungroup() # calculate the general year for that wave

#________________FIX YOB & AGE & AGE RANGE___________________#

# for some years, variables are empty but can be filled/reconstructed from previous or future waves...
main <- main %>%
  mutate(yob = case_when(wave_id == "2005" ~ yob + 1925,
                         TRUE ~ yob)) %>% 
  group_by(id) %>%
  fill(yob,.direction = 'downup') %>%  # NAs get replaced with neighboring yob for each respondent
  ungroup()

# identify respondents with not the same yob reported across the waves
main <- main %>%
  group_by(id) %>%
  mutate(non_unique_yob = length(unique(na.omit(yob))) > 1) %>% # 
  ungroup() 

# calculate age if missing
main <- main %>%
  mutate(age = case_when(!is.na(yob) ~ year(date) - yob,
                         # for waves 2003 & 2004 only the age group is recorded (choose halfway point)
                         wave_id == "2003"  & !is.na(agegroup) & is.na(yob) ~ ((agegroup*10)+5), 
                         wave_id == "2004"  & !is.na(agegroup) & is.na(yob) ~ ((agegroup*10)+5))) %>%  # see when the age is bordernile!!!!
  ungroup() %>% select(-c(agegroup, yob))


# differences in age between accross for each participant should not be too different (+/- 1 year) from the time difference accross waves
# main <- main %>% group_by(id) %>%
#   mutate(age_diff = max(age) - min(age),
#          time_diff = max(year(date)) - min(year(date))) %>% 
#   ungroup() %>% 
#   mutate(non_unique_yob2 = age_diff > (time_diff + 1) | age_diff < (time_diff - 1))


# only keep respondents aged between 20 & 90y.o. (based on data documentation adults 20-69 y.o were recruited)
main <- main %>%  filter(age %in% c(20:90)) 


#________________FIX VARS: GENDER___________________#

# recode gender
main <- main %>%
  mutate(gender = case_when(gender == 2 ~ 1,
                            gender == 1 ~ 0))

#no missing gender data

#  identify respondents with not the same gender reported across the waves
main <- main %>%
  group_by(id) %>%
  mutate(non_unique_gender = length(unique(na.omit(gender))) >1) %>%  
  ungroup()

inconsistent_dems <- main %>% 
  distinct(id, non_unique_gender, non_unique_yob) %>%
  summarise(perc_gender = 100*(sum(non_unique_gender)/n()),
            perc_yob = 100*(sum(non_unique_yob)/n())) %>% 
  mutate(panel = panel_name)

#________________FIX VARS: RISK PREF. MEASURES___________________#

# CHOICE_INCOME_GCOE_A*CUT
# Categorise responses from CHOICE_INCOME_GCOE_*CUT vars.
# Risk categorisation adapted from RAND HRS Longitudinal File 2018 8 (V1) Documentation (p.1505) 
# https://hrsdata.isr.umich.edu/sites/default/files/documentation/other/1615843861/randhrs1992_2018v1.pdf

# 4. Least risk averse 
# 3. 2nd most risk averse 
# 2. 3rd most risk averse 
# 1. Most risk averse

main <- main %>%  
  mutate(INCOME_CUT_GCOE_A_RISK = case_when(
  # accepts job with 50% chance of salary 50% cut (50% chance of doubling)
  CHOICE_INCOME_GCOE_A30CUT == 1 & is.na(CHOICE_INCOME_GCOE_A10CUT) & CHOICE_INCOME_GCOE_A50CUT == 1 ~ 4, 
  
  # accepts job with 50% chance of salary 30% cut (50% chance of doubling)
  CHOICE_INCOME_GCOE_A30CUT == 1 & is.na(CHOICE_INCOME_GCOE_A10CUT) & CHOICE_INCOME_GCOE_A50CUT == 2 ~ 3,
  
  # accepts job with 50% chance of salary 10% cut (50% chance of doubling)
  CHOICE_INCOME_GCOE_A30CUT == 2 & CHOICE_INCOME_GCOE_A10CUT == 1 & is.na(CHOICE_INCOME_GCOE_A50CUT) ~ 2,
  
  # accepts only safe job that guarantees %5 increase
  CHOICE_INCOME_GCOE_A30CUT == 2 & CHOICE_INCOME_GCOE_A10CUT == 2 & is.na(CHOICE_INCOME_GCOE_A50CUT) ~ 1)) 
 
# CHOICE_INCOME_GCOE_A*INC & CHOICE_INCOME_GCOE_B*INC
# Categorise responses from CHOICE_INCOME_GCOE_*INC vars.
# Risk categorisation adapted from RAND HRS Longitudinal File 2018 8 (V1) Documentation (p.1505) 
# https://hrsdata.isr.umich.edu/sites/default/files/documentation/other/1615843861/randhrs1992_2018v1.pdf

# 4. Least risk averse 
# 3. 2nd most risk averse 
# 2. 3rd most risk averse 
# 1. Most risk averse

main <- main %>%  
  mutate(INCOME_INC_GCOE_A_RISK = case_when(
    # accepts job with 50% chance of salary w 20% increase (50% chance of a 10% cut)
    CHOICE_INCOME_GCOE_A30INC == 1 & CHOICE_INCOME_GCOE_A20INC == 1 & is.na(CHOICE_INCOME_GCOE_A50INC)  ~ 4, 
    
    # accepts job with 50% chance of salary w 30% increase (50% chance of a 10% cut)
    CHOICE_INCOME_GCOE_A30INC == 1 & CHOICE_INCOME_GCOE_A20INC == 2 & is.na(CHOICE_INCOME_GCOE_A50INC) ~ 3,
    
    # accepts job with 50% chance of salary w 50% increase (50% chance of a 10% cut)
    CHOICE_INCOME_GCOE_A30INC == 2 & is.na(CHOICE_INCOME_GCOE_A20INC) & CHOICE_INCOME_GCOE_A50INC == 1 ~ 2,
    
    # accepts only safe job that guarantees salary w %5 increase
    CHOICE_INCOME_GCOE_A30INC == 2 & is.na(CHOICE_INCOME_GCOE_A20INC) & CHOICE_INCOME_GCOE_A50INC == 2 ~ 1)) 

main <- main %>%  
  mutate(INCOME_INC_GCOE_B_RISK = case_when(
    # accepts job with 50% chance of salary w 30% increase (50% chance of a 10% cut)
    CHOICE_INCOME_GCOE_B60INC == 1 & CHOICE_INCOME_GCOE_B30INC == 1 & is.na(CHOICE_INCOME_GCOE_B200INC)  ~ 4, 
    
    # accepts job with 50% chance of salary w 60% increase (50% chance of a 10% cut)
    CHOICE_INCOME_GCOE_B60INC == 1 & CHOICE_INCOME_GCOE_B30INC == 2 & is.na(CHOICE_INCOME_GCOE_B200INC) ~ 3,
    
    # accepts job with 50% chance of salary w 200% increase (50% chance of a 10% cut)
    CHOICE_INCOME_GCOE_B60INC == 2 & is.na(CHOICE_INCOME_GCOE_B30INC) & CHOICE_INCOME_GCOE_B200INC == 1 ~ 2,
    
    # accepts only safe job that guarantees salary w %5 increase
    CHOICE_INCOME_GCOE_B60INC == 2 & is.na(CHOICE_INCOME_GCOE_B30INC) & CHOICE_INCOME_GCOE_B200INC == 2 ~ 1)) 


# CHOICE_LOTTERY_JP_A* vars.
# Compute proportion of RISKY choices (i.e., choosing Option A (1) instead of Option B (2))
change_val_lott <- function(x){2-x}
main <- main %>% 
  mutate_at(vars(CHOICE_LOTTERY_JP_A1:CHOICE_LOTTERY_JP_A8),change_val_lott ) %>%
  rowwise() %>% 
  mutate(LOTTERY_JP_A_RISK = mean(c_across(CHOICE_LOTTERY_JP_A1:CHOICE_LOTTERY_JP_A8), na.rm = FALSE)) %>% 
  ungroup()


# CHOICE_INS_JP_A* vars.
# Compute proportion of RISKY choices (i.e., choosing Option B (2) instead of Option A (1))
change_val_ins <- function(x){x-1}
main <- main %>% mutate_at(vars(CHOICE_INS_JP_A1:CHOICE_INS_JP_A9), change_val_ins) %>% 
  rowwise() %>% 
  mutate(INS_JP_A_RISK = mean(c_across(CHOICE_INS_JP_A1:CHOICE_INS_JP_A9), na.rm = FALSE)) %>% 
  ungroup()


# CHOICE_INCOME_CUT_A* vars.
# Compute proportion of RISKY choices (i.e., choosing Option A (1) instead of Option B (2))
change_val_income <- function(x){2-x}
main <- main  %>%
  mutate_at(vars(CHOICE_INCOME_CUT_A1:CHOICE_INCOME_CUT_A7), change_val_income) %>% 
  rowwise() %>% 
  mutate(INCOME_LIST_A_RISK = mean(c_across(CHOICE_INCOME_CUT_A1:CHOICE_INCOME_CUT_A7), na.rm = FALSE)) %>% 
  ungroup()


# CHOICE_INCOME_CUT_B* vars.
# Compute proportion of RISKY choices (i.e., choosing Option A (1) instead of Option B (2))
main <- main  %>%
  mutate_at(vars(CHOICE_INCOME_CUT_B1:CHOICE_INCOME_CUT_B7), change_val_income) %>% 
  rowwise() %>% 
  mutate(INCOME_LIST_B_RISK = mean(c_across(CHOICE_INCOME_CUT_B1:CHOICE_INCOME_CUT_B7), na.rm = FALSE)) %>% 
  ungroup()


# CHOICE_INCOME_CUT_C* vars.
# Compute proportion of RISKY choices (i.e., choosing Option A (1) instead of Option B (2))
main <- main  %>%
  mutate_at(vars(CHOICE_INCOME_CUT_C1:CHOICE_INCOME_CUT_C7), change_val_income) %>% 
  rowwise() %>% 
  mutate(INCOME_LIST_C_RISK = mean(c_across(CHOICE_INCOME_CUT_C1:CHOICE_INCOME_CUT_C7), na.rm = FALSE)) %>% 
  ungroup()


# WTP_2K_LOTTERY_*JP vars
# Renumber responses from WTP_2K_LOTTERY_HIGH_JP and WTP_2K_LOTTERY_LOW_JP to define a single harmonized
#  WTP value regardless of response at CHOICE_BUY_2K_LOTTERY_JP
main <- main %>% 
  # checking response routing
  mutate(WTP_2K_LOTTERY_HIGH_JP = case_when(CHOICE_BUY_2K_LOTTERY_JP != 1 ~ NA_real_, 
                                            TRUE ~ WTP_2K_LOTTERY_HIGH_JP),
         WTP_2K_LOTTERY_LOW_JP = case_when(CHOICE_BUY_2K_LOTTERY_JP != 2 ~ NA_real_, 
                                            TRUE ~ WTP_2K_LOTTERY_LOW_JP)) %>% 
  # reverse WTP_2K_LOTTERY_LOW_JP scores + create harmonized WTP measure
   mutate(WTP_2K_LOTTERY_LOW_JP = 7 - WTP_2K_LOTTERY_LOW_JP, # (scores 1 - 6)
         WTP_2K_LOTTERY_HIGH_JP =  WTP_2K_LOTTERY_HIGH_JP + 6, # (scores 7 - 12)
         WTP_2K_LOTTERY_JP = coalesce(WTP_2K_LOTTERY_LOW_JP, WTP_2K_LOTTERY_HIGH_JP)) # (scores 1(lowest purchasing price) - 12 (highest purchasing price))

# WTP_100K_LOTTERY_*JP vars
# Renumber responses from WTP_100K_LOTTERY_HIGH_JP and WTP_100K_LOTTERY_LOW_JP to define a single harmonized
#  WTP value regardless of response at CHOICE_BUY_100K_LOTTERY_JP
main <- main %>% 
  # checking response routing
  mutate(WTP_100K_LOTTERY_HIGH_JP = case_when(CHOICE_BUY_100K_LOTTERY_JP != 1 ~ NA_real_, 
                                            TRUE ~ WTP_100K_LOTTERY_HIGH_JP),
         WTP_100K_LOTTERY_LOW_JP = case_when(CHOICE_BUY_100K_LOTTERY_JP != 2 ~ NA_real_, 
                                           TRUE ~ WTP_100K_LOTTERY_LOW_JP)) %>% 
  # reverse WTP_100K_LOTTERY_LOW_JP scores + create harmonized WTP measure
  mutate(WTP_100K_LOTTERY_LOW_JP = 7 - WTP_100K_LOTTERY_LOW_JP, # (scores 1 - 6)
         WTP_100K_LOTTERY_HIGH_JP =  WTP_100K_LOTTERY_HIGH_JP + 6, # (scores 7 - 12)
         WTP_100K_LOTTERY_JP = coalesce(WTP_100K_LOTTERY_LOW_JP, WTP_100K_LOTTERY_HIGH_JP)) # (scores 1(lowest purchasing price) - 12 (highest purchasing price))

# WTA_2K_LOTTERY_*JP vars
# Renumber responses from WTA_2K_LOTTERY_HIGH_JP and WTA_2K_LOTTERY_LOW_JP to define a single harmonized
#  WTA value regardless of response at CHOICE_SELL_2K_LOTTERY_JP
main <- main %>% 
  # checking response routing
  mutate(WTA_2K_LOTTERY_HIGH_JP = case_when(CHOICE_SELL_2K_LOTTERY_JP != 2 ~ NA_real_, 
                                              TRUE ~ WTA_2K_LOTTERY_HIGH_JP),
         WTA_2K_LOTTERY_LOW_JP = case_when(CHOICE_SELL_2K_LOTTERY_JP != 1 ~ NA_real_, 
                                             TRUE ~ WTA_2K_LOTTERY_LOW_JP)) %>% 
  # reverse WTA_2K_LOTTERY_LOW_JP scores + create harmonized WTS measure
  mutate(WTA_2K_LOTTERY_LOW_JP = 7 - WTA_2K_LOTTERY_LOW_JP, # (scores 1 - 6)
         WTA_2K_LOTTERY_HIGH_JP =  WTA_2K_LOTTERY_HIGH_JP + 6, # (scores 7 - 12)
         WTA_2K_LOTTERY_JP = coalesce(WTA_2K_LOTTERY_HIGH_JP, WTA_2K_LOTTERY_LOW_JP)) # (scores 1(lowest selling price) - 12 (highest selling price))


# WTP_INS_2K_*JP vars
# Renumber responses from WTP_INS_2K_HIGH_JP and WTP_INS_2K_LOW_JP to define a single harmonized
#  WTP value regardless of response at CHOICE_BUY_INS_2K_JP
main <- main %>% 
  # checking response routing
  mutate(WTP_INS_2K_LOW_JP = case_when(CHOICE_BUY_INS_2K_JP != 2 ~ NA_real_, 
                                         TRUE ~ WTP_INS_2K_LOW_JP),
         WTP_INS_2K_HIGH_JP = case_when(CHOICE_BUY_INS_2K_JP != 1 ~ NA_real_, 
                                          TRUE ~ WTP_INS_2K_HIGH_JP)) %>% 
  # reverse WTP_INS_2K_HIGH_JP scores + create harmonized WTP measure
  mutate(WTP_INS_2K_HIGH_JP = 7 - WTP_INS_2K_HIGH_JP, # (scores 1 - 6)
         WTP_INS_2K_LOW_JP =  WTP_INS_2K_LOW_JP + 6, # (scores 7 - 12)
         WTP_INS_2K_JP = coalesce(WTP_INS_2K_HIGH_JP, WTP_INS_2K_LOW_JP)) # (scores 1(pay most for insurance) - 12 (pay least for insurance))


# WTP_INS_100K_*JP vars
# Renumber responses from WTP_INS_100K_LOW_JP and WTP_INS_100K_HIGH_JP to define a single harmonized
#  WTP value regardless of response at CHOICE_BUY_INS_100K_JP
main <- main %>% 
  # checking response routing
  mutate(WTP_INS_100K_LOW_JP = case_when(CHOICE_BUY_INS_100K_JP != 2 ~ NA_real_, 
                                            TRUE ~ WTP_INS_100K_LOW_JP),
         WTP_INS_100K_HIGH_JP = case_when(CHOICE_BUY_INS_100K_JP != 1 ~ NA_real_, 
                                           TRUE ~ WTP_INS_100K_HIGH_JP)) %>% 
  # reverse WTP_INS_100K_HIGH_JP scores + create harmonized WTP measure
  mutate(WTP_INS_100K_HIGH_JP = 7 - WTP_INS_100K_HIGH_JP, # (scores 1 - 6)
         WTP_INS_100K_LOW_JP =  WTP_INS_100K_LOW_JP + 6, # (scores 7 - 12)
         WTP_INS_100K_JP = coalesce(WTP_INS_100K_HIGH_JP, WTP_INS_100K_LOW_JP)) # (scores 1(pay most for insurance) - 12 (pay least for insurance WTS))


# LOSS_MIX_LOTTERY_*JP vars
# Renumber responses from LOSS_MIX_LOTTERY_HIGH_JP and LOSS_MIX_LOTTERY_LOW_JP to define a single harmonized
# value regardless of response at CHOICE_LOSS_MIX_LOTTERY_JP
main <- main %>% 
  # checking response routing
  mutate(LOSS_MIX_LOTTERY_HIGH_JP = case_when(CHOICE_LOSS_MIX_LOTTERY_JP != 1 ~ NA_real_, 
                                         TRUE ~ LOSS_MIX_LOTTERY_HIGH_JP),
         LOSS_MIX_LOTTERY_LOW_JP = case_when(CHOICE_LOSS_MIX_LOTTERY_JP != 2 ~ NA_real_, 
                                          TRUE ~ LOSS_MIX_LOTTERY_LOW_JP)) %>%
  
  # reverse LOSS_MIX_LOTTERY_LOW_JP scores + create harmonized WTP measure
  mutate(LOSS_MIX_LOTTERY_LOW_JP = 7 - LOSS_MIX_LOTTERY_LOW_JP, # (scores 1 - 6)
         LOSS_MIX_LOTTERY_HIGH_JP =  LOSS_MIX_LOTTERY_HIGH_JP + 6, # (scores 7 - 12)
         LOSS_MIX_LOTTERY_JP = coalesce(LOSS_MIX_LOTTERY_HIGH_JP, LOSS_MIX_LOTTERY_LOW_JP)) # (scores 1(tolerates no loss) - 12 (tolerates highest loss))

# CHECK VARS --------------------------------------------------------------

#________________ CHECK VARS: FILTER OUT RESPONDENTS WITH MISSING DEMS___________________#

main <- main %>% 
  filter(!is.na(age)) %>% #keep rows that have age info
  filter(!is.na(gender)) %>%  #keep rows that have gender info
  filter(non_unique_gender == 0) %>% # filter out respondents with not the same gender reported across the waves 
  filter(non_unique_yob == 0)  # filter out respondents with not the same yob reported across the waves 


#________________ CHECK VARS: DATA VISUAL (I)___________________#

# distribution of responses:
# summarytools::view(by(main, main$wave_id, dfSummary))

#________________ CHECK VARS: MEASURE QUAL. CRITERIA ___________________#
# exclude measures if:
# - in none of waves there is at least 4 different values possible for a measure (i.e., limited range of responses)

dependency_vars <- c(unique(na.omit(risk_info$varcode[risk_info$check_var == 1])),
                     colnames(main)[grepl("CHECK", colnames(main))], # vars to check for dependencies
                     colnames(main)[grepl("HIGH", colnames(main))], # WTP/WTS options
                     colnames(main)[grepl("LOW", colnames(main))], # WTP/WTS options
                     colnames(main)[grepl("CHOICE", colnames(main))]) # vars of A/B choices in lottery

vars_to_keep <- main  %>% 
  pivot_longer(!c(id, wave_id, wave_year, date, gender, age, non_unique_gender, non_unique_yob), values_to = "response", names_to = "var_name") %>% 
  filter(!is.na(response)) %>% 
 filter(!var_name %in% dependency_vars) %>%  # ignore vars to check for dependencies
  group_by(var_name, wave_id) %>%
  # how many unique responses possible per measure 
  mutate(opt_per_var = length(unique(na.omit(response)))) %>%  
  ungroup() %>% 
  distinct(wave_id, var_name, opt_per_var) %>% 
  group_by(var_name) %>% 
  # In at least ONE wave is there at least 4 different values possible per measure?
  mutate(var_include = if_else(sum(opt_per_var >= 4) >= 1, 1, 0)) %>% 
  ungroup()

# list of vars to keep
vars_to_keep <- vars_to_keep  %>% 
  filter(var_include == 1) %>% distinct(var_name)

# create a descriptive overview of vars to analyse
risk_info_analyse <- risk_info %>% bind_rows(newvars_info) %>% distinct(panel, varcode, measure_category, general_domain, domain_name,
                                            scale_type,scale_length, time_frame, behav_type, behav_paid, item_num) %>% 
  mutate( var_consider = if_else(varcode %in% dependency_vars, 0, 1), # considering non-dependency vars (or binary vars) for calculating retest correlations
         var_include = if_else(varcode %in% vars_to_keep$var_name, 1,0)) # include or exclude from calculating retest correlations
# dim(risk_info_analyse) 80 rows x 13 cols


# CREATE CLEAN/PROCESSED DATAFRAME  --------------------------------------------------------------

#________________SELECT VARS OF INTEREST FOR RETEST CALC___________________#

proc_data <- main %>% 
  # keep necessary vars (i.e., vars_to_keep and exclude vars used to check for dependencies)
  select(id, age, gender, wave_id,wave_year,date, # dems
         all_of(vars_to_keep$var_name)) %>% #risk measures
  filter_at(all_of(vars_to_keep$var_name),any_vars(!is.na(.))) #keep rows that have at least one risk value
# dim(proc_data)  43002 rows x  21 cols

# long format
proc_data_l <- pivot_longer(proc_data,all_of(vars_to_keep$var_name),names_to = "varcode", values_to = "response")
# dim(proc_data_l)  645030 rows x  8 cols

# checking if all id-gender combinations are unique
# proc_data_l %>%  distinct(id, gender) %>% summarise(check = n() == length(unique(proc_data$id)))
# checking if all id-varcode-wave combinations are unique
# t <- proc_data_l %>%  group_by(id, wave_id, varcode) %>% summarise(check = n(), .groups = "drop"); sum(t$check > 1) == 0


#________________ CHECK VARS: DATA VISUAL (II)___________________#

# distribution of responses:
# summarytools::view(by(proc_data, proc_data$wave_id, dfSummary))



# ADD PANEL INFORMATION ---------------------------------------------------


proc_data_l <- proc_data_l %>% 
  bind_cols(panel_var$PanelInfo) %>% 
  relocate(panel, .before = id)



# SAVING OUTPUT --------------------------------------------------------------

# saving clean/processed data "long"
write_csv(proc_data_l,paste0(preproc_data_path, panel_name, "_proc_data.csv")) 

# saving description of vars to analyse
write_csv(risk_info_analyse,paste0(measure_info_path, panel_name, "_risk_var_info.csv")) 

# ids with inconsistent dems count
write_csv(inconsistent_dems,paste0(preproc_data_path, panel_name, "_inconsistent_dems.csv")) 

# distribution of responses:
summarytools::view(by(proc_data, proc_data$wave_id, dfSummary), file = paste0(preproc_data_path, panel_name, "_resp_overview.html"))

print(paste0(panel_name, " pre-processing done!"))
