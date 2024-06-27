
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
library(haven) # open dta file


var_book <- read_rds("var_info/panel_variable_info.rds")# panel risk measures
measure_info_path <- c("var_info/indv_panel_var_info/") 
panel_data_path <- c("~/Documents/TSRP/Data/GCOE/RawData/") # where the raw panel data is stored
preproc_data_path <- c("~/Documents/TSRP/Data/GCOE/ProcData/") # where to save processed panel data
panel_name <- "GCOE_USA"

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
  file_of_interest_name <- paste0(file_of_interest_name, ".dta") # dta file
  file_of_interest <- list.files(path = panel_data_path, pattern = file_of_interest_name, full.names = TRUE, recursive = TRUE) 
  
  # open the relevant file
  wave_data <- read_dta(file_of_interest) # opens the relevant file

  # select vars of interest
  wave_data <- wave_data %>% select(all_of(vars_of_interest)) %>%  mutate(wave_id = as.character(CurrWave))
  
  # replacing var codes
  colnames_found <- match(colnames(wave_data), vars_of_interest, nomatch = 0)
  colnames(wave_data)[colnames(wave_data) %in% vars_of_interest] <- sub_all_info$varcode[colnames_found]
  
  # place in list
  data_list[[wave_num]] <- wave_data
  wave_num <- wave_num + 1
  
}  # wave year loop



#________________BIND DATA FROM DIFFERENT WAVES TOGETHER___________________# 
options(scipen=999)
main <- bind_rows(data_list)
main <- main %>%  select(sort(names(.))) # sort columns name order
main <- main %>% mutate_if(is.integer, ~ as.numeric(.x))
# dim(main) 48117 rows  x  73 cols

# FIX VARS  --------------------------------------------------------------------

#________________FIX VARS: REMOVE DUPLICATE & ACCIDENTAL IDS ___________________#

#avoid answers from unintended household members (id_omit != 1, some instances in which id_omit == 2, these observations are not excluded)
main <- main %>% filter(id_omit != 1 | is.na(id_omit)) %>% select(-id_omit)


#avoid IDs that within a wave are not unique
main <- main %>% 
  group_by(id, wave_id) %>% 
  mutate(id_count = n()) %>%
  ungroup() 

main %>% group_by(wave_id) %>% summarise(sum(id_count > 1)) # some repetitions in 2013
main <- main %>%  filter(id_count == 1) %>% select(-id_count)


#________________FIX VARS: DATES ___________________#
# If month & date are missing replace with the mean of available dates.
# If date information is completely missing from the raw data AND cannot be estimated from panel documentation, use 15th of June 

#  No interview date data available. We use information from the SurveyOverview.pdf 
#  # in general data collection took place between Jan and March each year (except for 2010 where it took part Dec 2009 and and Jan 2010) 
main <- main %>% mutate(date = case_when(as.numeric(wave_id) != 2010 ~ as.Date(paste(as.numeric(wave_id),"01","01",sep="-"), "%Y-%m-%d"),
                                         TRUE ~ as.Date(paste(as.numeric(wave_id),"02","01",sep="-"), "%Y-%m-%d"))) %>% 
  group_by(wave_id) %>% 
  mutate(wave_year = round(mean(year(date)))) %>%  # calculate the general year for that wave
  ungroup()

#________________FIX YOB & AGE & AGE RANGE___________________#

# for some years, variables are empty but can be filled/reconstructed from previous or future waves...
main <- main %>% 
  group_by(id) %>% 
  fill(yob,.direction = 'downup') %>%  # NAs get replaced with neighboring yob for each respondent 
  ungroup() 

# from readme file: "Among the data we collected for Preference Parameters Study in the US by Osaka University, it
# has been confirmed that there are considerable amounts of survey responses that could have
# been collected from the different individuals in the same households across the multiple survey
# years...the proportion of the responses that could be collected from different individuals in the
#  same households across two consecutive years are indicated in the table... skyrocketed in 2012 and 2013."

# identify respondents with not the same yob reported across the waves
main <- main %>%
  group_by(id) %>%
  mutate(non_unique_yob = length(unique(na.omit(yob))) > 1) %>% # 
  ungroup() 

# use reported YOB to calculate the participant's age instead of the age defined by the panel
main <- main %>%
  mutate(age = year(date) - yob) %>% 
 select(-c(yob, age_dv)) # there are some cases with quite significant age vs. age_dv differences

# only keep respondents aged between 18 & 90y.o.
main <- main %>% filter(age %in% c(18:90)) 


#________________FIX VARS: GENDER___________________#


# recode gender (f = 1, m = 0)
main <- main %>%
  mutate(gender = case_when(gender == 2 ~ 1,
                            gender == 1 ~ 0))


# for some years, variables are empty but can be filled/reconstructed from previous or future waves...
main <- main %>% 
  group_by(id) %>% 
  fill(gender,.direction = 'downup') %>%  # NAs get replaced with neighboring gender for each respondent 
  ungroup() 


#  identify respondents with not the same gender reported across the waves
main <- main %>%
  group_by(id) %>%
  mutate(non_unique_gender = length(unique(na.omit(gender))) >1) %>%
  ungroup() %>% 
  select(-gender_dv)
# there are some cases with gender vs. gender_dv differences

# *from readme file: "Among the data we collected for Preference Parameters Study in the US by Osaka University, it
# has been confirmed that there are considerable amounts of survey responses that could have
# been collected from the different individuals in the same households across the multiple survey
# years...the proportion of the responses that could be collected from different individuals in the
#  same households across two consecutive years are indicated in the table... skyrocketed in 2012 and 2013."


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


# CHOICE_LOTTERY_USA_A* vars.
# Compute proportion of RISKY choices (i.e., choosing Option A (1) instead of Option B (2))
change_val_lott <- function(x){2-x}
main <- main %>% 
  mutate_at(vars(CHOICE_LOTTERY_USA_A1:CHOICE_LOTTERY_USA_A8),change_val_lott ) %>%
  rowwise() %>% 
  mutate(LOTTERY_USA_A_RISK = mean(c_across(CHOICE_LOTTERY_USA_A1:CHOICE_LOTTERY_USA_A8), na.rm = FALSE)) %>% 
  ungroup()


# CHOICE_INS_USA_A* vars.
# Compute proportion of RISKY choices (i.e., choosing Option B (2) instead of Option A (1))
change_val_ins <- function(x){x-1}
main <- main %>% mutate_at(vars(CHOICE_INS_USA_A1:CHOICE_INS_USA_A9), change_val_ins) %>% 
  rowwise() %>% 
  mutate(INS_USA_A_RISK = mean(c_across(CHOICE_INS_USA_A1:CHOICE_INS_USA_A9), na.rm = FALSE)) %>% 
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


# WTP_LOTTERY_20_*USA vars
# Renumber responses from WTP_LOTTERY_20_HIGH_USA and WTP_LOTTERY_20_LOW_USA to define a single harmonized
#  WTP value regardless of response at CHOICE_BUY_LOTTERY_20_USA
main <- main %>% 
  # checking response routing
  mutate(WTP_LOTTERY_20_HIGH_USA = case_when(CHOICE_BUY_LOTTERY_20_USA != 1 ~ NA_real_, 
                                            TRUE ~ WTP_LOTTERY_20_HIGH_USA),
         WTP_LOTTERY_20_LOW_USA = case_when(CHOICE_BUY_LOTTERY_20_USA != 2 ~ NA_real_, 
                                           TRUE ~ WTP_LOTTERY_20_LOW_USA)) %>% 
  # reverse WTP_LOTTERY_20_LOW_USA scores + create harmonized WTP measure
  mutate(WTP_LOTTERY_20_LOW_USA = 7 - WTP_LOTTERY_20_LOW_USA, # (scores 1 - 6)
         WTP_LOTTERY_20_HIGH_USA =  WTP_LOTTERY_20_HIGH_USA + 6, # (scores 7 - 12)
         WTP_LOTTERY_20_USA = coalesce(WTP_LOTTERY_20_LOW_USA, WTP_LOTTERY_20_HIGH_USA)) # (scores 1(lowest purchasing price) - 12 (highest purchasing price))

# WTP_LOTTERY_1K_*USA vars
# Renumber responses from WTP_LOTTERY_1K_HIGH_USA and WTP_LOTTERY_1K_LOW_USA to define a single harmonized
#  WTP value regardless of response at CHOICE_BUY_LOTTERY_1K_USA
main <- main %>% 
  # checking response routing
  mutate(WTP_LOTTERY_1K_HIGH_USA = case_when(CHOICE_BUY_LOTTERY_1K_USA != 1 ~ NA_real_, 
                                              TRUE ~ WTP_LOTTERY_1K_HIGH_USA),
         WTP_LOTTERY_1K_LOW_USA = case_when(CHOICE_BUY_LOTTERY_1K_USA != 2 ~ NA_real_, 
                                             TRUE ~ WTP_LOTTERY_1K_LOW_USA)) %>% 
  # reverse WTP_LOTTERY_1K_LOW_USA scores + create harmonized WTP measure
  mutate(WTP_LOTTERY_1K_LOW_USA = 7 - WTP_LOTTERY_1K_LOW_USA, # (scores 1 - 6)
         WTP_LOTTERY_1K_HIGH_USA =  WTP_LOTTERY_1K_HIGH_USA + 6, # (scores 7 - 12)
         WTP_LOTTERY_1K_USA = coalesce(WTP_LOTTERY_1K_LOW_USA, WTP_LOTTERY_1K_HIGH_USA)) # (scores 1(lowest purchasing price) - 12 (highest purchasing price))

# WTA_LOTTERY_20_*USA vars
# Renumber responses from WTA_LOTTERY_20_HIGH_USA and WTA_LOTTERY_20_LOW_USA to define a single harmonized
#  WTA value regardless of response at CHOICE_SELL_LOTTERY_20_USA
main <- main %>% 
  # checking response routing
  mutate(WTA_LOTTERY_20_HIGH_USA = case_when(CHOICE_SELL_LOTTERY_20_USA != 2 ~ NA_real_, 
                                            TRUE ~ WTA_LOTTERY_20_HIGH_USA),
         WTA_LOTTERY_20_LOW_USA = case_when(CHOICE_SELL_LOTTERY_20_USA != 1 ~ NA_real_, 
                                           TRUE ~ WTA_LOTTERY_20_LOW_USA)) %>% 
  # reverse WTA_LOTTERY_20_LOW_USA scores + create harmonized WTS measure
  mutate(WTA_LOTTERY_20_LOW_USA = 7 - WTA_LOTTERY_20_LOW_USA, # (scores 1 - 6)
         WTA_LOTTERY_20_HIGH_USA =  WTA_LOTTERY_20_HIGH_USA + 6, # (scores 7 - 12)
         WTA_LOTTERY_20_USA = coalesce(WTA_LOTTERY_20_HIGH_USA, WTA_LOTTERY_20_LOW_USA)) # (scores 1(lowest selling price) - 12 (highest selling price))


# WTP_INS_20_*USA vars
# Renumber responses from WTP_INS_20_HIGH_USA and WTP_INS_20_LOW_USA to define a single harmonized
#  WTP value regardless of response at CHOICE_BUY_INS_20_USA
main <- main %>% 
  # checking response routing
  mutate(WTP_INS_20_LOW_USA = case_when(CHOICE_BUY_INS_20_USA != 2 ~ NA_real_, 
                                       TRUE ~ WTP_INS_20_LOW_USA),
         WTP_INS_20_HIGH_USA = case_when(CHOICE_BUY_INS_20_USA != 1 ~ NA_real_, 
                                        TRUE ~ WTP_INS_20_HIGH_USA)) %>% 
  # reverse WTP_INS_20_HIGH_USA scores + create harmonized WTP measure
  mutate(WTP_INS_20_HIGH_USA = 7 - WTP_INS_20_HIGH_USA, # (scores 1 - 6)
         WTP_INS_20_LOW_USA =  WTP_INS_20_LOW_USA + 6, # (scores 7 - 12)
         WTP_INS_20_USA = coalesce(WTP_INS_20_HIGH_USA, WTP_INS_20_LOW_USA)) # (scores 1(pay most for insurance) - 12 (pay least for insurance))


# WTP_INS_1K_*USA vars
# Renumber responses from WTP_INS_1K_LOW_USA and WTP_INS_1K_HIGH_USA to define a single harmonized
#  WTP value regardless of response at CHOICE_BUY_INS_1K_USA
main <- main %>% 
  # checking response routing
  mutate(WTP_INS_1K_LOW_USA = case_when(CHOICE_BUY_INS_1K_USA != 2 ~ NA_real_, 
                                         TRUE ~ WTP_INS_1K_LOW_USA),
         WTP_INS_1K_HIGH_USA = case_when(CHOICE_BUY_INS_1K_USA != 1 ~ NA_real_, 
                                          TRUE ~ WTP_INS_1K_HIGH_USA)) %>% 
  # reverse WTP_INS_1K_HIGH_USA scores + create harmonized WTP measure
  mutate(WTP_INS_1K_HIGH_USA = 7 - WTP_INS_1K_HIGH_USA, # (scores 1 - 6)
         WTP_INS_1K_LOW_USA =  WTP_INS_1K_LOW_USA + 6, # (scores 7 - 12)
         WTP_INS_1K_USA = coalesce(WTP_INS_1K_HIGH_USA, WTP_INS_1K_LOW_USA)) # (scores 1 (willing to pay the most for insurance) - 12 (willing to pay the least for insurance WTS))


# LOSS_MIX_LOTTERY_*USA vars
# Renumber responses from LOSS_MIX_LOTTERY_HIGH_USA and LOSS_MIX_LOTTERY_LOW_USA to define a single harmonized
# value regardless of response at CHOICE_MIX_LOTTERY_USA
main <- main %>% 
  # checking response routing
  mutate(LOSS_MIX_LOTTERY_HIGH_USA = case_when(CHOICE_MIX_LOTTERY_USA != 1 ~ NA_real_, 
                                              TRUE ~ LOSS_MIX_LOTTERY_HIGH_USA),
         LOSS_MIX_LOTTERY_LOW_USA = case_when(CHOICE_MIX_LOTTERY_USA != 2 ~ NA_real_, 
                                             TRUE ~ LOSS_MIX_LOTTERY_LOW_USA)) %>%
  
  # reverse LOSS_MIX_LOTTERY_LOW_USA scores + create harmonized WTP measure
  mutate(LOSS_MIX_LOTTERY_LOW_USA = 7 - LOSS_MIX_LOTTERY_LOW_USA, # (scores 1 - 6)
         LOSS_MIX_LOTTERY_HIGH_USA =  LOSS_MIX_LOTTERY_HIGH_USA + 6, # (scores 7 - 12)
         LOSS_MIX_LOTTERY_USA = coalesce(LOSS_MIX_LOTTERY_HIGH_USA, LOSS_MIX_LOTTERY_LOW_USA)) # (scores 1(tolerates no loss) - 12 (tolerates highest loss))

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
                     colnames(main)[grepl("HIGH", colnames(main))], # WTP/WTA options
                     colnames(main)[grepl("LOW", colnames(main))], # WTP/WTA options
                     colnames(main)[grepl("CHOICE", colnames(main))]) # vars of A/B choices in lottery

vars_to_keep <- main  %>% 
  pivot_longer(!c(id, wave_id, wave_year, date, gender, age, non_unique_gender), values_to = "response", names_to = "var_name") %>% 
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
# dim(risk_info_analyse)  80 rows x  13 cols


# CREATE CLEAN/PROCESSED DATAFRAME  --------------------------------------------------------------

#________________SELECT VARS OF INTEREST FOR RETEST CALC___________________#

proc_data <- main %>% 
  # keep necessary vars (i.e., vars_to_keep and exclude vars used to check for dependencies)
  select(id, age, gender, wave_id,wave_year,date, # dems
         all_of(vars_to_keep$var_name)) %>% #risk measures
  filter_at(all_of(vars_to_keep$var_name),any_vars(!is.na(.))) #keep rows that have at least one risk value
# dim(proc_data)  35977 rows x  21 cols

# long format
proc_data_l <- pivot_longer(proc_data,all_of(vars_to_keep$var_name),names_to = "varcode", values_to = "response")
# dim(proc_data_l)  539655 rows x  8 cols

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
