
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
library(SAScii) # open SAS file


# PATH INFORMATION -----------------------------------------------------

var_book <- read_rds("var_info/panel_variable_info.rds")# panel risk measures
measure_info_path <- c("var_info/indv_panel_var_info/") 
panel_data_path <- c("") # where the raw panel data is stored
preproc_data_path <- c("") # where to save processed panel data
panel_name <- "HRS"

# VARIABLE INFORMATION ----------------------------------------------------

panel_var <- var_book[[panel_name]]
# combining id & risk information
all_info <- bind_rows(panel_var$Measures, panel_var$ID, panel_var$Dems, panel_var$Dates)
risk_info <- panel_var$Measures
newvars_info <- panel_var$NewVars

# READING DATA -----------------------------------------------------

#________________READ MAIN DATA FILES___________________# 

# list of unique waves
wave_list <- unique(na.omit(all_info$wave_id))

survey_data <- NULL
wave_num <- 1

for (CurrWave in wave_list) {
  
  data_list <- NULL
  
  sub_all_info <- all_info %>% filter(wave_id == CurrWave)
  
  file_list <- sort(unique(sub_all_info$varfile))
  
  file_num <- 1
  
  for (CurrFile in file_list) {
    # select file relevant info
    file_info <- sub_all_info %>% filter(varfile == CurrFile)
    
    # open file 
    file_path_sas <- list.files(path = panel_data_path, pattern = paste0(CurrFile, ".sas"), recursive = TRUE, full.names = TRUE,  ignore.case = TRUE)
    file_path_da <- list.files(path = panel_data_path, pattern = paste0(CurrFile, ".da"), recursive = TRUE, full.names = TRUE,  ignore.case = TRUE)
    data <- read.SAScii(fn = file_path_da, sas_ri = file_path_sas) 
    
    # select vars of interest
    vars_of_interest <- file_info$origin_varcode 
    data <- data %>% select(all_of(vars_of_interest)) 
    
    # replacing var codes
    colnames_found <- match(colnames(data), vars_of_interest, nomatch = 0)
    colnames(data)[colnames(data) %in% vars_of_interest] <- file_info$varcode[colnames_found]
    
    
    # place in list
    data_list[[file_num]] <- data  %>% mutate(id = paste0(HHID,"_",PN)) %>% select(-c(HHID, PN))
    file_num <- file_num + 1
    
  } # file loop
  
  # join variables from different files together
  wave_data <- reduce(data_list, full_join, by = "id") 
  
  survey_data[[wave_num]] <- wave_data %>%  mutate(wave_id = CurrWave)
  wave_num <- wave_num + 1
  
} # wave loop

#________________BIND DATA FROM DIFFERENT WAVES TOGETHER___________________# 

main <- bind_rows(survey_data)
main <- main %>% mutate_if(is.integer, ~ as.numeric(.x))
# dim(main) 265274 rows x 50 cols

# FIX VARS  --------------------------------------------------------------------

#________________FIX VARS: REMOVE DUPLICATE IDS ___________________#

#avoid IDs that within a wave are not unique
main <- main %>% 
  group_by(id, wave_id) %>% 
  mutate(id_count = n()) %>%
  ungroup() 

main %>% group_by(wave_id) %>% summarise(sum(id_count > 1)) 
main <- main %>%  filter(id_count == 1) %>% select(-id_count)

#________________FIX VARS: DATES ___________________#
# If month & date are missing replace with the mean of available dates.
# If date information is completely missing from the raw data AND cannot be estimated from panel documentation, use 15th of June 

main <- main %>%  mutate(YINT = if_else(YINT < 1992, NA_real_, YINT), 
                         date = as.Date(paste(as.character(YINT),as.character(MINT),"15",sep="-"), "%Y-%m-%d")) %>% select(-c(YINT,MINT))

main <- main %>%  group_by(wave_id) %>% mutate(date = if_else(is.na(date), mean(date, na.rm = TRUE), date),
                                               wave_year = round(mean(year(date)))) %>%  # calculate the general year for that wave
  ungroup() 


#________________FIX YOB & AGE & AGE RANGE___________________#

main <- main %>%  mutate(yob = if_else(yob %in% c(0, 9998, 9999), NA_real_, yob))

# for some years, variables are empty but can be filled/reconstructed from previous or future waves...
main <- main %>% 
  group_by(id) %>% 
  fill(yob,.direction = 'downup') %>%  # NAs get replaced with neighboring yob for each respondent 
  ungroup() 

# identify respondents with not the same yob reported across the waves
main <- main %>%
  group_by(id) %>%
  mutate(non_unique_yob = length(unique(na.omit(yob))) > 1) %>% # 
  ungroup() 


# calculate age
main <- main %>% mutate(age = year(date) - yob) %>% select(-yob)


# only keep respondents aged between 50 & 90y.o. (HRS focuses on adults aged 50+)
main <- main %>%  filter(age %in% c(50:90)) 

#________________FIX VARS: GENDER___________________#

main <- main %>%  mutate(gender = if_else(gender == 0, NA_real_, gender))

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
  ungroup()

inconsistent_dems <- main %>% 
  distinct(id, non_unique_gender, non_unique_yob) %>%
  summarise(perc_gender = 100*(sum(non_unique_gender)/n()),
            perc_yob = 100*(sum(non_unique_yob)/n())) %>% 
  mutate(panel = panel_name)

#________________FIX VARS: RISK PREF. MEASURES___________________#

## FREQ SMOKING ##
# Dependencies  + non-valid responses

main <- main %>%
  # SMKNOW_CHECK NAs are for people who SMKEVER_CHECK == 5 (No) (i.e., never smoked) so NAs in SMKNOW_CHECK are set to 5 (i.e., not currently smoking) and 
  # CIGS_DAY_OEE (i.e., cigarettes/day) is set to 0
  mutate(SMKEVER_CHECK = case_when(!SMKEVER_CHECK %in% c(1,5) ~ NA_real_,
                                   TRUE ~ SMKEVER_CHECK),
         SMKNOW_CHECK = case_when(SMKEVER_CHECK == 5 ~ 5,
                                  !SMKNOW_CHECK %in% c(1,5) ~ NA_real_,
                                  TRUE ~ SMKNOW_CHECK), 
         CIGS_DAY_OEE = case_when(SMKNOW_CHECK == 5 ~ 0, 
                                  CIGS_DAY_OEE < 0 ~ NA_real_,
                                  CIGS_DAY_OEE > 97  ~ NA_real_, 
                                  TRUE ~ CIGS_DAY_OEE),
         PACKS_DAY_OEE = case_when(SMKEVER_CHECK == 5 | SMKNOW_CHECK == 5 ~ 0,
                                   PACKS_DAY_OEE > 7 ~ NA_real_, 
                                   TRUE ~ PACKS_DAY_OEE))

# harmonize units: convert to cigs/day
main <- main %>%
  mutate(CIGS_DAY_OEE = case_when(
    wave_id != "1994" & !is.na(CIGS_DAY_OEE) ~ CIGS_DAY_OEE, # cig/day
    wave_id != "1994" & SMKNOW_CHECK == 1 & c(is.na(CIGS_DAY_OEE)|CIGS_DAY_OEE == 0) & !is.na(PACKS_DAY_OEE) ~ PACKS_DAY_OEE * 20, # pack/day (1 pack = 20 cigs)
    wave_id == "1994" & SMK_UNIT == 1 & SMK_TIME == 11 ~ CIGS_DAY_OEE, #cigs per day
    wave_id == "1994" & SMK_UNIT == 2 & SMK_TIME == 11 ~ 20*CIGS_DAY_OEE, # convert packs per day into cigs per day
    wave_id == "1994" & SMK_UNIT == 3 & SMK_TIME == 11 ~ 200*CIGS_DAY_OEE, # convert cartons per day into cigs per day
    wave_id == "1994" & SMK_UNIT == 1 & SMK_TIME == 2 ~ round(CIGS_DAY_OEE/7), # convert cigs per week into cigs per day
    wave_id == "1994" & SMK_UNIT == 2 & SMK_TIME == 2 ~ round((20*CIGS_DAY_OEE)/7), # convert packs per week into cigs per day
    wave_id == "1994" & SMK_UNIT == 3 & SMK_TIME == 2 ~ round((200*CIGS_DAY_OEE)/7), # convert cartons per week into cigs per day
    wave_id == "1994" & SMK_UNIT == 1 & SMK_TIME == 4 ~ round(CIGS_DAY_OEE/30), # convert cigs per month into cigs per day
    wave_id == "1994" & SMK_UNIT == 2 & SMK_TIME == 4 ~ round((20*CIGS_DAY_OEE)/30), # convert packs per month into cigs per day
    wave_id == "1994" & SMK_UNIT == 3 & SMK_TIME == 4 ~ round((200*CIGS_DAY_OEE)/30), # convert cartons per month into cigs per day
    wave_id == "1994" & SMK_UNIT == 1 & SMK_TIME == 6 ~ round(CIGS_DAY_OEE/365), # convert cigs per year into cigs per day
    wave_id == "1994" & SMK_UNIT == 2 & SMK_TIME == 6 ~ round((20*CIGS_DAY_OEE)/365), # convert packs per year into cigs per day
    wave_id == "1994" & SMK_UNIT == 3 & SMK_TIME == 6 ~ round((200*CIGS_DAY_OEE)/365))) # convert cartons per year into cigs per day

## FREQ DRINKING ## 
# Dependencies + non-valid responses
main <- main %>% 
  mutate(ALC_CHECK = case_when(!ALC_CHECK %in% c(3,5) ~ NA_real_,
                               TRUE ~ ALC_CHECK),
         # ALC_WK_DIS8A is skipped/response gets NA depending on response in ALC_CHECK. 
         #   If respodent reports not drinking alcohol (i.e., ALC_CHECK == 3 or 5) then they get 0 days 
         ALC_WK_DIS8A = case_when(ALC_CHECK %in% c(3,5) ~ 0,
                                  ALC_WK_DIS8A < 0 ~ NA_real_,
                                  ALC_WK_DIS8A > 7 ~ NA_real_,
                                  TRUE ~ ALC_WK_DIS8A),
         
         # ALC_DRINKS_DAY_OEA is skipped/response gets NA depending on response in ALC_CHECK and ALC_WK_DIS8A. 
         #   If respondent reports not drinking alcohol (i.e., ALC_CHECK == 3 or 5 OR ALC_WK_DIS8A == 0) then they get 0 drinks per day
         ALC_DRINKS_DAY_OEA = case_when(ALC_CHECK %in% c(3,5) |ALC_WK_DIS8A == 0  ~ 0,
                                        ALC_DRINKS_DAY_OEA < 0 ~ NA_real_,
                                        ALC_DRINKS_DAY_OEA > 92 ~ NA_real_,
                                        TRUE ~ ALC_DRINKS_DAY_OEA), 
         
         # ALC_4DRINKS_3MO_DIS93A is skipped/response gets NA depending on response in ALC_CHECK and ALC_WK_DIS8A. 
         #   If respondent reports not drinking alcohol (i.e., ALC_CHECK == 3 or 5 OR ALC_WK_DIS8A == 0) then they get 0 days drinking +4 drinks
         ALC_4DRINKS_3MO_DIS93A = case_when(ALC_CHECK %in% c(3,5) |ALC_WK_DIS8A == 0  ~ 0,
                                            ALC_4DRINKS_3MO_DIS93A > 92 ~ NA_real_,
                                            ALC_4DRINKS_3MO_DIS93A < 0 ~ NA_real_,
                                            TRUE ~ ALC_4DRINKS_3MO_DIS93A)) 

## BEHAVIOUR: CHOICE_INCOME_*CUT ## 
# Categorise responses from CHOICE_INCOME_*CUT vars.
# Risk categorisation based on RAND HRS Longitudinal File 2018 8 (V1) Documentation (p.1505) 
# https://hrsdata.isr.umich.edu/sites/default/files/documentation/other/1615843861/randhrs1992_2018v1.pdf

# 6. Least risk averse 
# 5. 2nd least risk averse  
# 4. 3rd least risk averse 
# 3. 3rd most risk averse 
# 2. 2nd most risk averse 
# 1. Most risk averse

main <- main %>%  mutate(INCOME_CUT_RISK = case_when(
  
  # accepts job with 50% chance of 75% cut in income
  CHOICE_INCOME_33CUT == 2 & is.na(CHOICE_INCOME_20CUT) & is.na(CHOICE_INCOME_10CUT) & CHOICE_INCOME_50CUT == 2 & CHOICE_INCOME_75CUT == 2 ~ 6,
  
  # accepts at most job with 50% chance of 50% cut in income
  CHOICE_INCOME_33CUT == 2 & is.na(CHOICE_INCOME_20CUT) & is.na(CHOICE_INCOME_10CUT) & CHOICE_INCOME_50CUT == 2 & CHOICE_INCOME_75CUT == 1 ~ 5,
  
  # accepts at most job with 50% chance of 33% cut income
  CHOICE_INCOME_33CUT == 2 & is.na(CHOICE_INCOME_20CUT) & is.na(CHOICE_INCOME_10CUT) & CHOICE_INCOME_50CUT == 1 & is.na(CHOICE_INCOME_75CUT) ~ 4, 
  
  # accepts at most job with 50% chance of 20% cut income
  CHOICE_INCOME_33CUT == 1 & CHOICE_INCOME_20CUT == 2 & is.na(CHOICE_INCOME_10CUT) & is.na(CHOICE_INCOME_50CUT)  & is.na(CHOICE_INCOME_75CUT) ~ 3,
  
  # accepts at most job with 50% chance of 10% cut income
  CHOICE_INCOME_33CUT == 1 & CHOICE_INCOME_20CUT == 1 & CHOICE_INCOME_10CUT == 2 & is.na(CHOICE_INCOME_50CUT)  & is.na(CHOICE_INCOME_75CUT) ~ 2,
  
  # only accepts 1st (i.e., safe) job
  CHOICE_INCOME_33CUT == 1 & CHOICE_INCOME_20CUT == 1 & CHOICE_INCOME_10CUT == 1 & is.na(CHOICE_INCOME_50CUT)  & is.na(CHOICE_INCOME_75CUT) ~ 1)) 


## BEHAVIOUR: CHOICE_INCOME_20INC*CUT ## 
# Categorise responses from CHOICE_INCOME_20INC*CUT vars.
# Risk categorisation based on CHOICE_INCOME_*CUT  vars. (see also RAND HRS Longitudinal File 2018 8 (V1) Documentation; p.1505) 
# https://hrsdata.isr.umich.edu/sites/default/files/documentation/other/1615843861/randhrs1992_2018v1.pdf

# 4. Least risk averse 
# 3. 3rd most risk averse 
# 2. 2nd most risk averse 
# 1. Most risk averse

# main: applies to waves 2000 & 2002
main <- main %>%  mutate(INCOME_CUT_RISKB = case_when(
  
  # accepts job with 50% chance of 15% cut in income (& 50% chance of 20% increase)
  CHOICE_INCOME_20INC10CUT == 2 & is.na(CHOICE_INCOME_20INC5CUT) & CHOICE_INCOME_20INC15CUT == 2 ~ 4,
  
  # accepts at most job with 50% chance of 10% cut in income (& 50% chance of 20% increase)
  CHOICE_INCOME_20INC10CUT == 2 & is.na(CHOICE_INCOME_20INC5CUT) & CHOICE_INCOME_20INC15CUT == 1 ~ 3,
  
  # accepts at most job with 50% chance of 5% cut in income (& 50% chance of 20% increase)
  CHOICE_INCOME_20INC10CUT == 1 & CHOICE_INCOME_20INC5CUT == 2 & is.na(CHOICE_INCOME_20INC15CUT) == 1 ~ 2,  
  
  # only accepts 1st (i.e., safe) job
  CHOICE_INCOME_20INC10CUT == 1 & CHOICE_INCOME_20INC5CUT == 1 & is.na(CHOICE_INCOME_20INC15CUT) == 1 ~ 1)) 

# In wave 2006, the order of CHOICE_INCOME_20INC*CUT questions was randomized (3 different orders), thus different question routing
# random assignment A
main <- main %>%  mutate(INCOME_CUT_RISKB_A = case_when(
  
  # accepts job with 50% chance of 15% cut in income (& 50% chance of 20% increase)
  CHOICE_INCOME_20INC5CUT_A == 2 & CHOICE_INCOME_20INC5CUT_A == 2 & CHOICE_INCOME_20INC15CUT_A == 2 ~ 4,
  
  # accepts at most job with 50% chance of 10% cut in income (& 50% chance of 20% increase)
  CHOICE_INCOME_20INC5CUT_A == 2 & CHOICE_INCOME_20INC5CUT_A == 2 & CHOICE_INCOME_20INC15CUT_A == 1 ~ 3,
  
  # accepts at most job with 50% chance of 5% cut in income (& 50% chance of 20% increase)
  CHOICE_INCOME_20INC5CUT_A == 2 & CHOICE_INCOME_20INC5CUT_A == 1 & is.na(CHOICE_INCOME_20INC15CUT_A) ~ 2,
  
  # only accepts 1st (i.e., safe) job
  CHOICE_INCOME_20INC5CUT_A == 1 & is.na(CHOICE_INCOME_20INC5CUT_A) & is.na(CHOICE_INCOME_20INC15CUT_A) ~ 1)) 

# random assignment B
main <- main %>%  mutate(INCOME_CUT_RISKB_B = case_when(
  
  # accepts job with 50% chance of 15% cut in income (& 50% chance of 20% increase)
  CHOICE_INCOME_20INC10CUT_B == 2 & is.na(CHOICE_INCOME_20INC5CUT_B) & CHOICE_INCOME_20INC15CUT_B == 2 ~ 4,
  
  # accepts at most job with 50% chance of 10% cut in income (& 50% chance of 20% increase)
  CHOICE_INCOME_20INC10CUT_B == 2 & is.na(CHOICE_INCOME_20INC5CUT_B) & CHOICE_INCOME_20INC15CUT_B == 1 ~ 3,
  
  # accepts at most job with 50% chance of 5% cut in income (& 50% chance of 20% increase)
  CHOICE_INCOME_20INC10CUT_B == 1 & CHOICE_INCOME_20INC5CUT_B == 2 & is.na(CHOICE_INCOME_20INC15CUT_B) == 1 ~ 2,  
  
  # only accepts 1st (i.e., safe) job
  CHOICE_INCOME_20INC10CUT_B == 1 & CHOICE_INCOME_20INC5CUT_B == 1 & is.na(CHOICE_INCOME_20INC15CUT_B) == 1 ~ 1)) 

# random assignment C
main <- main %>%  mutate(INCOME_CUT_RISKB_C = case_when(
  
  # accepts job with 50% chance of 15% cut in income (& 50% chance of 20% increase)
  CHOICE_INCOME_20INC15CUT_C == 2 & is.na(CHOICE_INCOME_20INC10CUT_C) & is.na(CHOICE_INCOME_20INC5CUT_C) ~ 4,
  
  # accepts at most job with 50% chance of 10% cut in income (& 50% chance of 20% increase)
  CHOICE_INCOME_20INC15CUT_C == 1 & CHOICE_INCOME_20INC10CUT_C == 2 & is.na(CHOICE_INCOME_20INC5CUT_C) ~ 3,
  
  # accepts at most job with 50% chance of 5% cut in income (& 50% chance of 20% increase)
  CHOICE_INCOME_20INC15CUT_C == 1 & CHOICE_INCOME_20INC10CUT_C == 1 & CHOICE_INCOME_20INC5CUT_C == 2 ~ 2,
  
  # only accepts 1st (i.e., safe) job
  CHOICE_INCOME_20INC15CUT_C == 1 & CHOICE_INCOME_20INC10CUT_C == 1 & CHOICE_INCOME_20INC5CUT_C == 1 ~ 1))

# merge all responses into one
main <- main %>%  mutate(INCOME_20INC_CUT_RISK = coalesce(INCOME_CUT_RISKB, INCOME_CUT_RISKB_A, INCOME_CUT_RISKB_B, INCOME_CUT_RISKB_C))



## BEHAVIOUR: CHOICE_INHERIT_*CUT ## 
# Categorise responses from CHOICE_INHERIT_*CUT vars.
# Risk categorisation based on CHOICE_INCOME_*CUT vars. (see also RAND HRS Longitudinal File 2018 8 (V1) Documentation; p.1505) 
# https://hrsdata.isr.umich.edu/sites/default/files/documentation/other/1615843861/randhrs1992_2018v1.pdf

# 6. Least risk averse 
# 5. 2nd least risk averse  
# 4. 3rd least risk averse 
# 3. 3rd most risk averse 
# 2. 2nd most risk averse 
# 1. Most risk averse


main <- main %>%  mutate(INHERIT_CUT_RISK = case_when(
  
  # accepts investment with 50% chance of 75% cut in inheritance
  CHOICE_INHERIT_33CUT == 1  & is.na(CHOICE_INHERIT_20CUT) & is.na(CHOICE_INHERIT_10CUT) & CHOICE_INHERIT_50CUT == 1 & CHOICE_INHERIT_75CUT == 1 ~ 6,
  
  # accepts investment with 50% chance of 50% cut in inheritance
  CHOICE_INHERIT_33CUT == 1  & is.na(CHOICE_INHERIT_20CUT) & is.na(CHOICE_INHERIT_10CUT) & CHOICE_INHERIT_50CUT == 1 & CHOICE_INHERIT_75CUT == 5 ~ 5,
  
  # accepts investment with 50% chance of 33% cut in inheritance
  CHOICE_INHERIT_33CUT == 1  & is.na(CHOICE_INHERIT_20CUT) & is.na(CHOICE_INHERIT_10CUT) & CHOICE_INHERIT_50CUT == 5 & is.na(CHOICE_INHERIT_75CUT) ~ 4 ,
  
  # accepts investment with 50% chance of 20% cut in inheritance
  CHOICE_INHERIT_33CUT == 5  & CHOICE_INHERIT_20CUT == 1 & is.na(CHOICE_INHERIT_10CUT) & is.na(CHOICE_INHERIT_50CUT) & is.na(CHOICE_INHERIT_75CUT) ~ 3,
  
  # accepts investment with 50% chance of 10% cut in inheritance
  CHOICE_INHERIT_33CUT == 5  & CHOICE_INHERIT_20CUT == 5 & CHOICE_INHERIT_10CUT == 1 & is.na(CHOICE_INHERIT_50CUT) & is.na(CHOICE_INHERIT_75CUT) ~ 2,
  
  #   # accepts none of the risky investments
  CHOICE_INHERIT_33CUT == 5  & CHOICE_INHERIT_20CUT == 5 & CHOICE_INHERIT_10CUT == 5 & is.na(CHOICE_INHERIT_50CUT) & is.na(CHOICE_INHERIT_75CUT) ~ 1))


## BEHAVIOUR: CHOICE_BUSINESS_*CUT ## 
# Categorise responses from CHOICE_BUSINESS_*CUT vars.
# Risk categorisation based on CHOICE_INCOME_*CUT vars. (see also RAND HRS Longitudinal File 2018 8 (V1) Documentation; p.1505) 
# https://hrsdata.isr.umich.edu/sites/default/files/documentation/other/1615843861/randhrs1992_2018v1.pdf

# 6. Least risk averse 
# 5. 2nd least risk averse  
# 4. 3rd least risk averse 
# 3. 3rd most risk averse 
# 2. 2nd most risk averse 
# 1. Most risk averse


main <- main %>%  mutate(BUSINESS_CUT_RISK = case_when(
  
  # accepts share in a private business with 50% chance of 75% cut
  CHOICE_BUSINESS_33CUT == 2  & is.na(CHOICE_BUSINESS_20CUT) & is.na(CHOICE_BUSINESS_10CUT) & CHOICE_BUSINESS_50CUT == 2 & CHOICE_BUSINESS_75CUT == 2 ~ 6,
  
  # accepts share in a private business with 50% chance of 50% cut
  CHOICE_BUSINESS_33CUT == 2  & is.na(CHOICE_BUSINESS_20CUT) & is.na(CHOICE_BUSINESS_10CUT) & CHOICE_BUSINESS_50CUT == 2 & CHOICE_BUSINESS_75CUT == 1 ~ 5,
  
  # accepts share in a private business with 50% chance of 33% cut
  CHOICE_BUSINESS_33CUT == 2  & is.na(CHOICE_BUSINESS_20CUT) & is.na(CHOICE_BUSINESS_10CUT) & CHOICE_BUSINESS_50CUT == 1 & is.na(CHOICE_BUSINESS_75CUT) ~ 4 ,
  
  # accepts share in a private business with 50% chance of 20% cut
  CHOICE_BUSINESS_33CUT == 1  & CHOICE_BUSINESS_20CUT == 2 & is.na(CHOICE_BUSINESS_10CUT) & is.na(CHOICE_BUSINESS_50CUT) & is.na(CHOICE_BUSINESS_75CUT) ~ 3,
  
  # accepts share in a private business with 50% chance of 10% cut
  CHOICE_BUSINESS_33CUT == 1  & CHOICE_BUSINESS_20CUT == 1 & CHOICE_BUSINESS_10CUT == 2 & is.na(CHOICE_BUSINESS_50CUT) & is.na(CHOICE_BUSINESS_75CUT) ~ 2,
  
  # does not accept share in a private business to potentially lose value
  CHOICE_BUSINESS_33CUT == 1  & CHOICE_BUSINESS_20CUT == 1 & CHOICE_BUSINESS_10CUT == 1 & is.na(CHOICE_BUSINESS_50CUT) & is.na(CHOICE_BUSINESS_75CUT) ~ 1))



## PROPENSTY ## 
main <- main %>%  mutate(RISK_GEN_ORD11A = if_else(RISK_GEN_ORD11A %in% c(0:10), RISK_GEN_ORD11A, NA_real_),
                         RISK_FIN_ORD11A = if_else(RISK_FIN_ORD11A %in% c(0:10),  RISK_FIN_ORD11A, NA_real_),
                         RISK_DRI_ORD11A = if_else(RISK_DRI_ORD11A  %in% c(0:10), RISK_DRI_ORD11A, NA_real_),
                         RISK_OCC_ORD11A = if_else(RISK_OCC_ORD11A  %in% c(0:10), RISK_OCC_ORD11A, NA_real_),
                         RISK_REC_ORD11A = if_else(RISK_REC_ORD11A  %in% c(0:10), RISK_REC_ORD11A, NA_real_),
                         RISK_HEA_ORD11A = if_else(RISK_HEA_ORD11A  %in% c(0:10), RISK_HEA_ORD11A, NA_real_),
                         # reverse coding
                         ADVENTU_ORD4A = 5-ADVENTU_ORD4A)

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

dependency_vars <- c(colnames(main)[grepl("*CHECK", colnames(main))], # vars to check for dependencies
                     colnames(main)[grepl("SMK_*", colnames(main))], # omit certain smoking variables
                     colnames(main)[grepl("PACKS_*", colnames(main))], # omit certain smoking variables
                     colnames(main)[grepl("RISKB_*", colnames(main))], # omit certain risk variables
                     colnames(main)[grepl("CHOICE", colnames(main))]) # vars of yes/no choices in lottery

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
risk_info_analyse <- risk_info %>% bind_rows(newvars_info) %>%  distinct(panel, varcode, measure_category, general_domain, domain_name,
                                                                         scale_type,scale_length, time_frame, behav_type, behav_paid) %>% 
  mutate(var_consider = if_else(varcode %in% dependency_vars, 0, 1), # considering non-dependency vars (or binary vars) for calculating retest correlations
         var_include = if_else(varcode %in% vars_to_keep$var_name, 1,0)) # include or exclude from calculating retest correlations
# dim(risk_info_analyse)  48 rows x  12 cols


# CREATE CLEAN/PROCESSED DATAFRAME  --------------------------------------------------------------

#________________SELECT VARS OF INTEREST FOR RETEST CALC___________________#

proc_data <- main %>% 
  # keep necessary vars (i.e., vars_to_keep and exclude vars used to check for dependencies)
  select(id, age, gender, wave_id,wave_year,date, # dems
         all_of(vars_to_keep$var_name)) %>% #risk measures
  filter_at(all_of(vars_to_keep$var_name),any_vars(!is.na(.))) #keep rows that have at least one risk value
# dim(proc_data)  235723 rows x  21 cols

# long format
proc_data_l <- pivot_longer(proc_data,all_of(vars_to_keep$var_name),names_to = "varcode", values_to = "response")
# dim(proc_data_l)  3535845 rows x  8 cols

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

