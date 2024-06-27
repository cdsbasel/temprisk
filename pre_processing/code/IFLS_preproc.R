
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


# PATH INFORMATION -----------------------------------------------------
var_book <- read_rds("var_info/panel_variable_info.rds")# panel risk measures
measure_info_path <- c("var_info/indv_panel_var_info/") 
panel_data_path <- c("~/Documents/TSRP/Data/IFLS/RawData/") # where the raw panel data is stored
preproc_data_path <- c("~/Documents/TSRP/Data/IFLS/ProcData/") # where to save processed panel data
panel_name <- "IFLS"

# VARIABLE INFORMATION ----------------------------------------------------

panel_var <- var_book[[panel_name]]

# combining id & risk information
all_info <- bind_rows(panel_var$Measures, panel_var$ID, panel_var$Dates, panel_var$Dems)
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
    file_path_dta <- list.files(path = paste0(panel_data_path, CurrWave, "/"), pattern = paste0(CurrFile, ".dta"), recursive = TRUE, full.names = TRUE,  ignore.case = TRUE)
    data <- read_dta(file_path_dta) 
    
    # select vars of interest
    vars_of_interest <-file_info$origin_varcode
    data <- data %>% select(all_of(vars_of_interest))
    
    # replacing var codes
    colnames_found <- match(colnames(data), vars_of_interest, nomatch = 0)
    colnames(data)[colnames(data) %in% vars_of_interest] <- file_info$varcode[colnames_found]
    
    
    if (CurrFile == "b3b_time") {
      # select 1st interview date
      data <- data %>% 
        filter(int_num == 1) %>% 
        group_by(id) %>% slice(1)
      
    }
    
    
    # place in list
    data_list[[file_num]] <- data 
    file_num <- file_num + 1
    
  } # file loop
  
  # join variables from different files together
  wave_data <- reduce(data_list, full_join, by = "id") 
  
  survey_data[[wave_num]] <- wave_data %>%  mutate(wave_id = CurrWave)
  wave_num <- wave_num + 1
  
} # wave loop


#________________BIND DATA FROM DIFFERENT WAVES TOGETHER___________________# 
options(scipen=999)
main <- bind_rows(survey_data)
main <- main %>% mutate_if(is.integer, ~ as.numeric(.x))
# dim(main) 127127 rows x 27 cols


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

main <- main %>% mutate(YINT = case_when(wave_id == "W3" ~ 2000, 
                                         wave_id == "W1" & MINT %in% c(1,2) ~ 1994,
                                         wave_id == "W1" & !MINT %in% c(1,2) ~ 1993,
                                         wave_id == "W4" ~ YINT + 2000,
                                         TRUE ~ YINT), 
                        DINT = case_when(DINT > 31 ~ NA_real_,
                                         TRUE ~ DINT),
                        MINT = case_when(MINT > 12 ~ NA_real_,
                                         TRUE ~ MINT),
                        date = as.Date(paste(as.character(YINT),as.character(MINT),as.character(DINT),sep="-"))) %>% 
  group_by(wave_id) %>% 
  # replace missing dates with the average of the available dates
  mutate(date = case_when(is.na(date) ~ mean.Date(date, na.rm = TRUE),
                          TRUE ~ as.Date(date))) %>% 
  ungroup() %>% 
  select(-c(YINT,MINT, DINT, int_num))


main <- main %>% group_by(wave_id) %>% mutate(wave_year = round(mean(year(date)))) %>% ungroup() # calculate the general year for that wave

#________________FIX YOB & AGE & AGE RANGE___________________#

# some missing age/yob data
main <- main %>%  mutate(yob = if_else(yob %in% c(0, 9998, 9999), NA_real_, yob))
main <- main %>%  mutate(age = if_else(age %in% c(99, 98,999, 998), NA_real_, age))
sum(is.na(main$age))
sum(is.na(main$yob))

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
main <- main %>% mutate(age = case_when(is.na(age) ~ year(date) - yob,
                                        TRUE ~ age),
                        age_yob = year(date) - yob)


#  a few ids with ages that do not match age calculated from yob
sum(abs(main$age_yob - main$age) > 2, na.rm = TRUE)
id_rem2 <- unique(na.omit(main$id[abs(main$age_yob - main$age) > 2]))

#still some missing age information, but most are in W5
sum(is.na(main$age))
sum(is.na(main$yob))


# using the age variable assess age difference across waves
main_temp <- main %>% group_by(id) %>% 
  summarise(age_diff = age[which.max(wave_year)] - age[which.min(wave_year)],
            max_wave = wave_year[which.max(wave_year)],
            min_wave = wave_year[which.min(wave_year)]) %>% 
  rowwise() %>% 
  mutate(year_diff = max_wave - min_wave) %>%  ungroup()

#  a few ids with ages not matching across waves
sum(abs(main_temp$year_diff - main_temp$age_diff) > 2, na.rm = TRUE)
id_rem <- na.omit(main_temp$id[abs(main_temp$year_diff - main_temp$age_diff) > 2])



# only keep respondents aged between 15 & 90y.o. (+ matching ages)
main <- main %>% 
  filter(!id %in% c(id_rem, id_rem2)) %>% # remove id with problematic age data
  filter(age %in% c(15:90)) %>% select(-c(yob, age_yob))


#________________FIX VARS: GENDER___________________#
# recode gender
main <- main %>%
  mutate(gender = case_when(gender == 1 ~ 0,
                            gender == 3 ~ 1))

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
  mutate(SMKEVER_CHECK = case_when(!SMKEVER_CHECK %in% c(1,3) ~ NA_real_,
                                   TRUE ~ SMKEVER_CHECK),
         SMKREC_CHECK = case_when(SMKEVER_CHECK == 3 ~ 3,
                                  !SMKREC_CHECK %in% c(1,5) ~ NA_real_,
                                  TRUE ~ SMKREC_CHECK), 
         SMK_TYPEA_CHECK = case_when(SMKEVER_CHECK == 3 ~ 3,
                                     SMKREC_CHECK == 3 ~3, 
                                     !SMK_TYPEA_CHECK %in% c(1,5) ~ NA_real_,
                                     TRUE ~ SMK_TYPEA_CHECK), 
         SMK_TYPEB_CHECK = case_when(SMKEVER_CHECK == 3 ~ 3,
                                     SMKREC_CHECK == 3 ~3, 
                                     !SMK_TYPEB_CHECK %in% c(1,5) ~ NA_real_,
                                     TRUE ~ SMK_TYPEB_CHECK), 
         SMK_TYPEC_CHECK = case_when(SMKEVER_CHECK == 3 ~ 3,
                                     SMKREC_CHECK == 3 ~3, 
                                     !SMK_TYPEC_CHECK %in% c(1,5) ~ NA_real_,
                                     TRUE ~ SMK_TYPEC_CHECK), 
         SMK_TYPED_CHECK = case_when(SMKEVER_CHECK == 3 ~ 3,
                                     SMKREC_CHECK == 3 ~3, 
                                     !SMK_TYPED_CHECK %in% c(1,5) ~ NA_real_,
                                     TRUE ~ SMK_TYPED_CHECK), 
         CIGS_DAY_OEC = case_when(SMKEVER_CHECK == 3  ~ 0, 
                                  SMKREC_CHECK == 3  ~ 0, 
                                  (SMK_TYPEC_CHECK == 3 & SMK_TYPED_CHECK == 3) ~ 0, # does not smoke cigaretes, 
                                  CIGS_DAY_OEC > 0 & CIGS_DAY_OEC < 1 ~ NA_real_, # some decimal numbers
                                  CIGS_DAY_OEC > 990  ~ NA_real_, 
                                  TRUE ~ CIGS_DAY_OEC),
         TOBACCO_WK_OEA  = case_when(SMKEVER_CHECK == 3  ~ 0, 
                                     SMKREC_CHECK == 3  ~ 0, 
                                     (SMK_TYPEC_CHECK == 3 & SMK_TYPEA_CHECK == 3 &  SMK_TYPEB_CHECK == 3)  ~ 0, #  does not chew tobacco/smokes pipes
                                     TOBACCO_WK_OEA > 90  ~ NA_real_, 
                                     TRUE ~ TOBACCO_WK_OEA))

## BEHAVIOUR: CHOICE_INCOME_* ## 
# Categorise responses from CHOICE_INCOME_* vars.


# 5. least risk averse  
# 4. 2nd lest risk averse 
# 3. 3rd most risk averse 
# 2. 2nd most risk averse 
# 1. Most risk averse


main <- main %>%  mutate(INCOME_RISK_A_IFLS = case_when(
  (CHOICE_INCOME_A_800K == 1 | CHOICE_INCOME_800K_CONF == 1 )  ~ 1, # only safe option
  (CHOICE_INCOME_A_800K == 2 | CHOICE_INCOME_800K_CONF == 2 ) & CHOICE_INCOME_A_400K == 1 & CHOICE_INCOME_A_600K == 1 ~ 2, # accepts the 1.6 million or Rp 800 thousand per month option
  (CHOICE_INCOME_A_800K == 2 | CHOICE_INCOME_800K_CONF == 2 ) & CHOICE_INCOME_A_400K == 1 & CHOICE_INCOME_A_600K == 2 ~ 3, # accepts the 1.6 million or Rp 600 thousand per month option
  (CHOICE_INCOME_A_800K == 2 | CHOICE_INCOME_800K_CONF == 2 ) & CHOICE_INCOME_A_400K == 2 & CHOICE_INCOME_A_200K == 1 ~ 4, # accepts the 1.6 million or Rp 400 thousand per month option
  (CHOICE_INCOME_A_800K == 2 | CHOICE_INCOME_800K_CONF == 2 ) & CHOICE_INCOME_A_400K == 2 & CHOICE_INCOME_A_200K == 2 ~ 5)) # accepts the 1.6 million or Rp 200 thousand per month option


main <- main %>%  mutate(INCOME_RISK_B_IFLS = case_when(
  (CHOICE_INCOME_B_2M == 1 | CHOICE_INCOME_B_2M_CONF == 1)  ~ 1, # only safe option
  (CHOICE_INCOME_B_2M == 2 | CHOICE_INCOME_B_2M_CONF == 2) & CHOICE_INCOME_B_12M == 1 & CHOICE_INCOME_B_8M == 1 ~ 2, # accepts the Rp 4 million or Rp 2 million per month option
  (CHOICE_INCOME_B_2M == 2 | CHOICE_INCOME_B_2M_CONF == 2) & CHOICE_INCOME_B_12M == 1 & CHOICE_INCOME_B_8M == 2 ~ 3, # accepts theRp 8 million or Rp 2 million per month option
  (CHOICE_INCOME_B_2M == 2 | CHOICE_INCOME_B_2M_CONF == 2) & CHOICE_INCOME_B_12M == 2 & CHOICE_INCOME_B_16M == 1 ~ 4, # accepts the Rp 12 million or Rp 0 per month option
  (CHOICE_INCOME_B_2M == 2 | CHOICE_INCOME_B_2M_CONF == 2) & CHOICE_INCOME_B_12M == 2 & CHOICE_INCOME_B_16M == 2 ~ 5)) # accepts the Rp 16 million or -Rp 2 million per month option




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
risk_info_analyse <- risk_info  %>% bind_rows(newvars_info) %>% distinct(panel, varcode, measure_category, general_domain, domain_name,
                                                                         scale_type,scale_length, time_frame, behav_type, behav_paid, item_num) %>% 
  mutate( var_consider = if_else(varcode %in% dependency_vars, 0, 1), # considering non-dependency vars (or binary vars) for calculating retest correlations
          var_include = if_else(varcode %in% vars_to_keep$var_name, 1,0)) # include or exclude from calculating retest correlations
# dim(risk_info_analyse) 20 rows x 13 cols


# CREATE CLEAN/PROCESSED DATAFRAME  --------------------------------------------------------------

#________________SELECT VARS OF INTEREST FOR RETEST CALC___________________#

proc_data <- main %>% 
  # keep necessary vars (i.e., vars_to_keep and exclude vars used to check for dependencies)
  select(id, age, gender, wave_id,wave_year,date, # dems
         all_of(vars_to_keep$var_name)) %>% #risk measures
  filter_at(all_of(vars_to_keep$var_name),any_vars(!is.na(.))) #keep rows that have at least one risk value
# dim(proc_data)  97046 rows x  10 cols

# long format
proc_data_l <- pivot_longer(proc_data,all_of(vars_to_keep$var_name),names_to = "varcode", values_to = "response")
# dim(proc_data_l)  388184 rows x  8 cols

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
