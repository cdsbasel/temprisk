
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
library(foreign) # open spss file
library(haven) # open dta file


# PATH INFORMATION -----------------------------------------------------

var_book <- read_rds("var_info/panel_variable_info.rds")# panel risk measures
measure_info_path <- c("var_info/indv_panel_var_info/") 
panel_data_path <- c("") # where the raw panel data is stored
preproc_data_path <- c("") # where to save processed panel data
panel_name <- "SOEP"

# VARIABLE INFORMATION ----------------------------------------------------

panel_var <- var_book[[panel_name]]
# combining id & risk information
all_info <- bind_rows(panel_var$Measures, panel_var$ID, panel_var$Dems, panel_var$Dates)
risk_info <- panel_var$Measures

# READING DATA -----------------------------------------------------

#________________READ MAIN DATA FILES___________________# 

# Read files listed in the all_var_info df --> 
files_of_interest <- unique(na.omit(all_info$varfile)) # in which files are the relevant vars stored

main_list <- NULL
file_num <- 1

for (CurrFile in files_of_interest) {
  
  if(CurrFile != "cov") {
    file_data <- data.table::fread(paste0(panel_data_path,CurrFile, ".csv"), header = TRUE) # open file
  }
  
  
  if(CurrFile == "cov") {
    file_data <- data.table::fread(paste0(panel_data_path, "raw/",CurrFile, ".csv"), header = TRUE) # open file
  }
  
  file_var_info <- all_info %>% filter(varfile == CurrFile) # select var info relevant to the file
  vars_of_interest <- file_var_info$origin_varcode # vars of interest
  
  # select vars of interest from the main file
  file_data <- file_data %>% select(all_of(vars_of_interest)) 
  
  # replacing var names
  colnames_found <- match(colnames(file_data), vars_of_interest, nomatch = 0)
  colnames(file_data)[colnames(file_data) %in% vars_of_interest] <- file_var_info$varcode[colnames_found]
  
  if(CurrFile != "cov") {
    
    main_list[[file_num]] <- file_data %>%  mutate(wave_id = as.character(SYEAR)) %>% select(-SYEAR)
    file_num <- file_num +1 }
  
  if(CurrFile == "cov") {
    main2 <- file_data
    
    
  }
  
}

#________________BIND DATA FROM DIFFERENT FILES TOGETHER___________________# 
main <- reduce(main_list, full_join, by = c("id", "wave_id")) 
# dim(main) 750485 rows x  23 cols
main <- main %>% mutate_if(is.integer, ~ as.numeric(.x))

# pre-process separately before binding rows
main2 <- main2 %>% mutate_if(is.integer, ~ as.numeric(.x))
# dim(main2) 14171 rows x  7 cols
# FIX VARS  --------------------------------------------------------------------

#________________FIX VARS: REMOVE DUPLICATE IDS ___________________#

#avoid IDs that within a wave are not unique
main <- main %>% 
  group_by(id, wave_id) %>% 
  mutate(id_count = n()) %>%
  ungroup() 

main %>% group_by(wave_id) %>% summarise(sum(id_count > 1)) 
main <- main %>%  filter(id_count == 1) %>% select(-id_count)



main2 <- main2 %>% 
  mutate(wave_id = paste0(SYEAR, "_cov")) %>% 
  group_by(id, wave_id) %>% 
  mutate(id_count = n()) %>%
  ungroup() 

main2 %>% group_by(wave_id) %>% summarise(sum(id_count > 1)) 
main2 <- main2 %>%  filter(id_count == 1) %>% select(-id_count)


#________________REPLACING MISSING & NON READABLE RESPONSES___________________#
main <- main %>% 
  replace(.< 0 , NA_real_ ) # any vars with values below 0 (i.e., invalid, DK, proxy, missing,inapplicable....) are replaced  

main2 <- main2 %>% 
  replace(.< 0 , NA_real_ ) # any vars with values below 0 (i.e., invalid, DK, proxy, missing,inapplicable....) are replaced  

#________________FIX VARS: DATES ___________________#
# If some dates are missing replace these with the mean of available dates.
# If date information is completely missing from the raw data AND cannot be estimated from panel documentation, use 15th of June + survey year

# adding by default 15th as the day of the month
main <- main %>% mutate(
  date = as.Date(paste(wave_id,MINT,"15",sep="-"), "%Y-%m-%d"))


main <- main %>% group_by(wave_id) %>% mutate(date = case_when(is.na(date) ~ mean(date, na.rm = T),
                                                               TRUE ~ date)) %>% ungroup() %>% select(-MINT)

main <- main %>% mutate(wave_year =as.numeric(wave_id)) 


main2 <- main2 %>%
  mutate(date = as.Date(paste(SYEAR,MINT,DINT,sep="-"), "%Y-%m-%d"),
         wave_year = SYEAR)%>% 
  group_by(wave_id) %>% 
  mutate(date = case_when(is.na(date) ~ mean(date, na.rm = T),
                          TRUE ~ date)) %>% ungroup() %>% select(-c(SYEAR,MINT,DINT))



#________________FIX YOB & AGE & AGE RANGE___________________#
# for some years, variables are empty but can be filled/reconstructed from previous or future waves...
main <- main %>% 
  group_by(id) %>% 
  fill(yob,.direction = 'downup') %>%  # NAs get replaced with neighboring yob for each respondent 
  ungroup() 


# calculate age
main <- main %>% mutate(age = year(date) - yob)

# for some years, variables are empty but can be filled/reconstructed from previous or future waves...
main2 <- main2 %>% 
  group_by(id) %>% 
  fill(yob,.direction = 'downup') %>%  # NAs get replaced with neighboring yob for each respondent 
  ungroup() 

# # identify respondents with not the same yob reported across the waves
# main2 <- main2 %>%
#   group_by(id) %>%
#   mutate(non_unique_yob = length(unique(na.omit(yob))) > 1) %>% # 
#   ungroup() 

# calculate age
main2 <- main2 %>% mutate(age = year(date) - yob) 


# main2 <- main2 %>%  filter(age %in% c(10:90)) 
#________________FIX GENDER___________________# 
#(for some years, variables are empty but can be filled/reconstructed from previous or future waves...)

# for some years, variables are empty but can be filled/reconstructed from previous or future waves...
main <- main %>% 
  mutate(gender = coalesce(gender1,gender2)) %>% # harmonize gender var 
  select(-c(gender1,gender2)) %>% 
  group_by(id) %>% 
  fill(gender,.direction = 'downup') %>%  # NAs get replaced with neighboring gender for each respondent 
  ungroup()


# recode gender 
main <- main %>%
  mutate(gender = case_when(gender == 2 ~ 1,
                            gender == 1 ~ 0)) # male = 0, female = 1



# for some years, variables are empty but can be filled/reconstructed from previous or future waves...
main2 <- main2 %>% 
  group_by(id) %>% 
  fill(gender,.direction = 'downup') %>%  # NAs get replaced with neighboring gender for each respondent 
  ungroup()

# recode gender
main2 <- main2 %>%
  mutate(gender = case_when(gender == 2 ~ 1,
                            gender == 1 ~ 0)) # male = 0, female = 1
# male = 0, female = 1

#  identify respondents with not the same gender reported across the waves
# main2 <- main2 %>%
#   group_by(id) %>%
#   mutate(non_unique_gender = length(unique(na.omit(gender))) >1) %>%  
#   ungroup()

# join sets
main <- bind_rows(main, main2)

# for this panel only keep respondents aged between 10 & 90y.o. 
main <- main %>%  filter(age %in% c(10:90))

#  identify respondents with not the same gender reported across the waves
main <- main %>%
  group_by(id) %>%
  mutate(non_unique_gender = length(unique(na.omit(gender))) >1) %>%  
  ungroup()


# identify respondents with not the same yob reported across the waves
main <- main %>%
  group_by(id) %>%
  mutate(non_unique_yob = length(unique(na.omit(yob))) > 1) %>% # 
  ungroup() %>% select(-yob)

inconsistent_dems <- main %>% 
  distinct(id, non_unique_gender, non_unique_yob) %>%
  summarise(perc_gender = 100*(sum(non_unique_gender)/n()),
            perc_yob = 100*(sum(non_unique_yob)/n())) %>% 
  mutate(panel = panel_name)


#________________FIX VARS: RISK PREF. MEASURES___________________#

# SMOKING
# taking into account missing answers because of previous answers (i.e., skipping questions)
main <- main %>%  
  # if people report not having smoked ever they get 0 cigarettes/cigars/pipes instead of NA
  mutate(CIGS_DAY_OEC = case_when(is.na(CIGS_DAY_OEC) & wave_year %in% c(2004, 2006, 2008, 2010, 2014,2016, 2018, 2020) & c(SMK_CHECK5 == 2 |SMK_CHECK == 2) ~ 0,  
                                  is.na(CIGS_DAY_OEC) & wave_year %in% c(2002,2012) & c(SMK_CHECK == 2 | SMK_CHECK3 == 2 | SMK_CHECK4 == 1) ~ 0,
                                  wave_year == 1998 & SMK_CHECK2 == 1 ~ TOBACCO_DAY_OEX, # can split 1998 ple0086_v1 data into cigarettes, cigars and pipes smoked  based on ple0080_v1 and link in to ple0086_v2-4
                                  wave_year == 1998 & SMK_CHECK2 == 4 ~ 0, # Not smoking at all 
                                  wave_year == 1998 & SMK_CHECK2 %in% c(2,3) ~ 0, #smoking cigars/pipes
                                  TRUE ~ CIGS_DAY_OEC),
         PIPES_DAY_OEB = case_when(is.na(PIPES_DAY_OEB) & wave_year %in% c(2004, 2006, 2008, 2010, 2014,2016, 2018) & c(CIGS_DAY_OEC != 0) ~ 0,  # smokes cigarettes and not pipes
                                   is.na(PIPES_DAY_OEB) & wave_year %in% c(2004, 2006, 2008, 2010, 2014,2016, 2018) & c(SMK_CHECK5 == 2 |SMK_CHECK == 2) ~ 0,  
                                   is.na(PIPES_DAY_OEB) & wave_year %in% c(2002,2012) & c(SMK_CHECK == 2 | SMK_CHECK3 == 2 | SMK_CHECK4 == 1) ~ 0,
                                   wave_year == 1998 & SMK_CHECK2 == 2 ~ TOBACCO_DAY_OEX, # can split 1998 ple0086_v1 data into cigarettes, cigars and pipes smoked  based on ple0080_v1 and link in to ple0086_v2-4
                                   wave_year == 1998 & SMK_CHECK2 == 4 ~ 0, # Not smoking at all 
                                   wave_year == 1998 & SMK_CHECK2 %in% c(1,3) ~ 0, #smoking cigars/cigarettes
                                   TRUE ~ PIPES_DAY_OEB),
         CIGARS_DAY_OEB = case_when(is.na(CIGARS_DAY_OEB) & wave_year %in% c(2004, 2006, 2008, 2010, 2014,2016, 2018) & c(CIGS_DAY_OEC != 0) ~ 0,  # smokes cigarettes and not cigars
                                    is.na(CIGARS_DAY_OEB) & wave_year %in% c(2004, 2006, 2008, 2010, 2014,2016, 2018) & c(SMK_CHECK5 == 2 |SMK_CHECK == 2) ~ 0,   
                                    is.na(CIGARS_DAY_OEB) & wave_year %in% c(2002,2012) & c(SMK_CHECK == 2 | SMK_CHECK3 == 2 | SMK_CHECK4 == 1) ~ 0,
                                    wave_year == 1998 & SMK_CHECK2 == 3 ~ TOBACCO_DAY_OEX, # can split 1998 ple0086_v1 data into cigarettes, cigars and pipes smoked  based on ple0080_v1 and link in to ple0086_v2-4
                                    wave_year == 1998 & SMK_CHECK2 == 4 ~ 0, # Not smoking at all 
                                    wave_year == 1998 & SMK_CHECK2 %in% c(1,2) ~ 0, #smoking cigars/cigarettes
                                    TRUE ~ CIGARS_DAY_OEB)) %>% 
  select(-TOBACCO_DAY_OEX)

# BANK ALLOCATION (behavioral task)

# reverse scoring (higher score == more risk taking)
main <- main %>%  
  mutate(BANK_ALLOC = 7 - BANK_ALLOC)

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

dependency_vars <- unique(na.omit(risk_info$varcode[risk_info$check_var == 1|risk_info$varcode == "TOBACCO_DAY_OEX"])) # vars to check for dependencies

vars_to_keep <- main  %>% 
  pivot_longer(!c(id, wave_id, wave_year, date, gender, age, non_unique_gender, non_unique_yob), values_to = "response", names_to = "var_name") %>% 
  filter(!var_name %in% dependency_vars) %>% 
  filter(!is.na(response)) %>% 
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
risk_info_analyse <- risk_info %>% distinct(panel, varcode, measure_category, general_domain, domain_name,
                                            scale_type,scale_length, time_frame, behav_type, behav_paid) %>% 
  mutate(var_consider = if_else(varcode %in% dependency_vars, 0, 1), # considering non-dependency vars (or binary vars) for calculating retest correlations
         var_include = if_else(varcode %in% vars_to_keep$var_name, 1,0)) # include or exclude from calculating retest correlations
# dim(risk_info_analyse)  17 rows x  12 cols


# CREATE CLEAN/PROCESSED DATAFRAME  --------------------------------------------------------------

#________________SELECT VARS OF INTEREST FOR RETEST CALC___________________#

proc_data <- main %>% 
  # keep necessary vars (i.e., vars_to_keep and exclude vars used to check for dependencies)
  select(id, age, gender, wave_id,wave_year,date, # dems
         all_of(vars_to_keep$var_name)) %>% #risk measures
  filter_at(all_of(vars_to_keep$var_name),any_vars(!is.na(.))) #keep rows that have at least one risk value
# dim(proc_data)  426703 rows x  17 cols

# long format
proc_data_l <- pivot_longer(proc_data,all_of(vars_to_keep$var_name),names_to = "varcode", values_to = "response")
# dim(proc_data_l)  4694733 rows x  8 cols

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
