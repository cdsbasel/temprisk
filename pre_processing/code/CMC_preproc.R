
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
library(haven)

# PATH INFORMATION -----------------------------------------------------

var_book <- read_rds("var_info/panel_variable_info.rds")# panel risk measures
measure_info_path <- c("var_info/indv_panel_var_info/") 
panel_data_path <- c("") # where the raw panel data is stored
preproc_data_path <- c("") # where to save processed panel data
panel_name <- "CMC"

# VARIABLE INFORMATION ----------------------------------------------------

panel_var <- var_book[[panel_name]]
# combining id & risk information
all_info <- bind_rows(panel_var$Measures, panel_var$ID, panel_var$Dems)
risk_info <- panel_var$Measures


# READING DATA -----------------------------------------------------

file_of_interest_name <- unique(all_info$varfile)
file_of_interest_name <- paste0(file_of_interest_name, ".sav")
# open file
file_of_interest <- list.files(path = panel_data_path, pattern = file_of_interest_name, recursive = T, full.names = T)
cmc_data <-read_sav(file_of_interest)

# create id var
cmc_data <- cmc_data %>% mutate(id = paste0("id_", as.character(1:n())))

#________________READ MAIN DATA FILES___________________# 

# list of unique wave pairs
wave_list <- sort(unique(na.omit(all_info$wave_id))) 
data_list <- NULL
wave_num <- 1
for (CurrWave in wave_list) {
  
  sub_all_info <- all_info %>% filter(wave_id == CurrWave)
  vars_of_interest <- sub_all_info$origin_varcode
  
  
  # select vars of interest
  data <- cmc_data %>% select(all_of(vars_of_interest)) 
  
  # replacing var codes
  colnames_found <- match(colnames(data), vars_of_interest, nomatch = 0)
  colnames(data)[colnames(data) %in% vars_of_interest] <- sub_all_info$varcode[colnames_found]
  
  # place in list
  data_list[[wave_num]] <- data %>%  mutate(wave_id = as.character(CurrWave))
  wave_num <- wave_num + 1
} # wave loop


#________________BIND DATA FROM DIFFERENT WAVES TOGETHER___________________#
main <- bind_rows(data_list)
main <- main %>% mutate_if(is.integer, ~ as.numeric(.x))
# dim(main) 8328 rows x 67 cols

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

# NO information on exact dates, can only use a estimate from the data page:
# Based on median date listed in https://search.gesis.org/research_data/ZA7480

main <- main %>% mutate(date = case_when(wave_id == "2000" ~ as.Date(paste("2000","03","23",sep="-"), "%Y-%m-%d"), # 17.01.2000 - 30.05.2000
                                         wave_id == "2001" ~ as.Date(paste("2001","03","10",sep="-"), "%Y-%m-%d"), #16.01.2001 - 03.05.2001
                                         wave_id == "2002" ~ as.Date(paste("2002","02","11",sep="-"), "%Y-%m-%d"), #21.01.2002 - 04.03.2002
                                         wave_id == "2003" ~ as.Date(paste("2003","02","03",sep="-"), "%Y-%m-%d"))) #14.01.2003 - 24.02.2003


main <- main %>% group_by(wave_id) %>% mutate(wave_year = round(mean(year(date)))) %>% ungroup() # calculate the general year for that wave




#________________FIX YOB & AGE & AGE RANGE___________________#
# quite a lot of cases (> 2'000) with missing age, trying to retrieve by creating a proxy yob derived from  the
# available age at a certain wave
main <- main %>% 
  mutate(temp_yob = wave_year - age)


main <- main %>% 
  group_by(id) %>% 
  fill(temp_yob,.direction = 'downup') %>%  # NAs get replaced with neighboring yob for each respondent 
  ungroup() %>% 
mutate(age = if_else(is.na(age), wave_year -temp_yob, age))

# no yob information, only age of respondent reported
# proxy method to verify consistency of yob: any respondents with an age difference that exceeds/is inferior to the range of wave years
age_diff_check_id <- main %>% 
  select(id, wave_id, age, date) %>% 
  group_by(id) %>% 
  summarise(diff_age = max(age)- min(age),
            yr_diff  = max(year(date))-min(year(date))) %>% 
  mutate(age_diff_check = if_else(abs(diff_age - yr_diff) > 2, 1, 0)) %>% 
  select(id, age_diff_check)
main <- main %>% left_join(age_diff_check_id, by = "id")     

# only keep respondents aged between 12 & 18y.o. (mainly middle/high schoolers)
main <- main %>%  filter(age %in% c(12:20)) %>% select(-temp_yob)

#________________FIX VARS: GENDER___________________#

# for some years, variables are empty but can be filled/reconstructed from previous or future waves...
main <- main %>% 
  group_by(id) %>% 
  fill(gender,.direction = 'downup') %>%  # NAs get replaced with neighboring gender for each respondent 
  ungroup() 

# recode gender
main <- main %>%
  mutate(gender = case_when(gender == 2 ~ 1,
                            gender == 1 ~ 0))


#  identify respondents with not the same gender reported across the waves
main <- main %>%
  group_by(id) %>%
  mutate(non_unique_gender = length(unique(na.omit(gender))) >1) %>%  
  ungroup()

inconsistent_dems <- main %>% 
  distinct(id, non_unique_gender, age_diff_check) %>%
  summarise(perc_gender = 100*(sum(non_unique_gender)/n()),
            perc_age =  100*(sum(age_diff_check)/n())) %>% 
  mutate(panel = panel_name)

#________________FIX VARS: RISK PREF. MEASURES___________________#


# DELIQUENCY
# Dependencies
main <- main %>% mutate(GRAFFITI_YR_OEB = case_when(GRAFFITI_YR_CHECK == 1 | GRAFFITI_EVER_CHECK == 1 ~ 0,
                                                    TRUE ~ GRAFFITI_YR_OEB),
                        DAMAGE_PROPER_YR_OEA = case_when(DAMAGE_YR_CHECK == 1 | DAMAGE_EVER_CHECK == 1 ~ 0,
                                                         TRUE ~ DAMAGE_PROPER_YR_OEA),
                        STEAL_VENDMACH_YR_OEA = case_when(STEAL_VENDMACH_YR_CHECK == 1 | STEAL_VENDMACH_EVER_CHECK == 1 ~ 0,
                                                          TRUE ~ STEAL_VENDMACH_YR_OEA),
                        STEAL_BIKE_YR_OEA = case_when(STEAL_BIKE_YR_CHECK == 1 | STEAL_BIKE_EVER_CHECK == 1 ~ 0,
                                                      TRUE ~ STEAL_BIKE_YR_OEA),
                        SHOPLIFT_YR_OEA = case_when(SHOPLIFT_YR_CHECK == 1 | SHOPLIFT_EVER_CHECK == 1 ~ 0,
                                                    TRUE ~ SHOPLIFT_YR_OEA),
                        STEAL_CAR_YR_OEA = case_when(STEAL_CAR_YR_CHECK == 1 | STEAL_CAR_EVER_CHECK == 1 ~ 0,
                                                     TRUE ~ STEAL_CAR_YR_OEA),
                        STEAL_BAG_YR_OEA = case_when(STEAL_BAG_YR_CHECK == 1 | STEAL_BAG_EVER_CHECK == 1 ~ 0,
                                                     TRUE ~ STEAL_BAG_YR_OEA),
                        STEAL_HOUSE_YR_OEA = case_when(STEAL_HOUSE_YR_CHECK == 1 | STEAL_HOUSE_EVER_CHECK == 1 ~ 0,
                                                       TRUE ~ STEAL_HOUSE_YR_OEA),
                        STEAL_OTH_YR_OEA = case_when(STEAL_OTH_YR_CHECK == 1 | STEAL_OTH_EVER_CHECK == 1 ~ 0,
                                                     TRUE ~ STEAL_OTH_YR_OEA),
                        STEAL_SELL_YR_OEA = case_when(STEAL_SELL_YR_CHECK == 1 | STEAL_SELL_EVER_CHECK == 1 ~ 0,
                                                      TRUE ~ STEAL_SELL_YR_OEA),
                        BREAKIN_CAR_YR_OEA = case_when(BREAKIN_CAR_YR_CHECK == 1 | BREAKIN_CAR_EVER_CHECK == 1 ~ 0,
                                                       TRUE ~ BREAKIN_CAR_YR_OEA),
                        ROBBERY_YR_OEA = case_when(ROBBERY_YR_CHECK == 1 | ROBBERY_EVER_CHECK == 1 ~ 0,
                                                   TRUE ~ ROBBERY_YR_OEA),
                        DRUG_SELL_YR_OEA = case_when(DRUG_SELL_YR_CHECK == 1 | DRUG_SELL_EVER_CHECK == 1 ~ 0,
                                                      TRUE ~ DRUG_SELL_YR_OEA),
                        HACK_YR_OEA = case_when(HACK_YR_CHECK == 1 | HACK_EVER_CHECK == 1 ~ 0,
                                                TRUE ~ HACK_YR_OEA),
                        ILLEGAL_DL_YR_OEA = case_when(ILLEGAL_DL_YR_CHECK == 1 | ILLEGAL_DL_EVER_CHECK == 1 ~ 0,
                                                      TRUE ~ ILLEGAL_DL_YR_OEA),
                        SCRATCHING_YR_OEA = case_when(SCRATCHING_YR_CHECK == 1 | SCRATCHING_EVER_CHECK == 1 ~ 0,
                                                      TRUE ~ SCRATCHING_YR_OEA))

# DRUGS
# Dependencies
main <- main %>% mutate(DRUG_YR_OEA = case_when(DRUG_YR_CHECK == 1 | DRUG_EVER_CHECK == 1 ~ 0,
                                                 TRUE ~ DRUG_YR_OEA))


# SOCIAL
# Dependencies
main <- main %>% mutate(FIGHT_WEAPON_YR_OEA = case_when(FIGHT_WEAPON_YR_CHECK == 1 | FIGHT_WEAPON_EVER_CHECK == 1 ~ 0,
                                                        TRUE ~ FIGHT_WEAPON_YR_OEA),
                        FIGHT_PHYS_YR_OEC = case_when(FIGHT_PHYS_YR_CHECK == 1 | FIGHT_PHYS_EVER_CHECK == 1 ~ 0,
                                                      TRUE ~ FIGHT_PHYS_YR_OEC))

# PROPENSITY
# reverse coding (higher score = more risk taking)
main <- main %>% 
  mutate(FIGHT_THREATEN_ORD4A = 5 - FIGHT_THREATEN_ORD4A,
         PRBML_POLICE_ORD4A = 5 - PRBML_POLICE_ORD4A,
         PRBML_TEACHER_ORD4A = 5 - PRBML_TEACHER_ORD4A,
         SHOPLIFT_ORD6A = 7 - SHOPLIFT_ORD6A,
         FIGHT_BACK_ORD4A = 5 - FIGHT_BACK_ORD4A)

# CHECK VARS --------------------------------------------------------------

#________________ CHECK VARS: FILTER OUT RESPONDENTS WITH MISSING DEMS___________________#

main <- main %>% 
  filter(!is.na(age)) %>% #keep rows that have age info
  filter(!is.na(gender)) %>%  #keep rows that have gender info
  filter(non_unique_gender == 0) %>% # filter out respondents with not the same gender reported across the waves 
  filter(age_diff_check == 0)  # filter out respondents with not the same yob reported across the waves 


#________________ CHECK VARS: DATA VISUAL (I)___________________#

# distribution of responses:
# summarytools::view(by(main, main$wave_id, dfSummary))


#________________ CHECK VARS: MEASURE QUAL. CRITERIA ___________________#
# exclude measures if:
# - in none of waves there is at least 4 different values possible for a measure (i.e., limited range of responses)

dependency_vars <- unique(na.omit(risk_info$varcode[risk_info$check_var == 1])) # vars to check for dependencies

vars_to_keep <- main  %>% 
  pivot_longer(!c(id, wave_id, wave_year, date, gender, age, non_unique_gender, age_diff_check), values_to = "response", names_to = "var_name") %>% 
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
risk_info_analyse <- risk_info %>% distinct(panel, varcode, measure_category, general_domain, domain_name,
                                            scale_type,scale_length, time_frame, behav_type, behav_paid) %>% 
  mutate(var_consider = if_else(varcode %in% dependency_vars, 0, 1), # considering non-dependency vars (or binary vars) for calculating retest correlations
         var_include = if_else(varcode %in% vars_to_keep$var_name, 1,0)) # include or exclude from calculating retest correlations
# dim(risk_info_analyse)  63 rows x  12 cols

# CREATE CLEAN/PROCESSED DATAFRAME  --------------------------------------------------------------

#________________SELECT VARS OF INTEREST FOR RETEST CALC___________________#

proc_data <- main %>% 
  # keep necessary vars (i.e., vars_to_keep and exclude vars used to check for dependencies)
  select(id, age, gender, wave_id,wave_year,date, # dems
         all_of(vars_to_keep$var_name)) %>% #risk measures
  filter_at(all_of(vars_to_keep$var_name),any_vars(!is.na(.))) #keep rows that have at least one risk value
# dim(proc_data)  6139 rows x  31 cols

# long format
proc_data_l <- pivot_longer(proc_data,all_of(vars_to_keep$var_name),names_to = "varcode", values_to = "response")
# dim(proc_data_l)  153475 rows x 8 cols

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

