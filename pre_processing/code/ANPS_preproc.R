
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



# PATH INFORMATION -----------------------------------------------------

var_book <- read_rds("var_info/panel_variable_info.rds")# panel risk measures
measure_info_path <- c("var_info/indv_panel_var_info/") 
panel_data_path <- c("~/Documents/TSRP/Data/ANPS/RawData/") # where the raw panel data is stored
preproc_data_path <- c("~/Documents/TSRP/Data/ANPS/ProcData/") # where to save processed panel data
panel_name <- "ANPS"

# VARIABLE INFORMATION ----------------------------------------------------

panel_var <- var_book[[panel_name]]
# combining id & risk information
all_info <- bind_rows(panel_var$Measures, panel_var$ID, panel_var$Dems, panel_var$Dates)
risk_info <- panel_var$Measures

# READING DATA -----------------------------------------------------

#________________READ MAIN DATA FILES___________________# 

# Read files listed in the all_var_info df --> 
files_of_interest <- list.files(path = panel_data_path,
                                recursive = TRUE,
                                pattern =  paste0(unique(na.omit(all_info$varfile)), ".csv"),
                                full.names = TRUE)

dat <- read_csv(files_of_interest) # open file

# list of unique wave 
wave_list <- sort(unique(na.omit(all_info$wave_id))) 
data_list <- NULL
wave_num <- 1

for (CurrWave in wave_list) {
  
  
  
  sub_all_info <- all_info %>% filter(wave_id == CurrWave)
  vars_of_interest <- sub_all_info$origin_varcode
  
  # select vars of interest from the main file
  wave_dat <- dat %>% filter(WAVE_dum == CurrWave) %>% 
    select(all_of(vars_of_interest)) 
  
  # replacing var names
  colnames_found <- match(colnames(wave_dat), vars_of_interest, nomatch = 0)
  colnames(wave_dat)[colnames(wave_dat) %in% vars_of_interest] <- sub_all_info$varcode[colnames_found]
  
  
  # place in list
  data_list[[wave_num]] <- wave_dat %>%  mutate(wave_id = paste0("W", wave_num)) %>% select(-SYEAR)
  wave_num <- wave_num + 1
} # wave loop


#________________BIND DATA FROM DIFFERENT WAVES TOGETHER___________________#
main <- bind_rows(data_list)
main <- main %>% filter(!country %in% c("Mexico", "India")) # exclude responses from Mexico and India because not enough data to compute retest correlations

main <- main %>% mutate_if(is.integer, ~ as.numeric(.x))
# dim(main) 2847 rows x 9 cols

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
# If some dates are missing replace these with the mean of available dates.
# If date information is completely missing from the raw data AND cannot be estimated from panel documentation, use 15th of June + survey year

# adding by default 15th as the day of the month
main <- main %>% 
  mutate(date = as.Date(date, format = "%d%b%Y")) %>% 
  group_by(wave_id) %>% 
  mutate(wave_year = round(mean(year(date)),0)) %>% ungroup()


#________________FIX YOB & AGE & AGE RANGE___________________#
# Some missing age; but these case are all of participants who took part in only one wave, and as there are two Waves, 
# we just need the W1 age.
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

# for this panel only keep respondents aged between 10 & 90y.o. 
main <- main %>%  filter(age %in% c(10:90)) 

#________________FIX GENDER___________________# 
# Some missing gender information; but these cases are all of participants who took part in only one wave, 


#  identify respondents with not the same gender reported across the waves
main <- main %>%
  group_by(id) %>%
  mutate(non_unique_gender = length(unique(na.omit(gender))) >1) %>%  
  ungroup()

main <- main %>%
  mutate(gender = case_when(gender == "Female" ~ 1, 
                            gender == "Male" ~ 0 )) 


inconsistent_dems <- main %>% 
  distinct(id, non_unique_gender, age_diff_check) %>%
  summarise(perc_gender = 100*(sum(non_unique_gender)/n()),
            perc_age =  100*(sum(age_diff_check)/n())) %>% 
  mutate(panel = panel_name)


#________________FIX VARS: RISK PREF. MEASURES___________________#

# NONE

# CHECK VARS --------------------------------------------------------------

#________________ CHECK VARS: FILTER OUT RESPONDENTS WITH MISSING DEMS___________________#

main <- main %>% 
  filter(!is.na(age)) %>% #keep rows that have age info
  filter(!is.na(gender)) %>%  #keep rows that have gender info
  filter(non_unique_gender == 0) %>%  # filter out respondents with not the same gender reported across the waves 
  filter(age_diff_check == 0)  # filter out respondents with very different ages reported across  waves 

#________________ CHECK VARS: DATA VISUAL (I)___________________#

# distribution of responses:
# summarytools::view(by(main, main$wave_id, dfSummary))

#________________ CHECK VARS: MEASURE QUAL. CRITERIA ___________________#
# exclude measures if:
# - in none of waves there is at least 4 different values possible for a measure (i.e., limited range of responses)


vars_to_keep <- main  %>% 
  pivot_longer(!c(id, country, wave_id, wave_year, date, gender, age, non_unique_gender, age_diff_check), values_to = "response", names_to = "var_name") %>% 
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
                                            scale_type,scale_length, time_frame, behav_type, behav_paid, item_num) %>% 
  mutate(var_consider = if_else(varcode %in% vars_to_keep$var_name, 1,0), # considering non-dependency vars (or binary vars) for calculating retest correlations
         var_include = if_else(varcode %in% vars_to_keep$var_name, 1,0)) # include or exclude from calculating retest correlations
# dim(risk_info_analyse)  2 rows x  12 cols


# CREATE CLEAN/PROCESSED DATAFRAME  --------------------------------------------------------------

#________________SELECT VARS OF INTEREST FOR RETEST CALC___________________#

proc_data <- main %>% 
  # keep necessary vars (i.e., vars_to_keep and exclude vars used to check for dependencies)
  select(id, country, age, gender, wave_id,wave_year,date, # dems
         all_of(vars_to_keep$var_name)) %>% #risk measures
  filter_at(all_of(vars_to_keep$var_name),any_vars(!is.na(.))) #keep rows that have at least one risk value
# dim(proc_data)  2627 rows x  9 cols

# long format
proc_data_l <- pivot_longer(proc_data,all_of(vars_to_keep$var_name),names_to = "varcode", values_to = "response")
# dim(proc_data_l)  5254 rows x  9 cols

# checking if all id-gender combinations are unique
# proc_data_l %>%  distinct(id, gender) %>% summarise(check = n() == length(unique(proc_data$id)))
# checking if all id-varcode-wave combinations are unique
# t <- proc_data_l %>%  group_by(id, wave_id, varcode) %>% summarise(check = n(), .groups = "drop"); sum(t$check > 1) == 0


#________________ CHECK VARS: DATA VISUAL (II)___________________#

# distribution of responses:
# summarytools::view(by(proc_data, proc_data$wave_id, dfSummary))



# ADD PANEL INFORMATION ---------------------------------------------------


proc_data_l <- proc_data_l %>% 
  mutate(panel = paste0(panel_name, "_", gsub(" ", "_", country))) %>% 
  select(-country) %>% 
  left_join(panel_var$PanelInfo, by =  "panel") %>% 
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

