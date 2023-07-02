
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
library(lubridate) # dealing with dates
library(summarytools) # data check



# PATH/FILES  -----------------------------------------------------

var_book <- read_rds("var_info/panel_variable_info.rds")# panel risk measures
measure_info_path <- c("var_info/indv_panel_var_info/") 
panel_data_path <- c("") # where the raw panel data is stored
preproc_data_path <- c("") # where to save processed panel data
panel_name <- "ENKAVI"

# VARIABLE INFORMATION ----------------------------------------------------

panel_var <- var_book[[panel_name]]

# combining id & risk information
all_info <- bind_rows(panel_var$Measures, panel_var$ID, panel_var$Dems, panel_var$Dates)


# READING DATA -----------------------------------------------------

#________________READ MAIN DATA FILES___________________# 
# list of unique waves 
wave_list <- sort(unique(na.omit(all_info$wave_id))) 
survey_data <- NULL
wave_num <- 1

for (CurrWave in wave_list) {
  data_list <- NULL
  
  sub_all_info <- all_info %>% filter(wave_id == CurrWave)
  
  # list of unique files 
  file_list <- sort(unique(sub_all_info$varfile))
  file_num <- 1
  
  for (CurrFile in file_list)  {
    
    # select file relevant info
    file_info <- sub_all_info %>% filter(varfile == CurrFile)
    
    vars_of_interest <- file_info$origin_varcode # vars of interest
    
    # open file 
    data <- read_csv(paste0(panel_data_path, CurrWave, "/", CurrFile), show_col_types = FALSE)
    
    if ("...1" %in% names(data)) {
      data <- data %>% rename(X1 = `...1`)
    }
    
    # select vars of interest
    data <- data %>% select(all_of(vars_of_interest)) 
    
    
    if (CurrFile == "demographics_survey.csv") {
      data <- data %>%  distinct(worker_id, .keep_all = T)}
    
    
    # replacing var codes
    colnames_found <- match(colnames(data), vars_of_interest, nomatch = 0)
    colnames(data)[colnames(data) %in% vars_of_interest] <- file_info$varcode[colnames_found]
    

    # place file specific data in list
    
    data_list[[file_num]] <- data 
    file_num <- file_num + 1
  } # file loop
  
  # join variables from different files together
  wave_data <- reduce(data_list, full_join, by = "id")
  
  survey_data[[wave_num]] <- wave_data %>%  mutate(wave_id = CurrWave)
  wave_num <- wave_num + 1
} # wave_id loop


#________________BIND DATA FROM DIFFERENT WAVES TOGETHER___________________#
main <- bind_rows(survey_data)
main <- main %>% mutate_if(is.integer, ~ as.numeric(.x)) 
# dim(main) 673 rows x 26 cols

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
main <- main %>%  mutate(date = as.Date(date))

main <- main %>% group_by(wave_id) %>% mutate(wave_year = round(mean(year(date)))) %>% ungroup() # calculate the general year for that wave

#________________ FIX VARS: AGE RANGE___________________#

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


# for this panel only keep respondents aged between 18 & 90y.o.
main <- main  %>%  filter(age %in% c(18:90))  

#________________FIX VARS: GENDER___________________#
# NA 

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


# DEPENDENCIES: CANNABIS/MARIJUANA QUESTIONS

main <- main %>% mutate(CANNAB_MEMO_6MO_ORD5A = case_when(CANNAB_CHECK == 0 ~ 0,
                                                TRUE ~ CANNAB_MEMO_6MO_ORD5A),
                        CANNAB_TIME_6MO_ORD5A = case_when(CANNAB_CHECK == 0 ~ 0,
                                                TRUE ~ CANNAB_TIME_6MO_ORD5A),
                        CANNAB_ACT_6MO_ORD5A = case_when(CANNAB_CHECK == 0 ~ 0,
                                               TRUE ~ CANNAB_ACT_6MO_ORD5A),
                        CANNAB_STOP_6MO_ORD5A = case_when(CANNAB_CHECK == 0 ~ 0,
                                                TRUE ~ CANNAB_STOP_6MO_ORD5A))

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

dependency_vars <- unique(na.omit(panel_var$Measures$varcode[panel_var$Measures$check_var == 1])) # vars to check for dependencies

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
risk_info_analyse <- panel_var$Measures %>% distinct(panel, varcode, measure_category, general_domain, domain_name,
                                                     scale_type,scale_length, time_frame, behav_type, behav_paid) %>% 
  mutate(var_consider = if_else(varcode %in% dependency_vars, 0, 1), # considering non-dependency vars (or binary vars) for calculating retest correlations
         var_include = if_else(varcode %in% vars_to_keep$var_name, 1,0)) # include or exclude from calculating retest correlations
# dim(risk_info_analyse) 21 rows x 12 cols

# CREATE CLEAN/PROCESSED DATAFRAME  --------------------------------------------------------------

#________________SELECT VARS OF INTEREST FOR RETEST CALC___________________#

proc_data <- main %>% 
  # keep necessary vars (i.e., vars_to_keep and exclude vars used to check for dependencies)
  select(id, age, gender, wave_id,wave_year,date, # dems
         all_of(vars_to_keep$var_name)) %>% #risk measures
  filter_at(all_of(vars_to_keep$var_name),any_vars(!is.na(.))) #keep rows that have at least one risk value
# dim(proc_data) 669 rows x  26 cols


# long format
proc_data_l <- pivot_longer(proc_data,all_of(vars_to_keep$var_name),names_to = "varcode", values_to = "response")
# dim(proc_data_l) 13380 rows x  8 cols

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

