
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
panel_data_path <- c("~/Documents/TSRP/Data/GLES/RawData/") # where the raw panel data is stored
preproc_data_path <- c("~/Documents/TSRP/Data/GLES/ProcData/") # where to save processed panel data
panel_name <- "GLES"

# VARIABLE INFORMATION ----------------------------------------------------

panel_var <- var_book[[panel_name]]
# combining id & risk information
all_info <- bind_rows(panel_var$Measures, panel_var$ID, panel_var$Dems, panel_var$Dates)
risk_info <- panel_var$Measures

# READING DATA -----------------------------------------------------

#________________READ MAIN DATA FILES___________________# 


data_list <- NULL
wave_num <- 1

for (CurrSample in unique(all_info$sample)) {
  
  samp_info <- all_info %>% filter(sample == CurrSample)
  
  panel <- unique(paste0(samp_info$panel,"_", samp_info$sample))
  
  # list of unique wave pairs
  wave_list <- sort(unique(na.omit(samp_info$wave_id))) 
  
  for (CurrWave in wave_list) {
    
    wave_info <- samp_info %>% filter(wave_id == CurrWave)
    
    
    # open file
    file_of_interest_name <- paste0(unique(wave_info$varfile), ".sav") # spss files
    file_of_interest <- list.files(path = panel_data_path, pattern = file_of_interest_name, recursive = T, full.names = T)
    data <-read.spss(file_of_interest, to.data.frame = T,use.value.labels = F, reencode = F)
    
    # select vars of interest
    vars_of_interest <- wave_info$origin_varcode 
    wave_data <- data %>% select(all_of(vars_of_interest)) 
    
    # replacing var codes
    colnames_found <- match(colnames(wave_data), vars_of_interest, nomatch = 0)
    colnames(wave_data)[colnames(wave_data) %in% vars_of_interest] <- wave_info$varcode[colnames_found]
    
    wave_data <- wave_data %>% 
      mutate(wave_id = CurrWave,
             panel = panel,
             id = paste0(as.character(set_id),"_",as.character(id))) 
    
    # place in list
    data_list[[wave_num]] <- wave_data
    wave_num <- wave_num + 1
  }
}
#________________BIND DATA FROM DIFFERENT WAVES TOGETHER___________________#
main <- bind_rows(data_list)
main <- main %>% mutate_if(is.integer, ~ as.numeric(.x))
# dim(main) 96608 rows x 8 cols


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

main <- main %>% 
  mutate(date = as.Date(as.POSIXct(date,format="%Y-%m-%d"))) %>%
  group_by(wave_id) %>% 
  mutate(date = case_when(is.na(date) ~ mean(date, na.rm = TRUE),
                          TRUE ~ date),
         wave_year = round(mean(year(date)))) %>% # calculate the general year for that wave
  ungroup()

#________________FIX VARS: YOB & AGE & AGE RANGE___________________#

# retrieve accurate yob (kpx_2290) from the restricted data file
age_info <- haven::read_dta(list.files(path = panel_data_path, pattern = "YOBVar", recursive = T, full.names = T))
age_info <- age_info %>% mutate(id = paste0(as.character(study),"_", as.character(lfdn))) %>% select(id, kpx_2290)

main <- main %>% left_join(age_info, by = "id") 
main <- main %>%  mutate(yob = case_when(is.na(yob) & set_id == 6838 ~ kpx_2290, # for the ZA6838 dataset accurate yob data can be obtained via a separate file. 
                                         TRUE ~ yob)) %>%
  select(-kpx_2290)

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


# only keep respondents aged between 16 & 90y.o. (based on panel documentation)
main <- main %>%  filter(age %in% c(16:90)) 

#________________FIX VARS: GENDER___________________#

# recode gender
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
  mutate(non_unique_gender = length(unique(na.omit(gender))) > 1) %>%  
  ungroup()

inconsistent_dems <- main %>% 
  group_by(panel) %>% 
  distinct(id, non_unique_gender, non_unique_yob) %>%
  summarise(perc_gender = 100*(sum(non_unique_gender)/n()),
            perc_yob = 100*(sum(non_unique_yob)/n())) 

#________________FIX VARS: RISK PREF. MEASURES___________________#

main <- main %>% mutate(RISK_GEN_ORD11A = case_when(RISK_GEN_ORD11A < 1  ~ NA_real_,
                                                    TRUE ~ RISK_GEN_ORD11A))

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

vars_to_keep <- main  %>% 
  pivot_longer(!c(id,panel, wave_id, wave_year, set_id, date, gender, age, non_unique_gender, non_unique_yob), values_to = "response", names_to = "var_name") %>% 
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
  mutate(var_consider = 1, # considering all for calculating retest correlations
         var_include = if_else(varcode %in% vars_to_keep$var_name, 1,0)) # include or exclude from calculating retest correlations
# dim(risk_info_analyse)  1 rows x  13 cols



# CREATE CLEAN/PROCESSED DATAFRAME  --------------------------------------------------------------

#________________SELECT VARS OF INTEREST FOR RETEST CALC___________________#

proc_data <- main %>% 
  # keep necessary vars (i.e., vars_to_keep and exclude vars used to check for dependencies)
  select(id, panel, age, gender, wave_id, wave_year,date, # dems
         all_of(vars_to_keep$var_name)) %>% #risk measures
  filter_at(all_of(vars_to_keep$var_name),any_vars(!is.na(.))) #keep rows that have at least one risk value
# dim(proc_data)  71548 rows x  8 cols

# long format
proc_data_l <- pivot_longer(proc_data,all_of(vars_to_keep$var_name),names_to = "varcode", values_to = "response")
# dim(proc_data_l)  71548 rows x  9 cols

# checking if all id-gender combinations are unique
# proc_data_l %>%  distinct(id, gender) %>% summarise(check = n() == length(unique(proc_data$id)))
# checking if all id-varcode-wave combinations are unique
# t <- proc_data_l %>%  group_by(id, wave_id, varcode) %>% summarise(check = n(), .groups = "drop"); sum(t$check > 1) == 0


#________________ CHECK VARS: DATA VISUAL (II)___________________#

# distribution of responses:
# summarytools::view(by(proc_data, proc_data$wave_id, dfSummary))



# ADD PANEL INFORMATION ---------------------------------------------------


proc_data_l <- proc_data_l %>% 
  left_join(panel_var$PanelInfo, by = "panel") %>% 
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
