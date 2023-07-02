
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



# PATH/FILES  -----------------------------------------------------

var_book <- read_rds("var_info/panel_variable_info.rds")# panel risk measures
measure_info_path <- c("var_info/indv_panel_var_info/") 
panel_data_path <- c("") # where the raw panel data is stored
preproc_data_path <- c("") # where to save processed panel data
panel_name <- "SAVE"

# VARIABLE INFORMATION ----------------------------------------------------

panel_var <- var_book[[panel_name]]

# combining id & risk information
all_info <- bind_rows(panel_var$Measures, panel_var$ID, panel_var$Dems, panel_var$Dates)
risk_info <- panel_var$Measures
id_info <- panel_var$ID
date_info <-panel_var$Dates
dems_info <- panel_var$Dems
# READING DATA -----------------------------------------------------

#________________READ MAIN DATA FILES___________________# 
# list of unique waves
file_list <- sort(unique(na.omit(all_info$varfile))) 

file_num <- 1
data_list <- NULL
for (CurrFile in file_list) {
  
  
  # select wave relevant info
  file_risk_info <- risk_info %>% filter(varfile == CurrFile)
  file_date_info <- date_info %>% filter(varfile == CurrFile)
  file_dem_info <- dems_info %>% filter(varfile == CurrFile)
  file_id_info <- id_info %>% filter(varfile == CurrFile)
  file_all_info <- all_info %>% filter(varfile == CurrFile)
  
  
  # open the relevant file
  # file find
  indicator_file <- list.files(path = panel_data_path, pattern = paste0(CurrFile, "_indicator.*\\.sav"), recursive = T) # search relevant file in main data folder
  imputed_file <- list.files(path = panel_data_path, pattern = paste0(CurrFile, "_1.*\\.sav"), recursive = T) # search relevant file in main data folder
  # 
  indicator_data <- read.spss(paste0(panel_data_path, indicator_file), to.data.frame=TRUE, use.value.labels = F) # opens the relevant file
  imputed_data <- read.spss(paste0(panel_data_path, imputed_file), to.data.frame=TRUE, use.value.labels = F) # opens the relevant file
  
  
  # Dems data
  dems_vars <- sort(unlist(c(paste0(file_dem_info$origin_varcode, "_imp"),file_id_info$origin_varcode)))
  dems_data <- imputed_data %>% select(all_of(dems_vars)) %>% arrange(respid)
  names(dems_data) <- sort(c(file_dem_info$origin_varcode ,file_id_info$origin_varcode)) # rename vars
  
  # year data
  date_vars <- c(file_date_info$origin_varcode,file_id_info$origin_varcode)
  date_data <- imputed_data %>% select(all_of(date_vars)) %>% arrange(respid) 
  
  
  # risk data: IMP
  imp_risk_vars <- sort(unlist(c(paste0(file_risk_info$origin_varcode, "_imp"),file_id_info$origin_varcode)))
  imp_risk_data <- imputed_data %>% select(all_of(imp_risk_vars)) %>% arrange(respid)  
  names(imp_risk_data) <- sort(c(file_risk_info$origin_varcode,file_id_info$origin_varcode)) # rename vars
  # risk data: IND 
  ind_risk_vars  <- sort(unlist(c(paste0(file_risk_info$origin_varcode, "_ind"),file_id_info$origin_varcode)))
  ind_risk_data <- indicator_data %>% select(all_of(ind_risk_vars)) %>% arrange(respid) 
  names(ind_risk_data) <- sort(c(file_risk_info$origin_varcode,file_id_info$origin_varcode)) # rename vars
  
  
  # imputation
  risk_data <- imp_risk_data
  for (CurrVar in file_risk_info$origin_varcode) {  risk_data[ind_risk_data[,CurrVar] == 1,CurrVar] = NA }
  
  #  merge: risk, date and dems info
  wave_data <- reduce(list(dems_data, date_data, risk_data), full_join, by = "respid")

  # replacing var codes
  colnames_found <- match(colnames(wave_data), file_all_info$origin_varcode, nomatch = 0)
  colnames(wave_data)[colnames(wave_data) %in% file_all_info$origin_varcode] <- file_all_info$varcode[colnames_found]
  

  # place in list
  data_list[[file_num]] <- wave_data
  file_num <- file_num + 1
}  # wave year loop

#________________BIND DATA FROM DIFFERENT WAVES TOGETHER___________________#  
main <- bind_rows(data_list)
main <- main %>% mutate(id = as.numeric(id))
main <- main %>% mutate_if(is.integer, ~ as.numeric(.x))
# dim(main) 22000 rows x 13 cols


# FIX VARS ----------------------------------------------------------------

#________________FIX VARS: DATES ___________________#
# If month & date are missing replace with the mean of available dates.
# If date information is completely missing from the raw data AND cannot be estimated from panel documentation, use 15th of June 

main <- main %>% mutate(date = as.Date(paste(year,"06","15",sep="-"), "%Y-%m-%d"))
main <- main %>% mutate(wave_year = year, # general year for that wave
                        wave_id = as.character(wave_year))


#________________FIX VARS: REMOVE DUPLICATE IDS ___________________#

#avoid IDs that within a wave are not unique
main <- main %>% 
  group_by(id, wave_id) %>% 
  mutate(id_count = n()) %>%
  ungroup() 

main %>% group_by(wave_id) %>% summarise(sum(id_count > 1)) 
main <- main %>%  filter(id_count == 1) %>% select(-id_count)


#________________FIX VARS: AGE & AGE-RANGE ___________________#

main <- main %>% 
  mutate(age =  year(date) - (yob + 1900))  # no missing age data

# identify respondents with not the same yob reported across the waves
main <- main %>%
  group_by(id) %>%
  mutate(non_unique_yob = length(unique(na.omit(yob))) > 1) %>% # 
  ungroup()  %>% 
  select(-c(year, yob))


# only keep respondents aged between 18 & 90y.o.
main <- main %>%  filter(age %in% c(18:90)) 


#________________FIX VARS: GENDER___________________#
# no missing gender data

#  identify respondents with not the same gender reported across the waves
main <- main %>%
  group_by(id) %>%
  mutate(non_unique_gender = length(unique(na.omit(gender))) >1) %>%  
  ungroup()

# recode gender 
main <- main %>%
  mutate(gender = case_when(gender == 2 ~ 1,
                            gender == 1 ~ 0)) # male = 0, female = 1



inconsistent_dems <- main %>% 
  distinct(id, non_unique_gender, non_unique_yob) %>%
  summarise(perc_gender = 100*(sum(non_unique_gender)/n()),
            perc_yob = 100*(sum(non_unique_yob)/n())) %>% 
  mutate(panel = panel_name)

#________________FIX VARS: RISK PREF. MEASURES___________________#
# ALCOHOL
# reverse scoring (higher score == more risk taking)
main <- main %>% mutate(ALC_6MO_ORD7A = 8 - ALC_6MO_ORD7A)

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
  pivot_longer(!c(id, wave_id, wave_year, date, gender, age, non_unique_gender, non_unique_yob), values_to = "response", names_to = "var_name") %>% 
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
  mutate(var_consider = 1, # considering all for calculating retest correlations
         var_include = if_else(varcode %in% vars_to_keep$var_name, 1,0)) # include or exclude from calculating retest correlations
# dim(risk_info_analyse)  9 rows x  12 cols

# CREATE CLEAN/PROCESSED DATAFRAME  --------------------------------------------------------------

#________________SELECT VARS OF INTEREST FOR RETEST CALC___________________#

proc_data <- main %>% 
  # keep necessary vars (i.e., vars_to_keep and exclude vars used to check for dependencies)
  select(id, age, gender, wave_id,wave_year,date, # dems
         all_of(vars_to_keep$var_name)) %>% #risk measures
  filter_at(all_of(vars_to_keep$var_name),any_vars(!is.na(.))) #keep rows that have at least one risk value
# dim(proc_data)  21493 rows x  15 cols

# long format
proc_data_l <- pivot_longer(proc_data,all_of(vars_to_keep$var_name),names_to = "varcode", values_to = "response")
# dim(proc_data_l)  193437 rows x  8 cols

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
