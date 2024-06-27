
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
library(haven) # open dta

# PATH INFORMATION -----------------------------------------------------

var_book <- read_rds("var_info/panel_variable_info.rds")# panel risk measures
measure_info_path <- c("var_info/indv_panel_var_info/") 
panel_data_path <- c("~/Documents/TSRP/Data/LIKS/RawData/") # where the raw panel data is stored
preproc_data_path <- c("~/Documents/TSRP/Data/LIKS/ProcData/") # where to save processed panel data
panel_name <- "LIKS"

# VARIABLE INFORMATION ----------------------------------------------------

panel_var <- var_book[[panel_name]]
# combining id & risk information
all_info <- bind_rows(panel_var$Measures, panel_var$ID, panel_var$Dates)
risk_info <- panel_var$Measures
dems_info <- panel_var$Dems


# DEMS VARIABLE INFORMATION -----------------------------------------------------
# open master file for demographic information
dems_folder_of_interest <- unique(dems_info$varfolder)
dems_vars_of_interest <- unique(dems_info$origin_varcode)
dems_file_of_interest <- list.files(path = paste0(panel_data_path,dems_folder_of_interest, "/"), pattern = unique(dems_info$varfile), recursive = T) # search relevant file in main data folder
# open file 
dems_data <- read_dta(paste0(panel_data_path,dems_folder_of_interest, "/",dems_file_of_interest)) 
# select vars of interest
dems_data <- dems_data %>% select(all_of(dems_vars_of_interest)) 

# replacing var codes
colnames_found <- match(colnames(dems_data), dems_vars_of_interest, nomatch = 0)
colnames(dems_data)[colnames(dems_data) %in% dems_vars_of_interest] <- dems_info$varcode[colnames_found]

# W to L file
dems_data <- pivot_longer(dems_data, c(`2010`:`2019`), names_to = "wave_id", values_to = "id")
dems_data <- dems_data %>% mutate(id = as.character(id)) %>% 
  mutate(wave_id = as.character(wave_id)) %>% filter(!is.na(id))

# a few ids are not unique within each wave, these are removed
dems_data_count <- dems_data %>% group_by(wave_id, id) %>%
  summarise(i = n()) %>% 
  filter(i > 1)

dems_data <- dems_data %>% filter(!id %in% dems_data_count$id)

# READING DATA -----------------------------------------------------

#________________READ MAIN DATA FILES___________________# 

# list of unique wave 
wave_list <- sort(unique(na.omit(all_info$wave_id))) 
survey_data <- NULL
wave_num <- 1

for (CurrWave in wave_list) {
  data_list <- NULL
  
  sub_all_info <- all_info %>% filter(wave_id == CurrWave)
  file_list <- sort(unique(sub_all_info$varfile))
  folder_of_interest <- unique(sub_all_info$varfolder)
  
  file_num <- 1
  for (CurrFile in file_list) {
    # select file relevant info
    file_info <- sub_all_info %>% filter(varfile == CurrFile)
    
    
    vars_of_interest <- tolower(file_info$origin_varcode) # vars of interest
    file_of_interest <- list.files(path = paste0(panel_data_path,folder_of_interest, "/"), pattern = CurrFile, recursive = T) # search relevant file in main data folder
    
    if (CurrWave != 2019) {
      # open file 
      data <- read.spss(paste0(panel_data_path,folder_of_interest, "/",file_of_interest), to.data.frame=TRUE, use.value.labels = F, reencode= F) 
    }
    
    
    if (CurrWave == 2019) {
      # open file 
      data <- read_dta(paste0(panel_data_path,folder_of_interest, "/",file_of_interest), encoding = "latin1") 
    }
    
    # select vars of interest
    data <- data %>% select(all_of(vars_of_interest)) 
    
    # replacing var codes
    colnames_found <- match(colnames(data), vars_of_interest, nomatch = 0)
    colnames(data)[colnames(data) %in% vars_of_interest] <- file_info$varcode[colnames_found]
    
    # creating id variable (same output as using hhid and pid to match with unique ids)
    data <- data %>% mutate(id = case_when(pid <= 9 ~ paste0(hhid, "0", pid),
                                           pid > 9 ~ paste0(hhid, pid))) %>% select(-c(hhid, pid))
    
    
    # creating id variable based on panel documentation
    # data <- data %>% mutate(id = (hhid+pid/100)*100) %>%
    #   select(-c(hhid, pid)) # ---> leads to many ids with no unique_id assigned to it (+4'000 rows) even if the respondent took part in the survey
    
    
    # place in list
    data_list[[file_num]] <- data 
    file_num <- file_num + 1
    
  } # file loop
  
  wave_data <- reduce(data_list, full_join, by = "id") 
  survey_data[[wave_num]] <- wave_data %>%  mutate(wave_id = as.character(CurrWave))
  wave_num <- wave_num + 1
  
} # year loop

#________________BIND DATA FROM DIFFERENT WAVES TOGETHER___________________# 
main <- bind_rows(survey_data) %>% as_tibble()
main <- main %>% mutate_if(is.integer, ~ as.numeric(.x))
main <- main %>% mutate(id = as.character(id)) %>% mutate(wave_id = as.character(wave_id)) 

# adding dems & unique_id information
# only keep the unique id (important for retest as id changes slightly across waves and these have all been linked to a single id)
main <- left_join(main, dems_data, by = c("id","wave_id")) %>% select(-id)
main <- main %>% rename(id = unique_id) 
# dim(main) 47202 rows x 15 cols

# FIX VARS  --------------------------------------------------------------------

#________________FIX VARS: REMOVE DUPLICATE IDS ___________________#

#avoid IDs that within a wave are not unique (a few of these are duplicated)
main <- main %>% 
  group_by(id, wave_id) %>% 
  mutate(id_count = n()) %>%
  ungroup() 

main %>% group_by(wave_id) %>% summarise(sum(id_count > 1)) 
main <- main %>%  filter(id_count == 1) %>% select(-id_count)

#________________FIX VARS: DATES ___________________#

# If month & date are missing replace with the mean of available dates.
# If date information is completely missing from the raw data AND cannot be estimated from panel documentation, use 15th of June 
spss2date <- function(p) as.Date(p/86400, origin = "1582-10-14") # from https://www.ndacan.acf.hhs.gov/user-support/General_Support/R_Code_for_Importing_SPSS_Data_Files.pdf

main <- main %>% mutate(date = case_when(is.na(date) ~ as.Date(paste(wave_id,"06","15",sep="-"), "%Y-%m-%d"),
                                         TRUE~ spss2date(date))) # only dates available in 2016

main <- main %>% group_by(wave_id) %>% mutate(wave_year = round(mean(year(date)))) %>% ungroup() # calculate the general year for that wave


#________________FIX YOB & AGE & AGE RANGE___________________#

# some yob information missing in the masterfile.
# identify respondents with not the same yob reported across the waves
main <- main %>%
  group_by(id) %>%
  mutate(non_unique_yob = length(unique(na.omit(yob))) > 1) %>% 
  ungroup()

main <- main %>%
  mutate(age = year(date)- yob)

# only keep respondents aged between 18 & 90y.o.
main <- main %>%  filter(age %in% c(18:90)) %>% select(-yob)

#________________FIX VARS: GENDER___________________#

# recode gender 
main <- main %>%
  mutate(gender = case_when(gender == 2 ~ 1,
                            gender == 1 ~ 0)) # male = 0, female = 1



# some gender information missing in the masterfile.
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
# SMOKING
# Dependencies
# CIGS_DAY_OEA is set to 0 (instead of NA) for those who do not currently smoke.

main <- main %>% mutate(CIGS_DAY_OEA = case_when(is.na(CIGS_DAY_OEA) & wave_id %in% c("2011","2012","2013","2016", "2019") & SMK_CHECK == 2 ~ 0, 
                                                 CIGS_DAY_OEA == 99  & wave_id %in% c("2011","2012","2013","2016", "2019") ~ NA_real_, # potentially not valid entry
                                                 TRUE ~ CIGS_DAY_OEA))


# ALCOHOL
# Dependencies
# ALC_MO_ORD5A and ALC_*_L_MO_OEA are set to 5 (ie., Didn't drink alcohol)and 0, respectively, instead of NA for those who do not drink at all or who did not drink recently

main <- main %>% mutate(ALC_MO_ORD5A = case_when(ALC_MO_ORD5A == 2 & wave_id == "2019" ~ 4,
                                                 ALC_MO_ORD5A == 1 & wave_id == "2019" ~ 3,
                                                 ALC_MO_ORD5A == 3 & wave_id == "2019" ~ 1,
                                                 ALC_MO_ORD5A == 4 & wave_id == "2019" ~ 2,
                                                 ALC_MO_ORD5A == 5 & wave_id == "2019" ~ 5,
                                                 TRUE ~ ALC_MO_ORD5A),
                        ALC_MO_ORD5A = case_when(is.na(ALC_MO_ORD5A)  & wave_id %in% c("2010","2011","2012","2013","2016", "2019") & ALC_CHECK == 2 ~ 5, 
                                                 TRUE ~ ALC_MO_ORD5A),
                        
                        ALC_BEER_DAY_OEA = case_when(is.na(ALC_BEER_DAY_OEA) & wave_id %in% c("2010","2011","2012","2013") & c(ALC_CHECK == 2 | ALC_MO_ORD5A == 5) ~ 0,
                                                     TRUE ~ ALC_BEER_DAY_OEA),
                        
                        ALC_VODKA_DAY_OEA = case_when(is.na(ALC_VODKA_DAY_OEA) & wave_id %in% c("2010","2011","2012","2013") & c(ALC_CHECK == 2 | ALC_MO_ORD5A == 5) ~ 0,
                                                      TRUE ~ ALC_VODKA_DAY_OEA),
                        
                        ALC_WINE_DAY_OEA = case_when(is.na(ALC_WINE_DAY_OEA) & wave_id %in% c("2010","2011","2012","2013") & c(ALC_CHECK == 2 | ALC_MO_ORD5A == 5) ~ 0,
                                                     TRUE ~ ALC_WINE_DAY_OEA),
                        
                        ALC_HOMEMADE_DAY_OEA = case_when(is.na(ALC_HOMEMADE_DAY_OEA) & wave_id %in% c("2010","2011","2012","2013") & c(ALC_CHECK == 2 | ALC_MO_ORD5A == 5) ~ 0,
                                                         TRUE ~ ALC_HOMEMADE_DAY_OEA),
                        
                        ALC_OTHER_DAY_OEA = case_when(is.na(ALC_OTHER_DAY_OEA) & wave_id %in% c("2010","2011","2012","2013") & c(ALC_CHECK == 2 | ALC_MO_ORD5A == 5) ~ 0,
                                                      TRUE ~ ALC_OTHER_DAY_OEA))

# reverse scoring
main <- main %>% mutate(ALC_MO_ORD5A = 6 - ALC_MO_ORD5A)

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

dependency_vars <- unique(na.omit(risk_info$varcode[risk_info$check_var == 1])) # vars to check for dependencies

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
risk_info_analyse <- risk_info %>% distinct(panel, varcode, measure_category, general_domain, domain_name,
                                            scale_type,scale_length, time_frame, behav_type, behav_paid, item_num) %>% 
  mutate(var_consider = if_else(varcode %in% dependency_vars, 0,1), # considering non-dependency vars (or binary vars) for calculating retest correlations
         var_include = if_else(varcode %in% vars_to_keep$var_name, 1,0)) # include or exclude from calculating retest correlations
# dim(risk_info_analyse)  10 rows x  13 cols


# CREATE CLEAN/PROCESSED DATAFRAME  --------------------------------------------------------------

#________________SELECT VARS OF INTEREST FOR RETEST CALC___________________#

proc_data <- main %>% 
  # keep necessary vars (i.e., vars_to_keep and exclude vars used to check for dependencies)
  select(id, age, gender, wave_id,wave_year,date, # dems
         all_of(vars_to_keep$var_name)) %>% #risk measures
  filter_at(all_of(vars_to_keep$var_name),any_vars(!is.na(.))) #keep rows that have at least one risk value
# dim(proc_data)  46877 rows x  14 cols

# long format
proc_data_l <- pivot_longer(proc_data,all_of(vars_to_keep$var_name),names_to = "varcode", values_to = "response")
# dim(proc_data_l)  375016 rows x  8 cols

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

