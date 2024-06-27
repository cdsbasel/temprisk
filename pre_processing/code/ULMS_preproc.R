
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

library(tidyverse) # data wrangling + plotting
library(readxl) # reading excel file
library(lubridate) # dealing with dates
library(summarytools) # data check
library(haven) # open dta file

# PATH INFORMATION -----------------------------------------------------

var_book <- read_rds("var_info/panel_variable_info.rds")# panel risk measures
measure_info_path <- c("var_info/indv_panel_var_info/") 
panel_data_path <- c("~/Documents/TSRP/Data/ULMS/RawData/") # where the raw panel data is stored
preproc_data_path <- c("~/Documents/TSRP/Data/ULMS/ProcData/") # where to save processed panel data
panel_name <- "ULMS"

# VARIABLE INFORMATION ----------------------------------------------------

panel_var <- var_book[[panel_name]]
# combining id & risk information
all_info <- bind_rows(panel_var$Measures, panel_var$ID, panel_var$Dems, panel_var$Dates)
risk_info <- panel_var$Measures

# READING DATA -----------------------------------------------------


#________________READ MAIN DATA FILES___________________# 

# list of unique wave pairs
wave_list <- sort(unique(na.omit(all_info$wave_id))) 
survey_data <- NULL
wave_num <- 1

for (CurrWave in wave_list) {
  data_list <- NULL
  year_info <- all_info %>% filter(wave_id == CurrWave)
  file_list <- sort(unique(year_info$varfile))
  
  file_num <- 1
  for (CurrFile in file_list) {
    # select file relevant info
    file_info <- year_info %>% filter(varfile == CurrFile)
    
    vars_of_interest <- file_info$origin_varcode # vars of interest
    
    # open file 
    file_of_interest <- list.files(path = panel_data_path, pattern = file_info$varfile, recursive = TRUE, full.names = TRUE) # search relevant file in main data fol
    data <- read_dta(paste0(file_of_interest))
    
    # select vars of interest
    data <- data %>% select(all_of(vars_of_interest)) 
    
    # replacing var codes
    colnames_found <- match(colnames(data), vars_of_interest, nomatch = 0)
    colnames(data)[colnames(data) %in% vars_of_interest] <- file_info$varcode[colnames_found]
    
    # place in list
    data_list[[file_num]] <- data %>% mutate(id = paste0(hid, "_", pid)) %>% select(-c(hid,pid))
    file_num <- file_num + 1
    
  } # file loop
  
  # join variables from different files together
  wave_data <- reduce(data_list, full_join, by = c("id"))
  
  survey_data[[wave_num]] <- wave_data %>%  mutate(wave_id = as.character(CurrWave))
  wave_num <- wave_num + 1
  
} # year loop

#________________BIND DATA FROM DIFFERENT WAVES TOGETHER___________________# 
main <- bind_rows(survey_data)
main <- zap_labels(main)
main <- main %>% mutate_if(is.integer, ~ as.numeric(.x))
# dim(main) 29746 rows x 39 cols


# FIX VARS  --------------------------------------------------------------------

#________________REPLACING MISSING & NON READABLE RESPONSES___________________#
col_names <- colnames(main)
# Specify the column names to apply the replacement to (exclude "od" column)
cols_to_replace <- setdiff(col_names, "id")

# Replace negative/high values with NA in selected columns
main[ , cols_to_replace] <- main[ , cols_to_replace] %>% replace(. < 0, NA_real_)


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

main <- main %>%  mutate(date = as.Date.character(paste0(as.character(wave_id),"-", MINT, "-", DINT), format = "%Y-%m-%d" ))
main <- main %>% group_by(wave_id) %>% mutate(date = case_when(is.na(date) ~ mean(date, na.rm = T),
                                                               TRUE ~ date)) %>% ungroup() %>% 
  select(-c(MINT,DINT))

main <- main %>% group_by(wave_id) %>% mutate(wave_year = round(mean(year(date)))) %>% ungroup() # calculate the general year for that wave

#________________FIX VARS: HARMONIZED IDS___________________#
# distinct respondent id within household for same person (demographics remain consistent) across years,
#  need to harmonize

id_harmo <- main %>%
  rowwise %>%
  mutate(full_id = (str_split(id, "_", n = 2)[1])) %>%
  mutate(hid = unlist(full_id)[1],
         wave_id_dem = paste0(hid,"_", yob, "_", gender, "_", wave_id)) %>%
  group_by(wave_id_dem) %>% summarise(same_dem_resp = n()) %>% 
  filter(same_dem_resp == 1) %>% # removing 42 IDS: respondents within same household cannot be distinguished across waves (same gender and yob)
  rowwise() %>% 
  mutate(full_id = (str_split(wave_id_dem, "_", n = 4)[1]),
         hid = unlist(full_id)[1],
         yob = unlist(full_id)[2],
         gender = unlist(full_id)[3],
         yob_gender = paste0(yob, "_", gender)) %>% 
  select(-full_id)


id_harmo2 <- id_harmo %>% 
  distinct(hid, yob_gender) %>% 
  group_by(hid) %>% 
  mutate(id_harmonized = paste0(hid,"_", 1:n()))


main <- main %>%
  rowwise %>%
  mutate(full_id = (str_split(id, "_", n = 2)[1])) %>%
  mutate(hid = unlist(full_id)[1],
         yob_gender = paste0(yob, "_", gender),
         wave_id_dem = paste0(hid,"_", yob, "_", gender, "_", wave_id)) %>%
  filter(wave_id_dem %in% id_harmo$wave_id_dem) %>% 
  left_join(id_harmo2, by = c("hid","yob_gender")) %>%
  select(-c(id,wave_id_dem,yob_gender,hid, full_id, hid2003, pid2003)) %>%
  rename(id = id_harmonized)

#________________FIX VARS: AGE,  YOB and AGE RANGE___________________#

# 
main <- main %>% mutate(age = year(date) - yob)

#no missing age
# identify respondents with not the same yob reported across the waves
main <- main %>%
  group_by(id) %>%
  mutate(non_unique_yob = length(unique(na.omit(yob))) > 1) %>% #
  ungroup() %>% select(-yob)

# for this panel only keep respondents aged between 15 & 90y.o.
main <- main  %>%  filter(age %in% c(15:90)) 


#________________FIX VARS: GENDER___________________#

# for some years, variables are empty but can be filled/reconstructed from previous or future waves...
main <- main %>% 
  group_by(id) %>% 
  fill(gender,.direction = 'downup') %>%  # NAs get replaced with neighboring gender for each respondent 
  ungroup() 


# recode gender 
main <- main %>%
  mutate(gender = case_when(gender == 2 ~ 1,
                            gender == 1 ~ 0)) # male = 0, female = 1



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
# ALCOHOL
# Dependencies (refer to codebook)
main <- main %>% mutate(
  ALC_3MO_ORD7B = case_when(is.na(ALC_3MO_ORD7B) & wave_id %in% c("2003", "2004") & ALC_CHECK == 2 ~ 7,
                            TRUE ~ ALC_3MO_ORD7B),
  ALC_MO_ORD7B = case_when(is.na(ALC_MO_ORD7B) & wave_id %in% c("2007", "2012") & ALC_CHECK == 2 ~ 7,
                           ALC_MO_ORD7B > 95 ~ NA,
                           TRUE ~ ALC_MO_ORD7B), 
  ALC_BEER_DAY_OEB = case_when(is.na(ALC_BEER_DAY_OEB) & wave_id %in% c("2007", "2012") & c(ALC_CHECK == 2 | ALC_MO_ORD7B == 7 | ALC_CHECK2 == 2) ~ 0,
                               TRUE ~ ALC_BEER_DAY_OEB),
  ALC_WINE_DAY_OEB = case_when(is.na(ALC_WINE_DAY_OEB) & wave_id %in% c("2007", "2012") & c(ALC_CHECK == 2 | ALC_MO_ORD7B == 7 | ALC_CHECK3 == 2) ~ 0,
                               TRUE ~ ALC_WINE_DAY_OEB),
  ALC_WINE_DAY_OEC = case_when(is.na(ALC_WINE_DAY_OEC) & wave_id %in% c("2007", "2012") & c(ALC_CHECK == 2 | ALC_MO_ORD7B == 7 | ALC_CHECK4 == 2) ~ 0,
                               TRUE ~ ALC_WINE_DAY_OEC),
  ALC_HOMEMADE_DAY_OEB = case_when(is.na(ALC_HOMEMADE_DAY_OEB) & wave_id %in% c("2007", "2012") & c(ALC_CHECK == 2 | ALC_MO_ORD7B == 7 | ALC_CHECK5 == 2) ~ 0,
                                   TRUE ~ ALC_HOMEMADE_DAY_OEB),
  ALC_VODKA_DAY_OEB = case_when(is.na(ALC_VODKA_DAY_OEB) & wave_id %in% c("2007", "2012") & c(ALC_CHECK == 2 | ALC_MO_ORD7B == 7 | ALC_CHECK6 == 2) ~ 0,
                                TRUE ~ ALC_VODKA_DAY_OEB),
  ALC_OTHER_DAY_OEB = case_when(is.na(ALC_OTHER_DAY_OEB) & wave_id %in% c("2007", "2012") & c(ALC_CHECK == 2 | ALC_MO_ORD7B == 7 | ALC_CHECK7 == 2) ~ 0,
                                TRUE ~ ALC_OTHER_DAY_OEB),
  ALC_BEER_WK_OEA = case_when(is.na(ALC_BEER_WK_OEA) & wave_id %in% c("2003", "2004") & c(ALC_CHECK == 2 | ALC_3MO_ORD7B == 7 | ALC_CHECK2 == 0) ~ 0,
                              TRUE ~ ALC_BEER_WK_OEA),
  ALC_WINE_WK_OEA = case_when(is.na(ALC_WINE_WK_OEA) & wave_id %in% c("2003", "2004") & c(ALC_CHECK == 2 | ALC_3MO_ORD7B == 7 | ALC_CHECK3 == 0) ~ 0,
                              TRUE ~ ALC_WINE_WK_OEA),
  ALC_WINE_WK_OEB = case_when(is.na(ALC_WINE_WK_OEB) & wave_id %in% c("2003", "2004") & c(ALC_CHECK == 2 | ALC_3MO_ORD7B == 7 | ALC_CHECK4 == 0) ~ 0,
                              TRUE ~ ALC_WINE_WK_OEB),
  ALC_HOMEMADE_WK_OEA = case_when(is.na(ALC_HOMEMADE_WK_OEA) & wave_id %in% c("2003", "2004")& c(ALC_CHECK == 2 | ALC_3MO_ORD7B == 7 | ALC_CHECK5 == 0) ~ 0,
                                  TRUE ~ ALC_HOMEMADE_WK_OEA),
  ALC_VODKA_WK_OEA = case_when(is.na(ALC_VODKA_WK_OEA) & wave_id %in% c("2003", "2004") & c(ALC_CHECK == 2 | ALC_3MO_ORD7B == 7 | ALC_CHECK6 == 0) ~ 0,
                               TRUE ~ ALC_VODKA_WK_OEA)) %>% 
  mutate(ALC_3MO_ORD7B = 8 - ALC_3MO_ORD7B, # reverse scoring
         ALC_MO_ORD7B = 8 - ALC_MO_ORD7B) # reverse scoring


#SMOKING
main <- main %>% mutate(
  CIGS_DAY_OEF = case_when(is.na(CIGS_DAY_OEF) & wave_id %in% c("2003", "2004", "2007") & c(SMO_CHECK == 2 | SMO_CHECK == 1 | SMO_CHECK3 == 5) ~ 0,
                           TRUE ~ CIGS_DAY_OEF))


## LOTTERY
main <- main %>% mutate(
  BANK_ALLOC = case_when(BANK_ALLOC > 6 ~ NA,
                         TRUE ~ BANK_ALLOC))%>% 
  mutate(BANK_ALLOC = 7 - BANK_ALLOC) # reverse scoring


## PROPENSITY
main <- main %>% mutate(
  RISK_GEN_ORD11A = case_when(RISK_GEN_ORD11A > 10 ~ NA,
                              TRUE ~ RISK_GEN_ORD11A),
  RISK_DRI_ORD11A = case_when(RISK_DRI_ORD11A > 10 ~ NA,
                              TRUE ~ RISK_DRI_ORD11A),
  RISK_FIN_ORD11A = case_when(RISK_FIN_ORD11A > 10 ~ NA,
                              TRUE ~ RISK_FIN_ORD11A),
  RISK_REC_ORD11A = case_when(RISK_REC_ORD11A > 10 ~ NA,
                              TRUE ~ RISK_REC_ORD11A),
  RISK_OCC_ORD11A = case_when(RISK_OCC_ORD11A > 10 ~ NA_real_,
                              TRUE ~ RISK_OCC_ORD11A),
  RISK_HEA_ORD11A = case_when(RISK_HEA_ORD11A > 10 ~ NA_real_,
                              TRUE ~ RISK_HEA_ORD11A)) 
# CHECK VARS --------------------------------------------------------------

#________________ CHECK VARS: FILTER OUT RESPONDENTS WITH MISSING DEMS___________________#

main <- main %>% 
  filter(!is.na(age)) %>% #keep rows that have age info
  filter(!is.na(gender)) %>%  #keep rows that have gender info
  filter(non_unique_gender == 0) %>%  # filter out respondents with not the same gender reported across the waves 
  filter(non_unique_yob == 0) 

#________________ CHECK VARS: DATA VISUAL (I)___________________#

# distribution of responses:
# summarytools::view(by(main, main$wave_id, dfSummary))

#________________ CHECK VARS: MEASURE QUAL. CRITERIA ___________________#
# exclude measures if:
# - in none of the waves there is at least 4 different values possible for a measure (i.e., limited range of responses)


dependency_vars <- unique(na.omit(risk_info$varcode[risk_info$check_var == 1])) # vars to check for dependencies

vars_to_keep <- main  %>% 
  pivot_longer(!c(id, wave_id, wave_year, date, gender, age, non_unique_gender, non_unique_yob), values_to = "response", names_to = "var_name") %>% 
  filter(!is.na(response)) %>% 
  group_by(var_name, wave_id) %>%
  filter(!var_name %in% dependency_vars) %>%  # ignore vars to check for dependencies
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
# dim(risk_info_analyse)  31 rows x  13 cols

# CREATE CLEAN/PROCESSED DATAFRAME  --------------------------------------------------------------

#________________SELECT VARS OF INTEREST FOR RETEST CALC___________________#

proc_data <- main %>% 
  # keep necessary vars (i.e., vars_to_keep and exclude vars used to check for dependencies)
  select(id, age, gender, wave_id,wave_year,date, # dems
         all_of(vars_to_keep$var_name)) %>% #risk measures
  filter_at(all_of(vars_to_keep$var_name),any_vars(!is.na(.))) #keep rows that have at least one risk value

# dim(proc_data)  29628 rows x 27 cols 

# long format
proc_data_l <- pivot_longer(proc_data,all_of(vars_to_keep$var_name),names_to = "varcode", values_to = "response")
# dim(proc_data_l)  622188  rows x 8 cols 

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

