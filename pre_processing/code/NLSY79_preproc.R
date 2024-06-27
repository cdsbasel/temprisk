
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


# PATH INFORMATION -----------------------------------------------------

var_book <- read_rds("var_info/panel_variable_info.rds")# panel risk measures
measure_info_path <- c("var_info/indv_panel_var_info/") 
panel_data_path <- c("~/Documents/TSRP/Data/NLSY79/RawData/") # where the raw panel data is stored
preproc_data_path <- c("~/Documents/TSRP/Data/NLSY79/ProcData/") # where to save processed panel data
panel_name <- "NLSY79"

# VARIABLE INFORMATION ----------------------------------------------------

panel_var <- var_book[[panel_name]]
# combining id & risk information
all_info <- bind_rows(panel_var$Measures, panel_var$ID, panel_var$Dems)
risk_info <- panel_var$Measures
newvars_info <- panel_var$NewVars

# READING DATA-----------------------------------------------------


file_of_interest_name <- unique(na.omit(all_info$varfile)) # in which file are the relevant vars stored
file_of_interest_name <- paste0(file_of_interest_name,".csv") # csv file
file_of_interest <- list.files(path = panel_data_path, pattern = file_of_interest_name, full.names = TRUE, recursive = TRUE)
# open file
main_data <-read_csv(file_of_interest)

#________________READ MAIN DATA FILES___________________# 

# list of unique waves
wave_list <- unique(na.omit(all_info$wave_id)) 
data_list <- NULL
wave_num <- 1

for (CurrWave in wave_list) {
  
  # select wave relevant info
  sub_all_info <- all_info %>% filter(wave_id == CurrWave)
  
  # select vars of interest
  vars_of_interest <- sub_all_info$origin_varcode
  wave_data <- main_data %>% select(all_of(vars_of_interest)) %>%  mutate(wave_id = as.character(CurrWave))
  
  # replacing var codes
  colnames_found <- match(colnames(wave_data), vars_of_interest, nomatch = 0)
  colnames(wave_data)[colnames(wave_data) %in% vars_of_interest] <- sub_all_info$varcode[colnames_found]
  
  # place in list
  data_list[[wave_num]] <- wave_data
  wave_num <- wave_num + 1
}


#________________BIND DATA FROM DIFFERENT WAVES TOGETHER___________________# 

main <- bind_rows(data_list)
main <- main %>% mutate_if(is.integer, ~ as.numeric(.x))
# dim(main) 317150 rows x 55 cols

# FIX VARS  --------------------------------------------------------------------

#________________REPLACING MISSING & NON READABLE RESPONSES___________________#
main <- main %>% 
  replace(.< 0 , NA_real_ ) # any vars with values below 0 (i.e., invalid, DK, proxy, missing,inapplicable....) are replaced  


#________________FIX VARS: RISK PREF. MEASURES___________________#

# ALCOHOL
# Dependencies (refer to codebook)
main <- main %>% mutate(
  ALC_6DRINKS_MO_ORD7A = case_when(is.na(ALC_6DRINKS_MO_ORD7A) & wave_id %in% as.character(c(1982, 1983, 1984, 1985, 1988, 1989, 1994, 2002)) & (ALCEVER_CHECK == 0 | ALCREC_CHECK == 0)  ~ 0,
                                   TRUE ~ ALC_6DRINKS_MO_ORD7A),
  ALC_6DRINKS_MO_ORD6A = case_when(is.na(ALC_6DRINKS_MO_ORD6A) & !wave_id %in% as.character(c(1982, 1983, 1984, 1985, 1988, 1989, 1994, 2002)) & (ALCEVER_CHECK == 0 | ALCREC_CHECK == 0)  ~ 0,
                                   TRUE ~ ALC_6DRINKS_MO_ORD6A),
  ALC_DRINKS_MO_DIS31B = case_when(is.na(ALC_DRINKS_MO_DIS31B) & wave_id %in% as.character(c(1983, 1984, 1985, 1988, 1989, 1992, 1994, 2002, 2006, 2008, 2010, 2012, 2014, 2018))  & (ALCEVER_CHECK == 0 | ALCREC_CHECK == 0)  ~ 0,
                                   TRUE ~ ALC_DRINKS_MO_DIS31B),
  ALC_DRINKS_DAY_OEC = case_when(is.na(ALC_DRINKS_DAY_OEC) & wave_id %in% as.character(c(1988, 1989, 1994, 2002, 2006, 2008, 2010, 2012, 2014, 2018))  & (ALCEVER_CHECK == 0 | ALCREC_CHECK == 0 | ALC_DRINKS_CHECK == 0)  ~ 0,
                                 is.na(ALC_DRINKS_DAY_OEC) & wave_id %in% as.character(1992)  & (ALC_DRINKS_CHECK == 0)  ~ 0,
                                 TRUE ~ ALC_DRINKS_DAY_OEC),
  ALC_1DRINK_MO_DIS31A = case_when(is.na(ALC_1DRINK_MO_DIS31A) & wave_id %in% as.character(c(1983:1985))  & (ALCEVER_CHECK == 0 | ALCREC_CHECK == 0)  ~ 0,
                                   TRUE ~ ALC_1DRINK_MO_DIS31A),
  ALC_2DRINKS_MO_DIS31A = case_when(is.na(ALC_2DRINKS_MO_DIS31A) & wave_id %in% as.character(c(1983:1985))  & (ALCEVER_CHECK == 0 | ALCREC_CHECK == 0)  ~ 0,
                                    TRUE ~ ALC_2DRINKS_MO_DIS31A),
  ALC_3DRINKS_MO_DIS31A = case_when(is.na(ALC_3DRINKS_MO_DIS31A) & wave_id %in% as.character(c(1983:1985))  & (ALCEVER_CHECK == 0 | ALCREC_CHECK == 0)  ~ 0,
                                    TRUE ~ ALC_3DRINKS_MO_DIS31A),
  ALC_4DRINKS_MO_DIS31A = case_when(is.na(ALC_4DRINKS_MO_DIS31A) & wave_id %in% as.character(c(1983:1985))  & (ALCEVER_CHECK == 0 | ALCREC_CHECK == 0)  ~ 0,
                                    TRUE ~ ALC_4DRINKS_MO_DIS31A),
  ALC_5DRINKS_MO_DIS31B = case_when(is.na(ALC_5DRINKS_MO_DIS31B) & wave_id %in% as.character(c(1983:1985))  & (ALCEVER_CHECK == 0 | ALCREC_CHECK == 0)  ~ 0,
                                    TRUE ~ ALC_5DRINKS_MO_DIS31B),
  ALC_6DRINKS_MO_DIS31A = case_when(is.na(ALC_6DRINKS_MO_DIS31A) & wave_id %in% as.character(c(1983:1985))  & (ALCEVER_CHECK == 0 | ALCREC_CHECK == 0)  ~ 0,
                                    TRUE ~ ALC_6DRINKS_MO_DIS31A),
  ALC_WK_DIS8C = case_when(is.na(ALC_WK_DIS8C) & wave_id %in% as.character(c(1982:1985))  & (ALCEVER_CHECK == 0 | ALCREC_CHECK == 0)  ~ 0,
                           TRUE ~ ALC_WK_DIS8C),
  BAR_VISIT_MO_ORD6A = case_when(is.na(BAR_VISIT_MO_ORD6A) & wave_id %in% as.character(c(1982:1984))  & (ALCEVER_CHECK == 0 | ALCREC_CHECK == 0)  ~ 0,
                                 TRUE ~ BAR_VISIT_MO_ORD6A),
  ALC_HUNGOVER_MO_DIS31 = case_when(is.na(ALC_HUNGOVER_MO_DIS31) & wave_id %in% as.character(c(1983:1985))  & (ALCEVER_CHECK == 0 | ALCREC_CHECK == 0)  ~ 0,
                                    TRUE ~ ALC_HUNGOVER_MO_DIS31),
  ALC_BEER_WK_OEA = case_when(is.na(ALC_BEER_WK_OEA) & wave_id %in% as.character(c(1982:1985))  & (ALCEVER_CHECK == 0 | ALCREC_CHECK == 0 | ALC_WK_DIS8C == 0)  ~ 0,
                              TRUE ~ ALC_BEER_WK_OEA),
  ALC_WINE_WK_OEA = case_when(is.na(ALC_WINE_WK_OEA) & wave_id %in% as.character(c(1982:1985))  & (ALCEVER_CHECK == 0 | ALCREC_CHECK == 0 | ALC_WK_DIS8C == 0)  ~ 0,
                              TRUE ~ ALC_WINE_WK_OEA),
  ALC_LIQUOR_WK_OEA = case_when(is.na(ALC_LIQUOR_WK_OEA) & wave_id %in% as.character(c(1982:1985))  & (ALCEVER_CHECK == 0 | ALCREC_CHECK == 0 | ALC_WK_DIS8C == 0)  ~ 0,
                                TRUE ~ ALC_LIQUOR_WK_OEA),
  ALC_STATEMENTA_YR_ORD5A = case_when(is.na(ALC_STATEMENTA_YR_ORD5A) & wave_id %in% as.character(c(1994,1989))  & (ALCEVER_CHECK == 0 | ALCREC_CHECK == 0)  ~ 5,
                                      TRUE ~ ALC_STATEMENTA_YR_ORD5A),
  ALC_STATEMENTB_YR_ORD5A = case_when(is.na(ALC_STATEMENTB_YR_ORD5A) & wave_id %in% as.character(c(1994,1989))  & (ALCEVER_CHECK == 0 | ALCREC_CHECK == 0)  ~ 5,
                                      TRUE ~ ALC_STATEMENTB_YR_ORD5A),
  ALC_STATEMENTC_YR_ORD5A = case_when(is.na(ALC_STATEMENTC_YR_ORD5A) & wave_id %in% as.character(c(1994,1989))  & (ALCEVER_CHECK == 0 | ALCREC_CHECK == 0)  ~ 5,
                                      TRUE ~ ALC_STATEMENTC_YR_ORD5A),
  ALC_STATEMENTD_YR_ORD5A = case_when(is.na(ALC_STATEMENTD_YR_ORD5A) & wave_id %in% as.character(c(1994,1989))  & (ALCEVER_CHECK == 0 | ALCREC_CHECK == 0)  ~ 5,
                                      TRUE ~ ALC_STATEMENTD_YR_ORD5A),
  ALC_STATEMENTE_YR_ORD5A = case_when(is.na(ALC_STATEMENTE_YR_ORD5A) & wave_id %in% as.character(c(1994,1989))  & (ALCEVER_CHECK == 0 | ALCREC_CHECK == 0)  ~ 5,
                                      TRUE ~ ALC_STATEMENTE_YR_ORD5A),
  ALC_STATEMENTF_YR_ORD5A = case_when(is.na(ALC_STATEMENTF_YR_ORD5A) & wave_id %in% as.character(c(1994,1989))  & (ALCEVER_CHECK == 0 | ALCREC_CHECK == 0)  ~ 5,
                                      TRUE ~ ALC_STATEMENTF_YR_ORD5A),
  ALC_STATEMENTG_YR_ORD5A = case_when(is.na(ALC_STATEMENTG_YR_ORD5A) & wave_id %in% as.character(c(1994,1989))  & (ALCEVER_CHECK == 0 | ALCREC_CHECK == 0)  ~ 5,
                                      TRUE ~ ALC_STATEMENTG_YR_ORD5A),
  ALC_STATEMENTH_YR_ORD5A = case_when(is.na(ALC_STATEMENTH_YR_ORD5A) & wave_id %in% as.character(c(1994,1989))  & (ALCEVER_CHECK == 0 | ALCREC_CHECK == 0)  ~ 5,
                                      TRUE ~ ALC_STATEMENTH_YR_ORD5A),
  ALC_STATEMENTI_YR_ORD5A = case_when(is.na(ALC_STATEMENTI_YR_ORD5A) & wave_id %in% as.character(c(1994,1989))  & (ALCEVER_CHECK == 0 | ALCREC_CHECK == 0)  ~ 5,
                                      TRUE ~ ALC_STATEMENTI_YR_ORD5A),
  ALC_STATEMENTJ_YR_ORD5A = case_when(is.na(ALC_STATEMENTJ_YR_ORD5A) & wave_id %in% as.character(c(1994,1989))  & (ALCEVER_CHECK == 0 | ALCREC_CHECK == 0)  ~ 5,
                                      TRUE ~ ALC_STATEMENTJ_YR_ORD5A),
  ALC_STATEMENTK_YR_ORD5A = case_when(is.na(ALC_STATEMENTK_YR_ORD5A) & wave_id %in% as.character(c(1994,1989))  & (ALCEVER_CHECK == 0 | ALCREC_CHECK == 0)  ~ 5,
                                      TRUE ~ ALC_STATEMENTK_YR_ORD5A),
  ALC_STATEMENTL_YR_ORD5A = case_when(is.na(ALC_STATEMENTL_YR_ORD5A) & wave_id %in% as.character(c(1994,1989))  & (ALCEVER_CHECK == 0 | ALCREC_CHECK == 0)  ~ 5,
                                      TRUE ~ ALC_STATEMENTL_YR_ORD5A),
  ALC_STATEMENTM_YR_ORD5A = case_when(is.na(ALC_STATEMENTM_YR_ORD5A) & wave_id %in% as.character(c(1994,1989))  & (ALCEVER_CHECK == 0 | ALCREC_CHECK == 0)  ~ 5,
                                      TRUE ~ ALC_STATEMENTM_YR_ORD5A),
  ALC_STATEMENTN_YR_ORD5A = case_when(is.na(ALC_STATEMENTN_YR_ORD5A) & wave_id %in% as.character(c(1994,1989))  & (ALCEVER_CHECK == 0 | ALCREC_CHECK == 0)  ~ 5,
                                      TRUE ~ ALC_STATEMENTN_YR_ORD5A),
  ALC_STATEMENTU_YR_ORD5A = case_when(is.na(ALC_STATEMENTU_YR_ORD5A) & wave_id %in% as.character(c(1994,1989))  & (ALCEVER_CHECK == 0 | ALCREC_CHECK == 0)  ~ 5,
                                      TRUE ~ ALC_STATEMENTU_YR_ORD5A),
  ALC_STATEMENTO_YR_ORD5A = case_when(is.na(ALC_STATEMENTO_YR_ORD5A) & wave_id %in% as.character(c(1994,1989))  & (ALCEVER_CHECK == 0 | ALCREC_CHECK == 0)  ~ 5,
                                      TRUE ~ ALC_STATEMENTO_YR_ORD5A),
  ALC_STATEMENTP_YR_ORD5A = case_when(is.na(ALC_STATEMENTP_YR_ORD5A) & wave_id %in% as.character(c(1994,1989))  & (ALCEVER_CHECK == 0 | ALCREC_CHECK == 0)  ~ 5,
                                      TRUE ~ ALC_STATEMENTP_YR_ORD5A),
  ALC_STATEMENTQ_YR_ORD5A = case_when(is.na(ALC_STATEMENTQ_YR_ORD5A) & wave_id %in% as.character(c(1994,1989))  & (ALCEVER_CHECK == 0 | ALCREC_CHECK == 0)  ~ 5,
                                      TRUE ~ ALC_STATEMENTQ_YR_ORD5A),
  ALC_STATEMENTR_YR_ORD5A = case_when(is.na(ALC_STATEMENTR_YR_ORD5A) & wave_id %in% as.character(c(1994,1989))  & (ALCEVER_CHECK == 0 | ALCREC_CHECK == 0)  ~ 5,
                                      TRUE ~ ALC_STATEMENTR_YR_ORD5A),
  ALC_STATEMENTS_YR_ORD5A = case_when(is.na(ALC_STATEMENTS_YR_ORD5A) & wave_id %in% as.character(c(1994,1989))  & (ALCEVER_CHECK == 0 | ALCREC_CHECK == 0)  ~ 5,
                                      TRUE ~ ALC_STATEMENTS_YR_ORD5A),
  ALC_STATEMENTT_YR_ORD5A = case_when(is.na(ALC_STATEMENTT_YR_ORD5A) & wave_id %in% as.character(c(1994,1989))  & (ALCEVER_CHECK == 0 | ALCREC_CHECK == 0)  ~ 5,
                                      TRUE ~ ALC_STATEMENTT_YR_ORD5A))



# SMOKING
# Dependencies (refer to codebook)
main <- main %>% mutate(
  CIGS_DAY_OEA = case_when(is.na(CIGS_DAY_OEA) & wave_id %in% as.character(c(1992, 1994, 1998, 2008, 2010, 2012, 2014, 2018))  & (SMKEVER_CHECK == 0 | SMKEVER_CHECK2 != 1 | SMKEVER_CHECK3 != 1)  ~ 0,
                           TRUE ~ CIGS_DAY_OEA))


# DRUGS
# Dependencies (refer to codebook)
main <- main %>% mutate(
  CANNAB_MO_ORD7B = case_when(is.na(CANNAB_MO_ORD7B) & wave_id == "1988" &  (!CANNABREC_CHECK %in% c(1,2) | CANNABEVER_CHECK == 0 ) ~ 0,
                              is.na(CANNAB_MO_ORD7B) & wave_id == "1984" &  (CANNABREC_CHECK2 != 84 | CANNABEVER_CHECK == 0 ) ~ 0,
                              TRUE ~ CANNAB_MO_ORD7B),
  COCAINE_MO_ORD7A = case_when(is.na(COCAINE_MO_ORD7A) & wave_id %in% c("1988")  &  (!COCREC_CHECK %in% c(1,2) | COCEVER_CHECK == 0 ) ~ 0,
                               is.na(COCAINE_MO_ORD7A) & wave_id %in% c("1984")  &  (!COCREC_CHECK %in% c(1) | COCEVER_CHECK == 0) ~ 0,
                               TRUE ~ COCAINE_MO_ORD7A))



## BEHAVIOUR: CHOICE_INCOME_*CUT ## 
# Categorise responses from CHOICE_CIUT_*CUT vars.
# see RAND HRS Longitudinal File 2018 8 (V1) Documentation; p.1505) 
# https://hrsdata.isr.umich.edu/sites/default/files/documentation/other/1615843861/randhrs1992_2018v1.pdf

# 4. Least risk averse 
# 3. 3rd most risk averse 
# 2. 2nd most risk averse 
# 1. Most risk averse

main <- main %>%  mutate(INCOME_CUT_RISKB = case_when(
  
  # accepts job with 50% chance of 50% cut in income
  CHOICE_INCOME_33CUT == 1  & is.na(CHOICE_INCOME_20CUT) & CHOICE_INCOME_50CUT == 1 ~ 4,
  
  # accepts job with 50% chance of 33% cut in income
  CHOICE_INCOME_33CUT == 1  & is.na(CHOICE_INCOME_20CUT) & CHOICE_INCOME_50CUT == 0 ~ 3,
  
  # accepts job with 50% chance of 20% cut in income
  CHOICE_INCOME_33CUT == 0  & CHOICE_INCOME_20CUT == 1 & is.na(CHOICE_INCOME_50CUT) ~ 2,
  
  # accepts only safe job 
  CHOICE_INCOME_33CUT == 0  & CHOICE_INCOME_20CUT == 0 & is.na(CHOICE_INCOME_50CUT) ~ 1))



#________________FIX VARS: INVERSE CODING ___________________#

# Define a function to apply to each column
inv_code <- function(column) {
  6 - column
}

# Apply the modification to columns matching the pattern
main <- main %>%
  mutate_at(vars(matches("ALC_STATEMENT*")), inv_code)




#________________FIX VARS: DATES ___________________#
# If month & date are missing replace with the mean of available dates.
# If date information is completely missing from the raw data AND cannot be estimated from panel documentation, use 15th of June 

# dates of interview are not available, therefore we construct a date based on the wave year & month of june
main <- main %>% mutate(date = as.Date(paste(wave_id,"06","15",sep="-"), "%Y-%m-%d"))
main <- main %>% group_by(wave_id) %>% mutate(wave_year = round(mean(year(date)))) %>% ungroup() # calculate the general year for that wave

#________________FIX YOB & AGE & AGE RANGE___________________#


# calculate age (some ages are negative because the ids are listed for all waves, for those entries all data is NA)
# NA

# only keep respondents aged between 10 & 90y.o.
main <- main %>%  filter(age %in% c(10:90)) 

#________________FIX VARS: GENDER___________________#

# missing gender information cannot be retrieved from previous waves

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
  distinct(id, non_unique_gender) %>%
  summarise(perc_gender = 100*(sum(non_unique_gender)/n()),
            perc_yob = NA) %>% 
  mutate(panel = panel_name)

# CHECK VARS --------------------------------------------------------------

#________________ CHECK VARS: FILTER OUT RESPONDENTS WITH MISSING DEMS___________________#

main <- main %>% 
  filter(!is.na(age)) %>% #keep rows that have age info
  filter(!is.na(gender)) %>%  #keep rows that have gender info
  filter(non_unique_gender == 0) # filter out respondents with not the same gender reported across the waves 


#________________ CHECK VARS: DATA VISUAL (I)___________________#

# distribution of responses:
# summarytools::view(by(main, main$wave_id, dfSummary))

#________________ CHECK VARS: MEASURE QUAL. CRITERIA ___________________#
# exclude measures if:
# - in none of waves there is at least 4 different values possible for a measure (i.e., limited range of responses)

dependency_vars <- c(colnames(main)[grepl("CHOICE", colnames(main))], # vars of yes/no choices in lottery
                     colnames(main)[grepl("_CHECK", colnames(main))]) # vars of yes/no choices in lottery

vars_to_keep <- main  %>% 
  pivot_longer(!c(id, wave_id, wave_year, date, gender, age, non_unique_gender), values_to = "response", names_to = "var_name") %>% 
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
risk_info_analyse <- risk_info %>% bind_rows(newvars_info) %>% distinct(panel, varcode, measure_category, general_domain, domain_name,
                                                                        scale_type,scale_length, time_frame, behav_type, behav_paid, item_num) %>% 
  mutate(var_consider = if_else(varcode %in% dependency_vars, 0, 1), # considering non-dependency vars (or binary vars) for calculating retest correlations
         var_include = if_else(varcode %in% vars_to_keep$var_name, 1,0)) # include or exclude from calculating retest correlations
# dim(risk_info_analyse)  56 rows x  13 cols


# CREATE CLEAN/PROCESSED DATAFRAME  --------------------------------------------------------------

#________________SELECT VARS OF INTEREST FOR RETEST CALC___________________#

proc_data <- main %>% 
  # keep necessary vars (i.e., vars_to_keep and exclude vars used to check for dependencies)
  select(id, age, gender, wave_id,wave_year,date, # dems
         all_of(vars_to_keep$var_name)) %>% #risk measures
  filter_at(all_of(vars_to_keep$var_name),any_vars(!is.na(.))) #keep rows that have at least one risk value

#________________FIX VARS: REMOVE DUPLICATE IDS ___________________#

#avoid IDs that within a wave are not unique
proc_data <- proc_data %>% 
  group_by(id, wave_id) %>% 
  mutate(id_count = n()) %>%
  ungroup() 

proc_data %>% group_by(wave_id) %>% summarise(sum(id_count > 1)) 
proc_data <- proc_data %>%  filter(id_count == 1) %>% select(-id_count)

# dim(proc_data)  162148 rows x  47 cols


# long format
proc_data_l <- pivot_longer(proc_data,all_of(vars_to_keep$var_name),names_to = "varcode", values_to = "response")
# dim(proc_data_l)  6648068 rows x  8 cols

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
