
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
panel_data_path <- c("~/Documents/TSRP/Data/ADDHEALTH/RawData/") # where the raw panel data is stored
preproc_data_path <- c("~/Documents/TSRP/Data/ADDHEALTH/ProcData/") # where to save processed panel data
panel_name <- "ADDHEALTH"

# VARIABLE INFORMATION ----------------------------------------------------

panel_var <- var_book[[panel_name]]

# combining id & risk information
all_info <- bind_rows(panel_var$Measures, panel_var$ID, panel_var$Dems, panel_var$Dates)
risk_info <- panel_var$Measures
# READING DATA -----------------------------------------------------
#________________READ MAIN DATA FILES___________________# 
# list of unique wave pairs
wave_list <- sort(unique(na.omit(all_info$wave_id))) 
data_list <- NULL
wave_num <- 1

for (CurrWave in wave_list) {
  
  # select wave relevant info
  sub_all_info <- all_info %>% filter(wave_id == CurrWave) 
  
  vars_of_interest <- sub_all_info$origin_varcode
  
  # open the relevant file
  file_of_interest <- list.files(path = panel_data_path, pattern = sub_all_info$varfile, recursive = TRUE) 
  wave_data <- read.spss(paste0(panel_data_path, file_of_interest), to.data.frame=TRUE, use.value.labels = FALSE, reencode = FALSE) 
  
  colnames(wave_data) <- toupper(colnames(wave_data)) 
  
  # select vars of interest
  wave_data <- wave_data %>% select(all_of(vars_of_interest)) %>%  mutate(wave_id = CurrWave)
  
  # replacing var codes
  colnames_found <- match(colnames(wave_data), vars_of_interest, nomatch = 0)
  colnames(wave_data)[colnames(wave_data) %in% vars_of_interest] <- sub_all_info$varcode[colnames_found]
  
  # place in list
  data_list[[wave_num]] <- wave_data
  wave_num <- wave_num +1
}  # wave year loop


#________________BIND DATA FROM DIFFERENT WAVES TOGETHER___________________# 
main <- bind_rows(data_list)
main <- main %>% mutate(id = as.numeric(id))
main <- main %>% mutate_if(is.integer, ~ as.numeric(.x))
main <- main %>%  select(sort(names(.))) # sort columns name order
# dim(main) 25530 rows x 76 cols


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

main <- main %>% mutate(YINT = if_else(YINT < 100, YINT + 1900, YINT))

# If month & date are missing replace with the mean of available dates.
# If date information is completely missing from the raw data AND cannot be estimated from panel documentation, use 15th of June 

main <- main %>% mutate(DINT = if_else(is.na(DINT), round(mean(DINT, na.rm = TRUE)), DINT),
                        MINT = if_else(is.na(MINT), round(mean(MINT, na.rm = TRUE)), MINT))
main <- main %>% mutate(date = as.Date(paste(YINT,MINT,DINT,sep="-"), "%Y-%m-%d")) 

main <- main %>% group_by(wave_id) %>% mutate(wave_year = round(mean(year(date))))  %>% ungroup() # calculate the general year for that wave


#________________FIX VARS: YOB, AGE & AGE RANGE___________________# 

# fix YINT and yob variable format
main <- main %>% mutate(yob = case_when(yob < 100 ~ yob + 1900,
                                        TRUE ~ yob))

# three obs missing yob information
main <- main %>% 
  group_by(id) %>% 
  fill(yob,.direction = 'downup') %>%  # NAs get replaced with neighboring yob for each respondent 
  ungroup() 

# use yob to calculate age if age is missing 
main <- main %>% 
  mutate(age = case_when(is.na(age) ~ year(date) - yob,
                         TRUE ~ age))

# identify respondents with not the same yob reported across the waves
# EXCEPTION: Quite a few cases (~40% of respondents) have YOBs that are slightly
#  different across waves (1-4 year difference), so as to avoid excluding too many cases,
#  here we exclude respondents were YOB across waves differs by more than one year.
# 
# 
main <- main %>%
  group_by(id) %>%
  mutate(non_unique_yob = max(yob)- min(yob) > 1) %>% 
  ungroup() 



# only keep respondents aged between 10 & 90y.o.
main <- main %>%  filter(age %in% c(10:90)) 


main <- main %>% select(-c(DINT, MINT, YINT, yob))


#________________FIX VARS: GENDER___________________#
# no gender information missing

# recode gender
main <- main %>%
  mutate(gender = case_when(gender == 2 ~ 1,
                            gender == 1 ~ 0))

#  identify respondents with not the same gender reported across the waves
main <- main %>%
  group_by(id) %>%
  mutate(non_unique_gender = length(unique(na.omit(gender))) > 1) %>%
  ungroup()


inconsistent_dems <- main %>% 
  distinct(id, non_unique_gender,non_unique_yob) %>%
  summarise(perc_gender = 100*(sum(non_unique_gender)/n()),
            perc_yob = 100*(sum(non_unique_yob)/n())) %>% 
  mutate(panel = panel_name)


#________________FIX VARS: REPLACING MISSING & NON READABLE RESPONSES___________________# 

## ORDINAL MEASURES

# based on ADDHEALTH codebook
add_na_ord4_5 <-  function(x) if_else(x > 5, NA_real_, x) # values of 6 and above == NA
add_na_ord7_8 <-  function(x) if_else(x > 7, NA_real_, x) # values of 8 and above == NA
add_na_dis31 <-  function(x) if_else(x > 30, NA_real_, x) # values of 31 and above == NA

ord_4_5_measures <- risk_info %>% filter(scale_type == "ord" & scale_length %in% c(4,5))
ord_7_8_measures <-  risk_info %>% filter(scale_type == "ord" & scale_length %in% c(7,8))
dis_31_measures <-  risk_info %>% filter(scale_type == "dis" & scale_length == 31)

main <- main %>%
  mutate_at(ord_4_5_measures$varcode, add_na_ord4_5) %>% 
  mutate_at(ord_7_8_measures$varcode, add_na_ord7_8) %>% 
  mutate_at(dis_31_measures$varcode, add_na_dis31) 

## OPEN MEASURES
# risk_info %>% filter(scale_type == "oe" & is.na(check_var)) %>% distinct(varcode)

main <- main %>% 
  mutate(ALC_DRINKS_PERTIME_OEA = case_when(ALC_DRINKS_PERTIME_OEA > 95 ~ NA_real_, 
                                            TRUE ~ ALC_DRINKS_PERTIME_OEA),
         ALC_DRINKS_PERTIME_OEB = case_when(ALC_DRINKS_PERTIME_OEB > 95 ~ NA_real_, 
                                            TRUE ~ ALC_DRINKS_PERTIME_OEB),
         SEX_PRTNR_NUM_OEA = case_when(SEX_PRTNR_NUM_OEA > 95 ~ NA_real_, 
                                            TRUE ~ SEX_PRTNR_NUM_OEA),
         CANNAB_MO_OEA = case_when(CANNAB_MO_OEA > 995 & wave_id %in% c("W1", "W2") ~ NA_real_, 
                                   CANNAB_MO_OEA > 9995 & wave_id %in% c("W3") ~ NA_real_, 
                                   TRUE ~ CANNAB_MO_OEA),
         CIGS_DAY_OEB = case_when(CIGS_DAY_OEB > 95 & wave_id %in% c("W1", "W2") ~ NA_real_, 
                                  CIGS_DAY_OEB > 995 & wave_id %in% c("W3", "W4", "W5") ~ NA_real_, 
                                  TRUE ~ CIGS_DAY_OEB),
         COCAINE_MO_OEA = case_when(COCAINE_MO_OEA > 995  ~ NA_real_, 
                                    TRUE ~ COCAINE_MO_OEA),
         DRUG_MO_OEA = case_when(DRUG_MO_OEA > 995  ~ NA_real_, 
                                 TRUE ~ DRUG_MO_OEA),
         FIGHT_PHYS_YR_OEA = case_when(FIGHT_PHYS_YR_OEA > 995 & wave_id %in% c("W1", "W2") ~ NA_real_, 
                                       FIGHT_PHYS_YR_OEA > 95 & wave_id %in% c("W3") ~ NA_real_, 
                                       TRUE ~ FIGHT_PHYS_YR_OEA),
         DRUG_NEEDLE_MO_OEA = case_when(DRUG_NEEDLE_MO_OEA > 995 & wave_id %in% c("W3") ~ NA_real_, 
                                        DRUG_NEEDLE_MO_OEA > 95 & wave_id %in% c("W4") ~ NA_real_, 
                                        TRUE ~ DRUG_NEEDLE_MO_OEA),
         INHALANT_MO_OEA = case_when(INHALANT_MO_OEA > 995  ~ NA_real_, 
                                     TRUE ~ INHALANT_MO_OEA))


#________________FIX VARS: RISK PROPENSITY___________________# 

#  reverse coding
main <- main %>%  mutate(RISK_GEN_ORD5A = 6 - RISK_GEN_ORD5A) # higher score --> more risk taking

#________________FIX VARS: SMOKING ___________________#

# taking into account missing answers because of previous answers (i.e., skipping questions)
main <- main %>%  
  
  # if people report not having smoked ever they get 0 cigars instead of NA
  mutate(CIGARS_MO_DIS31A = case_when(is.na(CIGARS_MO_DIS31A) & wave_id %in% c("W3","W4") & c(SMK_CHECK == 0 |SMK_CHECK3 == 0 |SMK_CHECK4 == 0|SMK_CHECK5 == 0) ~ 0, 
                                      TRUE ~ CIGARS_MO_DIS31A),
         
         CIGS_MO_DIS31A = case_when(is.na(CIGS_MO_DIS31A) & wave_id %in% c("W1","W2","W3","W4", "W5") & c(SMK_CHECK == 0 |SMK_CHECK2 == 0 |SMK_CHECK3 == 0|SMK_CHECK4 == 0) ~ 0, 
                                    TRUE ~ CIGS_MO_DIS31A),
         
         # if people report not having smoked ever they get 0 cigarettes instead of NA
         CIGS_DAY_OEB = case_when(is.na(CIGS_DAY_OEB)  & wave_id %in% c("W1","W2","W3","W4", "W5") & c(SMK_CHECK == 0 |SMK_CHECK2 == 0 |SMK_CHECK3 == 0|SMK_CHECK4 == 0 | CIGS_MO_DIS31A == 0) ~ 0, 
                                  TRUE ~ CIGS_DAY_OEB),
         
         # if people report not having chewed tobacco ever they get 0 days chewing tobacco instead of NA
         CHEWTOBACCO_MO_DIS31A = case_when(is.na(CHEWTOBACCO_MO_DIS31A) & wave_id == "W4" & CHEWTOBACCO_CHECK == 0  ~ 0, 
                                           TRUE ~ CHEWTOBACCO_MO_DIS31A))

#________________FIX VARS:  DRINKING ALCOHOL ___________________#

# harmonizing variables and reverse coding
main <- main %>%  mutate(ALC_5DRINKS_YR_ORD7A = case_when(wave_id %in% c("W1","W2") & !is.na(ALC_5DRINKS_YR_ORD7A) ~ 7 - ALC_5DRINKS_YR_ORD7A,
                                                          TRUE ~ ALC_5DRINKS_YR_ORD7A),
                         ALC_YR_ORD7A = case_when(wave_id %in% c("W1","W2") & !is.na(ALC_YR_ORD7A) ~ 7 - ALC_YR_ORD7A,
                                                  TRUE ~ ALC_YR_ORD7A),
                         ALC_DRUNK_YR_ORD7A = case_when(wave_id %in% c("W1","W2") & !is.na(ALC_DRUNK_YR_ORD7A) ~ 7 - ALC_DRUNK_YR_ORD7A,
                                                        TRUE ~ ALC_DRUNK_YR_ORD7A))


# taking into account missing answers because of previous answers (i.e., skipping questions)
main <- main %>% 
  # if people report not having ever drunk in the last year they get never/0 instead of NA
  mutate(ALC_YR_ORD7A = case_when(is.na(ALC_YR_ORD7A) & wave_id %in% c("W1","W2","W3","W4") & ALC_CHECK == 0 ~ 0, 
                                  TRUE ~ ALC_YR_ORD7A),
         ALC_MO_ORD7A = case_when(is.na(ALC_MO_ORD7A) & wave_id %in% c("W4", "W5") & c(ALC_CHECK == 0| ALC_YR_ORD7A == 0) ~ 0, 
                                  TRUE ~ ALC_MO_ORD7A),
         ALC_DATING_YR_ORD5A = case_when(is.na(ALC_DATING_YR_ORD5A) & wave_id %in% c("W1","W2","W3") & c(ALC_CHECK == 0| ALC_YR_ORD7A == 0) ~ 0, 
                                         TRUE ~ ALC_DATING_YR_ORD5A),
         ALC_DRINKS_PERTIME_OEA = case_when(is.na(ALC_DRINKS_PERTIME_OEA) & wave_id %in% c("W1","W2","W3","W4") & c(ALC_CHECK == 0| ALC_YR_ORD7A == 0) ~ 0, 
                                            TRUE ~ ALC_DRINKS_PERTIME_OEA),
         ALC_DRINKS_PERTIME_OEB = case_when(is.na(ALC_DRINKS_PERTIME_OEB) & wave_id %in% c("W4", "W5") & c(ALC_CHECK == 0| ALC_YR_ORD7A == 0| ALC_MO_ORD7A == 0) ~ 0, 
                                            TRUE ~ ALC_DRINKS_PERTIME_OEB),
         ALC_DRUNK_YR_ORD7A = case_when(is.na(ALC_DRUNK_YR_ORD7A) & wave_id %in% c("W1","W2","W3","W4") & c(ALC_CHECK == 0| ALC_YR_ORD7A == 0) ~ 0, 
                                        TRUE ~ ALC_DRUNK_YR_ORD7A),
         ALC_5DRINKS_YR_ORD7A = case_when(is.na(ALC_5DRINKS_YR_ORD7A) & wave_id %in% c("W1","W2","W3","W4", "W5") & c(ALC_CHECK == 0| ALC_YR_ORD7A == 0) ~ 0, 
                                          TRUE ~ ALC_5DRINKS_YR_ORD7A),
         ALC_FIGHT_YR_ORD5A = case_when(is.na(ALC_FIGHT_YR_ORD5A) & wave_id %in% c("W1","W2","W3") & c(ALC_CHECK == 0| ALC_YR_ORD7A == 0) ~ 0, 
                                        TRUE ~ ALC_FIGHT_YR_ORD5A),
         ALC_FRIENDS_YR_ORD5A = case_when(is.na(ALC_FRIENDS_YR_ORD5A) & wave_id %in% c("W1","W2","W3") & c(ALC_CHECK == 0| ALC_YR_ORD7A == 0) ~ 0, 
                                          TRUE ~ ALC_FRIENDS_YR_ORD5A),
         ALC_HUNGOVER_YR_ORD5A = case_when(is.na(ALC_HUNGOVER_YR_ORD5A) & wave_id %in% c("W1","W2","W3") & c(ALC_CHECK == 0| ALC_YR_ORD7A == 0) ~ 0, 
                                           TRUE ~ ALC_HUNGOVER_YR_ORD5A),
         ALC_REGRET_YR_ORD5A = case_when(is.na(ALC_REGRET_YR_ORD5A) & wave_id %in% c("W1","W2") & c(ALC_CHECK == 0| ALC_YR_ORD7A == 0) ~ 0, 
                                         TRUE ~ ALC_REGRET_YR_ORD5A),
         ALC_SCHOOL_YR_ORD5A = case_when(is.na(ALC_SCHOOL_YR_ORD5A) & wave_id %in% c("W1","W2","W3") & c(ALC_CHECK == 0| ALC_YR_ORD7A == 0) ~ 0, 
                                         TRUE ~ ALC_SCHOOL_YR_ORD5A),
         ALC_SEX_YR_ORD5A  = case_when(is.na(ALC_SEX_YR_ORD5A) & wave_id %in% c("W1","W2","W3") & c(ALC_CHECK == 0| ALC_YR_ORD7A == 0) ~ 0, 
                                       TRUE ~ ALC_SEX_YR_ORD5A),
         ALC_SICK_YR_ORD5A  = case_when(is.na(ALC_SICK_YR_ORD5A) & wave_id %in% c("W1","W2","W3") & c(ALC_CHECK == 0| ALC_YR_ORD7A == 0) ~ 0, 
                                        TRUE ~ ALC_SICK_YR_ORD5A),
         ALC_TRB_PARENTS_YR_ORD5A  = case_when(is.na(ALC_TRB_PARENTS_YR_ORD5A) & wave_id %in% c("W1","W2") & c(ALC_CHECK == 0| ALC_YR_ORD7A == 0) ~ 0,
                                               TRUE ~ ALC_TRB_PARENTS_YR_ORD5A),
         DRIVE_ALC_MO_ORD5A = case_when(is.na(DRIVE_ALC_MO_ORD5A) & wave_id == "W2" & c(ALC_CHECK == 0| ALC_YR_ORD7A == 0 |DRIVE_ALC_CHECK == 0) ~ 0,
                                        TRUE ~ DRIVE_ALC_MO_ORD5A))

#________________FIX VARS:  DRUGS ___________________#

# taking into account missing answers because of previous answers (i.e., skipping questions)
# if people report not having ever tried that drug they get 0  instead of NA
main <- main %>%  mutate(DRUG_MO_OEA = case_when(is.na(DRUG_MO_OEA) & wave_id %in% c("W1","W2","W3") & DRUG_CHECK == 0 ~ 0, 
                                                 DRUG_MO_OEA > 101 ~100, # cap values at 100
                                                 TRUE ~ DRUG_MO_OEA),
                         COCAINE_MO_OEA = case_when(is.na(COCAINE_MO_OEA) & wave_id %in% c("W1","W2","W3") & c(COCAINE_CHECK == 0| COCAINE_CHECK == 0) ~ 0, 
                                                    COCAINE_MO_OEA > 101 ~100, # cap values at 100
                                                    TRUE ~ COCAINE_MO_OEA),
                         INHALANT_MO_OEA = case_when(is.na(INHALANT_MO_OEA) & wave_id %in% c("W1","W2") & INHALANT_CHECK == 0 ~ 0, 
                                                     INHALANT_MO_OEA > 101 ~100, # cap values at 100
                                                     TRUE ~ INHALANT_MO_OEA),
                         DRUG_NEEDLE_MO_OEA = case_when(is.na(DRUG_NEEDLE_MO_OEA) & wave_id %in% c("W3","W4") & c(DRUG_CHECK2 == 0| DRUG_CHECK3 == 0) ~ 0, 
                                                        TRUE ~ DRUG_NEEDLE_MO_OEA),
                         CANNAB_MO_ORD7A = case_when(is.na(CANNAB_MO_ORD7A) & wave_id %in% c("W4", "W5") & c(CANNAB_CHECK == 0| CANNAB_CHECK2 == 0| CANNAB_CHECK3 == 0) ~ 0, 
                                                     TRUE ~ CANNAB_MO_ORD7A),
                         CANNAB_MO_OEA = case_when(is.na(CANNAB_MO_OEA) & wave_id %in% c("W1","W2","W3") & c(CANNAB_CHECK == 0| CANNAB_CHECK2 == 0) ~ 0, 
                                                   CANNAB_MO_OEA > 301 ~ 300, # cap values at 300
                                                   TRUE ~ CANNAB_MO_OEA))

#________________FIX VARS:  AGGRESSIVE / DELIQUENCY ___________________#

# taking into account missing answers because of previous answers (i.e., skipping questions)
# if people report not having having fought or having a gun/weapon they get 0  instead of NA
main <- main %>%  mutate(INJURE_YR_ORD4A = case_when(is.na(INJURE_YR_ORD4A) & wave_id %in% c("W2","W4")& FIGHT_PHYS_YR_ORD4A == 0 ~ 0, 
                                                     TRUE ~ INJURE_YR_ORD4A),
                         WEAPON_SCHOOL_MO_ORD5A = case_when(is.na(WEAPON_SCHOOL_MO_ORD5A) & wave_id == "W2" & WEAPON_CHECK == 0 ~ 0, 
                                                            TRUE ~ WEAPON_SCHOOL_MO_ORD5A),
                         FIGHT_WEAPON_YR_ORD4A = case_when(is.na(FIGHT_WEAPON_YR_ORD4A) & wave_id == "W2" & FIGHT_PHYS_YR_ORD4A == 0  ~ 0, 
                                                           TRUE ~ FIGHT_WEAPON_YR_ORD4A),
                         FIGHT_PHYS_YR_OEA = case_when(is.na(FIGHT_PHYS_YR_OEA) & wave_id == "W1" & FIGHT_CHECK == 7  ~ 0, 
                                                       is.na(FIGHT_PHYS_YR_OEA) & wave_id == "W2" & FIGHT_PHYS_YR_ORD4A == 0  ~ 0, 
                                                       TRUE ~ FIGHT_PHYS_YR_OEA))


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
  mutate(var_consider = if_else(varcode %in% dependency_vars, 0, 1), # considering non-dependency vars (or binary vars) for calculating retest correlations
         var_include = if_else(varcode %in% vars_to_keep$var_name, 1,0)) # include or exclude from calculating retest correlations
# dim(risk_info_analyse) 68 rows x 13 cols



# CREATE CLEAN/PROCESSED DATAFRAME  --------------------------------------------------------------

#________________SELECT VARS OF INTEREST FOR RETEST CALC___________________#

proc_data <- main %>% 
  # keep necessary vars (i.e., vars_to_keep and exclude vars used to check for dependencies)
  select(id, age, gender, wave_id, wave_year,date, # dems
         all_of(vars_to_keep$var_name)) %>% #risk measures
  filter_at(all_of(vars_to_keep$var_name),any_vars(!is.na(.))) #keep rows that have at least one risk value
# dim(proc_data) 25385 rows x 55 cols 


# long format
proc_data_l <- pivot_longer(proc_data,all_of(vars_to_keep$var_name),names_to = "varcode", values_to = "response")
# dim(proc_data_l) 1243865 rows x 8 cols

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

