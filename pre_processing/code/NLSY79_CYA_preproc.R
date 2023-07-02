
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
panel_data_path <- c("") # where the raw panel data is stored
preproc_data_path <- c("") # where to save processed panel data
panel_name <- "NLSY79_CYA"

# VARIABLE INFORMATION ----------------------------------------------------

panel_var <- var_book[[panel_name]]
# combining id & risk information
all_info <- bind_rows(panel_var$Measures, panel_var$ID, panel_var$Dems)
risk_info <- panel_var$Measures
newvars_info <- panel_var$NewVars

# READING DATA: CHILD SELF-REPORT  -----------------------------------------------------

# child self-report
csr_info <- all_info %>% filter(grepl("_C",varfile))
file_of_interest_name <- unique(na.omit(csr_info$varfile)) # in which file are the relevant vars stored
file_of_interest_name <- paste0(file_of_interest_name,".csv") # csv file
file_of_interest <- list.files(path = panel_data_path, pattern = file_of_interest_name, full.names = TRUE, recursive = TRUE)
# open file
csr_data <-read_csv(file_of_interest)

#________________READ MAIN DATA FILES___________________# 

# list of unique waves
wave_list <- unique(na.omit(csr_info$wave_id)) 
data_list <- NULL
wave_num <- 1

for (CurrWave in wave_list) {
  
  # select wave relevant info
  sub_all_info <- csr_info %>% filter(wave_id == CurrWave)
  
  # select vars of interest
  vars_of_interest <- sub_all_info$origin_varcode
  wave_data <- csr_data %>% select(all_of(vars_of_interest)) %>%  mutate(wave_id = as.character(CurrWave))
  
  # replacing var codes
  colnames_found <- match(colnames(wave_data), vars_of_interest, nomatch = 0)
  colnames(wave_data)[colnames(wave_data) %in% vars_of_interest] <- sub_all_info$varcode[colnames_found]
  
  # place in list
  data_list[[wave_num]] <- wave_data
  wave_num <- wave_num + 1
}


#________________BIND DATA FROM DIFFERENT WAVES TOGETHER___________________# 

main_csr <- bind_rows(data_list)
main_csr <- main_csr %>% mutate_if(is.integer, ~ as.numeric(.x))
# dim(main_csr) 161714 rows x 50 cols

# FIX VARS : CHILD SELF-REPORT  --------------------------------------------------------------------

#________________REPLACING MISSING & NON READABLE RESPONSES___________________#
main_csr <- main_csr %>% 
  replace(.< 0 , NA_real_ ) # any vars with values below 0 (i.e., invalid, DK, proxy, missing,inapplicable....) are replaced  


#________________FIX VARS: RISK PREF. MEASURES___________________#

# ALCOHOL
# Dependencies (refer to codebook)
main_csr <- main_csr %>% mutate(
  ALC_MO_ORD6A = case_when(is.na(ALC_DRINKS_DAY_OEC) & wave_id == "1994" & ALCRECENT_CHECK == 5 ~ 0,
                           is.na(ALC_DRINKS_DAY_OEC) & wave_id == "1996" & (ALCRECENT_CHECK == 7|ALCAGE_CHECK == 95|ALCEVER_CHECK == 0) ~ 0,
                           is.na(ALC_DRINKS_DAY_OEC) & wave_id %in% c("1998", "2000") & (ALCEVER_CHECK == 0 | ALCRECENT_CHECK == 0|ALCAGE_CHECK == 95) ~ 0,
                           TRUE ~ ALC_MO_ORD6A),
  ALC_DRINKS_DAY_OEC = case_when(is.na(ALC_DRINKS_DAY_OEC) & wave_id == "1994" & (ALCRECENT_CHECK == 5|ALC_MO_ORD6A == 0) ~ 0,
                                 is.na(ALC_DRINKS_DAY_OEC) & wave_id == "1996" & (ALCRECENT_CHECK == 7|ALC_MO_ORD6A == 0|ALCAGE_CHECK == 95|ALCEVER_CHECK == 0) ~ 0,
                                 is.na(ALC_DRINKS_DAY_OEC) & wave_id %in% c("1998","2000") & (ALCEVER_CHECK == 0 | ALCRECENT_CHECK == 0 | ALC_MO_ORD6A == 0 |ALCAGE_CHECK == 95) ~ 0,
                                 is.na(ALC_DRINKS_DAY_OEC) & wave_id %in% as.character(seq(2002,2014, 2)) & (ALCEVER_CHECK == 0 | ALCRECENT_CHECK != 1) ~ 0,
                                 ALC_DRINKS_DAY_OEC == 95 ~ 0,
                                 ALC_DRINKS_DAY_OEC > 95 ~ NA_real_,
                                 TRUE ~ ALC_DRINKS_DAY_OEC))

# SMOKING
# Dependencies (refer to codebook)
main_csr <- main_csr %>% mutate(
  CIGS_MO_ORD6A = case_when(is.na(CIGS_MO_ORD6A) & wave_id == "1994" & SMKRECENT_CHECK == 5 ~ 0,
                            is.na(CIGS_MO_ORD6A) & wave_id == "1996" & (SMKRECENT_CHECK == 7| SMKAGE_CHECK == 95| SMKCOUNT_CHECK == 7) ~ 0,
                            is.na(CIGS_MO_ORD6A) & wave_id %in% c("1998", "2000") & (SMKRECENT_CHECK == 0|SMKEVER_CHECK == 0| SMKAGE_CHECK == 95| SMKCOUNT_CHECK == 7) ~ 0,
                            TRUE ~ CIGS_MO_ORD6A),
  
  CIGS_DAY_OEB = case_when(is.na(CIGS_DAY_OEB) & wave_id == "1994" & (SMKRECENT_CHECK == 5|CIGS_MO_ORD6A == 0) ~ 0,
                           is.na(CIGS_DAY_OEB) & wave_id == "1996" & (SMKRECENT_CHECK == 7|CIGS_MO_ORD6A == 0|SMKEVER_CHECK == 0| SMKAGE_CHECK == 95| SMKCOUNT_CHECK == 7) ~ 0,
                           is.na(CIGS_DAY_OEB) & wave_id %in% c("1998","2000") & (SMKRECENT_CHECK == 0|CIGS_MO_ORD6A == 0|SMKEVER_CHECK == 0| SMKAGE_CHECK == 95| SMKCOUNT_CHECK == 7) ~ 0,
                           is.na(CIGS_DAY_OEB) & wave_id %in% as.character(seq(2002,2014, 2)) & (SMKEVER_CHECK == 0 | SMKRECENT_CHECK != 1) ~ 0,
                           CIGS_DAY_OEB == 95 ~ 0,
                           CIGS_DAY_OEB > 95 ~ NA_real_,
                           TRUE ~ CIGS_DAY_OEB))


# DRUGS
# Dependencies (refer to codebook)
main_csr <- main_csr %>% mutate(
  CANNAB_MO_ORD6A = case_when(is.na(CANNAB_MO_ORD6A) & wave_id == "1994" & CANNABREC_CHECK == 5 ~ 0,
                              is.na(CANNAB_MO_ORD6A) & wave_id == "1996" & (CANNABREC_CHECK == 7| CANNABAGE_CHECK == 95| CANNABCNT_CHECK == 7) ~ 0,
                              is.na(CANNAB_MO_ORD6A) & wave_id %in% c("1998", "2000") & (CANNABREC_CHECK == 0|CANNABEVER_CHECK == 0| CANNABAGE_CHECK == 95| CANNABCNT_CHECK == 0) ~ 0,
                              TRUE ~ CANNAB_MO_ORD6A),
  
  SUBST_SNIFF_MO_ORD6A = case_when(is.na(SUBST_SNIFF_MO_ORD6A) & wave_id == "1994" & SNIFFREC_CHECK == 5 ~ 0,
                                   is.na(SUBST_SNIFF_MO_ORD6A) & wave_id == "1996" & (SNIFFREC_CHECK == 7| SNIFFAGE_CHECK == 95| SNIFFCNT_CHECK == 7) ~ 0,
                                   is.na(SUBST_SNIFF_MO_ORD6A) & wave_id %in% c("1998", "2000") & (SNIFFREC_CHECK == 0|SNIFFEVER_CHECK == 0| SNIFFAGE_CHECK == 95| SNIFFCNT_CHECK == 0) ~ 0,
                                   TRUE ~ SUBST_SNIFF_MO_ORD6A),
  
  HALLU_MO_ORD6A = case_when(is.na(HALLU_MO_ORD6A) & wave_id %in% c("1998", "2000") & (HALLURECENT_CHECK == 0|HALLUEVER_CHECK == 0| HALLUAGE_CHECK == 95| HALLUCOUNT_CHECK == 0) ~ 0,
                             TRUE ~ HALLU_MO_ORD6A),
  
  COCAINE_MO_ORD6A = case_when(is.na(COCAINE_MO_ORD6A) & wave_id %in% c("1998", "2000") & (COCRECENT_CHECK == 0|COCEVER_CHECK == 0| COCAGE_CHECK == 95| COCCNT_CHECK == 0) ~ 0,
                               TRUE ~ COCAINE_MO_ORD6A),
  
  DOWNER_UPPER_MO_ORD6A = case_when(is.na(DOWNER_UPPER_MO_ORD6A) & wave_id %in% c("1998", "2000") & (DOWNREC_CHECK == 0|DOWNEVER_CHECK == 0| DOWNAGE_CHECK == 95| DOWNCOUNT_CHECK == 0) ~ 0,
                                    TRUE ~ DOWNER_UPPER_MO_ORD6A))

# fix + standardize scale with young adult questions
main_csr <- main_csr %>% mutate(
  ALC_DRUNK_YR_ORD4A = if_else(ALC_DRUNK_YR_ORD4A > 3, NA, ALC_DRUNK_YR_ORD4A + 1),
  DAMAGE_SCHL_YR_ORD4A = if_else(DAMAGE_SCHL_YR_ORD4A > 3, NA, DAMAGE_SCHL_YR_ORD4A + 1),
  INJURE_YR_ORD4B = if_else(INJURE_YR_ORD4B > 3, NA, INJURE_YR_ORD4B + 1),
  SKIP_SCHL_YR_ORD4A = if_else(SKIP_SCHL_YR_ORD4A > 3, NA, SKIP_SCHL_YR_ORD4A + 1),
  LIE_PARENTS_YR_ORD4B = if_else(LIE_PARENTS_YR_ORD4B > 3, NA, LIE_PARENTS_YR_ORD4B + 1),
  STAY_LATE_YR_ORD4A = if_else(STAY_LATE_YR_ORD4A > 3, NA, STAY_LATE_YR_ORD4A + 1),
  STAY_LATE_YR_ORD4B = if_else(STAY_LATE_YR_ORD4B > 3, NA, STAY_LATE_YR_ORD4B + 1),
  TROUBLE_SCHL_YR_ORD4A = if_else(TROUBLE_SCHL_YR_ORD4A > 3, NA, TROUBLE_SCHL_YR_ORD4A + 1),
  STEAL_STORE_YR_ORD4B = if_else(STEAL_STORE_YR_ORD4B > 3, NA, STEAL_STORE_YR_ORD4B + 1),
  RISK_GEN_ORD4B = if_else(RISK_GEN_ORD4B > 4, NA, 5 - RISK_GEN_ORD4B))



# READING DATA: YOUNG ADULT SELF-REPORT  -----------------------------------------------------

# ya self-report
yasr_info <- all_info %>% filter(grepl("_YA",varfile))
file_of_interest_name <- unique(na.omit(yasr_info$varfile)) # in which file are the relevant vars stored
file_of_interest_name <- paste0(file_of_interest_name,".csv") # csv file
file_of_interest <- list.files(path = panel_data_path, pattern = file_of_interest_name, full.names = TRUE, recursive = TRUE)
# open file
yasr_data <-read_csv(file_of_interest)
#________________READ MAIN DATA FILES___________________# 

# list of unique waves
wave_list <- unique(na.omit(yasr_info$wave_id)) 
data_list <- NULL
wave_num <- 1

for (CurrWave in wave_list) {
  
  # select wave relevant info
  sub_all_info <- yasr_info %>% filter(wave_id == CurrWave)
  
  
  # select vars of interest
  vars_of_interest <- sub_all_info$origin_varcode 
  wave_data <- yasr_data %>% select(all_of(vars_of_interest)) %>%  mutate(wave_id = as.character(CurrWave))
  
  # replacing var codes
  colnames_found <- match(colnames(wave_data), vars_of_interest, nomatch = 0)
  colnames(wave_data)[colnames(wave_data) %in% vars_of_interest] <- sub_all_info$varcode[colnames_found]
  
  # place in list
  data_list[[wave_num]] <- wave_data
  wave_num <- wave_num + 1
}


#________________BIND DATA FROM DIFFERENT WAVES TOGETHER___________________# 

main_yasr <- bind_rows(data_list)
main_yasr <- main_yasr %>% mutate_if(is.integer, ~ as.numeric(.x))
# dim(main_yasr) 161714 rows x 64 cols

# FIX VARS : YOUNG ADULT SELF-REPORT  --------------------------------------------------------------------

#________________REPLACING MISSING & NON READABLE RESPONSES___________________#
main_yasr <- main_yasr %>% 
  replace(.< 0 , NA_real_ ) # any vars with values below 0 (i.e., invalid, DK, proxy, missing,inapplicable....) are replaced  


#________________FIX VARS: RISK PREF. MEASURES___________________#

# ALCOHOL
# Dependencies (refer to codebook)
main_yasr <- main_yasr %>% mutate(
  ALC_DRINKS_MO_DIS31A = case_when(is.na(ALC_DRINKS_MO_DIS31A) & wave_id %in% c("1994", "1996","1998") & (ALCREC_CHECK != 1 | ALCAGE_CHECK == 95) ~ 0,
                                   TRUE ~ ALC_DRINKS_MO_DIS31A),
  
  ALC_MAX_DAY_MO_OEA = case_when(is.na(ALC_MAX_DAY_MO_OEA) & wave_id %in% c("1994", "1996","1998") & (ALCREC_CHECK != 1 | ALC_DRINKS_MO_DIS31A == 95 | ALCAGE_CHECK == 95) ~ 0,
                                 TRUE ~ ALC_MAX_DAY_MO_OEA),
  
  ALC_5DRINKS_MO_DIS31A = case_when(is.na(ALC_5DRINKS_MO_DIS31A) & wave_id %in% c("1994", "1996","1998") & (ALCREC_CHECK != 1 | ALC_DRINKS_MO_DIS31A == 95 | ALCAGE_CHECK == 95) ~ 0,
                                    ALC_5DRINKS_MO_DIS31A > 30 ~ NA, # cap values at 30
                                    TRUE ~ ALC_5DRINKS_MO_DIS31A),
  
  ALC_YR_ORD9A = case_when(is.na(ALC_YR_ORD9A) & wave_id %in% c("1994", "1996","1998") & (ALCREC_CHECK %in% c(4,5) | ALCAGE_CHECK == 95) ~ 1,
                           is.na(ALC_YR_ORD9A) & wave_id %in% as.character(seq(2000,2020, 2)) & ALCAGE_CHECK == 95  ~ 1,
                           TRUE ~ ALC_YR_ORD9A),
  
  ALC_DRINKS_DAY_OEC = case_when(is.na(ALC_DRINKS_DAY_OEC) & wave_id %in% c("1994", "1996","1998") & (ALCREC_CHECK != 1 | ALC_DRINKS_MO_DIS31A == 95 | ALCAGE_CHECK == 95) ~ 0,
                                 is.na(ALC_DRINKS_DAY_OEC) & wave_id %in% as.character(seq(2000,2020, 2)) & (ALCAGE_CHECK == 95 | ALC_YR_ORD9A == 1) ~ 0,
                                 TRUE ~ ALC_DRINKS_DAY_OEC)) %>% 
  
  mutate(ALC_DRINKS_MO_DIS31A = case_when(ALC_DRINKS_MO_DIS31A == 95 ~ 0,
                                          ALC_DRINKS_MO_DIS31A %in% c(31:94) ~ NA, # cap values at 30
                                          TRUE ~ ALC_DRINKS_MO_DIS31A))

# SMOKING
# Dependencies (refer to codebook)
main_yasr <- main_yasr %>% mutate(
  CIGS_MO_ORD6A = case_when(is.na(CIGS_MO_ORD6A) & wave_id == "1994" & SMKCOUNT_CHECK == 0 ~ 0,
                            wave_id == "1994" & CIGS_MO_ORD6A == 6 ~ 0,
                            is.na(CIGS_MO_ORD6A) & wave_id == "1996" & (SMKREC_CHECK == 7| SMKCOUNT_CHECK == 7) ~ 0,
                            is.na(CIGS_MO_ORD6A) & wave_id == "1998" & (SMKREC_CHECK == 7|SMKEVER_CHECK == 0|SMKCOUNT_CHECK == 7) ~ 0,
                            is.na(CIGS_MO_ORD6A) & wave_id %in% as.character(seq(2000,2020, 2)) & (SMKEVER_CHECK == 0 | SMKREC_CHECK != 1) ~ 0,
                            TRUE ~ CIGS_MO_ORD6A), 
  
  CIGS_DAY_OEB = case_when(is.na(CIGS_DAY_OEB) & wave_id == "1994" & (SMKCOUNT_CHECK == 0 | CIGS_MO_ORD6A == 0) ~ 0,
                           is.na(CIGS_DAY_OEB) & wave_id == "1996" & (SMKREC_CHECK == 7 | SMKCOUNT_CHECK == 7 | CIGS_MO_ORD6A == 0) ~ 0,
                           is.na(CIGS_DAY_OEB) & wave_id == "1998" & (SMKREC_CHECK == 7|SMKEVER_CHECK == 0|SMKCOUNT_CHECK == 7 | CIGS_MO_ORD6A == 0) ~ 0,
                           is.na(CIGS_DAY_OEB) & wave_id %in% as.character(seq(2000,2020, 2)) & (SMKEVER_CHECK == 0 | SMKREC_CHECK != 1 | CIGS_MO_ORD6A == 0) ~ 0,
                           TRUE ~ CIGS_DAY_OEB))

# SEX
# Dependencies (refer to codebook)
main_yasr <- main_yasr %>% mutate(
  SEX_PRTNR_YR_ORD5A = case_when(is.na(SEX_PRTNR_YR_ORD5A) & (wave_id %in% as.character(seq(1994,2020, 2))) & (SEX_CHECK == 0 ) ~ 0,
                                 SEX_PRTNR_YR_ORD5A > 4 ~ NA,
                                 TRUE ~ SEX_PRTNR_YR_ORD5A))



# DRUGS
# Dependencies (refer to codebook)
main_yasr <- main_yasr %>% mutate(
  
  DRUG_OTH_MO_ORD6A = case_when(is.na(DRUG_OTH_MO_ORD6A) & wave_id %in% as.character(seq(2000,2016, 2)) & (DRUGSOTH_CHECK2 == 0 |DRUGSOTH_CHECK == 0 ) ~ 0,
                                TRUE ~ DRUG_OTH_MO_ORD6A),
  
  
  DOWNER_MO_ORD6A = case_when(is.na(DOWNER_MO_ORD6A) & wave_id == "1998" & (DOWNEVER_CHECK == 0 | DOWNREC_CHECK == 7 | DOWNCNT_CHECK == 0) ~ 0,
                              is.na(DOWNER_MO_ORD6A) & wave_id %in% as.character(seq(2000,2016, 2)) & (DOWNEVER_CHECK == 0 |DRUGSOTH_CHECK == 0 ) ~ 0,
                              TRUE ~ DOWNER_MO_ORD6A), 
  
  
  HALLU_MO_ORD6A = case_when(is.na(HALLU_MO_ORD6A) & wave_id == "1998" & (HALLUEVER_CHECK == 0 | HALLUREC_CHECK == 7 | HALLUCNT_CHECK == 0) ~ 0,
                             is.na(HALLU_MO_ORD6A) & wave_id %in% as.character(seq(2000,2016, 2)) & (HALLUEVER_CHECK == 0 |DRUGSOTH_CHECK == 0 ) ~ 0,
                             TRUE ~ HALLU_MO_ORD6A), 
  
  COCAINE_MO_ORD6A = case_when(is.na(COCAINE_MO_ORD6A) & wave_id == "1994" & COCCNT_CHECK == 0 ~ 0,
                               is.na(COCAINE_MO_ORD6A) & wave_id == "1996" & (COCREC_CHECK == 7 | COCCNT_CHECK == 7) ~ 0,
                               is.na(COCAINE_MO_ORD6A) & wave_id == "1998" & (COCEVER_CHECK == 0 | COCREC_CHECK == 7 | COCCNT_CHECK == 0) ~ 0,
                               is.na(COCAINE_MO_ORD6A) & wave_id %in% as.character(seq(2000,2016, 2)) & (COCEVER_CHECK == 0 |DRUGSOTH_CHECK == 0 ) ~ 0,
                               TRUE ~ COCAINE_MO_ORD6A), 
  
  
  COCAINE_CRACK_MO_ORD6A = case_when(is.na(COCAINE_CRACK_MO_ORD6A) & wave_id == "1994" & CRACKCNT_CHECK == 0 ~ 0,
                                     is.na(COCAINE_CRACK_MO_ORD6A) & wave_id == "1996" & (CRACKREC_CHECK == 7 | CRACKCNT_CHECK == 7) ~ 0,
                                     is.na(COCAINE_CRACK_MO_ORD6A) & wave_id == "1998" & (CRACKEVER_CHECK == 0 | CRACKREC_CHECK == 7 | CRACKCNT_CHECK == 0) ~ 0,
                                     is.na(COCAINE_CRACK_MO_ORD6A) & wave_id %in% as.character(seq(2000,2016, 2)) & (CRACKEVER_CHECK == 0 |DRUGSOTH_CHECK == 0 ) ~ 0,
                                     TRUE ~ COCAINE_CRACK_MO_ORD6A), 
  
  
  SUBST_SNIFF_MO_ORD6A = case_when(is.na(SUBST_SNIFF_MO_ORD6A) & wave_id == "1994" & SNIFFCNT_CHECK == 0 ~ 0,
                                   is.na(SUBST_SNIFF_MO_ORD6A) & wave_id == "1996" & (SNIFFREC_CHECK == 7 | SNIFFCNT_CHECK == 7) ~ 0,
                                   is.na(SUBST_SNIFF_MO_ORD6A) & wave_id == "1998" & (SNIFFEVER_CHECK == 0 | SNIFFREC_CHECK == 7 | SNIFFCNT_CHECK == 0) ~ 0,
                                   is.na(SUBST_SNIFF_MO_ORD6A) & wave_id %in% as.character(seq(2000,2016, 2)) & (SNIFFEVER_CHECK == 0 |DRUGSOTH_CHECK == 0 ) ~ 0,
                                   TRUE ~ SUBST_SNIFF_MO_ORD6A), 
  
  DRUG_WORK_YR_ORD7A = case_when(is.na(DRUG_WORK_YR_ORD7A) & wave_id %in% as.character(seq(2000,2016, 2)) & (UPPEREVER_CHECK == 0 | UPPERREC_CHECK %in% c(5:7)) ~ 0,
                                 TRUE ~ DRUG_WORK_YR_ORD7A), 
  
  CANNAB_MO_ORD6A = case_when(is.na(CANNAB_MO_ORD6A) & wave_id == "1994" & CANNABCNT_CHECK == 0 ~ 0,
                              is.na(CANNAB_MO_ORD6A) & wave_id == "1996" & (CANNABREC_CHECK == 7 | CANNABCNT_CHECK == 7) ~ 0,
                              is.na(CANNAB_MO_ORD6A) & wave_id == "1998" & (CANNABEVER_CHECK == 0 |CANNABREC_CHECK == 7 | CANNABCNT_CHECK == 7) ~ 0,
                              is.na(CANNAB_MO_ORD6A) & wave_id %in% as.character(seq(2000,2020, 2)) & (CANNABEVER_CHECK == 0| CANNABREC_CHECK != 1) ~ 0,
                              TRUE ~ CANNAB_MO_ORD6A),
  
  CANNAB_WORK_YR_ORD7A =  case_when(is.na(CANNAB_WORK_YR_ORD7A) & wave_id %in% as.character(seq(2000,2020, 2)) & CANNABEVER_CHECK == 0 ~ 0,
                                    TRUE ~ CANNAB_WORK_YR_ORD7A),
  
  DRG_HIGH_YR_ORD7A =  case_when(is.na(DRG_HIGH_YR_ORD7A) & wave_id %in% as.character(seq(2018,2020, 2)) & DRUGSOTH_CHECK == 0 ~ 0,
                                 TRUE ~ DRG_HIGH_YR_ORD7A))

## BEHAVIOUR: CHOICE_INCOME_*CUT ## 
# Categorise responses from CHOICE_CIUT_*CUT vars.
# see RAND HRS Longitudinal File 2018 8 (V1) Documentation; p.1505) 
# https://hrsdata.isr.umich.edu/sites/default/files/documentation/other/1615843861/randhrs1992_2018v1.pdf

# 4. Least risk averse 
# 3. 3rd most risk averse 
# 2. 2nd most risk averse 
# 1. Most risk averse

main_yasr <- main_yasr %>%  mutate(INCOME_CUT_RISK = case_when(
  
  # accepts job with 50% chance of 50% cut in income
  CHOICE_INCOME_33CUT == 2  & is.na(CHOICE_INCOME_20CUT) & CHOICE_INCOME_50CUT == 2 ~ 4,
  
  # accepts job with 50% chance of 33% cut in income
  CHOICE_INCOME_33CUT == 2  & is.na(CHOICE_INCOME_20CUT) & CHOICE_INCOME_50CUT == 1 ~ 3,
  
  # accepts job with 50% chance of 20% cut in income
  CHOICE_INCOME_33CUT == 1  & CHOICE_INCOME_20CUT == 2 & is.na(CHOICE_INCOME_50CUT) ~ 2,
  
  # accepts only safe job 
  CHOICE_INCOME_33CUT == 1  & CHOICE_INCOME_20CUT == 1 & is.na(CHOICE_INCOME_50CUT) ~ 1))



# fix + standardize scale
main_yasr <- main_yasr %>% mutate(
  ALC_DRUNK_YR_ORD4A = if_else(ALC_DRUNK_YR_ORD4A > 4, NA, ALC_DRUNK_YR_ORD4A),
  DAMAGE_SCHL_YR_ORD4A = if_else(DAMAGE_SCHL_YR_ORD4A > 4, NA, DAMAGE_SCHL_YR_ORD4A),
  INJURE_YR_ORD4B = if_else(INJURE_YR_ORD4B > 4, NA, INJURE_YR_ORD4B),
  SKIP_SCHL_YR_ORD4A = if_else(SKIP_SCHL_YR_ORD4A > 4, NA, SKIP_SCHL_YR_ORD4A),
  LIE_PARENTS_YR_ORD4B = if_else(LIE_PARENTS_YR_ORD4B > 4, NA, LIE_PARENTS_YR_ORD4B),
  STAY_LATE_YR_ORD4A = if_else(STAY_LATE_YR_ORD4A > 4, NA, STAY_LATE_YR_ORD4A),
  STAY_LATE_YR_ORD4B = if_else(STAY_LATE_YR_ORD4B > 4, NA, STAY_LATE_YR_ORD4B),
  TROUBLE_SCHL_YR_ORD4A = if_else(TROUBLE_SCHL_YR_ORD4A > 4, NA, TROUBLE_SCHL_YR_ORD4A),
  STEAL_STORE_YR_ORD4B = if_else(STEAL_STORE_YR_ORD4B > 4, NA, STEAL_STORE_YR_ORD4B))



# CREATE MAIN DF WITH YOUTH, INDRESP AND HHRESP FILES  -----------------------------------------------------------------

main <- bind_rows(main_csr, main_yasr)

# FIX VARS: COMBINED FILE -------------------------------------------------


#________________FIX VARS: DATES ___________________#
# If month & date are missing replace with the mean of available dates.
# If date information is completely missing from the raw data AND cannot be estimated from panel documentation, use 15th of June 

# dates of interview are not available, therefore we construct a date based on the wave year & month of june
main <- main %>% mutate(date = as.Date(paste(wave_id,"06","15",sep="-"), "%Y-%m-%d"))
main <- main %>% group_by(wave_id) %>% mutate(wave_year = round(mean(year(date)))) %>% ungroup() # calculate the general year for that wave

#________________FIX YOB & AGE & AGE RANGE___________________#

# no missing age information

# identify respondents with not the same yob reported across the waves
main <- main %>%
  group_by(id) %>%
  mutate(non_unique_yob = length(unique(na.omit(yob))) > 1) %>% # 
  ungroup() 

# calculate age (some ages are negative because the ids are listed for all waves, for those entries all data is NA)
main <- main %>% mutate( age = year(date) - yob) %>% select(-yob)

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
  distinct(id, non_unique_gender, non_unique_yob) %>%
  summarise(perc_gender = 100*(sum(non_unique_gender)/n()),
            perc_yob = 100*(sum(non_unique_yob)/n())) %>% 
  mutate(panel = panel_name)

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

dependency_vars <- c(colnames(main)[grepl("CHOICE", colnames(main))], # vars of yes/no choices in lottery
                     colnames(main)[grepl("_CHECK", colnames(main))]) # vars of yes/no choices in lottery

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
risk_info_analyse <- risk_info %>% bind_rows(newvars_info) %>% distinct(panel, varcode, measure_category, general_domain, domain_name,
                                                                        scale_type,scale_length, time_frame, behav_type, behav_paid) %>% 
  mutate(var_consider = if_else(varcode %in% dependency_vars, 0, 1), # considering non-dependency vars (or binary vars) for calculating retest correlations
         var_include = if_else(varcode %in% vars_to_keep$var_name, 1,0)) # include or exclude from calculating retest correlations
# dim(risk_info_analyse)  94 rows x  12 cols


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

# dim(proc_data)  77005 rows x  37 cols


# long format
proc_data_l <- pivot_longer(proc_data,all_of(vars_to_keep$var_name),names_to = "varcode", values_to = "response")
# dim(proc_data_l)  2387155 rows x  8 cols

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
