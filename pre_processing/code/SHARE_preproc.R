
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
library(foreign)


# PATH/FILES  -----------------------------------------------------

var_book <- read_rds("var_info/panel_variable_info.rds")# panel risk measures
measure_info_path <- c("var_info/indv_panel_var_info/") 
panel_data_path <- c("") # where the raw panel data is stored
preproc_data_path <- c("") # where to save processed panel data
panel_name <- "SHARE"

# VARIABLE INFORMATION ----------------------------------------------------

panel_var <- var_book[[panel_name]]

# combining id & risk information
all_info <- bind_rows(panel_var$Measures, panel_var$ID, panel_var$Dems, panel_var$Dates)

risk_info <-panel_var$Measures

# READING DATA -----------------------------------------------------

#________________READ MAIN DATA FILES___________________# 
# list of unique wave pairs
wave_list <- sort(unique(na.omit(all_info$wave_id))) 
survey_data <- NULL
wave_num <- 1
for (CurrWave in wave_list) {
  data_list <- NULL
  
  sub_all_info <- all_info %>% filter(wave_id == CurrWave)
  
  file_list <- sort(unique(sub_all_info$varfile))
  
  file_num <- 1
  for (CurrFile in file_list) {
    # select file relevant info
    file_info <- sub_all_info %>% filter(varfile == CurrFile)
    
    vars_of_interest <- file_info$origin_varcode # vars of interest
    
    # open file 
    file_path <- list.files(path = panel_data_path, pattern = CurrFile, recursive = T)
    file_path <- paste0(panel_data_path,file_path)
    data <- read.spss(file_path,to.data.frame=TRUE, use.value.labels = F, reencode= F) 
    
    # select vars of interest
    data <- data %>% select(all_of(vars_of_interest)) 
    
    # replacing var codes
    colnames_found <- match(colnames(data), vars_of_interest, nomatch = 0)
    colnames(data)[colnames(data) %in% vars_of_interest] <- file_info$varcode[colnames_found]
    
    
    # place in list
    data_list[[file_num]] <- data
    file_num <- file_num + 1
  } # file loop
  
  # join variables from different files together
  wave_data <- reduce(data_list, full_join, by = "id") 
  
  survey_data[[wave_num]] <- wave_data %>%  mutate(wave_id = CurrWave)
  wave_num <- wave_num + 1
} # year loop

#________________BIND DATA FROM DIFFERENT WAVES TOGETHER___________________# 

main <- bind_rows(survey_data)
main <- main %>% mutate_if(is.integer, ~ as.numeric(.x))
# dim(main) 583542 rows x 21 cols


# FIX VARS  --------------------------------------------------------------------

#________________FIX VARS: REMOVE DUPLICATE IDS ___________________#

#avoid IDs that within a wave are not unique
main <- main %>% 
  group_by(id, wave_id) %>% 
  mutate(id_count = n()) %>%
  ungroup() 

main %>% group_by(wave_id) %>% summarise(sum(id_count > 1)) 
main <- main %>%  filter(id_count == 1) %>% select(-id_count)


#________________REPLACING MISSING & NON READABLE RESPONSES___________________#
main <- main %>% 
  replace(.< 0 , NA_real_ ) # any vars with values below 0 (i.e., invalid, DK, proxy, missing,inapplicable....) are replaced  

#________________FIX VARS: DATES  ___________________#
# If month & date are missing replace with the mean of available dates.
# If date information is completely missing from the raw data AND cannot be estimated from panel documentation, use 15th of June 

# adding by default 15th as the day of the month
main <- main %>% mutate(date = as.Date(paste(YINT,MINT,"15",sep="-"), "%Y-%m-%d")) %>% select(-c(YINT,MINT))
# many dates missing but data in all other columns is also missing

# calculate the general year for that wave
main <- main %>% group_by(wave_id) %>% mutate(wave_year = round(mean(year(date), na.rm = TRUE))) %>% ungroup()


#________________FIX YOB & AGE & AGE RANGE___________________#
# quite a lot of cases (~200'000) with missing age, retrieve by creating a proxy yob based on the
# avaialble age at a certain wave
main <- main %>% 
  mutate(temp_yob = year(date) - age)


main <- main %>% 
  group_by(id) %>% 
  fill(temp_yob,.direction = 'downup') %>%  # NAs get replaced with neighboring yob for each respondent 
  ungroup() %>% 
  mutate(age = if_else(is.na(age), year(date) - temp_yob, age)) %>% 
  filter(!is.na(age))


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

# for this panel only keep respondents aged between 50 & 90y.o. 
main <- main %>%  filter(age %in% c(50:90)) %>% select(-temp_yob)

#________________FIX VARS: GENDER___________________#

# no gender information missing
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
  distinct(id, non_unique_gender, age_diff_check) %>%
  summarise(perc_gender = 100*(sum(non_unique_gender)/n()),
            perc_age =  100*(sum(age_diff_check)/n())) %>% 
  mutate(panel = panel_name)



#________________FIX VARS: COUNTRY___________________#
# change id to country name (used labels from https://www.share-datadocutool.org/data_variables/view/45399)
country_id_name <- data.frame(
  stringsAsFactors = FALSE,
  country_id = c(11L,12L,13L,14L,15L,16L,
                 17L,18L,19L,20L,23L,25L,28L,29L,30L,31L,32L,33L,
                 34L,35L,47L,48L,51L,53L,55L,57L,59L,61L,63L),
  country = c("Austria","Germany","Sweden",
              "Netherlands","Spain","Italy","France","Denmark",
              "Greece","Switzerland","Belgium","Israel",
              "Czech_Rep","Poland","Ireland","Luxembourg","Hungary",
              "Portugal","Slovenia","Estonia","Croatia","Lithuania",
              "Bulgaria","Cyprus","Finland","Latvia","Malta","Romania",
              "Slovakia")
)

main <- main %>% left_join(country_id_name, by = "country_id") %>% select(-country_id)

#________________FIX VARS: RISK PREF. MEASURES___________________#

## FREQ SMOKING  ##
# Dependencies
# SMKEVER_CHECK has not many NAs. All SMKNOW_CHECK NAs are for people who SMKEVER_CHECK == NO (i.e., never smoked) so NAs in SMKNOW_CHECK are
#  set to 5 (i.e., not currently smoking). CIGS_DAY_OEA is set to 0 (instead of NA) for those who do not currently smoke.
#   Allows to have more available data and correctly compute retest correlations

main <- main %>% mutate(SMKNOW_CHECK = case_when(SMKEVER_CHECK == 5 & wave_id %in% c("W1", "W2", "W6", "W7", "W8") ~ 5, 
                                                 TRUE ~ SMKNOW_CHECK),
                        
                        SMK_CIGS_CHECK = case_when(SMKNOW_CHECK == 5 & wave_id %in% c("W1", "W2", "W6", "W7", "W8") ~ 0, # there can be values if people used to smoke (i.e., br001 = 1 and br002 = 5 ), but we only consider what is currently being smoked daily)
                                                   SMKNOW_CHECK == 1 & wave_id %in% c("W1", "W2", "W6", "W7", "W8") ~ SMK_CIGS_CHECK,
                                                   is.na(SMKNOW_CHECK) ~ NA_real_),
                        
                        CIGS_DAY_OEA = case_when(SMKNOW_CHECK == 5 & wave_id %in% c("W1", "W2", "W6", "W7", "W8") ~ 0,  # there can be values if people used to smoke (i.e., br001 = 1 and br002 = 5 ), but we only consider # of cigarettes currently being smoked daily) 
                                                 SMKNOW_CHECK == 1 & wave_id %in% c("W1", "W2", "W6", "W7", "W8") & SMK_CIGS_CHECK == 1  ~ CIGS_DAY_OEA, # smokes + smoking cigarettes
                                                 SMKNOW_CHECK == 1 & wave_id %in% c("W1", "W2", "W6", "W7", "W8") & SMK_CIGS_CHECK == 0  ~ 0,  # smokes + not smoking cigarettes
                                                 c(is.na(SMKNOW_CHECK) | is.na(SMK_CIGS_CHECK))  ~ NA_real_),
                        CIGARS_DAY_OEA = case_when(SMKNOW_CHECK == 5 & wave_id %in% c("W1", "W2") ~ 0,  # there can be values if people used to smoke (i.e., br001 = 1 and br002 = 5 ), but we only consider # of cigarettes currently being smoked daily) 
                                                 SMKNOW_CHECK == 1 & wave_id %in% c("W1", "W2") & SMK_CIGARS_CHECK == 1  ~ CIGS_DAY_OEA, # smokes + smoking cigarettes
                                                 SMKNOW_CHECK == 1 & wave_id %in% c("W1", "W2") & SMK_CIGARS_CHECK == 0  ~ 0,  # smokes + not smoking cigars
                                                 c(is.na(SMKNOW_CHECK) | is.na(SMK_CIGARS_CHECK))  ~ NA_real_),
                        PIPES_DAY_OEA = case_when(SMKNOW_CHECK == 5 & wave_id %in% c("W1", "W2") ~ 0,  # there can be values if people used to smoke (i.e., br001 = 1 and br002 = 5 ), but we only consider # of cigarettes currently being smoked daily) 
                                                   SMKNOW_CHECK == 1 & wave_id %in% c("W1", "W2") & SMK_PIPES_CHECK == 1  ~ CIGS_DAY_OEA, # smokes + smoking cigarettes
                                                   SMKNOW_CHECK == 1 & wave_id %in% c("W1", "W2") & SMK_PIPES_CHECK == 0  ~ 0,  # smokes + not smoking pipes
                                                   c(is.na(SMKNOW_CHECK) | is.na(SMK_PIPES_CHECK))  ~ NA_real_),
                        ) # no checks

## FREQ ALCOHOL ##
# Dependencies
# if ALC_CHECK == 5 and/or ALC_3MO_ORD7A == 7 some responses are set to NA, we set these to 0 or "did not drink" instead
main <-  main %>% mutate(ALC_UNITS_WK_OEA = case_when(ALC_CHECK == 5 & wave_id %in% c("W6", "W7", "W8") ~ 0,  
                                                      TRUE ~ ALC_UNITS_WK_OEA),
                         
                         ALC_6DRINKS_3MO_ORD7A = case_when(ALC_3MO_ORD7A == 7 & wave_id %in% c("W4","W5") ~ 7,  
                                                           TRUE ~ ALC_6DRINKS_3MO_ORD7A),
                         
                         ALC_DRINKS_DAY_OEA = case_when(ALC_3MO_ORD7A == 7 & wave_id %in% c("W2","W4","W5") ~ 0,  
                                                       TRUE ~ ALC_DRINKS_DAY_OEA))


# reverse scoring
main <-  main %>% mutate(ALC_3MO_ORD7A = 8- ALC_3MO_ORD7A,
                         ALC_6DRINKS_3MO_ORD7A = 8-ALC_6DRINKS_3MO_ORD7A)


## FIN RISK PROPENSITY ##
# reverse scoring
main <-  main %>% mutate(RISK_FIN_ORD4A = 5-RISK_FIN_ORD4A)

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

dependency_vars <- unique(na.omit(risk_info$varcode[risk_info$check_var == 1])) # vars to check for dependencies

vars_to_keep <- main  %>% 
  pivot_longer(!c(id, wave_id, country, wave_year, date, gender, age, non_unique_gender, age_diff_check), values_to = "response", names_to = "var_name") %>% 
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
  mutate(var_consider = if_else(varcode %in% dependency_vars, 0,1), # considering non-dependency vars (or binary vars) for calculating retest correlations
         var_include = if_else(varcode %in% vars_to_keep$var_name, 1,0)) # include or exclude from calculating retest correlations

# dim(risk_info_analyse)  14 rows x  12 cols

# CREATE CLEAN/PROCESSED DATAFRAME  --------------------------------------------------------------

#________________SELECT VARS OF INTEREST FOR RETEST CALC___________________#

proc_data <- main %>% 
  # keep necessary vars (i.e., vars_to_keep and exclude vars used to check for dependencies)
  select(id, age, gender, country, wave_id,wave_year,date, # dems
         all_of(vars_to_keep$var_name)) %>% #risk measures
  filter_at(all_of(vars_to_keep$var_name),any_vars(!is.na(.))) #keep rows that have at least one risk value
# dim(proc_data) 309692 rows x 15 cols

# long format
proc_data_l <- pivot_longer(proc_data,all_of(vars_to_keep$var_name),names_to = "varcode", values_to = "response")
# dim(proc_data_l) 2477536 rows x 9 cols

# checking if all id-gender combinations are unique
# proc_data_l %>%  distinct(id, gender) %>% summarise(check = n() == length(unique(proc_data$id)))


#________________ REMOVE COUNTRIES WITH LESS THAN TWO WAVES OF PROPENSITY MEASURES___________________#

countries_keep <- proc_data_l %>% 
  filter(varcode == "RISK_FIN_ORD4A") %>% 
  distinct(country,wave_id) %>% 
  group_by(country) %>% 
  summarise(num = n()) %>% 
  filter(num > 1)


proc_data_l <- proc_data_l %>% filter(country %in% countries_keep$country)
# dim(proc_data_l) 2408040 rows x 9 cols
proc_data <- proc_data %>% filter(country %in% countries_keep$country)
# dim(proc_data) 301005 rows x 15 cols


#________________ CHECK VARS: DATA VISUAL (II)___________________#

# distribution of responses:
# summarytools::view(by(proc_data, proc_data$wave_id, dfSummary))


# ADD PANEL INFORMATION ---------------------------------------------------


proc_data_l <- proc_data_l %>% 
  left_join(panel_var$PanelInfo, by = "country") %>% 
  relocate(panel, .before = id)

# checking if all id-gender combinations are unique
# proc_data_l %>%  distinct(id, gender) %>% summarise(check = n() == length(unique(proc_data$id)))
# checking if all id-varcode-wave combinations are unique
# t <- proc_data_l %>%  group_by(id, wave_id, varcode) %>% summarise(check = n(), .groups = "drop"); sum(t$check > 1) == 0



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
