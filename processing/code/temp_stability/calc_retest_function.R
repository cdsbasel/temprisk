# FUNCTION DESCRIPTION -------------------------------------------------------------

# Function that reads (processed/clean) panel data (in long format) and computes for all time intervals the test-retest correlations of different measures for different groups of respondents. 
# Correlations are only computed if there is at least 10 data points in the dataframe, the retest interval does not exceed 20 years, and their is variance in the responses at both time points.

# It takes as INPUT:
# panel = name of the panel (string; e.g., "SOEP")
# data = clean/processed panel data (dataframe; in LONG format; output of PANEL_preproc.R scripts)
# varcodes =  varcodes of the measures for which the retest correlations have to be calculated (character vector, must contain at least ONE string element; e.g., c("VAR_A", "VAR_B"))
# wave_ids = ordered waves (character vector, must contain at least TWO string elements; e.g., c("W1", "W2", "W3"))
# age_group = age group information (a data frame generated from the age_group.R script. Contains four columns and a row for each age group. first col = min age; second col = max age; third col = age group label; fourth col = age bin category)

# The OUTPUT of the function:
# 1) retest = data frame containing retest correlations for each age group, measure and time interval. Retest correlations are calculated for all respondents as well as for females and males separately
# The dataframe contains the following columns:
# panel <- panel name
# wave_id_t1 <-  wave id of survey at T1 
# wave_year_t1 <-  wave year of survey at T1 
# wave_id_t2 <-  wave id of survey at T2 
# wave_year_t2 <-  wave year of survey at T2
# time_diff_mean <- mean of time difference between interview dates (in years)
# time_diff_median <-  median of time difference between interview dates (in years)
# time_diff_min <-  min. time difference between interview dates (in years)
# time_diff_max <-  max. time difference between interview dates (in years)
# time_diff_sd <-  sd of time difference between interview dates (in years)
# year_age_group <- XYZ-year age group category (e.g., 5(year age groups))
# age_group <-  age group 
# age_mean <- mean age of respondents at T1
# age_median <-  median age of respondents at T1
# age_min <-  min. age of respondents at T1
# age_max <-  max. age of respondents at T1
# age_sd <-  sd of age of respondents at T1
# gender_group <-  all, female only, male only respondents
# prop_female <-  prop. of female respondents
# n <-  num of respondents with complete observations at T1  & T2
# attrition_rate <-  proportion of respondents with data at T1 but not T2
# varcode <- code/name of the variable
# cor_pearson <- pearson correlation coefficient
# cor_spearman <- spearman correlation coefficient
# icc2_1 <- Intra-class correlation coefficient
# cor_pearson_log <- pearson correlation coefficient w log-transformed data
# cor_spearman_log <- spearman correlation coefficient w log-transformed data
# icc2_1_log <- Intra-class correlation coefficient w log-transformed data
# coeff_var_t1 <-  coefficient of variation at T1
# coeff_var_t2 <- coefficient of variation at T2
# skewness_t1 <- skewness of responses at T1
# skewness_t2 <-  skewness of responses at T2


# 2) retest_w_id = same as retest, but for each correlation test it saves the id list of the respondents whose data was used.

# Certain packages need to be installed first


# Author(s): Alexandra Bagaini & Rui Mata, Centre for Cognitive and Decision Sciences, Faculty of Psychology, University of Basel.


# PACKAGES ---------------------------------------------------------------

library(tidyverse)
library(irr) # icc 2,1 calculation 
library(moments) # skewness calculation

# FUNCTION ----------------------------------------------------------------

calc_retest<-function(panel,data,varcodes,wave_ids, age_group) {
  
  #_________________BASIC STRUCTURES _________________________#
  
  retest <- NULL 
  retest_w_id <- NULL  
  
  #_________________SELECT: RISK MEASURE_________________________#
  
  for (CurrVarCode in varcodes) {
    
    
    #_________________SELECT: T1 DATA_________________________# 
    
    for (CurrWave1 in 1:(length(wave_ids)-1))  {
      wave_id_1 <- wave_ids[CurrWave1]
      
      data_t1 <- data %>%  # select T1 data
        filter(wave_id == wave_id_1) %>%     
        filter(varcode == CurrVarCode) %>% # select relevant varcode 
        rename(response_t1 = response,  # rename vars (except id and gender)
               age_t1 = age,
               date_t1 = date,
               wave_id_t1 = wave_id,
               wave_year_t1 = wave_year)
      
      #_________________SELECT: T2 DATA_________________________# 
      
      for (CurrWave2 in (CurrWave1+1):length(wave_ids)) {
        wave_id_2 <- wave_ids[CurrWave2]
        
        data_t2 <- data %>%  # select T2 data
          filter(wave_id == wave_id_2) %>%     
          filter(varcode == CurrVarCode) %>% # select relevant varcode 
          rename(response_t2 = response,  # rename vars (except id and gender)
                 age_t2 = age,
                 date_t2 = date,
                 wave_id_t2 = wave_id,
                 wave_year_t2 = wave_year)
        
        #_________________MERGE: T1 + T2 DATA_________________________#
        
        
        fullset <- left_join(data_t1,data_t2,by=c("id", "gender")) # merge two waves by "id" & "gender"
        
        #_________________SELECT: AGE_________________________# 
        
        for (CurrAgeGroup in age_group$group) {
          
          agerange <- age_group %>% 
            filter(group == CurrAgeGroup)   # select age group
          
          # filter based on age at T1
          set <- fullset %>% 
            filter(age_t1 %in% c(agerange$min:agerange$max))
          
          #_________________SELECT: GENDER_________________________# 
          
          for (CurrGender in c(0:2)) {
            
            # 0 == Male; 1 = Female; 2 = Male + Female
            
            if (CurrGender != 2) {
              
              # filter based on gender & complete cases 
              subset <- set %>% 
                filter(gender == CurrGender)
              
              resp_num_t1 <- sum(!is.na(subset$response_t1))
              
              # filter based on complete cases   
              subset <- subset %>%
                filter(complete.cases(response_t1,response_t2))
              
              # calculate attrition rate
              attrition_rate <- 1 - (nrow(subset)/resp_num_t1)
              
              gender_group <- ifelse(CurrGender == 1, "female", "male")
              
            }
            
            
            if (CurrGender == 2) {
              
              # calculate attrition rate
              resp_num_t1 <- sum(!is.na(set$response_t1))
              
              
              # filter based on complete cases 
              subset <- set %>% 
                filter(complete.cases(response_t1,response_t2))
              
              # calculate attrition rate
              attrition_rate <- 1 - (nrow(subset)/resp_num_t1)
              
              gender_group <- "all"
              
            }
            
            
            #_________________CALC TIME DIFFERENCE _________________________# 
            time_diff_mean <- mean(as.Date(subset$date_t2)-as.Date(subset$date_t1))/365  # mean of time difference between interview dates
            sd_check <- sd(subset$response_t1) & sd(subset$response_t2) != 0 # check if sd of responses at T1 & T2 are not 0 
            
            
            # ONLY COMPUTE CORRELATIONS IF THERE IS DATA ON MORE THAN 10 RESPONDENTS + TIME INTERVAL IS WITHIN 20 YEARS + VARIANCE IN DATA 
            
            if (nrow(subset) >= 10 & time_diff_mean <= 20 & sd_check == 1 ) { 
              
              #_________________COMPUTE CORRELATION & STORE SAMPLE + WAVE INFO_________________________#
              
              
              # creating data frame to save relevant information
              sub_retest <- NULL
              
              # save relevant information
              sub_retest$panel <- panel  # panel name
              sub_retest$wave_id_t1 <- as.character(unique(subset$wave_id_t1))  #  wave id of survey at T1 
              sub_retest$wave_year_t1 <- unique(subset$wave_year_t1)  #  wave year of survey at T1 
              sub_retest$wave_id_t2 <- as.character(unique(subset$wave_id_t2)) #  wave id of survey at T2 
              sub_retest$wave_year_t2 <- unique(subset$wave_year_t2) #  wave year of survey at T2
              sub_retest$time_diff_mean <- as.numeric(mean(as.Date(subset$date_t2)-as.Date(subset$date_t1))/365)  # mean of time difference between interview dates
              sub_retest$time_diff_median <- as.numeric(median(as.Date(subset$date_t2)-as.Date(subset$date_t1))/365)  # median of time difference between interview dates
              sub_retest$time_diff_min <- as.numeric(min(as.Date(subset$date_t2)-as.Date(subset$date_t1))/365)  # min. time difference between interview dates
              sub_retest$time_diff_max <-as.numeric( max(as.Date(subset$date_t2)-as.Date(subset$date_t1))/365)  # max. time difference between interview dates
              sub_retest$time_diff_sd <- as.numeric(sd(as.Date(subset$date_t2)-as.Date(subset$date_t1))/365)  # sd of time difference between interview dates
              sub_retest$year_age_group <- unique(agerange$age_bin_categ) #  XYZ-year age group  
              sub_retest$age_group <- CurrAgeGroup    #  age group  
              sub_retest$age_mean <- mean(subset$age_t1) # mean age of respondents at T1
              sub_retest$age_median <- median(subset$age_t1) # median age of respondents at T1
              sub_retest$age_min <- min(subset$age_t1) # min. age of respondents at T1
              sub_retest$age_max <- max(subset$age_t1) # min. age of respondents at T1
              sub_retest$age_sd <- sd(subset$age_t1) # sd of age of respondents at T1
              sub_retest$gender_group <- gender_group  # all, female only, male only respondents
              sub_retest$prop_female <- sum(subset$gender==1)/nrow(subset) # prop. of female respondents
              sub_retest$n <- nrow(subset)  # num of respondents with complete observations
              sub_retest$attrition_rate <- attrition_rate # proportion of respondents with responses at T1 but not T2
              sub_retest$varcode <- CurrVarCode  # code name of the variable
              sub_retest$cor_pearson <- cor(subset$response_t1,subset$response_t2,method="pearson")  # pearson correlation coefficient
              sub_retest$cor_spearman <- cor(subset$response_t1,subset$response_t2,method="spearman")  # spearman correlation coefficient
              sub_retest$icc2_1 <- icc(data.frame(subset$response_t1,subset$response_t2), model = "twoway", type = "agreement", unit = "single")$value #ICC2_1
              sub_retest$cor_pearson_log <- cor(log(1+(subset$response_t1)),log(1+(subset$response_t2)), method="pearson")  # pearson correlation coefficient w log transformed data (adding 1 to avoid "0" values in the data)
              sub_retest$cor_spearman_log <- cor(log(1+(subset$response_t1)),log(1+(subset$response_t2)), method="spearman")  # spearman correlation coefficient w log transformed data (adding 1 to avoid "0" values in the data)
              sub_retest$icc2_1_log <- icc(data.frame(log(1+(subset$response_t1)),log(1+(subset$response_t2))), model = "twoway", type = "agreement", unit = "single")$value #ICC2_1 w log transformed data (adding 1 to avoid "0" values in the data)
              sub_retest$coeff_var_t1 <- sd(subset$response_t1)/mean(subset$response_t1) # calculate the coefficient of variation at T1
              sub_retest$coeff_var_t2 <- sd(subset$response_t2)/mean(subset$response_t2) # calculate the coefficient of variation at T2
              sub_retest$skewness_t1 <- skewness(subset$response_t1, na.rm = FALSE) # calculate skewness of responses at T1
              sub_retest$skewness_t2 <- skewness(subset$response_t2, na.rm = FALSE) # calculate skewness of responses at T2
              
              
              # converting NaN to NA 
              # sub_retest[sapply(sub_retest, is.nan)] <- NA
              
              
              #_________________STORE RETEST INFO_________________________#
              
              # save respondent + response ids (& attach panel name) in separate df
              sub_retest_w_id <- sub_retest 
              sub_retest_w_id$id_list <- list(paste0(panel, "_", tolower(as.character(subset$id))))
              sub_retest_var_resp_id_t1 <- paste0(panel, "_", tolower(as.character(subset$id)), "_", CurrVarCode, "_", subset$wave_id_t1)
              sub_retest_var_resp_id_t2 <- paste0(panel, "_", tolower(as.character(subset$id)), "_", CurrVarCode, "_", subset$wave_id_t2)
              sub_retest_w_id$resp_id_list <- list(c(sub_retest_var_resp_id_t1, sub_retest_var_resp_id_t2))
              
              # save output from each loop
              retest_w_id <- bind_rows(retest_w_id, sub_retest_w_id)
              retest <- bind_rows(retest, sub_retest)
              
              
            } # condition that there are enough responses and variance in dataset
            
          } # gender loop
        } # age loop 
      }# wave 2 loop
    } # wave 1 loop
  } # varcode list loop
  
  
  outcome <- list(retest,retest_w_id)
  return(outcome)
  
}  # end of function


