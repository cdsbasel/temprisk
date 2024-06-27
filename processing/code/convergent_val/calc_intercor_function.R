# FUNCTION DESCRIPTION -------------------------------------------------------------

# Function that reads (processed/clean) panel data (in long format) and computes for all waves of data the intercorrelation between different measures for different groups of respondents. 


# It takes as INPUT:
# panel = name of the panel (string; e.g., "SOEP")
# data = clean/processed panel data (dataframe; in LONG format; output of PANEL_preproc.R scripts)
# varcodes =  varcodes of pairs of measures for which the intercorrelations have to be calculated (character vector, must contain at least TWO string elements; e.g., c("VAR_A", "VAR_B"))
# wave_ids = ordered waves (character vector, must contain at least ONE string element; e.g., c("W1", "W2", "W3"))
# age_group = age group information (a data frame generated from the age_group.R script. Contains four columns and a row for each age group. first col = min age; second col = max age; third col = age group label; fourth col = age bin category)

# The OUTPUT of the function:
# 1) interor = data frame containing  intercorrelations for each age group, measure and time interval. Intercorrelations are calculated for all respondents as well as for females and males separately
# The dataframe contains the following columns:
# panel <- panel name
# wave_id <-  wave id of survey 
# wave_year <-  wave year of survey 
# year_age_group <- XYZ-year age group category (e.g., 5(year age groups))
# age_group <-  age group 
# age_mean <- mean age of respondents 
# age_median <-  median age of respondents 
# age_min <-  min. age of respondents 
# age_max <-  max. age of respondents 
# age_sd <-  sd of age of respondents 
# gender_group <-  all, female only, male only respondents
# prop_female <-  prop. of female respondents
# n <-  num of respondents with complete observations for var A  & B
# varcode_a <- code/name of the variable A
# varcode_b <- code/name of the variable B
# cor_pearson <- pearson correlation coefficient between var A & B
# cor_spearman <- spearman correlation coefficient  between var A & B
# icc2_1 <- Intra-class correlation coefficient  between var A & B
# cor_pearson_log <- pearson correlation coefficient w log-transformed data
# cor_spearman_log <- spearman correlation coefficient w log-transformed data
# icc2_1_log <- Intra-class correlation coefficient w log-transformed data
# coeff_var_a <-  coefficient of variation of var A
# coeff_var_b <- coefficient of variation of var B
# skewness_a <- skewness of responses of var A
# skewness_b <-  skewness of responses of var B


# 2) intercor_w_id = same as intercor, but for each correlation it saves the id list of the respondents whose data was used.

# Certain packages need to be installed first


# Author(s): Alexandra Bagaini & Rui Mata, Centre for Cognitive and Decision Sciences, Faculty of Psychology, University of Basel.


# PACKAGES ---------------------------------------------------------------

library(tidyverse)
library(irr) # icc 2,1 calculation 
library(moments) # skewness calculation

# FUNCTION ----------------------------------------------------------------

calc_intercor <-function(panel,data,varcodes,wave_ids, age_group) {
  
  #_________________BASIC STRUCTURES _________________________#
  
  intercor <- NULL 
  intercor_w_id <- NULL  
  
  
  
  # ONLY NEED HALF OF THE PAIRS
  varcodes0 <- varcodes %>%
    mutate(pair = pmap_chr(list(var_a, var_b), ~paste(sort(c(...)), collapse = " "))) %>%
    distinct(pair, .keep_all = TRUE)
  
  
  
  #_________________SELECT: RISK MEASURE PAIR_________________________#
  
  for (CurrVarCode in 1:nrow(varcodes0)) {
    
    
    #_________________SELECT: A & B DATA_________________________# 
    
    for (CurrWave in 1:length(wave_ids))  {
      curr_wave_id <- wave_ids[CurrWave]
      
      data_a <- data %>%  # select A data
        filter(wave_id == curr_wave_id) %>%     
        filter(varcode == varcodes0$var_a[CurrVarCode]) %>% # select relevant varcode 
        rename(response_a = response,  # rename vars (except id and gender)
               varcode_a = varcode) %>% 
        select(wave_id, wave_year, id, gender, age, varcode_a, response_a)
      
      
      data_b <- data %>%  # select B data
        filter(wave_id == curr_wave_id) %>%     
        filter(varcode == varcodes0$var_b[CurrVarCode]) %>% # select relevant varcode 
        rename(response_b = response,  # rename vars (except id and gender)
               varcode_b = varcode) %>% 
        select(id, gender, age, varcode_b, response_b)
      
      #_________________MERGE: A + B DATA_________________________#
      
      fullset <- left_join(data_a,data_b,by=c("id", "gender", "age")) # merge two sets by "id" & "gender" & "age
      
      #_________________SELECT: AGE_________________________# 
      
      for (CurrAgeGroup in age_group$group) {
        
        agerange <- age_group %>% 
          filter(group == CurrAgeGroup)   # select age group
        
        # filter based on age
        set <- fullset %>% 
          filter(age %in% c(agerange$min:agerange$max))
        
        #_________________SELECT: GENDER_________________________# 
        
        for (CurrGender in c(0:2)) {
          
          # 0 == Male; 1 = Female; 2 = Male + Female
          
          if (CurrGender != 2) {
            
            # filter based on gender & complete cases 
            subset <- set %>% 
              filter(gender == CurrGender) %>%
              filter(complete.cases(response_a,response_b))
            
            gender_group <- ifelse(CurrGender == 1, "female", "male")
            
          }
          
          
          if (CurrGender == 2) {
            
            # filter based on complete cases 
            subset <- set %>% 
              filter(complete.cases(response_a,response_b))
            
            
            gender_group <- "all"
            
          }
          
          
          #_________________SD CHECK _________________________# 
          sd_check <- sd(subset$response_a) & sd(subset$response_b) != 0 # check if sd for data A & B are not 0 
          
          
          # ONLY COMPUTE CORRELATIONS IF THERE IS DATA ON MORE THAN 10 RESPONDENTS + VARIANCE IN DATA 
          
          if (nrow(subset) >= 10 & sd_check == 1 ) { 
            
            #_________________COMPUTE CORRELATION & STORE SAMPLE + WAVE INFO_________________________#
            
            # creating data frame to save relevant information
            sub_intercor <- NULL

            
            # save relevant information
            sub_intercor$panel <- panel  # panel name
            sub_intercor$wave_id <- as.character(unique(subset$wave_id))  #  wave id of survey
            sub_intercor$wave_year <- unique(subset$wave_year)  #  wave year of survey 
            sub_intercor$year_age_group <- unique(agerange$age_bin_categ) #  XYZ-year age group  
            sub_intercor$age_group <- CurrAgeGroup    #  age group  
            sub_intercor$age_mean <- mean(subset$age) # mean age of respondents 
            sub_intercor$age_median <- median(subset$age) # median age of respondents 
            sub_intercor$age_min <- min(subset$age) # min. age of respondents 
            sub_intercor$age_max <- max(subset$age) # min. age of respondents 
            sub_intercor$age_sd <- sd(subset$age) # sd of age of respondents 
            sub_intercor$gender_group <- gender_group  # all, female only, male only respondents
            sub_intercor$prop_female <- sum(subset$gender==1)/nrow(subset) # prop. of female respondents
            sub_intercor$n <- nrow(subset)  # num of respondents with complete observations
            sub_intercor$varcode_a <- varcodes0$var_a[CurrVarCode]  # code name of the variable of var A
            sub_intercor$varcode_b <- varcodes0$var_b[CurrVarCode]  # code name of the variable of var B
            sub_intercor$cor_pearson <- cor(subset$response_a,subset$response_b,method="pearson")  # pearson correlation coefficient
            sub_intercor$cor_spearman <- cor(subset$response_a,subset$response_b,method="spearman")  # spearman correlation coefficient
            sub_intercor$icc2_1 <- icc(data.frame(subset$response_a,subset$response_b), model = "twoway", type = "agreement", unit = "single")$value #ICC2_1
            sub_intercor$cor_pearson_log <- cor(log(1+(subset$response_a)),log(1+(subset$response_b)), method="pearson")  # pearson correlation coefficient w log transformed data (adding 1 to avoid "0" values in the data)
            sub_intercor$cor_spearman_log <- cor(log(1+(subset$response_a)),log(1+(subset$response_b)), method="spearman")  # spearman correlation coefficient w log transformed data (adding 1 to avoid "0" values in the data)
            sub_intercor$icc2_1_log <- icc(data.frame(log(1+(subset$response_a)),log(1+(subset$response_b))), model = "twoway", type = "agreement", unit = "single")$value #ICC2_1 w log transformed data (adding 1 to avoid "0" values in the data)
            sub_intercor$coeff_var_a <- sd(subset$response_a)/mean(subset$response_a) # calculate the coefficient of variation of var A
            sub_intercor$coeff_var_b <- sd(subset$response_b)/mean(subset$response_b) # calculate the coefficient of variation of var B
            sub_intercor$skewness_a <- skewness(subset$response_a, na.rm = FALSE) # calculate skewness of responses of var A
            sub_intercor$skewness_b <- skewness(subset$response_b, na.rm = FALSE) # calculate skewness of responses of var B
            
            # converting NaN to NA 
            # sub_intercor[sapply(sub_intercor, is.nan)] <- NA
            
            
            #_________________STORE INTERCOR INFO_________________________#
            
            # save respondent + response ids (& attach panel name) in separate df
            sub_intercor_w_id <- sub_intercor 
            sub_intercor_w_id$id_list <- list(paste0(panel, "_", tolower(as.character(subset$id))))
            sub_intercor_var_resp_id_a <- paste0(panel, "_", tolower(as.character(subset$id)), "_", varcodes0$var_a[CurrVarCode], "_", subset$wave_id)
            sub_intercor_var_resp_id_b <- paste0(panel, "_", tolower(as.character(subset$id)), "_", varcodes0$var_b[CurrVarCode], "_", subset$wave_id)
            sub_intercor_w_id$resp_id_list <- list(c(sub_intercor_var_resp_id_a, sub_intercor_var_resp_id_b))
            
            # save output from each loop
            intercor_w_id <- bind_rows(intercor_w_id, sub_intercor_w_id)
            intercor <- bind_rows(intercor, sub_intercor)
            
            
          } # condition that there are enough responses and variance in dataset
          
        } # gender loop
      } # age loop 
    }# wave  loop
  } # varcode list loop
  
  
  
  
  outcome <- list(intercor,intercor_w_id)
  return(outcome)
  
}  # end of function


