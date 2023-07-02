
#  DESCRIPTION -------------------------------------------------------------


# Script reads the complete set of  intercorrelations and computes Shapley values for 
# each predictor of interest for different subsets and formats of the data. Multiverse analysis. 


# Author(s): Alexandra Bagaini(1) 
# (1)Centre for Cognitive and Decision Sciences, Faculty of Psychology, University of Basel.


# PACKAGE -----------------------------------------------------------------



library(tidyverse)


# FUNCTIONS ---------------------------------------------------------------

source("helper_functions.R")

# PATHS  ---------------------------------------------------

data_path <- c("processing/output/convergent_val/") # where is the input file stored
retest_file <- "complete_intercor.csv" # name of merged retest data 
output_path <- c("analysis/output/convergent_val/") # where to store the output 

masc_dat_file <- "analysis/output/convergent_val/masc_nlpar_pred.csv"
dat_masc <- read_csv(masc_dat_file)

# select relevant estimates (using only estimates from one age group,)
dat_masc <- dat_masc %>% 
  filter(age_group == "40-49") %>% 
  mutate(measure = case_when(measure == "Propensity" ~"pro",
                             measure == "Frequency" ~"fre",
                             measure == "Behaviour"  ~ "beh")) %>% 
  filter(nlpar == "Reliability")  %>% 
  select(measure,x, .epred, gender_group, sample) 

dat_masc_a <- dat_masc  %>% rename(estimate_a = .epred, measure_category_a = measure,
                                   domain_name_a = x)
dat_masc_b <- dat_masc  %>% rename(estimate_b = .epred, measure_category_b = measure,
                                   domain_name_b = x)


# READ DATA ---------------------------------------------------------------


dat <-read_csv(paste0(data_path,retest_file))


# PREP. DATA ---------------------------------------------------------------


data_fltr <-  dat %>%
  filter(age_group != "10-90" &
           gender_group != "all") 



age_bin = c(5,10,20)
min_n = c(30,100,250)
dt_transform = c("none", "log")
cor_metric = c("pearson", "spearman", "icc2_1")
comb_dt <- crossing(age_bin, min_n, dt_transform, cor_metric)



# OVERALL -----------------------------------------------------------------

t_df <- NULL
counter <- 1
coef_check <- NULL
m_rel <- NULL

for (curr_comb in 1:nrow(comb_dt)) {
  
  curr_age_bin <- comb_dt$age_bin[curr_comb]
  curr_min_n <- comb_dt$min_n[curr_comb]
  curr_metric <- comb_dt$cor_metric[curr_comb]
  curr_transform <- comb_dt$dt_transform[curr_comb]
  
  
  
  data_fltr_all <-  data_fltr %>%
    filter(year_age_group == curr_age_bin & n >= curr_min_n) %>%  
    mutate( sample_size = n,
            scale_type_pair_same = ifelse(scale_type_a == scale_type_b, 1,0),
            measure_category_pair_same = ifelse(measure_category_a == measure_category_b, 1,0),
            domain_name_pair_same = ifelse(domain_name_a == domain_name_b, 1,0),
            icc_log_c = as.vector(scale(icc2_1_log, center = TRUE, scale = TRUE)),
            cor_log_c = as.vector(scale(cor_pearson_log, center = TRUE, scale = TRUE)),
            spear_log_c = as.vector(scale(cor_spearman_log, center = TRUE, scale = TRUE)),
            icc_c = as.vector(scale(icc2_1, center = TRUE, scale = TRUE)),
            cor_c = as.vector(scale(cor_pearson, center = TRUE, scale = TRUE)),
            spear_c = as.vector(scale(cor_spearman, center = TRUE, scale = TRUE))) %>% 
    mutate(sample_size = n,
           scale_type_pair_same = ifelse(scale_type_a == scale_type_b, 1,0),
           measure_category_pair_same = ifelse(measure_category_a == measure_category_b, 1,0),
           domain_name_pair_same = ifelse(domain_name_a == domain_name_b, 1,0),
           cor_c = as.vector(scale(cor_spearman, center = TRUE, scale = TRUE))) 
  
  
  #### add the reliability parameter information
  
  
  
  data_fltr_all <- data_fltr_all %>% 
    left_join(dat_masc_a, by = c("domain_name_a","measure_category_a", "gender_group", "sample" )) %>% 
    left_join(dat_masc_b, by = c("domain_name_b","measure_category_b", "gender_group", "sample" )) %>% 
    rowwise() %>% 
    mutate(mean_rel = mean(c(estimate_a, estimate_b))) %>% 
    ungroup()
  
  
  m_rel[counter] <- sum(is.na(data_fltr_all$mean_rel)) > 0
  
  predictors <- c("panel",# sample/panel characteristics
                  "sample_size",  "age_group", "gender_group", # respondent characteristics  
                  "scale_type_pair_same", "domain_name_pair_same", "measure_category_pair_same", "mean_rel")  # measure characteristics)  
  
  
  
  outcome <-  case_when(curr_metric == "pearson" & curr_transform == "none" ~ "cor_c", 
                        curr_metric == "pearson" & curr_transform == "log" ~ "cor_log_c", 
                        curr_metric == "spearman" & curr_transform == "none" ~  "spear_c", 
                        curr_metric == "spearman" & curr_transform == "log" ~  "spear_log_c", 
                        curr_metric == "icc2_1" & curr_transform == "none" ~ "icc_c",
                        curr_metric == "icc2_1" & curr_transform == "log" ~ "icc_log_c")
  
  
  
  
  
  dv <- paste0(outcome, "~ 1")
  
  reg_mat <- expand.grid(rep(list(c(TRUE, FALSE)), length(predictors))) 
  names(reg_mat) <- predictors
  
  
  allModelsList <- apply(reg_mat, 1, function(x) as.formula(paste(c(dv, predictors[x]),
                                                                  collapse=" + ")))
  
  
  data <- data_fltr_all[,c(outcome,predictors)]
  
  
  if (sum(list_categorical_variables(data)$Levels < 2) == 0) {
    # detect singularity issues
    fit <-  lm(as.formula(paste(c(dv, predictors),collapse=" + ")), data = data)
    singul_issue <- sum(is.na(fit$coefficients)) > 0
  }
  
  if (sum(list_categorical_variables(data)$Levels < 2) != 0) {
    singul_issue <- TRUE}
  
  
  if (sum(list_categorical_variables(data)$Levels < 2) == 0 & singul_issue == FALSE) {
    
    allModelsResults <- NULL
    
    for (curr_model in 1:length(allModelsList)) {
      
      fit <-  lm(allModelsList[[curr_model]], data=data)
      coef_check[counter] <- sum(is.na(fit$coefficients)) > 0
      
      R2  <- summary(fit)$r.squared
      R2adj  <- summary(fit)$adj.r.squared
      regressor_vec <- paste(sort(attr(summary(fit)$terms,"term.labels")), collapse = ";")
      
      
      results <- tibble( model = curr_model,
                         regressor_vec = regressor_vec,
                         R2adj = R2adj,
                         R2 = R2) %>%
        rowwise() %>%
        mutate(regressor_list = list((str_split(regressor_vec, ";")))) %>%
        ungroup()
      
      
      
      
      allModelsResults <- bind_rows(allModelsResults, results)
      counter <- counter + 1
      print(curr_model)
    }
    
    
    ### CALC SHAPPLEY VALUE
    dat_r_change <- NULL
    
    for (var_int in predictors){
      
      without_reg <- allModelsResults %>%  filter(!grepl(var_int, regressor_vec))
      
      with_reg <- allModelsResults %>%  filter(grepl(var_int, regressor_vec))
      
      for (curr_set in 1:nrow(without_reg)) {
        
        list_set_without <- sort(unlist(without_reg$regressor_list[curr_set]))
        reg_num_without <- length(list_set_without)
        var_set_without <- paste(sort(c(list_set_without)), collapse = ";")
        
        if (var_set_without == "") {
          var_set_with <- var_int
          reg_num_without <- 0}
        
        
        if (var_set_without != "") {
          var_set_with <- paste(sort(c(list_set_without, var_int)), collapse = ";")
        }
        
        r2_with <- with_reg$R2[with_reg$regressor_vec == var_set_with]
        r2_without <- without_reg$R2[without_reg$regressor_vec == var_set_without]
        r2_increment <-  r2_with - r2_without
        
        r2adj_with <- with_reg$R2adj[with_reg$regressor_vec == var_set_with]
        r2adj_without <- without_reg$R2adj[without_reg$regressor_vec == var_set_without]
        r2adj_increment <-  r2adj_with - r2adj_without
        
        dt <- tibble(r2_increment = r2_increment,
                     r2_with = r2_with,
                     r2_without = r2_without,
                     r2adj_increment = r2adj_increment,
                     r2adj_with = r2adj_with,
                     r2adj_without = r2adj_without,
                     x = var_int,
                     n_reg_with = reg_num_without+1,
                     supple = var_set_without)
        
        dat_r_change <- bind_rows(dat_r_change, dt)
        
      }
    }
    
    
    
    main_shapley_vals <- dat_r_change %>%
      mutate(measure_category = "Omnibus")
    
    predictors <- unique(main_shapley_vals$x)
    w_df <- main_shapley_vals %>%
      group_by(n_reg_with, x, measure_category ) %>%
      summarise(w = 1/(length(predictors)*n()),
                .groups = "drop")
    
    
    t <- main_shapley_vals %>%
      left_join(w_df, by = c("n_reg_with", "x", "measure_category")) %>%
      group_by(x, measure_category) %>%
      summarise(m = weighted.mean(r2adj_increment, w = w),
                .groups = "drop")%>%
      rowwise() %>% 
      mutate(x_lbl = lbl_pred_replace(x),
             categ_lbl = lbl_categ_replace(x)) %>% 
      ungroup() %>% 
      mutate(age_bin = curr_age_bin,
             min_n = curr_min_n,
             corr_metric = curr_metric,
             data_transform = curr_transform,
             n_obs = nrow(data))
    
    
    t_df <- bind_rows(t, t_df)
    
  }   
}
write_csv(t_df, paste0(output_path, "shapley_values_multiv_overall",".csv"))
t <- tibble(check = coef_check)
u <- tibble(mean_rel_check = m_rel)
write_csv(t, paste0(output_path, "shapley_values_multiv_overall", "_check.csv"))
write_csv(u, paste0(output_path, "shapley_values_multiv_overall", "_check2.csv"))