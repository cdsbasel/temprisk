
#  DESCRIPTION -------------------------------------------------------------

# Script reads the complete set of retest correlations and computes Shapley values for 
# each predictor of interest for different subsets and formats of the data. Multiverse analysis.

# Author(s): Alexandra Bagaini(1)
# (1)Centre for Cognitive and Decision Sciences, Faculty of Psychology, University of Basel.


# PACKAGE -----------------------------------------------------------------



library(tidyverse)


# FUNCTIONS ---------------------------------------------------------------

source("helper_functions.R")




# PATHS  ---------------------------------------------------

data_path <- c("processing/output/temp_stability/") # where is the input file stored
retest_file <- "complete_retest.csv" # name of merged retest data 
output_path <- c("analysis/output/temp_stability/") # where to store the output 



# READ DATA ---------------------------------------------------------------
col_spec <-cols(
  panel = col_character(),
  wave_id_t1 = col_character(),
  wave_year_t1 = col_double(),
  wave_id_t2 = col_character(),
  wave_year_t2 = col_double(),
  time_diff_mean = col_double(),
  time_diff_median = col_double(),
  time_diff_min = col_double(),
  time_diff_max = col_double(),
  time_diff_sd = col_double(),
  year_age_group = col_double(),
  age_group = col_character(),
  age_mean = col_double(),
  age_median = col_double(),
  age_min = col_double(),
  age_max = col_double(),
  age_sd = col_double(),
  gender_group = col_character(),
  prop_female = col_double(),
  n = col_double(),
  attrition_rate = col_double(),
  varcode = col_character(),
  cor_pearson = col_double(),
  cor_spearman = col_double(),
  icc2_1 = col_double(),
  cor_pearson_log = col_double(),
  cor_spearman_log = col_double(),
  icc2_1_log = col_double(),
  coeff_var_t1 = col_double(),
  coeff_var_t2 = col_double(),
  skewness_t1 = col_double(),
  skewness_t2 = col_double(),
  measure_category = col_character(),
  general_domain = col_character(),
  domain_name = col_character(),
  scale_type = col_character(),
  scale_length = col_double(),
  time_frame = col_double(),
  behav_type = col_character(),
  behav_paid = col_character(),
  continent = col_character(),
  country = col_character(),
  language = col_character(),
  data_collect_mode = col_character(),
  sample_type = col_character()
)

dat <-read_csv(paste0(data_path,retest_file), col_types = col_spec)


# PREP. DATA ---------------------------------------------------------------


data_fltr <-  dat %>%
  filter(age_group != "10-90" &
           gender_group != "all")  %>%
  #set negative retest correlations to 0 (see also Enkavi et al., 2019, PNAS)
  mutate(icc2_1_log = if_else(icc2_1_log < 0 , 0, icc2_1_log),
         cor_pearson_log = if_else(cor_pearson_log < 0 , 0, cor_pearson_log),
         cor_spearman_log = if_else(cor_spearman_log < 0 , 0, cor_spearman_log),
         icc2_1 = if_else(icc2_1 < 0 , 0, icc2_1),
         cor_pearson = if_else(icc2_1_log < 0 , 0, icc2_1_log),
         cor_spearman = if_else(cor_spearman < 0 , 0, cor_spearman),
         # need to rename/group the levels of certain categ. variables, 
         scale_type = if_else(scale_type %in% c("ord", "dis"), "ord-dis", "oe-comp")) # open ended/composite  vs. ordinal/discrete measure)




age_bin = c(5,10,20)
min_n = c(30,100,250)
dt_transform = c("none", "log")
cor_metric = c("pearson", "spearman", "icc2_1")
comb_dt <- crossing(age_bin, min_n, dt_transform, cor_metric)


# OMNIBUS -----------------------------------------------------------------

t_df <- NULL
counter <- 1
coef_check <- NULL

for (curr_comb in 1:nrow(comb_dt)) {
  
  curr_age_bin <- comb_dt$age_bin[curr_comb]
  curr_min_n <- comb_dt$min_n[curr_comb]
  curr_metric <- comb_dt$cor_metric[curr_comb]
  curr_transform <- comb_dt$dt_transform[curr_comb]
  
  
  data_fltr_all <- data_fltr %>%
    filter(year_age_group == curr_age_bin & n >= curr_min_n) %>% 
    mutate(sample_size = n,
           icc_log_c = as.vector(scale(icc2_1_log, center = TRUE, scale = TRUE)),
           cor_log_c = as.vector(scale(cor_pearson_log, center = TRUE, scale = TRUE)),
           spear_log_c = as.vector(scale(cor_spearman_log, center = TRUE, scale = TRUE)),
           icc_c = as.vector(scale(icc2_1, center = TRUE, scale = TRUE)),
           cor_c = as.vector(scale(cor_pearson, center = TRUE, scale = TRUE)),
           spear_c = as.vector(scale(cor_spearman, center = TRUE, scale = TRUE)))
  
  data_fltr_all$row_id <- sample(1:nrow(data_fltr_all))
  
  
  predictors <- c("panel", # panel characteristics
                  "age_group", "gender_group", "sample_size", # respondent characteristics
                  "measure_category", "domain_name", "scale_type", "time_diff_mean") # measure charactiristics
  
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
  
  # detect singularity issues
  # fit <-  lm(as.formula(paste(c(dv, predictors),collapse=" + ")), data = data)
  # singul_issue <- sum(is.na(fit$coefficients)) > 0
  
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
        
      } # check vars for singularity
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
             # singul_issue = singul_issue,
             n_obs = nrow(data))
    
    
    t_df <- bind_rows(t, t_df)
  }
}   

write_csv(t_df, paste0(output_path, "shapley_values_multiv_omnibus",".csv"))

t <- tibble(check = coef_check)
write_csv(t, paste0(output_path, "shapley_values_multiv_omnibus_check.csv"))

# MEASURE -----------------------------------------------------------------


for (curr_meas in c("pro", "fre", "beh")) {
  
  t_df <- NULL
  counter <- 1
  coef_check <- NULL
  for (curr_comb in 1:nrow(comb_dt)) {
    
    curr_age_bin <- comb_dt$age_bin[curr_comb]
    curr_min_n <- comb_dt$min_n[curr_comb]
    curr_metric <- comb_dt$cor_metric[curr_comb]
    curr_transform <- comb_dt$dt_transform[curr_comb]
    
    
    data_fltr_all <- data_fltr %>%
      filter(measure_category == curr_meas) %>% 
      filter(year_age_group == curr_age_bin & n >= curr_min_n) %>% 
      mutate(sample_size = n,
             icc_log_c = as.vector(scale(icc2_1_log, center = TRUE, scale = TRUE)),
             cor_log_c = as.vector(scale(cor_pearson_log, center = TRUE, scale = TRUE)),
             spear_log_c = as.vector(scale(cor_spearman_log, center = TRUE, scale = TRUE)),
             icc_c = as.vector(scale(icc2_1, center = TRUE, scale = TRUE)),
             cor_c = as.vector(scale(cor_pearson, center = TRUE, scale = TRUE)),
             spear_c = as.vector(scale(cor_spearman, center = TRUE, scale = TRUE)))
    
    predictors <- c("panel", # panel characteristics
                    "age_group", "gender_group", "sample_size", # respondent characteristics
                    "domain_name", "scale_type", "time_diff_mean") # measure characteristics
    
   
    
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
        mutate(measure_category = curr_meas)
      
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
               # singul_issue = singul_issue,
               n_obs = nrow(data))
      
      
      t_df <- bind_rows(t, t_df)
      
      
      
      
      
    } # vars are okay to be estimated
  } # curr comnbination
  t <- tibble(check = coef_check)
  write_csv(t, paste0(output_path, "shapley_values_multiv_", curr_meas, "_check.csv"))
  write_csv(t_df, paste0(output_path, "shapley_values_multiv_", curr_meas,".csv"))
  
} # curr_measure


