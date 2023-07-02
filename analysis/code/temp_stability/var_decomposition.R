
#  DESCRIPTION -------------------------------------------------------------

# Script reads the complete set of retest correlations and computes Shapley values for 
# each predictor of interest for different subsets of the data. Includes boostrapping. 
# Saves as output the Shapley values for the available and boostraped data sets separately

# Author(s): Alexandra Bagaini(1)
# (1)Centre for Cognitive and Decision Sciences, Faculty of Psychology, University of Basel.


# PACKAGES ----------------------------------------------------------------


library(tidyverse)

# FILES  ---------------------------------------------------

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

dat <- read_csv(paste0(data_path,retest_file), col_types = col_spec)


# PREP. DATA ---------------------------------------------------------------


dat_fltr <-  dat %>%
  mutate(cor_pearson = if_else(cor_pearson < 0, 0, cor_pearson)) %>% # set negative retest correlations to 0 (see also Enkavi et al., 2019, PNAS)
  filter(age_group != "10-90" &
           gender_group != "all" & 
           year_age_group == 10 & 
           n >= 30)  %>% 
  ungroup() %>% 
  mutate(sample_size = n,
         cor_c = as.vector(scale(cor_pearson, center = TRUE, scale = TRUE)) ,
         # need to rename/group the levels of certain categ. variables to avoid singularities
          scale_type = if_else(scale_type %in% c("ord","dis"), "ord-dis",  "oe-comp")) # open ended/composite  vs. ordinal/discrete measure)


# OMINIBUS: NO BOOT  ----------------------------------------------------------


predictors <- c("panel", # panel characteristics
                "age_group", "gender_group", "sample_size", # respondent characteristics
                "measure_category", "domain_name", "scale_type", "time_diff_mean") # measure charactiristics


outcome <- "cor_c" # 

reg_mat <- expand.grid(rep(list(c(TRUE, FALSE)), length(predictors))) 
names(reg_mat) <- predictors

allModelsList <- apply(reg_mat, 1, function(x) as.formula(paste(c(paste0(outcome, " ~ 1"), predictors[x]),
                                                                collapse=" + ")))

data <- dat_fltr[,c(outcome,predictors)] 

fit <-  lm(as.formula(paste(c("cor_c ~ 1", predictors),collapse=" + ")), data = data)
 summary(fit)

allModelsResults <- NULL
for (curr_model in 1:length(allModelsList)) {
  
  fit <-  lm(allModelsList[[curr_model]], data=data)
  
  
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
  
  print(curr_model) 
}


# write_rds(allModelsResults, paste0(output_path, "shapley_decomp_omni_retest.rds"))
# allModelsResults <- read_rds(paste0(output_path, "shapley_decomp_omni_retest.rds"))
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
dat_r_change <- dat_r_change %>% mutate(row_id = 1:n()) %>% relocate(row_id)
write_csv(dat_r_change, paste0(output_path, "shapley_values_omni_retest.csv"))


# OMNIBUS: BOOT -----------------------------------------------------------
allModelsResults <- NULL
n_boot <- 100
counter <- 1
coef_check <- NULL
for (curr_boot in 1:n_boot) {
  
  #Creating a resampled dataset from the sample data
  boot_data = data[sample(nrow(data), replace = TRUE), ]
  
  #parametric bootstrapping (?)
  # boot_data <- data
  # boot_data$cor_c <-stats::simulate(fit, nsim = 1)
  
  
  for (curr_model in 1:length(allModelsList)) {
    fit <-  lm(allModelsList[[curr_model]], data = boot_data)
    
    coef_check[counter] <- sum(is.na(fit$coefficients)) > 0
    
    R2  <- summary(fit)$r.squared
    R2adj  <- summary(fit)$adj.r.squared
    regressor_vec <- paste(sort(attr(summary(fit)$terms,"term.labels")), collapse = ";")
    
    
    results <- tibble( model = curr_model,
                       boot_num = curr_boot,
                       regressor_vec = regressor_vec,
                       R2adj = R2adj,
                       R2 = R2) %>% 
      rowwise() %>% 
      mutate(regressor_list = list((str_split(regressor_vec, ";")))) %>% 
      ungroup()
    
    
    
    counter <- counter + 1
    allModelsResults <- bind_rows(allModelsResults, results)
    print(sprintf("model num = %d; boot num = %d",curr_model, curr_boot) )
  }
}


# write_rds(allModelsResults, "shapley_decomp_omni_retest_boot.rds")
# allModelsResults <- read_rds("shapley_decomp_omni_retest_boot.rds")
### CALC SHAPPLEY VALUE

dat_r_change <- NULL

for (curr_boot in unique(allModelsResults$boot_num)) {
  
  model_res <- allModelsResults %>% filter(boot_num == curr_boot)
  
  for (var_int in predictors){
    
    without_reg <- model_res %>%  filter(!grepl(var_int, regressor_vec)) 
    
    with_reg <- model_res %>%  filter(grepl(var_int, regressor_vec))  
    
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
                   boot_num = curr_boot,
                   n_reg_with = reg_num_without+1,
                   supple = var_set_without)
      
      
      
      
      dat_r_change <- bind_rows(dat_r_change, dt)
    }
  }
  print(curr_boot)
}

t <- tibble(check = coef_check)
write_csv(dat_r_change,  paste0(output_path, "shapley_values_omni_retest_boot.csv"))
write_csv(t, paste0(output_path, "shapley_values_omni_retest_boot_check.csv"))















# MEASURES: NO BOOT  ----------------------------------------------------------

for (curr_meas in c("pro", "fre", "beh")) {
  

  print(curr_meas)
  

  predictors <- c("panel", # panel characteristics
                  "age_group", "gender_group", "sample_size", # respondent characteristics
                  "domain_name", "scale_type", "time_diff_mean") # measure characteristics
  
  
  outcome <- "cor_c" # 
  

  
  reg_mat <- expand.grid(rep(list(c(TRUE, FALSE)), length(predictors))) 
  names(reg_mat) <- predictors
  
  allModelsList <- apply(reg_mat, 1, function(x) as.formula(paste(c(paste0(outcome, " ~ 1"), predictors[x]),
                                                                  collapse=" + ")))
  
  data <- dat_fltr %>% filter(measure_category == curr_meas)
  data <- data[,c(outcome,predictors)] 

  fit <-  lm(as.formula(paste(c("cor_c ~ 1", predictors),collapse=" + ")), data = data)
  summary(fit)

  allModelsResults <- NULL
  
  for (curr_model in 1:length(allModelsList)) {
    
    fit <-  lm(allModelsList[[curr_model]], data=data)
    
    
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
    
    print(curr_model) 
  }
  
  
  # write_rds(allModelsResults, "shapley_decomp_fre_retest.rds")
  # allModelsResults <- read_rds("shapley_decomp_fre_retest.rds")
  
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
  
  dat_r_change <- dat_r_change %>% mutate(row_id = 1:n()) %>% relocate(row_id)
  write_csv(dat_r_change, paste0(output_path,"shapley_values_", curr_meas, "_retest.csv"))
  
  
  
  # MEASURE: BOOT -----------------------------------------------------------
  
  allModelsResults <- NULL
  n_boot <- 100
  counter <- 1
  coef_check <- NULL 
  for (curr_boot in 1:n_boot) {
    
    #Creating a resampled dataset from the sample data
    boot_data = data[sample(nrow(data), replace = TRUE), ]
    
    
    
    for (curr_model in 1:length(allModelsList)) {
      
      fit <-  lm(allModelsList[[curr_model]], data = boot_data)
      
      coef_check[counter] <- sum(is.na(fit$coefficients)) > 0
      
      R2  <- summary(fit)$r.squared
      R2adj  <- summary(fit)$adj.r.squared
      regressor_vec <- paste(sort(attr(summary(fit)$terms,"term.labels")), collapse = ";")
      
      
      results <- tibble( model = curr_model,
                         boot_num = curr_boot,
                         regressor_vec = regressor_vec,
                         R2adj = R2adj,
                         R2 = R2) %>% 
        rowwise() %>% 
        mutate(regressor_list = list((str_split(regressor_vec, ";")))) %>% 
        ungroup()
      
      
      
      counter <- counter + 1
      allModelsResults <- bind_rows(allModelsResults, results)
      
      print(sprintf("model num = %d; boot num = %d",curr_model, curr_boot) )
    }
  }
  
  
  # write_rds(allModelsResults, "shapley_decomp_fre_retest_boot.rds")
  # allModelsResults <- read_rds("shapley_decomp_fre_retest_boot.rds")
  ### CALC SHAPPLEY VALUE
  
  dat_r_change <- NULL
  
  for (curr_boot in unique(allModelsResults$boot_num)) {
    
    model_res <- allModelsResults %>% filter(boot_num == curr_boot)
    
    for (var_int in predictors){
      
      without_reg <- model_res %>%  filter(!grepl(var_int, regressor_vec)) 
      
      with_reg <- model_res %>%  filter(grepl(var_int, regressor_vec))  
      
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
                     boot_num = curr_boot,
                     n_reg_with = reg_num_without+1,
                     supple = var_set_without)
        
        
        
        dat_r_change <- bind_rows(dat_r_change, dt)
      } # combinations
    } # predictor
    print(curr_boot)
  } # bootstrap
  
  t <- tibble(check = coef_check)
  write_csv(dat_r_change,paste0(output_path, "shapley_values_", curr_meas, "_retest_boot.csv"))
  write_csv(t, paste0(output_path,"shapley_values_", curr_meas, "_retest_boot_check.csv"))
} #measure loop

beepr::beep()




