
#  DESCRIPTION -------------------------------------------------------------

# Script reads the complete set of  intercorrelations and computes Shapley values for 
# each predictor of interest (this script also includes "item_match_type" as a predictor).


# Author(s): Alexandra Bagaini(1) 
# (1)Centre for Cognitive and Decision Sciences, Faculty of Psychology, University of Basel.


# PACKAGES ----------------------------------------------------------------


library(tidyverse)

source("helper_functions.R")

# FILES  ---------------------------------------------------

data_path <- c("processing/output/convergent_val/") # where is the input file stored
intercor_file <- "complete_intercor.csv" # name of merged retest data 
output_path <- c("analysis/output/convergent_val/") # where to store the output 
masc_dat_file <- "analysis/output/convergent_val/masc_nlpar_pred.csv"

# READ DATA ---------------------------------------------------------------
col_spec <- cols(
  panel = col_character(),
  sample = col_character(),
  wave_id = col_character(),
  wave_year = col_double(),
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
  varcode_a = col_character(),
  varcode_b = col_character(),
  cor_pearson = col_double(),
  cor_spearman = col_double(),
  icc2_1 = col_double(),
  cor_pearson_log = col_double(),
  cor_spearman_log = col_double(),
  icc2_1_log = col_double(),
  coeff_var_a = col_double(),
  coeff_var_b = col_double(),
  skewness_a = col_double(),
  skewness_b = col_double(),
  measure_category_a = col_character(),
  general_domain_a = col_character(),
  domain_name_a = col_character(),
  scale_type_a = col_character(),
  scale_length_a = col_double(),
  time_frame_a = col_double(),
  behav_type_a = col_character(),
  behav_paid_a = col_character(),
  measure_category_b = col_character(),
  general_domain_b = col_character(),
  domain_name_b = col_character(),
  scale_type_b = col_character(),
  scale_length_b = col_double(),
  time_frame_b = col_double(),
  behav_type_b = col_character(),
  behav_paid_b = col_character(),
  continent = col_character(),
  country = col_character(),
  language = col_character(),
  data_collect_mode = col_character(),
  sample_type = col_character()
)

dat <-read_csv(paste0(data_path,intercor_file), col_types = col_spec)

dat_masc <- read_csv(masc_dat_file)


# PREP. DATA ---------------------------------------------------------------

dat_fltr <-  dat %>%
  filter(age_group != "10-90" &
           gender_group != "all" & 
           year_age_group == 10 & 
           n >= 30) %>% 
  mutate(sample_size = n,
         item_num_a = if_else(item_num_a == 1, "single", "multi"),
         item_num_b = if_else(item_num_b == 1, "single", "multi"),
         item_num_type = case_when(item_num_b == "single" & item_num_a == "single"  ~ "both_single",
                                   item_num_b != "single" & item_num_a != "single"  ~ "both_multi",
                                   TRUE ~"mixed"),
         scale_type_pair_same = ifelse(scale_type_a == scale_type_b, 1,0),
         measure_category_pair_same = ifelse(measure_category_a == measure_category_b, 1,0),
         domain_name_pair_same = ifelse(domain_name_a == domain_name_b, 1,0),
         cor_c = as.vector(scale(cor_spearman, center = TRUE, scale = TRUE))) 


#### add the reliability parameter information
# select relevant estimates
dat_masc <- dat_masc %>% 
  mutate(measure = case_when(measure == "Propensity" ~"pro",
                             measure == "Frequency" ~"fre",
                             measure == "Behaviour"  ~ "beh")) %>% 
  filter(nlpar == "Reliability")  %>% 
  select(measure,x, .epred, age_group, gender_group, sample) 

dat_masc_a <- dat_masc  %>% rename(estimate_a = .epred, measure_category_a = measure,
                                   domain_name_a = x)
dat_masc_b <- dat_masc  %>% rename(estimate_b = .epred, measure_category_b = measure,
                                   domain_name_b = x)

dat_fltr <- dat_fltr %>% 
  left_join(dat_masc_a, by = c("domain_name_a","measure_category_a", "age_group", "gender_group", "sample" )) %>% 
  left_join(dat_masc_b, by = c("domain_name_b","measure_category_b", "age_group", "gender_group", "sample" )) %>% 
  rowwise() %>% 
  mutate(mean_rel = mean(c(estimate_a, estimate_b))) %>% 
  ungroup()

# CONVERGENT: NO BOOT  ----------------------------------------------------------


predictors <- c("panel",# sample/panel characteristics
                "sample_size",  "age_group", "gender_group", # respondent characteristics  
                "scale_type_pair_same", "domain_name_pair_same", "measure_category_pair_same", "mean_rel", "item_num_type")  # measure characteristics)  


outcome <- "cor_c" # 

reg_mat <- expand.grid(rep(list(c(TRUE, FALSE)), length(predictors))) 
names(reg_mat) <- predictors

allModelsList <- apply(reg_mat, 1, function(x) as.formula(paste(c("cor_c ~ 1", predictors[x]),
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


write_csv(dat_r_change, paste0(output_path, "shapley_values_intercor_item.csv"))


# CONVERGENT: BOOT -----------------------------------------------------------

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

### CALC R2 INCREMENTS

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
write_csv(dat_r_change, paste0(output_path, "shapley_values_intercor_item_boot.csv"))
write_csv(t, paste0(output_path, "shapley_values_intercor_item_boot_check.csv"))





# SUMMARISE OUTPUT FOR PLOTTING: CALC SHAPLEY VALS -------------------------------------------

main_shapley_vals <- read_csv(paste0(output_path, "shapley_values_intercor_item.csv")) %>% 
  mutate(measure_category = "Omnibus")

predictors <- unique(main_shapley_vals$x)

w_df <- main_shapley_vals %>% 
  group_by(n_reg_with, x, measure_category ) %>% 
  summarise(w = 1/(length(predictors)*n())) 


t <- main_shapley_vals %>% 
  left_join(w_df, by = c("n_reg_with", "x", "measure_category")) %>% 
  group_by(x, measure_category) %>% 
  summarise(m = weighted.mean(r2adj_increment, w = w))%>% 
  rowwise() %>% 
  mutate(x_lbl = lbl_pred_replace(x),
         categ_lbl = lbl_categ_replace(x)) %>% 
  ungroup()


boot_shapley_vals <-  read_csv(paste0(output_path, "shapley_values_intercor_item_boot.csv")) %>% 
  mutate(measure_category = "Omnibus")


boot_w_df <- boot_shapley_vals %>% 
  group_by(n_reg_with, x, boot_num, measure_category) %>% 
  summarise(w = 1/(length(predictors)*n())) 


boot_t <- boot_shapley_vals %>% 
  left_join(boot_w_df, by = c("n_reg_with", "x", "boot_num", "measure_category")) %>% 
  group_by(x, boot_num, measure_category) %>% 
  summarise(m = weighted.mean(r2adj_increment, w = w)) %>% 
  group_by(x, measure_category) %>% 
  mean_qi(m, .width = c(.5,.8, .95)) %>% 
  pivot_wider(names_from = .width, values_from = c(.lower,.upper)) %>% 
  ungroup() %>% 
  rowwise() %>% 
  mutate(x_lbl = lbl_pred_replace(x),
         categ_lbl = lbl_categ_replace(x)) %>% 
  ungroup()


write_csv(t, paste0(output_path,"summary_shapley_values_intercor_item.csv"))
write_csv(boot_t, paste0(output_path,"summary_shapley_values_intercor_item_boot.csv"))





