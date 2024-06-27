

# SUPPLEMENTARY TABLE LABELS----------------------------------------------------------

abbrev_categ <- function(vec) {
  
  v <- str_to_sentence(unique(vec))
  v <- substr(v,1,1)
  v <- paste0(v, collapse = ", ")
  
  return(v)
}


abbrev_domain <- function(vec) {
  v <- unique(vec)
  v <- gsub("alc_conseq", "alc. conseq", v)
  v <- gsub("alc_consum", "alc. consum", v)
  v <- str_to_sentence(v)
  v <- paste0(v, ".")
  v <- paste0(v, collapse = ", ")
  
  return(v)
}



# DOMAIN LABELS --------------------------------------------------------



rename_domain <- function(x){
  x_lbl = case_when( x == "soc" ~ "Social",
                     x == "sex" ~ "Sexual Intercourse",
                     x == "eth" ~ "Ethical",
                     x == "inv" ~ "Investment",
                     x == "gam" ~ "Gambling",
                     x == "hea_gen" ~ "General Health",
                     x == "gen" ~ "General Risk",
                     x == "all" ~  "**Overall**",
                     x == "dri" ~ "Driving",
                     x == "smo" ~ "Smoking",
                     x == "alc" ~ "Alcohol",
                     x == "ins" ~ "Insurance",
                     x == "occ" ~ "Occupational",
                     x == "dru" ~ "Drugs",
                     x == "gen" ~ "General",
                     x == "eth" ~ "Ethical",
                     x == "rec" ~ "Recreational")
  
  return(x_lbl)
}



# CONTRAST/DUMMY/SUM CODING -----------------------------------------------------

# function by: PB
sum_coding <- function(x, lvls = levels(x)) {
  # codes the first category with -1
  nlvls <- length(lvls)
  stopifnot(nlvls > 1)
  cont <- diag(nlvls)[, -nlvls, drop = FALSE]
  cont[nlvls, ] <- -1
  cont <- cont[c(nlvls, 1:(nlvls - 1)), , drop = FALSE]
  colnames(cont) <- lvls[-1]
  x <- factor(x, levels = lvls)
  contrasts(x) <- cont
  x
}


# function by: PB
dummy_coding <- function(x, lvls = levels(x)) {
  x <- factor(x, levels = lvls)
  contrasts(x) <- contr.treatment(levels(x))
  x
}




# NLPAR ANALYSIS ----------------------------------------------------------



pwr_10 <- function(x) {v <- x^.1
return(v)}



# PLOTTING DATA -----------------------------------------------------------


# plotting num of correlations for each sample
plot_obs_per_grp <-  function(dat) {
  
  p <- ggplot(dat, aes(fct_infreq(panel))) + 
    geom_bar(fill = "grey60", color = "grey30", width = .75, linewidth = .8) + coord_flip() + 
    theme_bw()+
    scale_x_discrete(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    labs(y = "# obs per panel", x = "panel")
  
  return(p)
  
}


# ANUSIC & SCHIMMACK ANALYSIS ---------------------------------------------




age_binning <- function(x, age_bin) {
  cut(x, breaks = seq(10, 90, by = age_bin), 
      labels = paste0(seq(10, 85, by =age_bin),
                      "-", c(seq((10+(age_bin-1)), 85-(age_bin-5), by = age_bin), 90)))
}




# AGGREGATION: GENERAL -------------------------------------------------------------

# creating time bins by rounding the retest intervals
# (e.g., if year_bin = 0.25/0.5/1 then that means binning in 3/6/12 months intervals)
time_binning <- function(x, year_bin) {
  t <- ceiling(x/year_bin)*year_bin
  return(t)}


# function by Pustejovsky, J.E. (2019). Sometimes, aggregating effect sizes is fine. https://www.jepusto.com/sometimes-aggregating-effect-sizes-is-fine/)
agg_cor <- function(yi, vi, r) {
  corr_mat <- r + diag(1 - r, nrow = length(vi))
  sd_mat <- tcrossprod(sqrt(vi))
  V_inv_mat <- chol2inv(chol(sd_mat * corr_mat))
  V <- 1 / sum(V_inv_mat)
  es <- V * sum(yi * V_inv_mat)
  return(es)
  
}

# function from: Pustejovsky, J.E. (2019). Sometimes, aggregating effect sizes is fine. https://www.jepusto.com/sometimes-aggregating-effect-sizes-is-fine/)

agg_v <- function(yi, vi, r) {
  corr_mat <- r + diag(1 - r, nrow = length(vi))
  sd_mat <- tcrossprod(sqrt(vi))
  V_inv_mat <- chol2inv(chol(sd_mat * corr_mat))
  V <- 1 / sum(V_inv_mat)
  return(V)
}




# INTERCORR LABELS --------------------------------------------------------


# renaming measure pairs 
measure_pair_lbl <- function(m1, m2) {
  
  lbl <- case_when(m1 == "fre" & m2 == "fre" ~ "Frequency_Frequency",
                   m1 == "beh" & m2 == "beh"  ~ "Behaviour_Behaviour",
                   m1 == "pro" & m2 == "pro" ~ "Propensity_Propensity",
                   (m1 == "beh" & m2 == "fre")|(m2 == "beh" & m1 == "fre") ~ "Frequency_Behaviour",
                   (m1 == "beh" & m2 == "pro")|(m2 == "beh" & m1 == "pro") ~ "Propensity_Behaviour",
                   (m1 == "fre" & m2 == "pro")|(m2 == "fre" & m1 == "pro") ~ "Propensity_Frequency")
  
  return(lbl)
}

# renaming domain-measure pairs 
name_lbl <- function(domain, measure) {
  
  lbl <-  case_when(domain == "inv" & measure == "pro" ~ "Propensity - Investment",
                    domain == "gam" & measure == "pro" ~ "Propensity - Gambling",
                    domain == "hea_gen" & measure == "pro" ~ "Propensity - Gen.Health",
                    domain == "gen" & measure == "pro" ~ "Propensity - General",
                    domain == "dri" & measure == "pro" ~ "Propensity - Driving",
                    domain == "occ" & measure == "pro" ~ "Propensity - Occupational",
                    domain == "rec" & measure == "pro" ~ "Propensity - Recreational",
                    domain == "eth" & measure == "pro" ~ "Propensity - Ethical",
                    domain == "soc" & measure == "pro" ~ "Propensity - Social",
                    
                    
                    domain == "ins" & measure == "beh" ~ "Behaviour - Insurance",
                    domain == "occ" & measure == "beh" ~ "Behaviour - Occupational",
                    domain == "inv" & measure == "beh" ~ "Behaviour - Investment",
                    domain == "gam" & measure == "beh" ~ "Behaviour - Gambling",
                    domain == "fin" & measure == "beh" ~ "Behaviour - Financial",
                    
                    domain == "alc" & measure == "fre" ~ "Frequency - Alcohol",
                    domain == "dri" & measure == "fre" ~ "Frequency - Driving",
                    domain == "dru" & measure == "fre" ~ "Frequency - Drugs",
                    domain == "eth" & measure == "fre" ~ "Frequency - Ethical",
                    domain == "gam" & measure == "fre" ~ "Frequency - Gambling",
                    domain == "occ" & measure == "fre" ~ "Frequency - Occupational",
                    domain == "sex" & measure == "fre" ~ "Frequency - Sexual.Intercourse",
                    domain == "smo" & measure == "fre" ~ "Frequency - Smoking")
  return(lbl)
}

# Sorting letters in a character string by alphabetical order
# Function by Chan, Martin; copied from https://gist.github.com/martinctc/56b3fb701a182f5b8dffceecd65b6d86
str_arrange <- function(x){
  x %>%
    stringr::str_split("") %>% # Split string into letters
    purrr::map(~sort(.) %>% paste(collapse = "")) %>% # Sort and re-combine
    as_vector() # Convert list into vector
}


# abbrev. labels
abbrev_lbl <- function(lbl) {
  
  abbrev_lbl <-  case_when(lbl == "Propensity"~ "pro" ,
                           lbl == "Frequency"~ "fre" ,
                           lbl == "Behaviour"~ "beh" ,
                           lbl == "Investment"~ "inv" ,
                           lbl == "Gambling"~ "gam" ,
                           lbl == "General"~ "gen" ,
                           lbl == "Financial"~ "fin" ,
                           lbl == "Driving"~ "dri" ,
                           lbl == "Occupational"~ "occ" ,
                           lbl == "Recreational"~ "rec" ,
                           lbl == "Ethical"~ "eth" ,
                           lbl == "Social"~ "soc" ,
                           lbl == "Insurance"~ "ins" ,
                           lbl == "Alcohol"~ "alc" ,
                           lbl == "Drugs"~ "dru" ,
                           lbl == "Sexual.Intercourse"~ "sex" ,
                           lbl == "Smoking"~ "smo" ,
                           lbl == "Gen.Health"~ "hea_gen")
  return(abbrev_lbl)
}



abbrev_rev_lbl <- function(lbl) {
  
  abbrev_lbl <-  case_when(lbl == "pro" ~"Propensity" ,
                           lbl == "fre" ~"Frequency" ,
                           lbl == "beh" ~ "Behaviour")
  return(abbrev_lbl)
}


# VARIANCE DECOMPOSITION ------------------------------------------

lbl_pred_replace <- function(x){
  x_lbl = case_when( x == "panel" ~ "Panel",
                     x == "sample_size" ~ "n",
                     x == "age_group" ~ "Age",
                     x == "item_num" ~ "Num. of Items",
                     x == "domain_name" ~ "Domain",
                     x == "gender_group" ~ "Gender",
                     x == "scale_type" ~ "Scale Type",
                     x == "scale_type_pair_same" ~ "Same Scale Type",
                     x == "domain_name_pair_same" ~ "Same Domain",
                     x == "measure_category_pair_same" ~ "Same Category",
                     x == "measure_category" ~ "Category",
                     x == "mean_rel" ~ "Reliability",
                     x == "item_num_type" ~ "Item Num. Pair Type",
                     x == "time_diff_mean" ~ "Retest Interval")
  
  return(x_lbl)
}


lbl_categ_replace <- function(x){
  x_categ = case_when( x == "panel" ~ "Panel",
                       x == "sample_size" ~ "Respondent",
                       x == "age_group" ~ "Respondent",
                       x == "gender_group" ~ "Respondent",
                       x == "domain_name" ~ "Measure",
                       x == "item_num" ~ "Measure",
                       x == "scale_type" ~ "Measure",
                       x == "scale_type_pair_same" ~ "Measure",
                       x == "domain_name_pair_same" ~ "Measure",
                       x == "measure_category_pair_same" ~ "Measure",
                       x == "measure_category" ~ "Measure",
                       x == "mean_rel" ~ "Measure",
                       x == "item_num_type" ~ "Measure",
                       x == "time_diff_mean" ~ "Measure")
  return(x_categ)
}





list_categorical_variables <- function(data) {
  # Find categorical variables
  categorical_vars <- sapply(data, is.character)
  
  # Extract unique values and count for each categorical variable
  variable_levels <- sapply(data[categorical_vars], function(x) length(unique(x)))
  
  # Create a dataframe with variable names and level counts
  result <- data.frame(Variable = names(variable_levels), Levels = variable_levels)
  
  return(result)
}





# BRMS MASC DIAGNOSTICS ---------------------------------------------------


library(brms)
library(posterior)
library(bayesplot)
library(rstanarm)
library(MetBrewer)


plot_rhat <- function(fit, regex) {
  
  
  rhats <- data.frame(rhat = brms::rhat(fit))
  df <- rownames_to_column(rhats, var = "variable") %>% 
    filter(grepl(regex, variable)) %>% 
    mutate(thresholds = case_when(rhat < 1.05 ~ "good",
                                  rhat >= 1.05 & rhat <= 1.1 ~ "ok",
                                  rhat > 1.1 ~ "too high"))
  
  
  blues <- color_scheme_get(scheme = "blue", i = c(6, 3, 1))
  blues <- unname(unlist(blues))
  
  
  p <- df %>% 
    ggplot(aes(y = variable, x = rhat, fill = thresholds)) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "grey50", size = .25) +
    geom_vline(xintercept = 1.05, linetype = "dashed", color = "grey50", size = .25) +
    geom_vline(xintercept = 1.1, linetype = "dashed", color = "grey50", size = .25) +
    geom_segment(aes(y=variable, yend=variable, x = 0.98, xend=rhat,  color = thresholds), size = .75) +
    geom_point(shape = 21, size = 2.5, stroke = .5, color = "grey70") + 
    scale_x_continuous(limits = c(0.98, max(1.15, max(df$rhat))), expand = c(0,0),breaks = c(1, 1.05, 1.1)) +
    theme_minimal() +
    labs(title = "",
         fill = "",
         color = "",
         y = "",
         x = "Rhat") +
    theme(legend.position = "right",
          legend.box.margin = margin(0,0,0,0),
          plot.title.position = "plot",
          legend.title = element_text( hjust = .5),
          panel.background = element_rect(fill = NA, color = "grey25", size = 1),
          panel.grid = element_blank(),
          title = element_text(face = "bold")) +
    scale_fill_manual(values=c("#011f4b", "#6497b1", "#d1e1ec"),
                      labels =c("good","ok","too high"),
                      limits = c("good","ok","too high"),
                      drop = FALSE) +
    scale_color_manual(values=c("#011f4b", "#6497b1", "#d1e1ec"),
                       labels = c("good","ok","too high"),
                       drop = FALSE) +
    guides(color = FALSE,
           fill = guide_legend(label.position = "right", title.position = "bottom", override.aes = list(size = 2.5)))
  
  return(p)
  
  
}


# function adapted from:
# Aki Vehtari, Andrew Gelman, Daniel Simpson, Bob Carpenter, and Paul-Christian Bürkner (2021). 
# Rank-normalization, folding, and localization: An improved R-hat for assessing convergence of
#  MCMC (with discussion). Bayesian Data Analysis. 16(2), 667-–718. doi:10.1214/20-BA1221
plot_change_ess <- function(fit, variable, regex, breaks, yaxis, ncol) {
  # yaxis <- match.arg(yaxis)
  sims <- as.array(fit, variable =  variable, regex = regex)
  var_names <- bind_rows(dimnames(sims)[3])$variable
  df_all <- NULL
  
  for (j in seq(dim(sims)[3])) {
    sims_j <- sims[, , j]  
    var <- var_names[j]
    iter_breaks <- round(breaks * NROW(sims_j))
    nbreaks <- length(iter_breaks)
    bulk_seff <- tail_seff <- bulk_reff <- 
      tail_reff <- rep(NA, length(nbreaks))
    
    for (i in seq_along(iter_breaks)) {
      
      sims_i <- sims_j[seq_len(iter_breaks[i]), ]
      nsamples <- prod(dim(sims_i))
      bulk_seff[i] <- ess_bulk(sims_i)
      tail_seff[i] <- ess_tail(sims_i)
      bulk_reff[i] <- bulk_seff[i] / nsamples
      tail_reff[i] <- tail_seff[i] / nsamples
      variable[i] <- var
      
    }
    
    df <- data.frame(
      breaks = breaks,
      ndraws = iter_breaks * NCOL(sims),
      seff = c(bulk_seff, tail_seff),
      reff = c(bulk_reff, tail_reff), 
      variable = variable,
      type = rep(c("bulk", "tail"), each = nbreaks))
    
    
    df_all <- rbind(df_all, df)
    
  }
  
  
  
  
  if (yaxis == "absolute") {
    p <- p <- ggplot(df_all, aes(x = ndraws, y = seff, color = type)) +
      labs(title = "Estimated ESS as a Function of Number of Draws", y ="ESS")
  }
  
  if (yaxis == "relative") {
    p <- p <- ggplot(df_all, aes(x = ndraws, y = reff, color = type)) +
      labs(title = "Estimated ESS/Draws as a Function of Number of Draws", y ="ESS/Draws")
  }
  
  
  p <- p +
    facet_wrap(.~variable, ncol = ncol) +  
    geom_line() +
    geom_point(size = .5) +
    theme_minimal() +
    labs(color = "Type",
         x = "Total number of draws") +
    theme(legend.position = "right",
          legend.box.margin = margin(0,0,0,0),
          plot.title.position = "plot",
          legend.title = element_text( hjust = .5),
          strip.text = element_text(size = 9.5, face = "bold"),
          panel.background = element_rect(fill = NA, color = "grey25", size = 1),
          title = element_text(face = "bold")) +
    scale_color_manual(values=met.brewer("Archambault")) +
    guides(fill = guide_legend(label.position = "right", title.position = "bottom", override.aes = list(size = 4)))
  
  return(p)
  
}



plot_psis_loo <- function(model_loo) {
  
  plot_df <- tibble(pareto_k = model_loo$diagnostics$pareto_k,
                    data_point = 1:length(model_loo$diagnostics$pareto_k))
  
  
  p <- ggplot(plot_df, aes(x = data_point, y = pareto_k)) +
    geom_point(shape = 3, size = 1.5, color = "#86bbbd", stroke = 1) +
    geom_hline(yintercept = .5, linetype = "dotted", color = "darkred") +
    geom_hline(yintercept = .7, linetype = "dashed", color = "darkred") +
    theme_linedraw() + labs(x = "Data point", y = "Pareto k", title = "Estimated Pareto k parameters")+
    theme(title = element_text(face = "bold"))
  
  p 
  
}





# README.MD FILE FORMATTING -----------------------------------------------



# function that reads main_data_files_codebook.xlsx and creates markdown output for the readme-md file (adapted from chatGPT output)

# Example usage
#file_path <- "main_data_files_codebook.xlsx"  # Replace with your CSV file path
#csv_to_markdown(file_path) # copy and paste console output into the readme-md file
csv_to_markdown <- function(file_path) {
  library(readxl)
  library(knitr)
  library(tidyverse)
  library(data.table)
  sheets <- length(excel_sheets(file_path))
  titles <- NULL
  for (i in 1:sheets) {
    
    suppressMessages({
      
      heading <- excel_sheets(file_path)[i]
      header = paste0("#### ", heading)
      
      data <- read_xlsx(file_path, skip = 0, n_max = 1, col_names = F, sheet = i)
      title = paste0("- **filename(s): ", data$...1, "**")
      
      
      data <- read_xlsx(file_path, skip = 1, n_max = 1, col_names = F, sheet = i)
      desc = paste0("- ", data$...1)
      
      data <- read_xlsx(file_path, skip = 2, n_max = 1, col_names = F, sheet = i)
      loc = paste0("- ", data$...1)
      
      # read table with file desc
      data <- read_xlsx(file_path, skip = 3, col_names = T, sheet = i)
      data <- data %>% mutate(column = paste0("`", column, "`"))
      
      # Convert the data frame to a markdown table
      markdown_table <- kable(data, format = "markdown")
      
      # Print the markdown table
      cat(header, "\n", title,"\n",  desc,"\n", loc,"\n", markdown_table, "\n <br><br> \n", sep = "\n")
      
      
      titles <-  tibble(file_name = paste0("- [", heading, "](#", tolower(heading), ")", "\n"),
                        main_loc = case_when(loc %like% "analysis"~ "##### Analysis",
                                             loc %like% "processing"~ "##### Processing", 
                                             TRUE ~ "##### Variable Info")) %>% bind_rows(titles)
    })
  }
  
  ert <- split(titles, titles$main_loc)
  result <- ""
  for (category in names(ert)) {
    # Add the category header
    result <- paste0(result, category, "\n")
    
    # Add each file under the category
    for (file in ert[[category]]$file_name) {
      result <- paste0(result, file)
    }
    
    # Add a newline for separation between categories
    result <- paste0(result, "\n")
  }
  # Print the result
  cat(result)
}


