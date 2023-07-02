
#  DESCRIPTION -------------------------------------------------------------

# This script reads the list of risk-related measures for each panel and combines it into a single codebook for processing

# Author(s): Alexandra Bagaini and Rui Mata, Centre for Cognitive and Decision Sciences, Faculty of Psychology, University of Basel.


# PACKAGES ---------------------------------------------------------------

library(tidyverse) # data wrangling



# PATH INFORMATION -----------------------------------------------------

file_list <- tibble(file_path =
                      list.files(path = "var_info/indv_panel_var_info/", pattern = "risk_var_info.csv", 
                                 all.files = TRUE,
                                 recursive = TRUE, full.names = TRUE)) 


# VARIABLE INFORMATION ----------------------------------------------------
panel_risk_info <- list()
for (curr_file in file_list$file_path) {
  
  filename <- basename(curr_file) # extract filename
  panel_name <- sub("_risk_var_info.csv$", "", filename) 
  
  
  #create list for each panel with sheets as tibbles
  panel_data <- tibble(read_csv(curr_file, show_col_types = FALSE))
  
  
  # put all panels together
  panel_risk_info[[panel_name]] <- panel_data
  
}



write_rds(panel_risk_info, file = "var_info/panel_risk_info.rds")

print("combining risk info done!")
