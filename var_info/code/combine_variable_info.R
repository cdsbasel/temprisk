
#  DESCRIPTION -------------------------------------------------------------

# This script reads the variable data for each panel and combines it into a single variable codebook for preprocessing

# Author(s): Alexandra Bagaini and Rui Mata, Centre for Cognitive and Decision Sciences, Faculty of Psychology, University of Basel.


# PACKAGES ---------------------------------------------------------------

library(tidyverse) # data wrangling
library(readxl) # reading excel file


# PATH INFORMATION -----------------------------------------------------

file_list <- tibble(file_path =
                      list.files(path = "var_info/indv_panel_var_info/", pattern = "VariableInfo.xlsx", 
                                 all.files = TRUE,
                                 recursive = TRUE, full.names = TRUE))


# VARIABLE INFORMATION ----------------------------------------------------
panel_variable_info <- list()
for (curr_file in file_list$file_path) {
  
  filename <- basename(curr_file) # extract filename
  panel_name <- sub("_VariableInfo.xlsx$", "", filename) 
  
  sheets <- excel_sheets(curr_file)
  
  panel_data <- list()
  for (curr_sheet in sheets) {
    
    
    panel_data[[curr_sheet]] <- tibble(read_excel(curr_file, sheet = curr_sheet))
    
  }
  
  panel_variable_info[[panel_name]] <- panel_data
}



write_rds(panel_variable_info, file = "var_info/panel_variable_info.rds")


print("combining variable info done!")
