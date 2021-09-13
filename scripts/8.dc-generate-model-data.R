#-------------------------#
#--------LIBRARIES--------#
#-------------------------#
library(dplyr)
# library(zoo)




#-------------------------#
#-----GLOBAL VARIABLES----#
#-------------------------#
# script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
# project_dir <- dirname(script_dir)
# setwd(project_dir)
# 
# source(file.path(script_dir, 'us-common-functions.R'))




#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
generate_daywise_model_data <- function() {
  physiological_data_path <- file.path(project_dir, curated_data_dir, physiological_data_dir)
  
  full_df <- custom_read_csv(file.path(physiological_data_path, full_df_file_name))
  mean_df <- custom_read_csv(file.path(physiological_data_path, qc1_normalized_mean_v1_file_name))
  
  
  
  # View(full_df)
  # View(mean_df)
}




#-------------------------#
#-------Main Program------#
#-------------------------#
generate_daywise_model_data()



