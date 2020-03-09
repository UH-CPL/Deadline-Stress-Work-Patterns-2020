#-------------------------#
#--------LIBRARIES--------#
#-------------------------#
library(tidyverse) 
library(dplyr)



#-------------------------#
#-----GLOBAL VARIABLES----#
#-------------------------#
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
project_dir <- dirname(script_dir)

source(file.path(script_dir, 'us-common-functions.R'))




#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
generate_mean_data <- function(df) {
  mean_df <- df %>%
    select(-Timestamp, -Sinterface_Time, -TreatmentTime) %>%
    group_by(Participant_ID,	Day, Treatment) %>%
    summarize_all(mean, na.rm=T) %>%
    ungroup()
  
  return(mean_df)
}


generate_mean_file <- function(input_file_name, output_file_name) {
  df <- custom_read_csv(file.path(project_dir, curated_data_dir, physiological_data_dir, input_file_name))
  mean_df <- generate_mean_data(df)
  convert_to_csv(mean_df, file.path(project_dir, curated_data_dir, physiological_data_dir, output_file_name))
}


generate_mean_files <- function() {
  generate_mean_file(qc1_file_name, qc1_treatment_mean_file_name)
}



#-------------------------#
#-------Main Program------#
#-------------------------#
generate_mean_files()





