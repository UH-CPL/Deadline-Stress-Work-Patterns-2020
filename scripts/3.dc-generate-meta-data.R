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

mean_log_file <- file.path(project_dir, log_dir, paste0('mean-log-', format(Sys.Date(), format='%m-%d-%y'), '.txt'))
file.create(mean_log_file)




#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
# process_mean_data <- function() {
#   qc0_df <- custom_read_csv(file.path(curated_data_dir, physiological_data_dir, qc0_file_name))
#   
#   qc0_session_mean_df <- qc0_df %>%
#     select(-Timestamp, -Sinterface_Time, -TreatmentTime, 
#            -Activities, -Activities_QC1, -Activities_QC2, 
#            -Application, -Application_QC1, 
#            -Application_QC2, -Application_QC3) %>%
#     group_by(Participant_ID, Day, Treatment) %>%
#     summarize_all(mean, na.rm=T) %>%
#     ungroup()
#   
#   View(qc0_session_mean_df)
#   convert_to_csv(qc0_session_mean_df, file.path(curated_data_dir, physiological_data_dir, qc0_session_mean_file_name))
#   
#   
#   
#   
#   qc0_activity_mean_df <- qc0_df %>%
#     select(-Timestamp, -Sinterface_Time, -TreatmentTime, 
#            -Activities, -Activities_QC1,
#            -Application, -Application_QC1, 
#            -Application_QC2, -Application_QC3) %>%
#     group_by(Participant_ID, Day, Treatment, Activities_QC2) %>%
#     summarize_all(mean, na.rm=T) %>%
#     ungroup()
#   
#   View(qc0_activity_mean_df)
#   convert_to_csv(qc0_activity_mean_df, file.path(curated_data_dir, physiological_data_dir, qc0_activity_mean_file_name))
# }



generate_mean_df <- function(df) {
  mean_df <- df %>%
    select(-Timestamp, -Sinterface_Time, -TreatmentTime) %>%
    group_by(Participant_ID,	Day, Treatment) %>%
    summarize_all(mean, na.rm=T) %>%
    ungroup()
  
  return(mean_df)
}


generate_mean_data <- function(input_file_name, output_file_name) {
  df <- custom_read_csv(file.path(project_dir, curated_data_dir, physiological_data_dir, input_file_name))
  mean_df <- generate_mean_df(df)
  convert_to_csv(mean_df, file.path(project_dir, curated_data_dir, physiological_data_dir, output_file_name))
}


generate_mean_data_all <- function() {
  generate_mean_data(qc1_file_name, qc1_treatment_mean_file_name)
}








#-------------------------#
#-------Main Program------#
#-------------------------#
# process_mean_data()

generate_mean_data_all()





