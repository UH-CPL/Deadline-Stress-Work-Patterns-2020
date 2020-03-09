#-------------------------#
#--------LIBRARIES--------#
#-------------------------#
library(dplyr)






#-------------------------#
#-----GLOBAL VARIABLES----#
#-------------------------#
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
source(file.path(script_dir, 'us-common-functions.R'))

current_dir <- dirname(script_dir)
setwd(current_dir)

mean_log_file <- file.path(log_dir, paste0('mean-log-', format(Sys.Date(), format='%m-%d-%y'), '.txt'))
file.create(mean_log_file)



#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
process_mean_data <- function() {
  qc0_df <- custom_read_csv(file.path(curated_data_dir, physiological_data_dir, qc0_file_name))
  
  qc0_session_mean_df <- qc0_df %>%
    select(-Timestamp, -Sinterface_Time, -TreatmentTime, 
           -Activities, -Activities_QC1, -Activities_QC2, 
           -Application, -Application_QC1, 
           -Application_QC2, -Application_QC3) %>%
    group_by(Participant_ID, Day, Treatment) %>%
    summarize_all(mean, na.rm=T) %>%
    ungroup()
  
  View(qc0_session_mean_df)
  convert_to_csv(qc0_session_mean_df, file.path(curated_data_dir, physiological_data_dir, qc0_session_mean_file_name))
  
  
  
  
  qc0_activity_mean_df <- qc0_df %>%
    select(-Timestamp, -Sinterface_Time, -TreatmentTime, 
           -Activities, -Activities_QC1,
           -Application, -Application_QC1, 
           -Application_QC2, -Application_QC3) %>%
    group_by(Participant_ID, Day, Treatment, Activities_QC2) %>%
    summarize_all(mean, na.rm=T) %>%
    ungroup()
  
  View(qc0_activity_mean_df)
  convert_to_csv(qc0_activity_mean_df, file.path(curated_data_dir, physiological_data_dir, qc0_activity_mean_file_name))
}



#-------------------------#
#-------Main Program------#
#-------------------------#
process_mean_data()

