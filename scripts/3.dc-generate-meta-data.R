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




qc1_mean_df <- tibble()


# signal_name_list <- c('PP')
signal_name_list <- c('PP', 'E4_HR', 'E4_EDA', 'iWatch_HR')




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
  
  return(mean_df)
}


generate_treatment_mean_data <- function() {
  qc1_mean_df <<- generate_mean_data(qc1_file_name, qc1_treatment_mean_file_name)
}



# read_treatment_mean_files <- function() {
#   qc1_mean_df <<- custom_read_csv(file.path(project_dir, curated_data_dir, physiological_data_dir, qc1_treatment_mean_file_name))
#   # print_msg(colnames(qC1_df))  # "Participant_ID" "Day" "Treatment" "Timestamp" "Sinterface_Time" "TreatmentTime" "Raw_PP" "PP" "E4_HR" "E4_EDA" "iWatch_HR"
#   # print_msg(head(qc1_df, 2))
# }

get_signal_val <- function(df, day, signal_name) {
  # print(df[df$Day==day, signal_name])
  return(df[df$Day==day, signal_name])
}

generate_daywise_mean_data <- function() {
  subj_list <- unique(qc1_mean_df$Participant_ID)
  
  sapply(subj_list, function(subj) {
    qc1_mean_subj_df <- qc1_mean_df %>%
      filter(Participant_ID==subj & Treatment == 'WS')
    
    for (signal_name in signal_name_list) {
      
      day3_val = get_signal_val(qc1_mean_subj_df, 'Day3', signal_name)
      day4_val = get_signal_val(qc1_mean_subj_df, 'Day4', signal_name)
      vanilla_day_mean_val=(day3_val+day4_val)/2
      
      print(paste(subj, signal_name, vanilla_day_mean_val))
      
      
      # print(qc1_mean_subj_df)
      qc1_mean_subj_df <- qc1_mean_subj_df %>%
        filter(Day %in% c('Day1', 'Day2')) %>%
        mutate(!!signal_name:=0)
        # mutate(!!signal_name:=case_when(is.na(!!signal_name) | is.na(vanilla_day_mean_val)~NaN,
        #                     TRUE~!!signal_name-vanilla_day_mean_val))
      print(qc1_mean_subj_df)
    }
    
  })
  
  # qc1_deadline_mean_df <<- qc1_mean_df %>%
  #   filter(Treatment == 'WS') %>% 
  #   group_by(Participant_ID) %>% 
  #   summarise_at(vars(signal_name_list), funs(mean=get_mean, frames=get_total_frame))
}


#-------------------------#
#-------Main Program------#
#-------------------------#
# process_mean_data()


# generate_treatment_mean_data()
read_treatment_mean_files()

generate_daywise_mean_data()





