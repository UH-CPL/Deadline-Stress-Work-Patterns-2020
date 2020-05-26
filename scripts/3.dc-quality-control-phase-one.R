#-------------------------#
#--------LIBRARIES--------#
#-------------------------#
library(tidyverse) 
library(dplyr)
library(plyr) 



#-------------------------#
#-----GLOBAL VARIABLES----#
#-------------------------#
# script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
# project_dir <- dirname(script_dir)
# 
# source(file.path(script_dir, 'us-common-functions.R'))

qc1_log_file <- file.path(project_dir, log_dir, paste0('quality-control-phase-one-log-', format(Sys.Date(), format='%m-%d-%y'), '.txt'))
file.create(qc1_log_file)


qc0_df <- tibble()
qc1_df <- tibble()


# signal_name_list <- c('PP')
signal_name_list <- c('PP', 'E4_HR', 'E4_EDA', 'iWatch_HR')




#-------------------------#
#------  Functions  ------#
#-------------------------#
#--- CHANGE HERE ---#
read_data <- function() {
  qc0_df <<- custom_read_csv(file.path(project_dir, curated_data_dir, physiological_data_dir, qc0_final_file_name)) %>% 
    select(Participant_ID, Day, Treatment, TreatmentTime, PP, E4_HR, E4_EDA, iWatch_HR, Mask)
  
  # print_msg(colnames(qc0_df))  # "Participant_ID" "Day" "Treatment" "TreatmentTime" "PP" "E4_HR" "E4_EDA" "iWatch_HR"
  # print_msg(head(qc0_df, 2))
}


get_valid_range <- function(signal) {
  if (grepl('HR', signal)) {
    return(c(40, 140))
  } else if (grepl('EDA', signal)) {
    return(c(0.01, 100))
  } else if (grepl('BR', signal)) {
    return(c(4, 40))
  } 
  
  return(c(0, 100))
}

#--- CHANGE HERE ---#
remove_data_out_of_range <- function() {
  filtered_df <- tibble()
  qc1_df <<- qc0_df
  
  for (signal in signal_name_list) {
    range_list <- get_valid_range(signal)
    
    temp_filtered_df <- qc1_df %>% 
      select(Participant_ID, Day, Treatment, TreatmentTime, !!signal) %>% 
      filter(qc1_df[[signal]] < range_list[1] | qc1_df[[signal]] > range_list[2]) %>% 
      gather(Signal_Name, Value, !!signal)
    
    filtered_df <- rbind.fill(filtered_df, temp_filtered_df)
    
    if (nrow(temp_filtered_df)>0) {
      qc1_df[[signal]][qc1_df[[signal]] < range_list[1] | qc1_df[[signal]] > range_list[2]] <<- NA
    }
  }
  
  # print(filtered_df)
  
  convert_to_csv(qc1_df, file.path(project_dir, curated_data_dir, physiological_data_dir, qc1_file_name))
  convert_to_csv(filtered_df, file.path(project_dir, curated_data_dir, physiological_data_dir, qc1_filtered_data_file_name))
}


# log_transform_linear_model <- function() {
#   qc1_lm_df <- custom_read_csv(file.path(project_dir, curated_data_dir, physiological_data_dir, qc1_file_name)) %>%
#     mutate(PP=log(PP+get_shift_val(qc1_df, 'PP')), 
#            E4_EDA=log(E4_EDA+get_shift_val(qc1_df, 'E4_EDA')),
#            E4_HR=log(E4_HR+get_shift_val(qc1_df, 'E4_HR')),
#            iWatch_HR=log(iWatch_HR+get_shift_val(qc1_df, 'iWatch_HR')),
#     )
#   
#   convert_to_csv(qc1_lm_df, file.path(project_dir, curated_data_dir, physiological_data_dir, qc1_lm_file_name))
# }


#--- CHANGE HERE ---#
process_quality_control_phase_one <- function() {
  read_data()
  remove_data_out_of_range()
  generate_mean_data(input_file_name=qc1_file_name, 
                     output_v1_file_name=qc1_raw_mean_v1_file_name, 
                     output_v2_file_name=qc1_raw_mean_v2_file_name)
}


#------------ Don't Delete ------------#
# process_qc1_mean_data <- function() {
#   generate_mean_data(input_file_name=qc1_file_name, 
#                      output_v1_file_name=qc1_raw_mean_v1_file_name, 
#                      output_v2_file_name=qc1_raw_mean_v2_file_name)
# }

#------------ Don't Delete ------------#
# process_lm_mean_data <- function() {
#   log_transform_linear_model()
#   
#   generate_mean_data(input_file_name=qc1_lm_file_name,
#                      output_v1_file_name=qc1_lm_mean_v1_file_name,
#                      output_v2_file_name=qc1_lm_mean_v2_file_name)
# }

#-------------------------#
#-------Main Program------#
#-------------------------#
# process_quality_control_phase_one()



### process_qc1_mean_data()
### process_lm_mean_data






