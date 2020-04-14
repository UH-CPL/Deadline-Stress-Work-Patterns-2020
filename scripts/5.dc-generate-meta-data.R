#-------------------------#
#--------LIBRARIES--------#
#-------------------------#
library(tidyverse) 
library(dplyr)
library(plyr) 




#-------------------------#
#-----GLOBAL VARIABLES----#
#-------------------------#
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
project_dir <- dirname(script_dir)


source(file.path(script_dir, 'us-common-functions.R'))

mean_log_file <- file.path(project_dir, log_dir, paste0('mean-log-', format(Sys.Date(), format='%m-%d-%y'), '.txt'))
file.create(mean_log_file)




qc1_mean_v1_df <- tibble()
qc1_ws_mean_df <- tibble()
qc1_deadline_mean_df <- tibble()


# signal_name_list <- c('PP')
signal_name_list <- c('PP', 'E4_HR', 'E4_EDA', 'iWatch_HR')




#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
generate_mean_df <- function(df) {
  mean_df <- df %>%
    # select(-Timestamp, -Sinterface_Time, -TreatmentTime) %>%
    select(Participant_ID,	Day, Treatment, Mask, PP, E4_HR, E4_EDA, iWatch_HR) %>%
    group_by(Participant_ID,	Day, Treatment) %>%
    filter(Mask==1) %>%
    summarize_all(mean, na.rm=T) %>%
    ungroup() %>% 
    select(-Mask)
  
  return(mean_df)
}




generate_mean_data <- function(input_file_name, output_file_name) {
  df <- custom_read_csv(file.path(project_dir, curated_data_dir, physiological_data_dir, input_file_name))
  
  mean_df <- generate_mean_df(df)
  convert_to_csv(mean_df, file.path(project_dir, curated_data_dir, physiological_data_dir, output_file_name))
  
  return(mean_df)
}


generate_treatment_mean_data <- function() {
  qc1_mean_v1_df <<- generate_mean_data(qc1_log_trans_file_name, qc1_log_trans_mean_v1_file_name)
}



# read_treatment_mean_files <- function() {
#   qc1_mean_v1_df <<- custom_read_csv(file.path(project_dir, curated_data_dir, physiological_data_dir, qc1_normalized_mean_v1_file_name))
#   # print_msg(colnames(qC1_df))  # "Participant_ID" "Day" "Treatment" "Timestamp" "Sinterface_Time" "TreatmentTime" "Raw_PP" "PP" "E4_HR" "E4_EDA" "iWatch_HR"
#   # print_msg(head(qc1_df, 2))
# }

get_signal_val <- function(df, day, signal_name) {
  # print(df[df$Day==day, signal_name])
  return(df[df$Day==day, signal_name])
}

get_day3_day4_mean_val <- function(df, signal_name) {
  day3_val = get_signal_val(df, 'Day3', signal_name)
  day4_val = get_signal_val(df, 'Day4', signal_name)
  
  # print(paste('day3_val: ', day3_val, 'day4_val: ' , day4_val))
  
  if(!is_null(day3_val) & !is_null(day3_val)) {
    return(vanilla_day_mean_val=(day3_val+day4_val)/2)
  } else if(!is_null(day3_val)) {
    return(day3_val)
  } else if(!is_null(day4_val)) {
    return(day4_val)
  }
  
  return(NaN)
}



generate_daywise_mean_data <- function() {
  # print(head(qc1_mean_v1_df, 2))
  
  qc1_mean_long_df <<- qc1_mean_v1_df %>%
    # filter(Treatment == 'WS') %>%
    gather(Signal, Mean_Value, -Participant_ID, -Day, -Treatment) %>% 
    spread(Day, Mean_Value) %>%
    mutate(Day3_Day4_Mean = case_when(
      !is.na(Day3) & !is.na(Day4)~(Day3+Day4)/2,
      !is.na(Day3)~Day3,
      !is.na(Day4)~Day4,
      TRUE~Day3)) %>%
    mutate(Day3_Day4_Min = pmin(Day3, Day4, na.rm = TRUE))
  
  if (t_test_comparison==day3_day4_ws_mean) {
    qc1_mean_long_df <<- qc1_mean_long_df %>%
      mutate(Day1_Normalize=Day1-Day3_Day4_Mean,
             Day2_Normalize=Day2-Day3_Day4_Mean)

  } else if (t_test_comparison==day3_day4_ws_min) {
    qc1_mean_long_df <<- qc1_mean_long_df %>%
      mutate(Day1_Normalize=Day1-Day3_Day4_Min,
             Day2_Normalize=Day2-Day3_Day4_Min)
  }
    
  
  convert_to_csv(qc1_mean_long_df, file.path(curated_data_dir, physiological_data_dir, qc1_log_trans_mean_v2_file_name))
  
}



generate_ws_chunk_mean_data <- function() {
  
}

#-------------------------#
#-------Main Program------#
#-------------------------#
# generate_treatment_mean_data()
# # read_treatment_mean_files()
# 
# generate_daywise_mean_data()





