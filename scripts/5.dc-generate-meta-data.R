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

get_shift_val <- function(df, signal) {
  shift_val <- 0 
  
  if (min(df[[signal]], na.rm = TRUE) <= 0) { 
    shift_val <- abs(min(df[[signal]], na.rm = TRUE)) + 0.001 
  }
  
  print(paste0(signal, ' - ', shift_val))
  shift_val
}


generate_mean_data <- function(input_file_name, output_log_file_name, output_file_name) {
  df <- custom_read_csv(file.path(project_dir, curated_data_dir, physiological_data_dir, input_file_name))
  
  if (enable_log_transformation==TRUE) {
    df <- df %>% 
      mutate(PP=log(PP+get_shift_val(df, 'PP')), 
             E4_EDA=log(E4_EDA+get_shift_val(df, 'E4_EDA')),
             E4_HR=log(E4_HR+get_shift_val(df, 'E4_HR')),
             iWatch_HR=log(iWatch_HR+get_shift_val(df, 'iWatch_HR')),
      )
      # mutate(PP=log(PP) + get_shift_val(df, 'PP'),
      #        E4_EDA=log(E4_EDA) + get_shift_val(df, 'E4_EDA'),
      #        E4_HR=log(E4_HR) + get_shift_val(df, 'E4_HR'),
      #        iWatch_HR=log(iWatch_HR) + get_shift_val(df, 'iWatch_HR'),
      #        )
    
    convert_to_csv(df, file.path(project_dir, curated_data_dir, physiological_data_dir, output_log_file_name))
  }
  
  mean_df <- generate_mean_df(df)
  convert_to_csv(mean_df, file.path(project_dir, curated_data_dir, physiological_data_dir, output_file_name))
  
  return(mean_df)
}


generate_treatment_mean_data <- function() {
  qc1_mean_v1_df <<- generate_mean_data(qc1_normalized_file_name, qc1_log_trans_file_name, qc1_log_trans_mean_v1_file_name)
}



read_treatment_mean_files <- function() {
  qc1_mean_v1_df <<- custom_read_csv(file.path(project_dir, curated_data_dir, physiological_data_dir, qc1_normalized_mean_v1_file_name))
  # print_msg(colnames(qC1_df))  # "Participant_ID" "Day" "Treatment" "Timestamp" "Sinterface_Time" "TreatmentTime" "Raw_PP" "PP" "E4_HR" "E4_EDA" "iWatch_HR"
  # print_msg(head(qc1_df, 2))
}

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

# generate_daywise_mean_data <- function() {
#   subj_list <- unique(qc1_mean_v1_df$Participant_ID)
#   
#   sapply(subj_list, function(subj) {
#     qc1_mean_subj_df <- qc1_mean_v1_df %>%
#       filter(Participant_ID==subj & Treatment == 'WS')
#     
#     qc1_ws_mean_df <<- rbind.fill(qc1_ws_mean_df, qc1_mean_subj_df)
#     # convert_to_csv(qc1_mean_subj_df, file.path(curated_data_dir, physiological_data_dir, qc1_ws_mean_file_name))
#     
#     temp_qc1_mean_subj_df <- qc1_mean_subj_df %>%
#       filter(Day %in% c('Day1', 'Day2')) 
#     
#     # mutate(!!signal_name:=0)
#     # mutate(qc1_mean_subj_df[[signal_name]]=qc1_mean_subj_df[[signal_name]]-vanilla_day_mean_val)
#     # mutate(!!signal_name:=!!signal_name-vanilla_day_mean_val)
#     # mutate(!!signal_name:=case_when(is.na(!!signal_name) | is.na(vanilla_day_mean_val)~NaN,
#     #                     TRUE~!!signal_name-vanilla_day_mean_val))
#     
#     for (signal_name in signal_name_list) {
#       vanilla_day_mean_val = get_day3_day4_mean_val(qc1_mean_subj_df, signal_name)
#       print(paste(subj, signal_name, vanilla_day_mean_val))
#       
#       temp_qc1_mean_subj_df[temp_qc1_mean_subj_df$Day=="Day1", signal_name] = qc1_mean_subj_df[qc1_mean_subj_df$Day=="Day1", signal_name]-vanilla_day_mean_val
#       temp_qc1_mean_subj_df[temp_qc1_mean_subj_df$Day=="Day2", signal_name] = qc1_mean_subj_df[qc1_mean_subj_df$Day=="Day2", signal_name]-vanilla_day_mean_val
#     }
#     
#     qc1_deadline_mean_df <<- rbind.fill(qc1_deadline_mean_df, temp_qc1_mean_subj_df)
#   })
#   
#   convert_to_csv(qc1_ws_mean_df, file.path(curated_data_dir, physiological_data_dir, qc1_ws_mean_file_name))
#   convert_to_csv(qc1_deadline_mean_df, file.path(curated_data_dir, physiological_data_dir, qc1_deadline_mean_file_name))
# }



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


#-------------------------#
#-------Main Program------#
#-------------------------#
# generate_treatment_mean_data()
# # read_treatment_mean_files()
# 
# generate_daywise_mean_data()





