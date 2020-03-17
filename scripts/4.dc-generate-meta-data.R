#-------------------------#
#--------LIBRARIES--------#
#-------------------------#
library(tidyverse) 
library(dplyr)
library(plyr) 



enable_log_transformation=TRUE

#-------------------------#
#-----GLOBAL VARIABLES----#
#-------------------------#
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
project_dir <- dirname(script_dir)

source(file.path(script_dir, 'us-common-functions.R'))

mean_log_file <- file.path(project_dir, log_dir, paste0('mean-log-', format(Sys.Date(), format='%m-%d-%y'), '.txt'))
file.create(mean_log_file)




qc1_mean_df <- tibble()
qc1_ws_mean_df <- tibble()
qc1_deadline_mean_df <- tibble()


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
    # select(-Timestamp, -Sinterface_Time, -TreatmentTime) %>%
    select(Participant_ID,	Day, Treatment, Mask, PP, E4_HR, E4_EDA, iWatch_HR) %>%
    group_by(Participant_ID,	Day, Treatment) %>%
    filter(Mask==1) %>%
    summarize_all(mean, na.rm=T) %>%
    ungroup()
  
  return(mean_df)
}


generate_mean_data <- function(input_file_name, output_log_file_name, output_file_name) {
  df <- custom_read_csv(file.path(project_dir, curated_data_dir, physiological_data_dir, input_file_name))
  if (enable_log_transformation==TRUE) {
    pp_shift_val <- 0 
    eda_shift_val <- 0 
    
    if (min(df['PP'], na.rm = TRUE) <= 0) { 
      pp_shift_val <- min(df['PP'], na.rm = TRUE) + 0.001 
    }
    
    if (min(df['E4_EDA'], na.rm = TRUE) <= 0) { 
      eda_shift_val <- min(df['E4_EDA'], na.rm = TRUE) + 0.001 
    }
    
    print(head(df, 2))
    df <- df %>% 
      mutate(PP=log(PP)+pp_shift_val, 
             E4_EDA=log(E4_EDA)+eda_shift_val) 
    # %>% 
    #   mutate(PP=PP-min(PP, na.rm = TRUE), 
    #          E4_EDA=E4_EDA-min(E4_EDA, na.rm = TRUE))
    print(head(df, 2))
    
    convert_to_csv(df, file.path(project_dir, curated_data_dir, physiological_data_dir, output_log_file_name))
  }
  
  mean_df <- generate_mean_df(df)
  convert_to_csv(mean_df, file.path(project_dir, curated_data_dir, physiological_data_dir, output_file_name))
  
  return(mean_df)
}


generate_treatment_mean_data <- function() {
  # qc0_mean_df <<- generate_mean_data(qc0_final_file_name, qc0_log_transformed_file_name, qc0_treatment_mean_file_name)
  qc1_mean_df <<- generate_mean_data(qc1_file_name, qc1_log_transformed_file_name, qc1_treatment_mean_file_name)
}



read_treatment_mean_files <- function() {
  qc1_mean_df <<- custom_read_csv(file.path(project_dir, curated_data_dir, physiological_data_dir, qc1_treatment_mean_file_name))
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

generate_daywise_mean_data <- function() {
  subj_list <- unique(qc1_mean_df$Participant_ID)
  
  sapply(subj_list, function(subj) {
    qc1_mean_subj_df <- qc1_mean_df %>%
      filter(Participant_ID==subj & Treatment == 'WS')
    
    qc1_ws_mean_df <<- rbind.fill(qc1_ws_mean_df, qc1_mean_subj_df)
    # convert_to_csv(qc1_mean_subj_df, file.path(curated_data_dir, physiological_data_dir, qc1_ws_mean_file_name))
    
    temp_qc1_mean_subj_df <- qc1_mean_subj_df %>%
      filter(Day %in% c('Day1', 'Day2')) 
    
    # mutate(!!signal_name:=0)
    # mutate(qc1_mean_subj_df[[signal_name]]=qc1_mean_subj_df[[signal_name]]-vanilla_day_mean_val)
    # mutate(!!signal_name:=!!signal_name-vanilla_day_mean_val)
    # mutate(!!signal_name:=case_when(is.na(!!signal_name) | is.na(vanilla_day_mean_val)~NaN,
    #                     TRUE~!!signal_name-vanilla_day_mean_val))
    
    for (signal_name in signal_name_list) {
      vanilla_day_mean_val = get_day3_day4_mean_val(qc1_mean_subj_df, signal_name)
      print(paste(subj, signal_name, vanilla_day_mean_val))
      
      temp_qc1_mean_subj_df[temp_qc1_mean_subj_df$Day=="Day1", signal_name] = qc1_mean_subj_df[qc1_mean_subj_df$Day=="Day1", signal_name]-vanilla_day_mean_val
      temp_qc1_mean_subj_df[temp_qc1_mean_subj_df$Day=="Day2", signal_name] = qc1_mean_subj_df[qc1_mean_subj_df$Day=="Day2", signal_name]-vanilla_day_mean_val
    }
    
    qc1_deadline_mean_df <<- rbind.fill(qc1_deadline_mean_df, temp_qc1_mean_subj_df)
  })
  
  convert_to_csv(qc1_ws_mean_df, file.path(curated_data_dir, physiological_data_dir, qc1_ws_mean_file_name))
  convert_to_csv(qc1_deadline_mean_df, file.path(curated_data_dir, physiological_data_dir, qc1_deadline_mean_file_name))
}


#-------------------------#
#-------Main Program------#
#-------------------------#
# process_mean_data()

generate_treatment_mean_data()
# read_treatment_mean_files()

generate_daywise_mean_data()





