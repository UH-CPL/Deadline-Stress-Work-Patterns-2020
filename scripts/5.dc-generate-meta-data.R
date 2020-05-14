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


# signal_name_list <- c('PP')
signal_name_list <- c('PP', 'E4_HR', 'E4_EDA', 'iWatch_HR')



input_file_name <- qc1_log_trans_file_name 
treatement_mean_file_name <- qc1_log_trans_mean_v1_file_name
daywise_mean_file_name <- qc1_log_trans_mean_v2_file_name

chunk_mean_file_name <- remove_rigth_substr(qc1_log_trans_mean_chunk_file_name, 4)

#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
generate_treatment_mean_data <- function() {
  df <- custom_read_csv(file.path(project_dir, curated_data_dir, physiological_data_dir, input_file_name))
  mean_df <<- generate_mean_df(df)
  convert_to_csv(mean_df, file.path(project_dir, curated_data_dir, physiological_data_dir, treatement_mean_file_name))
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
  # print(head(qc1_mean_v1_df, 2))
  
  mean_long_df <<- mean_df %>%
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
    mean_long_df <<- mean_long_df %>%
      mutate(Day1_Normalize=Day1-Day3_Day4_Mean,
             Day2_Normalize=Day2-Day3_Day4_Mean)

  } else if (t_test_comparison==day3_day4_ws_min) {
    mean_long_df <<- mean_long_df %>%
      mutate(Day1_Normalize=Day1-Day3_Day4_Min,
             Day2_Normalize=Day2-Day3_Day4_Min)
  }

  convert_to_csv(mean_long_df, file.path(curated_data_dir, physiological_data_dir, daywise_mean_file_name))
  
}


generate_chunk_mean_df <- function(df, chunk_size_minute, signal) {
  chunk_mean_df <- tibble()
  chunk_size_sec <- chunk_size_minute*60
  
  print(paste0('For chunk size: ', chunk_size_minute, ' minute'))
  print('-----------------------------------------------------')
  
  for (subj in unique(df$Participant_ID)) {
  # for (subj in c('T001')) {
    
    print('')
    print(subj)
    print('-------')
    
    subj_df <- df %>% 
      filter(Participant_ID==subj)
    
    
    for (day in unique(subj_df$Day)) {
    # for (day in c('Day1')) {
      
      print(day)
      day_df <- subj_df %>% 
        filter(Day==day) 
      # %>% 
      #   filter(Mask==1)
      
      # # total_row <- nrow(day_df)
      # # print(total_row)
      # # print(day_df$TreatmentTime[nrow(day_df)])
      
      i <- 0
      end_treatment_sec <- day_df$TreatmentTime[nrow(day_df)]
      
      while (i*chunk_size_sec<end_treatment_sec) {
      # while (i<10) {
        
        temp_chunk_df <- day_df %>% 
          filter(TreatmentTime>=i*chunk_size_sec & TreatmentTime<=(i+1)*chunk_size_sec-1)
        
        if (nrow(temp_chunk_df)>1) {
          temp_chunk_df <- temp_chunk_df %>% 
            select(Participant_ID,	Day, Treatment, Mask, !!signal) %>% 
            group_by(Participant_ID,	Day, Treatment, Mask) %>% 
            summarize_all(list(
              Mean_Val = ~mean(., na.rm=TRUE),
              Total_Rows = ~n(),
              Total_Non_NA_Rows = ~sum(!is.na(.))
              
              ### Median = ~median(., na.rm=TRUE),
              ### Sd = ~sd(., na.rm=TRUE),
              
            )) %>%
            mutate(
              SampleNo=i+1,
              StartTreatmentTime=i*chunk_size_sec,
              EndTreatmentTime=min((i+1)*chunk_size_sec-1, end_treatment_sec),
              DiffTreatmentTime=EndTreatmentTime-StartTreatmentTime
            )
          ## ungroup() %>% 
          
          
          chunk_mean_df <- rbind.fill(chunk_mean_df, temp_chunk_df)
          
          ## print(paste(i, i*chunk_size_sec, (i+1)*chunk_size_sec-1))
          ## print(temp_chunk_df)
        }
         
        i=i+1 
      }
    }
  }
   
  chunk_mean_df 
}

generate_ws_chunk_mean_data <- function() {
  df <- custom_read_csv(file.path(project_dir, curated_data_dir, physiological_data_dir, input_file_name))
  
  for (signal in signal_name_list) {
    for (chunk_size in chunk_sizes) {
      mean_chunk_df <- generate_chunk_mean_df(df, chunk_size, signal)
      convert_to_csv(mean_chunk_df, file.path(project_dir, 
                                              curated_data_dir, 
                                              physiological_data_dir, 
                                              paste0(chunk_mean_file_name, '_', signal, '_', chunk_size, '_minute.csv')))
    }
  }
  
}


process_normalized_mean_data <- function() {
  generate_treatment_mean_data()
  generate_daywise_mean_data()
}

#-------------------------#
#-------Main Program------#
#-------------------------#
# process_normalized_mean_data()



# chunk_sizes <- c(1)
# chunk_sizes <- c(5, 10, 15)
# generate_ws_chunk_mean_data()





