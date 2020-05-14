#-------------------------#
#--------LIBRARIES--------#
#-------------------------#


#-------------------------#
#-----GLOBAL VARIABLES----#
#-------------------------#
# script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
# project_dir <- dirname(script_dir)
# 
# source(file.path(script_dir, 'us-common-functions.R'))



full_df <- tibble()
variance_test_df <- tibble()


signal_name_list <- c('PP')
# signal_name_list <- c('PP', 'E4_HR', 'E4_EDA', 'iWatch_HR')




#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
read_data <- function() {
  full_df <<- custom_read_csv(file.path(project_dir, curated_data_dir, physiological_data_dir, qc1_log_trans_file_name))
}

variance_test <- function(df) {
  # print(head(df, 2))
  
  for (signal_name in signal_name_list) {
    for (deadline_day in c('Day1', 'Day2')) {
      for (vanilla_day in c('Day3', 'Day4')) {
        
        deadline_day_df <- df %>% 
          filter(Day==deadline_day)
        
        vanilla_day_df <- df %>% 
          filter(Day==vanilla_day)
        
        # print(signal_name)
        # print(head(deadline_day_df, 2))
        # print(head(vanilla_day_df, 2))
        
        variance_val <- var.test(deadline_day_df[[signal_name]], vanilla_day_df[[signal_name]])$p.value
        variance_greater_val <- var.test(deadline_day_df[[signal_name]], vanilla_day_df[[signal_name]], alternative='greater')$p.value
        variance_less_val <- var.test(deadline_day_df[[signal_name]], vanilla_day_df[[signal_name]], alternative='less')$p.value
        
        
        # print(paste(variance_val, variance_greater_val, variance_less_val))
        
        variance_test_df <<- rbind.fill(variance_test_df, tibble(Participant_ID = unique(df$Participant_ID),
                                                                 Signal = signal_name,
                                                                 Day = paste0(deadline_day, ' - ', vanilla_day),
                                                                 deadline_n = nrow(deadline_day_df),
                                                                 vanilla_n = nrow(vanilla_day_df),
                                                                 
                                                                 variance_val = variance_val,
                                                                 variance_val_sig = get_significance_sign(variance_val),
                                                                 
                                                                 variance_greater_val = variance_greater_val,
                                                                 variance_greater_val_sig = get_significance_sign(variance_greater_val),
                                                                 
                                                                 variance_less_val = variance_less_val,
                                                                 variance_less_val_sig = get_significance_sign(variance_less_val)
                                                                 ))
      }
    }
  }
  
  convert_to_csv(variance_test_df, file.path(curated_data_dir, physiological_data_dir, variance_test_file_name))
  
  df
}

process_variance_test <- function() {
  full_df %>% 
    filter(Treatment=='WS' & Mask==1) %>% 
    group_by(Participant_ID) %>% 
    do(variance_test(.))
}

conduct_variance_tests <- function() {
  read_data()
  process_variance_test()
}


#-------------------------#
#-------Main Program------#
#-------------------------#
# conduct_variance_tests()





