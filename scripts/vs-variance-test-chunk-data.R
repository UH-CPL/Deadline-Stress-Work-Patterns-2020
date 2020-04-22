#-------------------------#
#--------LIBRARIES--------#
#-------------------------#


#-------------------------#
#-----GLOBAL VARIABLES----#
#-------------------------#
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
project_dir <- dirname(script_dir)


source(file.path(script_dir, 'us-common-functions.R'))



# full_df <- tibble()
# variance_test_df <- tibble()


# signal_name_list <- c('Mean_PP')
signal_name_list <- c('PP', 'E4_HR', 'E4_EDA', 'iWatch_HR')


chunk_mean_file_name <- remove_rigth_substr(qc1_log_trans_mean_chunk_file_name, 4)
variance_test_chunk_mean_file_name <- remove_rigth_substr(variance_test_chunk_mean_file_name, 4)

#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
read_data <- function(chunk_size_minute, signal) {
  mean_df <<- custom_read_csv(file.path(project_dir, 
                                        curated_data_dir, 
                                        physiological_data_dir,
                                        paste0(chunk_mean_file_name, '_', signal, '_', chunk_size_minute, '_minute.csv')))
                                        # paste0(chunk_mean_file_name, '_', chunk_size_minute, '_minute.csv')))
  # print('I am here')
  # print(head(mean_df, 2))
  
}

variance_test <- function(df, chunk_size_minute, signal) {
  # print(df)
  # print(nrow(df))
  # print(head(df, 2))
  
  # for (signal_name in signal_name_list) {
  for (deadline_day in c('Day1', 'Day2')) {
    for (vanilla_day in c('Day3', 'Day4')) {
      
      deadline_day_df <- df %>% 
        filter(Day==deadline_day)
      
      vanilla_day_df <- df %>% 
        filter(Day==vanilla_day)
      
      print(paste0('For ', deadline_day, ', total_deadline_row: ', nrow(deadline_day_df),
      '      For ', vanilla_day, ', total_deadline_row: ', nrow(vanilla_day_df)))
      
      # print(signal_name)
      # print(head(deadline_day_df, 2))
      # print(head(vanilla_day_df, 2))
      
      tryCatch({
        # variance_val <- var.test(deadline_day_df[[signal_name]], vanilla_day_df[[signal_name]])$p.value
        # variance_greater_val <- var.test(deadline_day_df[[signal_name]], vanilla_day_df[[signal_name]], alternative='greater')$p.value
        # variance_less_val <- var.test(deadline_day_df[[signal_name]], vanilla_day_df[[signal_name]], alternative='less')$p.value
        
        variance_val <- var.test(deadline_day_df$Mean_Val, vanilla_day_df$Mean_Val)$p.value
        variance_greater_val <- var.test(deadline_day_df$Mean_Val, vanilla_day_df$Mean_Val, alternative='greater')$p.value
        variance_less_val <- var.test(deadline_day_df$Mean_Val, vanilla_day_df$Mean_Val, alternative='less')$p.value
        
        
        # print(paste(variance_val, variance_greater_val, variance_less_val))
        
        variance_test_df <<- rbind.fill(variance_test_df, tibble(Participant_ID = unique(df$Participant_ID),
                                                                 Signal = signal,
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
      
      },
      error=function(err) {
        print(paste0('!!!!!ERROR: ', unique(df$Participant_ID), ' - ', paste0(deadline_day, '-', vanilla_day), '!!!!!'))
        print(paste0(err))
      })
    }
    # }
  }
  
  convert_to_csv(variance_test_df, file.path(project_dir,
                                             curated_data_dir, 
                                             physiological_data_dir,
                                             paste0(variance_test_chunk_mean_file_name, '_', signal, '_', chunk_size_minute, '_minute.csv')))
                                             # paste0(variance_test_chunk_mean_file_name, '_', chunk_size_minute, '_minute.csv')))
  
  df
}

process_variance_test <- function(chunk_size_minute, signal) {
  variance_test_df <<- tibble()

  # print('hi')
  # print(head(mean_df, 2))
  mean_df %>%
    filter(Mask==1,
           Total_Non_NA_Rows>chunk_size_minute*60*discard_rate/100
           ) %>%
    group_by(Participant_ID) %>%
    do(variance_test(., chunk_size_minute, signal))
}

conduct_variance_tests_chunk_data <- function() {
  print(chunk_sizes)
  for (chunk_size_minute in chunk_sizes) {
    for (signal in signal_name_list) {
      read_data(chunk_size_minute, signal)
      process_variance_test(chunk_size_minute, signal)
    }
  }
}


#-------------------------#
#-------Main Program------#
#-------------------------#
# chunk_sizes <- c(15)
# chunk_sizes <- c(1, 5, 10, 15)
# percentage <- 10

# conduct_variance_tests_chunk_data()





