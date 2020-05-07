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

rr_log_file <- file.path(log_dir, paste0('rr-log-', format(Sys.Date(), format='%m-%d-%y'), '.txt'))
file.create(rr_log_file)



#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
process_subj_day_rr <- function(subj, day) {
  day_dir <- file.path(raw_data_dir, grp_dir, subj, day)

  rr_df <- read_csv(file.path(day_dir, get_matched_file_names_recursively(day_dir, rr_file_pattern)), 
                    col_names=F, 
                    col_types = cols())

  ## 1st row indicates the start time
  base_time <- as.numeric(rr_df[1, 1])

  ## The actual signal starts from 2nd row
  rr_df <- rr_df[-(1:1), ]

  colnames(rr_df) <- c('RR_Time', 'RR')
  timestamp_vector <- c(base_time + as.double(rr_df$RR_Time[1]))

  ## Add a CovertedTime base_time
  for (i in 2:nrow(rr_df)) {
    timestamp_vector <- c(timestamp_vector, base_time + as.double(rr_df$RR_Time[i]))
  }
  
  # rr_df$Timestamp <- as.POSIXct(timestamp_vector, origin='1970-01-01', tz='')
  rr_df$Timestamp <- strftime(as.POSIXct(timestamp_vector, origin='1970-01-01', tz='America/Chicago'), # timestamp with Houston timezone
                              format='%Y-%m-%d %H:%M:%S',
                              tz='')

  rr_df
}


merge_with_other_channels <- function(rr_df) {
  all_subj_df <- custom_read_csv(file.path(curated_data_dir, physiological_data_dir, qc0_final_file_name)) %>% 
    select(-Raw_PP,	-PP,	-E4_HR,	-E4_EDA, -iWatch_HR)
  
  print(all_subj_df$Timestamp[1])
  print(rr_df$Timestamp[1])
  
  #########################################################################################################
  # all_subj_df <- merge(all_subj_df, rr_df, by='Timestamp', all.y=T)   ## CHECK!!! - all vs. all.x
  all_subj_df <- merge(all_subj_df, rr_df, by='Timestamp')
  #########################################################################################################
  
  # convert_to_csv(all_subj_df, file.path(curated_data_dir, physiological_data_dir, qc0_rr_file_name))
  convert_to_csv(all_subj_df, file.path(curated_data_dir, physiological_data_dir, qc0_rr_file_name))
}

process_rr_data <- function() {
  all_subj_rr_df <<- tibble()

  subj_list <- custom_read_csv(file.path(curated_data_dir, utility_data_dir, subj_list_file_name))$Subject
  
  sapply(subj_list, function(subj) {
  # sapply(subj_list[1], function(subj) {
  # sapply(c('T001', 'T003'), function(subj) {
    
    day_list <- get_dir_list(file.path(raw_data_dir, grp_dir, subj))
    
    sapply(day_list, function(day) {
    # sapply(day_list[1], function(day) {
      
      tryCatch({
        write_log_msg(paste0('\n----------\n', subj, '-', day, "\n----------"), rr_log_file)
        
        write_log_msg('Processing.....RR', rr_log_file)
        full_day_df <- process_subj_day_rr(subj, day)
        
        write_log_msg('Concatenating.....all subj data\n', rr_log_file)
        all_subj_rr_df <<- rbind.fill(all_subj_rr_df, full_day_df)
        
      },
      error=function(err) {
        write_log_msg(paste0('\n', decorator_hash, '\n', subj, '-', day, ': ERROR!'), rr_log_file)
        write_log_msg(paste0(err, decorator_hash), rr_log_file)
      })
    })
  })
  
  write_log_msg('Merging.....all subj data\n', rr_log_file)
  merge_with_other_channels(all_subj_rr_df)
}




#-------------------------#
#-------Main Program------#
#-------------------------#
process_rr_data()



