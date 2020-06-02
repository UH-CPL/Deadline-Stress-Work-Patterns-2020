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

rr_log_file <- file.path(log_dir, paste0('rr-log-', format(Sys.Date(), format='%m-%d-%y'), '.txt'))
file.create(rr_log_file)


sd_outlier <- 2
one_sec <- 1000


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
  # all_subj_rr_df <- merge(all_subj_df, rr_df, by='Timestamp', all.y=T)   ## CHECK!!! - all vs. all.x
  #########################################################################################################
  all_subj_rr_df <- merge(all_subj_df, rr_df, by='Timestamp') %>% 
    dplyr::rename(RR_Raw=RR) %>% 
    mutate(RR=floor(as.numeric(RR_Raw)*one_sec)) %>% 
    select(Participant_ID, Day, Treatment, Sinterface_Time, TreatmentTime, Timestamp, RR_Time, RR_Raw, RR, everything())
  
  convert_to_csv(all_subj_rr_df, file.path(curated_data_dir, physiological_data_dir, qc0_rr_file_name))
}

gather_rr_data <- function() {
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

discard_outliers <- function(subj, sess, temp_rr_df) {
  mean <- mean(temp_rr_df$RR)
  sd <- sd(temp_rr_df$RR)
  
  print(paste('Filtering extremest points: ', subj, ', ', sess))
  write(paste('Filtering extremest points: ', subj, ', ', sess), file=rr_log_file, append=TRUE)
  # print(paste0('mean: ', mean, ', sd: ', sd))
  
  outliers <- temp_rr_df %>%
    filter(temp_rr_df$RR < mean-sd_outlier*sd | temp_rr_df$RR > mean+sd_outlier*sd) %>%
    select(RR) %>%
    unlist()
  
  rr_df[rr_df$Participant_ID==subj &
          rr_df$Treatment==sess &
          rr_df$RR %in% as.list(outliers),
        'RR'] <<- NA
}

qc1_clean_rr_data <- function() {
  rr_df <<- custom_read_csv(file.path(curated_data_dir, physiological_data_dir, qc0_rr_file_name))
  # print(head(rr_df))
  
  for (subj in levels(factor(rr_df$Participant_ID))) {
    for (treatment in levels(factor(rr_df$Treatment))) {
      temp_rr_df <- rr_df %>%
        filter(Participant_ID == subj, Treatment == treatment)
      
      discard_outliers(subj, treatment, temp_rr_df)
      
      
      # --------------   Don't delet. Need to check why this was done on email study    --------------#
      # --------------   Check multi-modal-email-study/vs-validation-plot-hrv/filter_extreme_rr() method
      # temp_qc2_filtered_subj_df <- qc2_filtered_subj_df %>% 
      #   filter(Subject == subj, Session == sess)
      # 
      # if (nrow(temp_qc2_filtered_subj_df)>0 & nrow(temp_rr_df)==0) { 
      #   write(paste('Missing rr data for: ', subj, ', ', sess), file=log.file, append=TRUE)
      # } else if (nrow(temp_qc2_filtered_subj_df)==0 & nrow(temp_rr_df)>0) { 
      #   write(paste('Invalid rr data for: ', subj, ', ', sess), file=log.file, append=TRUE)
      # } else {
      #   discard_outliers(subj, sess, temp_rr_df)
      # }
      # -------------------------------------------------------------------------------------------- #
      
    }
  }
  
  convert_to_csv(rr_df, file.path(curated_data_dir, physiological_data_dir, qc1_rr_file_name))
}



#-------------------------#
#-------Main Program------#
#-------------------------#
# gather_rr_data()
# remove_bad_sensor_rr_data()
# qc1_clean_rr_data()



