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

  processed_rr_file <- get_matched_file_names(file.path(curated_data_dir, subj_data_dir), 
                                                     paste0(paste0('Group1_', subj_name, '_', day_serial, '_'), 'IBI'))
  
  
  if(is_empty(processed_rr_file)) {
    rr_file <- get_matched_file_names_recursively(day_dir, rr_file_pattern)

    # convert_to_csv(e4_df, file.path(curated_data_dir, subj_data_dir, paste0('Group1_', subj_name, '_', day_serial, '_', sub('.csv', '', sub('.*/', '', e4_file_name)), '.csv')))
    
  } else {

  }
  
  ##############################################################################################
  full_day_df <- merge(full_day_df, rr_df, by='Timestamp', all=T)   ## CHECK!!! - all vs. all.x
  ##############################################################################################
  
  full_day_df
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
        
        write_log_msg('Merging.....all subj data\n', rr_log_file)
        all_subj_rr_df <<- rbind.fill(all_subj_rr_df, full_day_df)
        
      },
      error=function(err) {
        write_log_msg(paste0('\n', decorator_hash, '\n', subj, '-', day, ': ERROR!'), rr_log_file)
        write_log_msg(paste0(err, decorator_hash), rr_log_file)
      })
    })
  })
}




#-------------------------#
#-------Main Program------#
#-------------------------#
process_rr_data()



