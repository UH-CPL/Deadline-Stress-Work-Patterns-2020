#-------------------------#
#--------LIBRARIES--------#
#-------------------------#
# library(XLConnect)
# library(scales)
# library(ggplot2)
library(dplyr)
# library(readr)
# 
# require(xlsx)
# library(readxl)
# library(lubridate)







#-------------------------#
#-----GLOBAL VARIABLES----#
#-------------------------#
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
source(file.path(script_dir, 'us-common-functions.R'))
source(file.path(script_dir, 'us-filter-pp.R'))
source(file.path(script_dir, 'us-down-sample-pp.R'))


current_dir <- dirname(script_dir)
setwd(current_dir)

curation_log_file <- file.path(log_dir, paste0('curation-log-', format(Sys.Date(), format='%m-%d-%y'), '.txt'))
file.create(curation_log_file)


all_subj_df <- tibble()


#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
refactor_and_export_all_subj_data <- function() {
  # all_subj_df <<- all_subj_df %>% 
    
  convert_to_csv(all_subj_df, file.path(curated_data_dir, physiological_data_dir, 'quantitative_df_qc0.csv'))
}





reduce_noise_and_downsample <- function(session_dir, pp_file_name) {
  # session_dir <- file.path(raw_data_dir, grp_dir, subj_name, day_serial, session_name)
  # pp_file_name <- get_matched_file_names(session_dir, pp_file_pattern)
  
  pp_df <- read.csv(file.path(session_dir, pp_file_name))
  names(pp_df) <- c("Frame",	"Time",	"Timestamp", "PP")
  pp_df$Timestamp <- as.POSIXct(strptime(pp_df$Timestamp, format=s_interface_date_format)) 
  
  pp_df$NR_PP <- remove_noise(pp_df$PP)
  downsampled_pp_df <- downsample_using_mean(pp_df, c('PP', 'NR_PP'))
  
  # write_log_msg(nrow(downsampled_pp_df), curation_log_file)
  
  convert_to_csv(downsampled_pp_df, file.path(curated_data_dir, subj_data_dir, paste0(substr(pp_file_name, 1, nchar(pp_file_name)-7), '_pp_nr.csv')))
  
  return(downsampled_pp_df)
}


get_downsampled_pp <- function(nr_pp_file_name) {
  # nr_pp_file_name <- get_matched_file_names(file.path(curated_data_dir, subj_data_dir), paste0('.*', subj_name, '.*', day_serial, '.*', session_name, nr_pp_file_pattern))
  
  downsampled_pp_df <- read.csv(file.path(curated_data_dir, subj_data_dir, nr_pp_file_name))
  downsampled_pp_df$Timestamp <- as.POSIXct(downsampled_pp_df$Timestamp)
  
  return(downsampled_pp_df)
}

get_ontologies <- function(ontologies_df, pp_df) {
  
}

curate_session_data <- function(subj_name, day_serial, session_name) {
  session_dir <- file.path(raw_data_dir, grp_dir, subj_name, day_serial, session_name)
  pp_file_name <- get_matched_file_names(session_dir, pp_file_pattern)
  nr_pp_file_name <- get_matched_file_names(file.path(curated_data_dir, subj_data_dir), paste0('.*', subj_name, '.*', day_serial, '.*', session_name, nr_pp_file_pattern))
  
  marker_file_name <- get_matched_file_names(session_dir, marker_file_pattern)
  marker_df <- read.csv(file.path(session_dir, marker_file_name))
  # marker_df <- convertTimestampSessionMarkers(marker_df, subj_interface_df, subj_name) 
  
  if(!is_empty(pp_file_name)) {
    if(is_empty(nr_pp_file_name)) {
      write_log_msg('--- noise reduced pp file NOT found ---', curation_log_file)
      downsampled_pp_df <- reduce_noise_and_downsample(session_dir, pp_file_name)
    } else {
      write_log_msg('--- noise reduced pp file found ---', curation_log_file)
      downsampled_pp_df <- get_downsampled_pp(nr_pp_file_name)
    }
  }
  
  # downsampled_pp_df <- reduce_noise_and_downsample(subj_name, day_serial, session_name)
  
  
  
  ###################################################
  ## Now do specific works for spedific session
  ## Baseline - find out exact 4-5min session
  ## Working Session - Find out the ontologies
  ## Working Session - Find out the activity log
  ###################################################
  if (session_name==session_list[1]) { ## Baseline
    ## Adding session name
    downsampled_pp_df$Treatment <- 'RB'
    downsampled_pp_df$Ontologies <- NA
    
    ###################################################
    ## Find out exact 4-5min session
    ###################################################
    
    
  } else { ## WorkingSession
    ## Adding session name
    downsampled_pp_df$Treatment <- 'WS'
    
    
    ## Find out the ontologies
    downsampled_pp_df <- get_ontologies(marker_df, downsampled_pp_df)
    
    
    
    ###################################################
    ## Find out the activity log
    ###################################################
  }
  
  
  
  ## We will merge all signal data in merged_data
  merged_df <- downsampled_pp_df
  
  
  
  ###################################################
  ## Now add e4 data
  ###################################################
  
  
  
  
  
  ###################################################
  ## Now add iWatch data
  ###################################################
  
  
  
  
  
  all_subj_df <<- rbind(all_subj_df, merged_df)
}

# curate_day_data <- function(subj_name, day_serial) {
#   day_dir <- file.path(raw_data_dir, grp_dir, subj_name, day_serial)
#   sapply(session_list, function(session_name) {
#     tryCatch({
#       # write_log_msg(paste0('Strating Processing...', subj_name, '-', day_serial, '-', session_name), curation_log_file)
#       curate_session_data(subj_name, day_serial, session_name)
#       write_log_msg(paste0(subj_name, '-', day_serial, '-', session_name, ': SUCCESSFUL'), curation_log_file)
#     },
#     error=function(err) {
#       write_log_msg('----------------------------------------------------------', curation_log_file)
#       write_log_msg(paste0(subj_name, '-', day_serial, '-', session_name, ': ERROR!'), curation_log_file)
#       write_log_msg(paste0(err, '\n'), curation_log_file)
#     })
#   })
# }



curate_data <- function() {
  # subj_list <- get_dir_list(file.path(raw_data_dir, grp_dir))
  subj_list <- read.csv(file.path(curated_data_dir, utility_data_dir, subj_list_file_name))$Subject
  
  # sapply(subj_list, function(subj_name) {
  sapply(subj_list[1], function(subj_name) {
    
    subj_dir <- file.path(raw_data_dir, grp_dir, subj_name)
    day_list <- get_dir_list(subj_dir)
    
    # sapply(day_list, function(day_serial) {
    sapply(day_list[1], function(day_serial) {
      # curate_day_data(subj_name, day_serial)
      
      day_dir <- file.path(raw_data_dir, grp_dir, subj_name, day_serial)
      sapply(session_list, function(session_name) {
        tryCatch({
          # write_log_msg(paste0('Strating Processing...', subj_name, '-', day_serial, '-', session_name), curation_log_file)
          curate_session_data(subj_name, day_serial, session_name)
          write_log_msg(paste0(subj_name, '-', day_serial, '-', session_name, ': SUCCESSFUL'), curation_log_file)
        },
        error=function(err) {
          write_log_msg('----------------------------------------------------------', curation_log_file)
          write_log_msg(paste0(subj_name, '-', day_serial, '-', session_name, ': ERROR!'), curation_log_file)
          write_log_msg(paste0(err, '\n'), curation_log_file)
        })
      })
    })
  })
  
  
  refactor_and_export_all_subj_data()
}






#-------------------------#
#-------Main Program------#
#-------------------------#
curate_data()

