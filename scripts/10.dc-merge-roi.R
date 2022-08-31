#-------------------------#
#--------LIBRARIES--------#
#-------------------------#
# library(XLConnect)
# library(scales)
# library(ggplot2)

library(plyr)      ## for func rbind.fill()
library(readr)
library(magrittr)  ## for func set_colnames()
library(gsubfn)    ## for func read.pattern()
library(zoo)       ## for func na.locf()
library(data.table)
library(dplyr)     ## load it after loading all other libraries





#-------------------------#
#-----GLOBAL VARIABLES----#
#-------------------------#
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
project_dir <- dirname(script_dir)
setwd(project_dir)

source(file.path(script_dir, 'us-common-functions.R'))



#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
read_br_data <- function(subj_name, day_serial, session_name) {
  session_dir <- file.path(raw_data_dir, grp_dir, subj_name, day_serial, session_name)
  br_file_name <- get_matched_file_names(session_dir, br_file_pattern)
  
  br_df <- custom_read_csv(file.path(session_dir, br_file_name))
  names(br_df) <- c("Frame",	"Time",	"Timestamp", "ROI", "Breathing")
  br_df$Timestamp <- convert_s_interface_date(convert_marker_date(br_df$Timestamp))

  br_df
}


merge_breathing_data <- function(subj_name, day_serial){
  
  br_df <- tibble()
  day_dir <- file.path(raw_data_dir, grp_dir, subj_name, day_serial)
  
  sessions <- c('Baseline', 'WorkingSession')
  
  for (session in sessions) {
    raw_br_df <- read_br_data(subj_name, day_serial, session) %>% 
      mutate(Participant_ID=subj_name,
             Day=day_serial,
             Treatment=session)
    
    br_df <- rbind.fill(br_df, raw_br_df)
  }
  
  br_df
}


merge_data <- function() {
  all_subj_df <<- tibble()
  subj_list <- custom_read_csv(file.path(curated_data_dir, utility_data_dir, subj_list_file_name))$Subject
  
  sapply(subj_list, function(subj_name) {
  # sapply(c('T003', 'T005'), function(subj_name) {
  # sapply(c('T003'), function(subj_name) {

    subj_dir <- file.path(raw_data_dir, grp_dir, subj_name)
    day_list <- get_dir_list(subj_dir)
    
    sapply(day_list, function(day_serial) {
    # sapply(day_list[1], function(day_serial) {
      
      print(paste(subj_name, day_serial))
      
      tryCatch({
        full_day_df <- merge_breathing_data(subj_name, day_serial)
        all_subj_df <<- rbind.fill(all_subj_df, full_day_df)
      },
      error=function(err) {
        print(paste0('\n', decorator_hash, '\n', subj_name, '-', day_serial, ': ERROR!'))
        print(paste0(err, decorator_hash))
      })
    })
  })

  all_subj_df <<- all_subj_df %>% 
    dplyr::select(
      Participant_ID,
      Day,
      Treatment,
      Time,
      Timestamp,
      ROI
      )
  convert_to_csv(all_subj_df, file.path(curated_data_dir, physiological_data_dir, 'roi_highest_frequency.csv'))
}






#-------------------------#
#-------Main Program------#
#-------------------------#
merge_data()


