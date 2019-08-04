#-------------------------#
#--------LIBRARIES--------#
#-------------------------#



#-------------------------#
#-----GLOBAL VARIABLES----#
#-------------------------#
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
source(file.path(script_dir, 'us-common-functions.R'))


current_dir <- dirname(script_dir)
setwd(current_dir)

activity_log_file <- file.path(log_dir, paste0('activity-log-', format(Sys.Date(), format='%m-%d-%y'), '.txt'))
file.create(activity_log_file)


all_subj_df <- tibble()


#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
get_final_activities <- function(all_subj_df) {
  
}

get_exact_computer_app_usage_time <- function(all_subj_df) {
  
}

get_final_app_usage <- function(all_subj_df) {
  
}


get_final_activities <- function(all_subj_df) {
  
  ## 1. Rafactor and get final activity column
  all_subj_df <- get_final_activities(all_subj_df)
  
  
  ## 2. Remove app usage data except C-R & C-W
  all_subj_df <- get_exact_computer_app_usage_time(all_subj_df)
  
        
  ## 3. Rafactor and get final app usage column
  all_subj_df <- get_final_app_usage(all_subj_df)
  
}

format_activity_data <- function() {
  all_subj_df <- custom_read_csv(file.path(curated_data_dir, physiological_data_dir, qc0_file_name))
  
  ## 1. Get final ontologies column
  ## 2. Remove app usage data except C-R & C-W
  ## 3. Get final app usage column
  all_subj_df <- get_final_activities(all_subj_df)
  
  
  convert_to_csv(all_subj_df, file.path(curated_data_dir, physiological_data_dir, qc99_file_name))
}


#-------------------------#
#-------Main Program------#
#-------------------------#
## activity denotes both subjects work activity and app usage activity
format_activity_data()

