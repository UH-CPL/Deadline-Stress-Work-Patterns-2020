#-------------------------#
#--------LIBRARIES--------#
#-------------------------#
library(dplyr)
library(stringr)    ## for func str_detect()


#-------------------------#
#-----GLOBAL VARIABLES----#
#-------------------------#
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
source(file.path(script_dir, 'us-common-functions.R'))


current_dir <- dirname(script_dir)
setwd(current_dir)

# activity_log_file <- file.path(log_dir, paste0('activity-log-', format(Sys.Date(), format='%m-%d-%y'), '.txt'))
# file.create(activity_log_file)



#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
get_final_activities <- function(all_subj_df) {
  all_subj_df <- all_subj_df %>% 
    mutate(Activities_QC1=Activities) %>% ## @TANIM - CHANGE HERE!!!
    mutate(Activities_QC2=case_when(str_detect(Activities_QC1, computer_usage_pattern)~'Computer Working',
                                    Treatment=='WS'~'Other Activities')) 
  
  return(all_subj_df)
}

get_exact_computer_app_usage_time <- function(all_subj_df) {
  
  all_subj_df <- all_subj_df %>% 
    mutate(Application_QC2=case_when(str_detect(Activities_QC1, computer_usage_pattern)~Application_QC1))
  ## CHECK FOR DEFAULT VALUE!!!
  
  
  # View(na.locf(all_subj_df$Application))
  # View(all_subj_df$Application)
  
  return(all_subj_df)
}

get_final_app_usage <- function(all_subj_df) {
  all_subj_df <- all_subj_df %>% 
    mutate(Application_QC3=Application_QC2) ## @TANIM - CHANGE HERE!!!
  
  return(all_subj_df)
}


get_final_activities_and_app_usage <- function(all_subj_df) {
  
  ## 1. Rafactor and get final activity column
  all_subj_df <- get_final_activities(all_subj_df)
  
  
  ## 2. Remove app usage data except C-R & C-W
  all_subj_df <- get_exact_computer_app_usage_time(all_subj_df)
  
        
  ## 3. Rafactor and get final app usage column
  all_subj_df <- get_final_app_usage(all_subj_df)
  
  
  return(all_subj_df)
}

format_activity_data <- function() {
  all_subj_df <- custom_read_csv(file.path(curated_data_dir, physiological_data_dir, qc0_file_name))
  
  ## 1. Get final ontologies column
  ## 2. Remove app usage data except C-R & C-W
  ## 3. Get final app usage column
  all_subj_df <- get_final_activities_and_app_usage(all_subj_df) %>% 
    select(Participant_ID,
           Day,
           Treatment,
           Timestamp,
           Sinterface_Time,
           TreatmentTime,
           
           PP,
           NR_PP,
           
           E4_HR,
           E4_EDA,
           iWatch_HR,
           
           Activities,
           Activities_QC1,
           Activities_QC2,
           
           Application,
           Application_QC1,
           Application_QC2,
           Application_QC3
    )

  
  View(all_subj_df)
  convert_to_csv(all_subj_df, file.path(curated_data_dir, physiological_data_dir, qc0_file_name))
  convert_to_csv(all_subj_df, file.path(curated_data_dir, physiological_data_dir, qc99_file_name))
}


#-------------------------#
#-------Main Program------#
#-------------------------#
## activity denotes both subjects work activity and app usage activity
format_activity_data()

