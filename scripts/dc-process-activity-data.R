#-------------------------#
#--------LIBRARIES--------#
#-------------------------#
library(dplyr)
library(stringr)    ## for func str_detect()
library(reshape2)
library(tidyr)

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
  #all_subj_df <- all_subj_df %>% mutate(Activities_QC1=Activities) %>% ## @TANIM - CHANGE HERE!!!
    
    all_subj_df$Activities1<-all_subj_df$Activities
    all_subj_df<-all_subj_df %>%separate(Activities1, c("A", "B", "C"), ",")
    all_subj_df$A<-trimws(all_subj_df$A)
    all_subj_df$B<-trimws(all_subj_df$B)
    
    
    if (is.na(all_subj_df$C)!=TRUE) {
      all_subj_df$C<-trimws(all_subj_df$c)
    }
    
    
    #unique(all_subj_df$A)
    all_subj_df$A<-gsub("\\(|\\)", "", all_subj_df$A)
    all_subj_df$B<-gsub("\\(|\\)", "", all_subj_df$B)
    all_subj_df$C<-gsub("\\(|\\)", "", all_subj_df$C)
    
    all_subj_df$A<-case_when(str_detect(all_subj_df$A, Out)~"Out",
                    str_detect(all_subj_df$A, PW)~"PW",
                    str_detect(all_subj_df$A, PR)~"PR",
                    str_detect(all_subj_df$A, CW)~"CW",
                    str_detect(all_subj_df$A, CR)~"CR",
                    str_detect(all_subj_df$A, EiP)~"EiP",
                    str_detect(all_subj_df$A, OB)~"OB",
                    str_detect(all_subj_df$A, Working)~"Working",
                    str_detect(all_subj_df$A, Thinking)~"T",
                    str_detect(all_subj_df$A, SP)~"SP",
                    str_detect(all_subj_df$A, ELD)~"ELD",
                    str_detect(all_subj_df$A, PI)~"PI", 
                    str_detect(all_subj_df$A, VI)~"VI"
                    
    )
    all_subj_df$B<-case_when(str_detect(all_subj_df$B, Out)~"Out",
                    str_detect(all_subj_df$B, PW)~"PW",
                    str_detect(all_subj_df$B, PR)~"PR",
                    str_detect(all_subj_df$B, CW)~"CW",
                    str_detect(all_subj_df$B, CR)~"CR",
                    str_detect(all_subj_df$B, EiP)~"EiP",
                    str_detect(all_subj_df$B, OB)~"OB",
                    str_detect(all_subj_df$B, Working)~"Working",
                    str_detect(all_subj_df$B, Thinking)~"T",
                    str_detect(all_subj_df$B, SP)~"SP",
                    str_detect(all_subj_df$B, ELD)~"ELD",
                    str_detect(all_subj_df$B, PI)~"PI",
                    str_detect(all_subj_df$B, VI)~"VI"
    )
    all_subj_df$C<-case_when(str_detect(all_subj_df$C, Out)~"Out",
                    str_detect(all_subj_df$C, PW)~"PW",
                    str_detect(all_subj_df$C, PR)~"PR",
                    str_detect(all_subj_df$C, CW)~"CW",
                    str_detect(all_subj_df$C, CR)~"CR",
                    str_detect(all_subj_df$C, EiP)~"EiP",
                    str_detect(all_subj_df$C, OB)~"OB",
                    str_detect(all_subj_df$C, Working)~"Working",
                    str_detect(all_subj_df$C, Thinking)~"T",
                    str_detect(all_subj_df$C, SP)~"SP",
                    str_detect(all_subj_df$C, ELD)~"ELD",
                    str_detect(all_subj_df$C, PI)~"PI",
                    str_detect(all_subj_df$C, VI)~"VI"
    )
    all_subj_df$C[is.na(all_subj_df$C)] <- ""
    all_subj_df$B[is.na(all_subj_df$B)] <- ""
    all_subj_df$D<-paste(all_subj_df$A,all_subj_df$B,all_subj_df$C, sep="+")
    all_subj_df<-all_subj_df%>%
      mutate(D1=gsub("(\\+)*$", "", D))
    
    # unique(all_subj_df$Activities)
    # unique(all_subj_df$D1)
    all_subj_df$Activities_QC1<-all_subj_df$D1
    
    
    
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

