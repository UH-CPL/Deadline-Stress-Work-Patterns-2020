#-------------------------#
#--------LIBRARIES--------#
#-------------------------#
library(stringr)    ## for func str_detect()
library(reshape2)
library(tidyr)
library(dplyr)



#-------------------------#
#-----GLOBAL VARIABLES----#
#-------------------------#
# script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
# source(file.path(script_dir, 'us-common-functions.R'))


# activity_log_file <- file.path(log_dir, paste0('activity-log-', format(Sys.Date(), format='%m-%d-%y'), '.txt'))
# file.create(activity_log_file)





#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
get_final_activities <- function(all_subj_df) {
  #all_subj_df <- all_subj_df %>% mutate(Activities_QC1=Activities) %>% ## @TANIM - CHANGE HERE!!!
  all_subj_df <- all_subj_df %>% 
    mutate(Activities_QC1=Activities) %>% ## @TANIM - CHANGE HERE!!!
    mutate(Activities_QC2=case_when(str_detect(Activities_QC1, computer_usage_pattern)~'Computer Working',
                                    Treatment=='WS'~'Other Activities')) 
  
  
  all_subj_df$Activities1<-all_subj_df$Activities
  all_subj_df<-all_subj_df %>%separate(Activities1, c("A", "B", "C"), ",")
  all_subj_df$A<-trimws(all_subj_df$A)
  all_subj_df$B<-trimws(all_subj_df$B)
  all_subj_df$C<-trimws(all_subj_df$C)
  
  
  
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
  all_subj_df <- all_subj_df %>% 
    dplyr::rename(Activity_One=A,Activity_Two=B,Activity_Three=C)
  
  #######################################Reduced Ontoloties########################
 
  all_subj_df$Reduced_Activity_One<- all_subj_df$Activity_One
  all_subj_df$Reduced_Activity_Two<- all_subj_df$Activity_Two
  all_subj_df$Reduced_Activity_Three<- all_subj_df$Activity_Three


  R= '^CR$|^PR$|^T$'
  W = '^CW$|^PW$'
  I = '^PI|^VI'
  Out = '^Out$'
  SP = '^SP$'
  SA = '^ELD$|^EiP$|^OB$'

  all_subj_df$Reduced_Activity_One<-case_when(
    str_detect(all_subj_df$Activity_One, Out)~"Out",
    str_detect(all_subj_df$Activity_One, W)~"W",
    str_detect(all_subj_df$Activity_One, R)~"R",
    #str_detect(all_subj_df$Activity_One, Thinking)~"T",
    str_detect(all_subj_df$Activity_One, SP)~"SP",
    str_detect(all_subj_df$Activity_One, SA)~"SA",
    str_detect(all_subj_df$Activity_One, I)~"I")
  
  all_subj_df$Reduced_Activity_Two<-case_when(
    str_detect(all_subj_df$Activity_Two, Out)~"Out",
    str_detect(all_subj_df$Activity_Two, W)~"W",
    str_detect(all_subj_df$Activity_Two, R)~"R",
    #str_detect(all_subj_df$Activity_Two, Thinking)~"T",
    str_detect(all_subj_df$Activity_Two, SP)~"SP",
    str_detect(all_subj_df$Activity_Two, SA)~"SA",
    str_detect(all_subj_df$Activity_Two, I)~"I")
  
  all_subj_df$Reduced_Activity_Three<-case_when(
    str_detect(all_subj_df$Activity_Three, Out)~"Out",
    str_detect(all_subj_df$Activity_Three, W)~"W",
    str_detect(all_subj_df$Activity_Three, R)~"R",
    #str_detect(all_subj_df$Activity_Three, Thinking)~"T",
    str_detect(all_subj_df$Activity_Three, SP)~"SP",
    str_detect(all_subj_df$Activity_Three, SA)~"SA",
    str_detect(all_subj_df$Activity_Three, I)~"I")
  
  
  # all_subj_df$Reduced_Activity_One[is.na(all_subj_df$Reduced_Activity_One)] <- ""
  all_subj_df$Reduced_Activity_Two[is.na(all_subj_df$Reduced_Activity_Two)] <- ""
  all_subj_df$Reduced_Activity_Three[is.na(all_subj_df$Reduced_Activity_Three)] <- ""
  
  all_subj_df$Reduce_Activities<-paste(all_subj_df$Reduced_Activity_One,all_subj_df$Reduced_Activity_Two,all_subj_df$Reduced_Activity_Three, sep="+")
  all_subj_df<-all_subj_df%>%mutate(Reduced_Activities_QC1=gsub("(\\+)*$", "", Reduce_Activities))
  
  
  for ( cell in 1:nrow(all_subj_df)) {
    # print(cell)
    # print(all_subj_df$Reduced_Activity_One[cell])
    # print(all_subj_df$Reduced_Activity_Two[cell])
    
      if(all_subj_df$Reduced_Activity_Two[cell]!="" & (all_subj_df$Reduced_Activity_One[cell]==all_subj_df$Reduced_Activity_Two[cell])){
        all_subj_df$Reduced_Activity_Two[cell]= all_subj_df$Reduced_Activity_Three[cell]
        all_subj_df$Reduced_Activity_Three[cell] = ""
        all_subj_df$Reduced_Activities_QC1[cell]=paste(all_subj_df$Reduced_Activity_One[cell],all_subj_df$Reduced_Activity_Two[cell],all_subj_df$Reduced_Activity_Three[cell], sep="+")
        all_subj_df$Reduced_Activities_QC1[cell]<-gsub("(\\+)*$", "", all_subj_df$Reduced_Activities_QC1[cell])
      }

    
    
    if(all_subj_df$Reduced_Activity_Three[cell]!="" & (all_subj_df$Reduced_Activity_Three[cell]==all_subj_df$Reduced_Activity_Two[cell])){
      all_subj_df$Reduced_Activity_Two[cell]= all_subj_df$Reduced_Activity_Three[cell]
      all_subj_df$Reduced_Activity_Three[cell] = ""
      all_subj_df$Reduced_Activities_QC1[cell]=paste(all_subj_df$Reduced_Activity_One[cell],all_subj_df$Reduced_Activity_Two[cell],all_subj_df$Reduced_Activity_Three[cell], sep="+")
      all_subj_df$Reduced_Activities_QC1[cell]<-gsub("(\\+)*$", "", all_subj_df$Reduced_Activities_QC1[cell])
    }
    
      if ((all_subj_df$Reduced_Activity_One[cell]=="SA" & all_subj_df$Reduced_Activity_Two[cell]=="R") | (all_subj_df$Reduced_Activity_One[cell]=="SA" & all_subj_df$Reduced_Activity_Two[cell]=="W")){
        temp=all_subj_df$Reduced_Activity_One[cell]
        all_subj_df$Reduced_Activity_One[cell]=all_subj_df$Reduced_Activity_Two[cell]
        all_subj_df$Reduced_Activity_Two[cell]=temp

      }
  }

  #######################################Reduced Ontoloties########################
  

  
  ############################--------------------Application--------------------############################
  all_subj_df$Application_QC1<-trimws(all_subj_df$Application_QC1)
  all_subj_df<- all_subj_df %>%
    mutate(Reduced_Application = case_when(str_detect(Application_QC1, DocumentsApp)~'Document Apps',
                                           str_detect(Application_QC1, Email)~'Email',
                                           str_detect(Application_QC1, WebBrowsingApps)~'Web Browsing Apps',
                                           str_detect(Application_QC1, EntertainingApps)~'Entertaining Apps',
                                           str_detect(Application_QC1, UtilitiesApps)~'Utilities Apps',
                                           str_detect(Application_QC1, ProgrammingApps)~'Programming Apps',
                                           str_detect(Application_QC1, VirtualCommunicationApps)~'Virtual Communication Apps'))
  
  
  all_subj_df <- all_subj_df %>% mutate(Reduced_Application_final=case_when(str_detect(Activities_QC2, application_usage_pattern)~'NA',
                                                          Treatment=='WS'~ Reduced_Application)) 
  ############################--------------------Application--------------------############################
  
  
  
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


get_masked_data <- function(all_subj_df) {
  all_subj_df <- all_subj_df %>% 
    mutate(Mask=case_when(Reduced_Activities_QC1=='Out'~0, 
                          TRUE~1))
  
  return(all_subj_df)
}

remove_pp_for_out <- function(all_subj_df) {
  # all_subj_df <- all_subj_df %>%
  #   mutate(Raw_PP = replace(PP, Reduced_Activities_QC1=="Out" & Raw_PP!="NA", NA),
  #          PP = replace(PP, Reduced_Activities_QC1=="Out" & PP!="NA", NA))
  
  all_subj_df <- all_subj_df %>%
    mutate(Raw_PP = replace(PP, Reduced_Activities_QC1=="Out", NA),
           PP = replace(PP, Reduced_Activities_QC1=="Out", NA))
}


get_final_activities_and_app_usage <- function(all_subj_df) {
  
  ## 1. Rafactor and get final activity column
  all_subj_df <- get_final_activities(all_subj_df)
  
  
  ## 2. Remove app usage data except C-R & C-W
  all_subj_df <- get_exact_computer_app_usage_time(all_subj_df)
  
  
  ## 3. Rafactor and get final app usage column
  all_subj_df <- get_final_app_usage(all_subj_df)
  
  
  ## 4. Generate masked data
  all_subj_df <- get_masked_data(all_subj_df)
  
  
  ## 5. Remove PP when out
  all_subj_df <- remove_pp_for_out(all_subj_df)
  
  
  return(all_subj_df)
}

format_activity_app_usage_data <- function() {
  all_subj_df <- custom_read_csv(file.path(curated_data_dir, physiological_data_dir, qc0_raw_file_name))
  
  ## 1. Get final ontologies column
  ## 2. Remove app usage data except C-R & C-W
  ## 3. Get final app usage column
  all_subj_df <- get_final_activities_and_app_usage(all_subj_df) %>% 
    dplyr::select(Participant_ID,
           Day,
           Treatment,
           Timestamp,
           Sinterface_Time,
           # BaseTreatmentTime,
           TreatmentTime,
           
           Raw_PP,
           PP,
           
           Raw_E4_EDA,
           E4_EDA,
           
           Raw_E4_HR,
           E4_HR,
           
           Raw_iWatch_HR,
           iWatch_HR,
           
           Activities,
           Activities_QC1,
           Activities_QC2,
           
           Activity_One,
           Activity_Two,
           Activity_Three,
           
           Reduced_Activities_QC1,
           
           Reduced_Activity_One,
           Reduced_Activity_Two,
           Reduced_Activity_Three,
           
           Application,
           Application_QC1,
           Application_QC2,
           Application_QC3,
           
           Reduced_Application,
           Reduced_Application_final,
           
           Mask
    )
  
  
  # View(all_subj_df)
  # convert_to_csv(all_subj_df, file.path(curated_data_dir, physiological_data_dir, qc0_raw_file_name))
  convert_to_csv(all_subj_df, file.path(curated_data_dir, physiological_data_dir, qc0_final_file_name))
}


#-------------------------#
#-------Main Program------#
#-------------------------#
## activity denotes both subjects work activity and app usage activity
# format_activity_app_usage_data()

