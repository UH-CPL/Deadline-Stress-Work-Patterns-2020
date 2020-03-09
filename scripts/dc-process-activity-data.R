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
  all_subj_df <- all_subj_df %>% 
    mutate(Activities_QC1=Activities) %>% ## @TANIM - CHANGE HERE!!!
    mutate(Activities_QC2=case_when(str_detect(Activities_QC1, computer_usage_pattern)~'Computer Working',
                                    Treatment=='WS'~'Other Activities')) 
  
  
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
  all_subj_df <- all_subj_df %>% rename(Ontology_One=A,Ontology_Two=B,Ontology_Three=C)
  
  #######################################Reduced Ontoloties########################
 
  all_subj_df$Reduced_Ontology_One<- all_subj_df$Ontology_One
  all_subj_df$Reduced_Ontology_Two<- all_subj_df$Ontology_Two
  all_subj_df$Reduced_Ontology_Three<- all_subj_df$Ontology_Three


  R= '^CR$|^PR$|^T$'
  W = '^CW$|^PW$'
  I = '^PI|^VI'
  Out = '^Out$'
  SP = '^SP$'
  SA = '^ELD$|^EiP$'
  #Thinking = '^T$'


  all_subj_df$Reduced_Ontology_One<-case_when(
    str_detect(all_subj_df$Ontology_One, Out)~"Out",
    str_detect(all_subj_df$Ontology_One, W)~"W",
    str_detect(all_subj_df$Ontology_One, R)~"R",
    #str_detect(all_subj_df$Ontology_One, Thinking)~"T",
    str_detect(all_subj_df$Ontology_One, SP)~"SP",
    str_detect(all_subj_df$Ontology_One, SA)~"SA",
    str_detect(all_subj_df$Ontology_One, I)~"I")
  
  all_subj_df$Reduced_Ontology_Two<-case_when(
    str_detect(all_subj_df$Ontology_Two, Out)~"Out",
    str_detect(all_subj_df$Ontology_Two, W)~"W",
    str_detect(all_subj_df$Ontology_Two, R)~"R",
    #str_detect(all_subj_df$Ontology_Two, Thinking)~"T",
    str_detect(all_subj_df$Ontology_Two, SP)~"SP",
    str_detect(all_subj_df$Ontology_Two, SA)~"SA",
    str_detect(all_subj_df$Ontology_Two, I)~"I")
  
  all_subj_df$Reduced_Ontology_Three<-case_when(
    str_detect(all_subj_df$Ontology_Three, Out)~"Out",
    str_detect(all_subj_df$Ontology_Three, W)~"W",
    str_detect(all_subj_df$Ontology_Three, R)~"R",
    #str_detect(all_subj_df$Ontology_Three, Thinking)~"T",
    str_detect(all_subj_df$Ontology_Three, SP)~"SP",
    str_detect(all_subj_df$Ontology_Three, SA)~"SA",
    str_detect(all_subj_df$Ontology_Three, I)~"I")
  
  
  #all_subj_df$Reduced_Ontology_One[is.na(all_subj_df$Reduced_Ontology_One)] <- ""
  all_subj_df$Reduced_Ontology_Two[is.na(all_subj_df$Reduced_Ontology_Two)] <- ""
  all_subj_df$Reduced_Ontology_Three[is.na(all_subj_df$Reduced_Ontology_Three)] <- ""
  
  all_subj_df$Reduce_Activities<-paste(all_subj_df$Reduced_Ontology_One,all_subj_df$Reduced_Ontology_Two,all_subj_df$Reduced_Ontology_Three, sep="+")
  all_subj_df<-all_subj_df%>%mutate(Reduce_Activities_QC1=gsub("(\\+)*$", "", Reduce_Activities))

  #######################################Reduced Ontoloties########################
  

  
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
  all_subj_df <- custom_read_csv(file.path(curated_data_dir, physiological_data_dir, qc0_raw_file_name))
  
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
           
           Raw_PP,
           PP,
           
           E4_HR,
           E4_EDA,
           iWatch_HR,
           
           Activities,
           Activities_QC1,
           Activities_QC2,
           
           Application,
           Application_QC1,
           Application_QC2,
           Application_QC3,
           Ontology_One,
           Ontology_Two,
           Ontology_Three,
           
           Reduce_Activities_QC1,
           Reduced_Ontology_One,
           Reduced_Ontology_Two,
           Reduced_Ontology_Three
    )
  
  
  View(all_subj_df)
  # convert_to_csv(all_subj_df, file.path(curated_data_dir, physiological_data_dir, qc0_raw_file_name))
  convert_to_csv(all_subj_df, file.path(curated_data_dir, physiological_data_dir, qc0_masked_file_name))
}


#-------------------------#
#-------Main Program------#
#-------------------------#
## activity denotes both subjects work activity and app usage activity
format_activity_data()

