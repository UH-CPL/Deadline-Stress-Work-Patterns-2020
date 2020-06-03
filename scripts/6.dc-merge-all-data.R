#-------------------------#
#--------LIBRARIES--------#
#-------------------------#
library(dplyr)




#-------------------------#
#-----GLOBAL VARIABLES----#
#-------------------------#





#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
merge_all_data <- function() {
  physiological_data_path <- file.path(project_dir, curated_data_dir, physiological_data_dir)
  
  
  # "Participant_ID"         "Day"                    "Treatment"              "Timestamp"             
  # "Sinterface_Time"        "TreatmentTime"          "Raw_PP"                 "PP"                    
  # "Raw_E4_EDA"             "E4_EDA"                 "Raw_E4_HR"              "E4_HR"                 
  # "Raw_iWatch_HR"          "iWatch_HR"              "Activities"             "Activities_QC1"        
  # "Activities_QC2"         "Activity_One"           "Activity_Two"           "Activity_Three"        
  # "Reduced_Activities_QC1" "Reduced_Activity_One"   "Reduced_Activity_Two"   "Reduced_Activity_Three"
  # "Application"            "Application_QC1"        "Application_QC2"        "Application_QC3"       
  # "Mask" 
  # qc0_df <- custom_read_csv(file.path(physiological_data_path, qc0_final_file_name))
  # print(colnames(qc0_df))
  
  qc0_df <- custom_read_csv(file.path(physiological_data_path, qc0_final_file_name)) %>%
    select(Participant_ID, Day, Treatment, Timestamp, Sinterface_Time, TreatmentTime, 
           Raw_PP, Raw_E4_EDA, Raw_E4_HR, Raw_iWatch_HR, 
           Activities, Activities_QC1, Activities_QC2, Activity_One, Activity_Two, Activity_Three,
           Reduced_Activities_QC1, Reduced_Activity_One, Reduced_Activity_Two, Reduced_Activity_Three,
           Application, Application_QC1, Application_QC2, Application_QC3, 
           Mask)
  
  ############################################################################################
  #                             This might be QC1 or QC2
  ############################################################################################
  qc_df <- custom_read_csv(file.path(physiological_data_path, qc1_file_name)) %>% 
    select(Timestamp, PP, E4_HR, E4_EDA, iWatch_HR) 
  
  
  # select(Participant_ID, Day, Treatment, Timestamp, TreatmentTime, PP, E4_HR, E4_EDA, iWatch_HR, Mask) 
  # %>% 
  #   dplyr::rename(
  #     Raw_E4_HR=E4_HR,
  #     Raw_iWatch_HR=iWatch_HR
  #   )
  
  transformed_df <- custom_read_csv(file.path(physiological_data_path, qc1_transformed_file_name)) %>% 
    select(Timestamp, PP, E4_HR, E4_EDA, iWatch_HR) %>% 
    dplyr::rename(
      Trans_PP=PP,
      Trans_E4_HR=E4_HR,
      Trans_E4_EDA=E4_EDA,
      Trans_iWatch_HR=iWatch_HR
    )
  ############################################################################################
  
  
  
  # print_msg(head(qc0_df, 2))
  # print_msg(head(qc_df, 2))
  # print_msg(head(transformed_df, 2))
  
  
  print_msg('Merging data...')
  full_df <- transformed_df %>%
    #   # merge(qc0_df, by=c('Participant_ID', 'Day', 'Treatment', 'TreatmentTime'), all=T) %>%
    #   # merge(qc_df, by=c('Participant_ID', 'Day', 'Treatment', 'TreatmentTime'), all=T) %>%

    # merge(qc0_df, by='Timestamp', all=T) %>%
    # merge(qc_df, by='Timestamp', all=T) %>%

    dplyr::full_join(qc0_df, by='Timestamp') %>%
    dplyr::full_join(qc_df, by='Timestamp') %>%


    arrange(Participant_ID, Day, Treatment, TreatmentTime) %>%
    select(
      Participant_ID,
      Day,
      Treatment,
      Timestamp,
      TreatmentTime,

      Raw_PP,
      PP,
      Trans_PP,

      Raw_E4_EDA,
      E4_EDA,
      Trans_E4_EDA,

      Raw_E4_HR,
      E4_HR,
      Trans_E4_HR,

      Raw_iWatch_HR,
      iWatch_HR,
      Trans_iWatch_HR,

      Mask
    )

  print_msg('Done merging data...exporting')
  convert_to_csv(full_df, file.path(physiological_data_path, full_df_file_name))
}




#-------------------------#
#-------Main Program------#
#-------------------------#
# merge_all_data()