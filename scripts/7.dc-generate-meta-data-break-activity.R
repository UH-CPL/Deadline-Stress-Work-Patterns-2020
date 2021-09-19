#-------------------------#
#--------LIBRARIES--------#
#-------------------------#
library(dplyr)
library(zoo)




#-------------------------#
#-----GLOBAL VARIABLES----#
#-------------------------#
# script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
# project_dir <- dirname(script_dir)
# setwd(project_dir)
# 
# source(file.path(script_dir, 'us-common-functions.R'))




#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
generate_meta_data_break_activity <- function() {
  physiological_data_path <- file.path(project_dir, curated_data_dir, physiological_data_dir)
  
  #################################################################################################################
  # data_file_name <- 'Full_Df_Segment.csv'
  # data_file_name <- 'mini_full_df.csv'
  data_file_name <- full_df_file_name


  segment_df <- custom_read_csv(file.path(physiological_data_path, data_file_name)) %>%
    filter(!is.na(Segments_Activity)) %>%  ## What is Segments_Activity and why removing NA??
    dplyr::select(Participant_ID, Day, Treatment,
                  Timestamp, Sinterface_Time, TreatmentTime,
                  Trans_PP,
                  Segments_Activity,
                  Mask) %>%
    dplyr::mutate(Segments_Activity=case_when(Treatment=="RB"~"Out",
                                            TRUE~.$Segments_Activity)) %>%
    dplyr::group_by(Participant_ID, Day) %>%
    dplyr::mutate(Counter=sequence(rle(as.character(Segments_Activity))$lengths),
           Segment=case_when(Segments_Activity=="Out" & Counter==1~1, TRUE~0),
           Segment=ifelse(Segment==1, cumsum(Segment==1), NA),
           Segment=na.locf0(Segment)) %>%
    dplyr::select(-Counter)

  # View(segment_df)
  convert_to_csv(segment_df, file.path(physiological_data_path, segment_df_file_name))
  #################################################################################################################

  
  
  
  ################################################################################################################################
  #           (end time - start time) vs. total row  --> Because after RB there was a time gap + Activity might not be continuous
  ################################################################################################################################
  segment_df <- custom_read_csv(file.path(physiological_data_path, segment_df_file_name))

  segment_meta_data_df_1 <- segment_df %>%
    dplyr::group_by(Participant_ID, Day) %>%
    dplyr::summarize(Length_Day=n(),  ## After removing NA from Segments_Activity, is it okay?? ##------------!!
              Mean_PP_RestingBaseline=mean(Trans_PP[Segments_Activity=="Out" & Segment==1], na.rm = TRUE), ##------------!!
              Length_RestingBaseline=length(Trans_PP[Segments_Activity=="Out" & Segment==1])) %>% ##------------!!
    ungroup()
    
  segment_meta_data_df <- segment_df %>%
    dplyr::group_by(Participant_ID, Day, Segment) %>%
    dplyr::summarize(
          StartTime=head(Timestamp, 1),
          EndTime=tail(Timestamp, 1),
          DiffTimeStamp=as.numeric(difftime(EndTime, StartTime, units = "secs")+1),
          Length_Segment=n(),
          DiffTimeSec=DiffTimeStamp-Length_Segment,
          DiffTimePercentage=100*(DiffTimeSec)/DiffTimeStamp,
          
          Length_Break=sum(Segments_Activity=="Out", na.rm = TRUE),
          Length_Reading_Writing=sum(Segments_Activity=="RW", na.rm = TRUE),
          Length_Other_Activities=sum(Segments_Activity=="Other", na.rm = TRUE),
          
          Mean_PP_Reading_Writing=mean(Trans_PP[Segments_Activity=="RW"], na.rm = TRUE),
          Mean_PP_Other_Activities=mean(Trans_PP[Segments_Activity=="Other"], na.rm = TRUE)
          
          
          ) %>% 
    dplyr::ungroup() %>% 
    
    dplyr::mutate(Segment_Order_Percentage=lag(Length_Segment),
                  Segment_Order_Percentage=case_when(Segment==1~0,
                                                     TRUE~as.double(Segment_Order_Percentage))) %>% 
  
    dplyr::group_by(Participant_ID, Day) %>%
    dplyr::mutate(Segment_Order_Percentage=cumsum(Segment_Order_Percentage)) %>% 
      
    merge(segment_meta_data_df_1, by=c("Participant_ID", "Day")) %>%
    dplyr::mutate(Segment_Order_Percentage=round(100*Segment_Order_Percentage/Length_Day, 0),
                  Segment_Order_Percentage=ifelse(Segment_Order_Percentage==0, 0.05, Segment_Order_Percentage)) %>%

    dplyr::select(
      Participant_ID,
      Day,
      Length_Day,
      StartTime,
      EndTime,
      DiffTimeStamp,
      DiffTimePercentage,
      DiffTimeSec,
      Segment,
      Length_Segment,
      Segment_Order_Percentage,
      Length_RestingBaseline,
      Mean_PP_RestingBaseline,
      Length_Break,
      Length_Reading_Writing,
      Mean_PP_Reading_Writing,
      Length_Other_Activities,
      Mean_PP_Other_Activities
    )

  View(segment_meta_data_df)
  convert_to_csv(segment_meta_data_df, file.path(physiological_data_path, segment_meta_data_df_file_name))
  #################################################################################################################
  
  
  
  
  #################################################################################################################
  # segment_meta_data_df <- segment_meta_data_df %>%
  #   dplyr::select(
  #     Participant_ID,
  #     Day,
  #     StartTime,
  #     EndTime,
  #     Segment
  #     )
  # convert_to_csv(segment_meta_data_df, file.path(physiological_data_path, "segment_start_end_time.csv"))
  #################################################################################################################
  
  
  
  
  #################################################################################################################
  #    Eiii Jaura, Eikhane Code Korbi :P
  #################################################################################################################
}


investigate_data <- function() {
  physiological_data_path <- file.path(project_dir, curated_data_dir, physiological_data_dir)
  
  #################################################################################################################
  # data_file_name <- qc0_raw_file_name
  # data_file_name <- qc0_final_file_name
  # data_file_name <- qc1_file_name
  data_file_name <- full_df_file_name
  
  investigation_df <- custom_read_csv(file.path(physiological_data_path, data_file_name)) %>%
    dplyr::group_by(Participant_ID, Day, Treatment, Sinterface_Time) %>%
    dplyr::summarize(Duplicate_Row=n()) %>% 
    filter(Duplicate_Row>1)
  
  # View(segment_df)
  convert_to_csv(investigation_df, file.path(physiological_data_path, paste0("investigation_", data_file_name)))
  #################################################################################################################
}


#-------------------------#
#-------Main Program------#
#-------------------------#
# generate_meta_data_break_activity()
# investigate_data()



