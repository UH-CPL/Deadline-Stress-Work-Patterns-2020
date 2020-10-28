#-------------------------#
#--------LIBRARIES--------#
#-------------------------#
library(dplyr)




#-------------------------#
#-----GLOBAL VARIABLES----#
#-------------------------#
source(file.path(script_dir, 'us-common-functions.R'))




#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
generate_meta_data_break_activity <- function() {
  physiological_data_path <- file.path(project_dir, curated_data_dir, physiological_data_dir)
  
  
  
  
  #################################################################################################################
  # ### 'mini_full_df.csv'
  # ### full_df_file_name
  # data_file_name <- 'mini_full_df.csv'
  # 
  # segment_df <- custom_read_csv(file.path(physiological_data_path, data_file_name)) %>%
  #   dplyr::select(Participant_ID, Day, Treatment, 
  #                 Timestamp, Sinterface_Time, TreatmentTime, 
  #                 Trans_PP, 
  #                 Reduced_Activities_QC1,
  #                 Mask) %>% 
  #   mutate(Reduced_Activities_QC1=case_when(Treatment=="RB"~"Out", 
  #                                           TRUE~.$Reduced_Activities_QC1)) %>% 
  #   dplyr::group_by(Participant_ID, Day) %>%
  #   mutate(Counter=sequence(rle(as.character(Reduced_Activities_QC1))$lengths),
  #          Segment=case_when(Reduced_Activities_QC1=="Out" & Counter==1~1, TRUE~0),
  #          Segment=ifelse(Segment==1, cumsum(Segment==1), NA),
  #          Segment=na.locf0(Segment)) %>% 
  #   select(-Counter)
  # 
  # View(segment_df)
  # 
  # convert_to_csv(segment_df, file.path(physiological_data_path, segment_df_file_name))
  #################################################################################################################

  
  segment_df <- custom_read_csv(file.path(physiological_data_path, segment_df_file_name))
  
  segment_meta_data_df <- segment_df %>%
    dplyr::group_by(Participant_ID, Day, Segment) %>%
    summarize(
          # StartTime=head(Timestamp, 1),
          # EndTime=tail(Timestamp, 1),
          Mean_Trans_PP=mean(Trans_PP, na.rm = TRUE),
          SegmentTime=n())
  
  convert_to_csv(segment_meta_data_df, file.path(physiological_data_path, segment_meta_data_df_file_name))
}


#-------------------------#
#-------Main Program------#
#-------------------------#
generate_meta_data_break_activity()