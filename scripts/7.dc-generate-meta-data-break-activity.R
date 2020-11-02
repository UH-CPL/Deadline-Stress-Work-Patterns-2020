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
  #                 Segments_Activity,
  #                 Mask) %>%
  #   mutate(Segments_Activity=case_when(Treatment=="RB"~"Out",
  #                                           TRUE~.$Segments_Activity)) %>%
  #   dplyr::group_by(Participant_ID, Day) %>%
  #   mutate(Counter=sequence(rle(as.character(Segments_Activity))$lengths),
  #          Segment=case_when(Segments_Activity=="Out" & Counter==1~1, TRUE~0),
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
          SegmentTime=n(),
          BreakTime=sum(Segments_Activity=="Out", na.rm = T),
          WorkingTime=SegmentTime-BreakTime)

  convert_to_csv(segment_meta_data_df, file.path(physiological_data_path, segment_meta_data_df_file_name))
}


#-------------------------#
#-------Main Program------#
#-------------------------#
generate_meta_data_break_activity()