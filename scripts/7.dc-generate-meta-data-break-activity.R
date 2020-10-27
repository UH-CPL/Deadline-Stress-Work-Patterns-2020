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
generate_meta_data_break_activity <- function() {
  physiological_data_path <- file.path(project_dir, curated_data_dir, physiological_data_dir)
  
  # 'mini_full_df.csv'
  # full_df_file_name
  segment_df <- custom_read_csv(file.path(physiological_data_path, 'mini_full_df.csv')) %>%
    dplyr::select(Participant_ID, Day, Treatment, 
                  Timestamp, Sinterface_Time, TreatmentTime, 
                  Trans_PP, 
                  Reduced_Activities_QC1,
                  Mask) %>% 
    mutate(Reduced_Activities_QC1=case_when(Treatment=="RB"~"Out", 
                                            TRUE~.$Reduced_Activities_QC1)) %>% 
    dplyr::group_by(Participant_ID, Day) %>%
    mutate(Counter=sequence(rle(as.character(Reduced_Activities_QC1))$lengths),
           Segment=case_when(Reduced_Activities_QC1=="Out" & Counter==1~1, TRUE~0),
           Segment=ifelse(Segment==1, cumsum(Segment==1), NA),
           Segment=na.locf0(Segment)) %>% 
    select(-Counter)

  View(segment_df)
  
  convert_to_csv(segment_df, file.path(physiological_data_path, 'segment_df.csv'))
}


#-------------------------#
#-------Main Program------#
#-------------------------#
generate_meta_data_break_activity()