#-------------------------#
#--------LIBRARIES--------#
#-------------------------#
library(dplyr)
library(tidyr)
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
  
  # #################################################################################################################
  # # data_file_name <- 'Full_Df_Segment.csv'
  # # data_file_name <- 'mini_full_df.csv'
  # data_file_name <- full_df_file_name
  # 
  # 
  # segment_df <- custom_read_csv(file.path(physiological_data_path, data_file_name)) %>%
  #   dplyr::select(Participant_ID, Day, Treatment,
  #                 Timestamp, Sinterface_Time, TreatmentTime,
  #                 Trans_PP,
  #                 Segments_Activity,
  #                 Reduced_Application_final,
  #                 Mask) %>%
  #   dplyr::mutate(Applications=Reduced_Application_final) %>%
  # 
  #   # filter(!is.na(Segments_Activity)) %>%  ## What is Segments_Activity and why removing NA??
  #   replace_na(list(Segments_Activity = "Missing Activity")) %>%
  # 
  #   dplyr::mutate(Segments_Activity=case_when(Treatment=="RB"~"Out",
  #                                           TRUE~.$Segments_Activity)) %>%
  #   dplyr::group_by(Participant_ID, Day) %>%
  #   dplyr::mutate(Counter=sequence(rle(as.character(Segments_Activity))$lengths),
  #          Segment=case_when(Segments_Activity=="Out" & Counter==1~1, TRUE~0),
  #          Segment=ifelse(Segment==1, cumsum(Segment==1), NA),
  #          Segment=na.locf0(Segment)) %>%
  #   dplyr::select(-Counter)
  # 
  # # View(segment_df)
  # convert_to_csv(segment_df, file.path(physiological_data_path, segment_df_file_name))
  # #################################################################################################################

  
  
  
  ################################################################################################################################
  #           (end time - start time) vs. total row  --> Because after RB there was a time gap + Activity might not be continuous
  ################################################################################################################################
  segment_df <- custom_read_csv(file.path(physiological_data_path, segment_df_file_name))

  segment_meta_data_df_1 <- segment_df %>%
    dplyr::group_by(Participant_ID, Day) %>%
    dplyr::summarize(
          Mean_PP_RestingBaseline=mean(Trans_PP[Segments_Activity=="Out" & Segment==1], na.rm = TRUE), ##------------!!
          Length_RestingBaseline=length(Trans_PP[Segments_Activity=="Out" & Segment==1])) %>% ##------------!!
    ungroup() 
  
  segment_meta_data_df_2 <- segment_df %>%
    dplyr::filter(Treatment=='WS') %>% 
    dplyr::group_by(Participant_ID, Day) %>%
    dplyr::summarize(
      Length_Day=n(),
      Length_Day_Timestamp=as.numeric(difftime(tail(Timestamp, 1), head(Timestamp, 1), units = "secs")+1),
      DiffLengthDaySec=Length_Day_Timestamp-Length_Day,
      DiffLengthDayPercentage=100*(DiffLengthDaySec)/Length_Day_Timestamp,
      ) %>%
    ungroup()
    
  segment_meta_data_df <- segment_df %>%
    dplyr::group_by(Participant_ID, Day, Segment) %>%
    dplyr::summarize(
      
          StartSegmentTime=head(Timestamp, 1),
          EndSegmentTime=tail(Timestamp, 1),
          DiffSegmentTimeStamp=as.numeric(difftime(EndSegmentTime, StartSegmentTime, units = "secs")+1),
          
          Length_Segment=n(),
          DiffSegmentTimeSec=DiffSegmentTimeStamp-Length_Segment,
          DiffSegmentTimePercentage=100*(DiffSegmentTimeSec)/DiffSegmentTimeStamp,
          
          Length_Break=sum(Segments_Activity=="Out", na.rm = TRUE),
          Length_RW=sum(Segments_Activity=="RW", na.rm = TRUE),
          Length_SP=sum(Segments_Activity=="SP", na.rm = TRUE),
          Length_SA=sum(Segments_Activity=="SA", na.rm = TRUE),
          Length_MT=sum(Segments_Activity=="MT", na.rm = TRUE),
          Length_Missing_Activity=sum(Segments_Activity=="Missing Activity", na.rm = TRUE),
          Length_Other_Activities=sum(Segments_Activity=="Other", na.rm = TRUE),
          
        
          # WP_Sec=length(Applications[Applications=="Document Apps" & !is.na(Applications)]),
          # EM_Sec=length(Applications[Applications=="Email" & !is.na(Applications)]),
          # EA_Sec=length(Applications[Applications=="Entertaining Apps" & !is.na(Applications)]),
          # PA_Sec=length(Applications[Applications=="Programming Apps" & !is.na(Applications)]),
          # VC_Sec=length(Applications[Applications=="Virtual Communication Apps" & !is.na(Applications)]),
          # UT_Sec=length(Applications[Applications=="Utilities Apps" & !is.na(Applications)]),
          # WB_Sec=length(Applications[Applications=="Web Browsing Apps" & !is.na(Applications)]),
          # NO_APP_Sec=length(Applications[is.na(Applications)]),
          
          WP_Sec=sum(Applications=="Document Apps", na.rm = TRUE),
          EM_Sec=sum(Applications=="Email", na.rm = TRUE),
          EA_Sec=sum(Applications=="Entertaining Apps", na.rm = TRUE),
          PA_Sec=sum(Applications=="Programming Apps", na.rm = TRUE),
          VC_Sec=sum(Applications=="Virtual Communication Apps", na.rm = TRUE),
          UT_Sec=sum(Applications=="Utilities Apps", na.rm = TRUE),
          WB_Sec=sum(Applications=="Web Browsing Apps", na.rm = TRUE),
          NO_APP_Sec=sum(is.na(Applications)),
          
          Mean_PP_RW=mean(Trans_PP[Segments_Activity=="RW"], na.rm = TRUE),
          Mean_PP_Other_Activities=mean(Trans_PP[Segments_Activity=="Other"], na.rm = TRUE),
          
          ) %>% 
    dplyr::ungroup() %>% 
    
    dplyr::mutate(Segment_Order_Percentage=lag(Length_Segment),
                  Segment_Order_Percentage=case_when(Segment==1~0,
                                                     TRUE~as.double(Segment_Order_Percentage))) %>% 
  
    dplyr::group_by(Participant_ID, Day) %>%
    dplyr::mutate(Segment_Order_Percentage=cumsum(Segment_Order_Percentage),
                  
                  Cum_T_WP=cumsum(WP_Sec),
                  Cum_T_EM=cumsum(EM_Sec),
                  Cum_T_EA=cumsum(EA_Sec),
                  Cum_T_PA=cumsum(PA_Sec),
                  Cum_T_VC=cumsum(VC_Sec),
                  Cum_T_UT=cumsum(UT_Sec),
                  Cum_T_WB=cumsum(WB_Sec),
                  Cum_T_NO_APP=cumsum(NO_APP_Sec),
                  
                  Cum_T_Segment=cumsum(Length_Segment),
                  Cum_T_Break=cumsum(Length_Break),
                  Cum_T_RW=cumsum(Length_RW),
                  Cum_T_SP=cumsum(Length_SP),
                  Cum_T_SA=cumsum(Length_SA),
                  Cum_T_MT=cumsum(Length_MT),
                  Cum_T_Missing_Activity=cumsum(Length_Missing_Activity),
                  Cum_T_Other_Activities=cumsum(Length_Other_Activities),
                  ) %>% 
      
    merge(segment_meta_data_df_1, by=c("Participant_ID", "Day")) %>%
    merge(segment_meta_data_df_2, by=c("Participant_ID", "Day")) %>%
    
    dplyr::mutate(
                  ################################################################################
                  Segment_Order_Percentage=round(100*Segment_Order_Percentage/Length_Day, 0),
                  Segment_Order_Percentage=ifelse(Segment_Order_Percentage==0, 0.05, Segment_Order_Percentage),
                  ################################################################################

                  
                  ################################################################################
                  CT_SL=round(100*Cum_T_Segment/Length_Day, 2), ## For some cases, the CT_SL exceeds 100, because Cum_T_Segment includes RB, but Length_Day does not
                  
                  CT_RW=round(100*Cum_T_RW/Length_Day, 2),
                  CT_Out=round(100*Cum_T_Break/Length_Day, 2),
                  CT_SP=round(100*Cum_T_SP/Length_Day, 2),
                  CT_SA=round(100*Cum_T_SA/Length_Day, 2),
                  CT_MT=round(100*Cum_T_MT/Length_Day, 2),
                  CT_Missing_Activity=round(100*Cum_T_Missing_Activity/Length_Day, 2),
                  CT_Other_Activities=round(100*Cum_T_Other_Activities/Length_Day, 2),
                  
                  CT_Activity_Sum=CT_RW+CT_Out+CT_SP+CT_SA+CT_MT+CT_Missing_Activity+CT_Other_Activities,
                  ################################################################################
                  
                  
                  ################################################################################
                  CT_WP=round(100*Cum_T_WP/Length_Day, 2),
                  CT_EM=round(100*Cum_T_EM/Length_Day, 2),
                  CT_EA=round(100*Cum_T_EA/Length_Day, 2),
                  CT_PA=round(100*Cum_T_PA/Length_Day, 2),
                  CT_VC=round(100*Cum_T_VC/Length_Day, 2),
                  CT_UT=round(100*Cum_T_UT/Length_Day, 2),
                  CT_WB=round(100*Cum_T_WB/Length_Day, 2),
                  CT_NO_APP=round(100*Cum_T_NO_APP/Length_Day, 2),
                  
                  CT_Application_Sum=CT_WP+CT_EM+CT_EA+CT_PA+CT_VC+CT_UT+CT_WB+CT_NO_APP,
                  ################################################################################
                  
                  
                  ################################################################################
                  Mean_PP_RW_Normalized=Mean_PP_RW - Mean_PP_RestingBaseline,
                  Mean_PP_Other_Activities_Normalized=Mean_PP_Other_Activities - Mean_PP_RestingBaseline,
                  ################################################################################
                  ) %>%

    
    dplyr::mutate(T_D=Length_Day) %>% 
    dplyr::select(
      Participant_ID,
      Day,
      
      Length_Day,
      Length_Day_Timestamp,
      DiffLengthDaySec,
      DiffLengthDayPercentage,
      
      StartSegmentTime,
      EndSegmentTime,
      DiffSegmentTimeStamp,
      DiffSegmentTimePercentage,
      DiffSegmentTimeSec,
      
      Segment,
      Length_Segment,
      Segment_Order_Percentage,
      
      Length_RestingBaseline,
      Mean_PP_RestingBaseline,
      
      T_D, ## Exactly same as Length_Day
      Length_Break,
      Length_RW,
      Length_Missing_Activity,
      Length_Other_Activities,
      
      CT_SL,
      CT_RW,
      CT_Out,
      CT_SP,
      CT_SA,
      CT_MT,
      CT_Missing_Activity,
      CT_Other_Activities,
      CT_Activity_Sum,
      
      CT_WP,
      CT_EM,
      CT_EA,
      CT_PA,
      CT_VC,
      CT_UT,
      CT_WB,
      CT_NO_APP,
      CT_Application_Sum,
      
      Mean_PP_RW,
      Mean_PP_Other_Activities,
      
      Mean_PP_RW_Normalized,
      Mean_PP_Other_Activities_Normalized,
    )

  View(segment_meta_data_df)
  convert_to_csv(segment_meta_data_df, file.path(physiological_data_path, segment_meta_data_df_file_name))
  #################################################################################################################
  
  
  
  #################################################################################################################
  segment_meta_data_mini_df <- segment_meta_data_df %>%
    dplyr::select(
      Participant_ID,
      Day,
      Segment,
      CT_SL,
      CT_Activity_Sum,
      CT_Application_Sum
      )
  View(segment_meta_data_mini_df)
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
  
  View(investigation_df)
  convert_to_csv(investigation_df, file.path(physiological_data_path, paste0("investigation_", data_file_name)))
  #################################################################################################################
}


#-------------------------#
#-------Main Program------#
#-------------------------#
generate_meta_data_break_activity()
### investigate_data()



