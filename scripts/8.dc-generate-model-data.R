#-------------------------#
#--------LIBRARIES--------#
#-------------------------#
library(dplyr)
# library(zoo)




#-------------------------#
#-----GLOBAL VARIABLES----#
#-------------------------#
# script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
# project_dir <- dirname(script_dir)
# setwd(project_dir)

# source(file.path(script_dir, 'us-common-functions.R'))




#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
generate_daywise_model_data <- function() {
  physiological_data_path <- file.path(project_dir, curated_data_dir, physiological_data_dir)
  
  full_df <- custom_read_csv(file.path(physiological_data_path, full_df_file_name))
  mean_df <- custom_read_csv(file.path(physiological_data_path, qc1_normalized_mean_v1_file_name))
  segment_meta_data_df <- custom_read_csv(file.path(physiological_data_path, segment_meta_data_df_file_name)) %>% 
    dplyr::select(Participant_ID,	Day, Segment) %>% 
    dplyr::group_by(Participant_ID, Day) %>%
    dplyr::summarise(Max_Segment = max(Segment)) %>% 
    dplyr::ungroup()
  
  ##------------!!
  # View(segment_meta_data_df)
  
  model_df <- full_df %>%
    ### Activity1, Activity2, Activity3, Activities
    # dplyr::select(Participant_ID,	Day, Treatment, Applications, Segments_Activity) %>%
    
    dplyr::select(Participant_ID,	Day, Treatment, Reduced_Application_final, Segments_Activity) %>%
    dplyr::rename(Applications=Reduced_Application_final) %>% 
    
    dplyr::filter(Treatment=='WS') %>%
    dplyr::group_by(Participant_ID, Day) %>%
    dplyr::summarize(T_D=n(), ##------------!!
                     Break_Time=sum(Segments_Activity=="Out", na.rm = TRUE), ##------------!!
                     
                     # WP_Sec=coalesce(sum(Applications=="Document Apps", na.rm = T), 0),
                     # EM_Sec=coalesce(sum(Applications=="Email", na.rm = T), 0),
                     # EA_Sec=coalesce(sum(Applications=="Entertaining Apps", na.rm = T), 0),
                     # PA_Sec=coalesce(sum(Applications=="Programming Apps", na.rm = T), 0),
                     # VC_Sec=coalesce(sum(Applications=="Virtual Communication Apps", na.rm = T), 0),
                     # UT_Sec=coalesce(sum(Applications=="Utilities Apps", na.rm = T), 0),
                     # WB_Sec=coalesce(sum(Applications=="Web Browsing Apps", na.rm = T), 0),
                     # NO_APP_Sec=coalesce(sum(is.na(Applications)), 0),
                     # 
                     # T_WP=round(100*WP_Sec/T_D, 2),
                     # T_EM=round(100*EM_Sec/T_D, 2),
                     # T_EA=round(100*EA_Sec/T_D, 2),
                     # T_PA=round(100*PA_Sec/T_D, 2),
                     # T_VC=round(100*VC_Sec/T_D, 2),
                     # T_UT=round(100*UT_Sec/T_D, 2),
                     # T_WB=round(100*WB_Sec/T_D, 2),
                     # T_NO_APP=round(100*NO_APP_Sec/T_D, 2),
                     
                     WP_Sec=length(Applications[Applications=="Document Apps" & !is.na(Applications)]),
                     EM_Sec=length(Applications[Applications=="Email" & !is.na(Applications)]),
                     EA_Sec=length(Applications[Applications=="Entertaining Apps" & !is.na(Applications)]),
                     PA_Sec=length(Applications[Applications=="Programming Apps" & !is.na(Applications)]),
                     VC_Sec=length(Applications[Applications=="Virtual Communication Apps" & !is.na(Applications)]),
                     UT_Sec=length(Applications[Applications=="Utilities Apps" & !is.na(Applications)]),
                     WB_Sec=length(Applications[Applications=="Web Browsing Apps" & !is.na(Applications)]),
                     NO_APP_Sec=length(Applications[is.na(Applications)]),

                     T_WP=round(100*WP_Sec/T_D, 2),
                     T_EM=round(100*EM_Sec/T_D, 2),
                     T_EA=round(100*EA_Sec/T_D, 2),
                     T_PA=round(100*PA_Sec/T_D, 2),
                     T_VC=round(100*VC_Sec/T_D, 2),
                     T_UT=round(100*UT_Sec/T_D, 2),
                     T_WB=round(100*WB_Sec/T_D, 2),
                     T_NO_APP=round(100*NO_APP_Sec/T_D, 2),
                     
                     T_Percentage_Sum=T_WP+T_EM+T_EA+T_PA+T_VC+T_UT+T_WB+T_NO_APP
                     
                     ) %>% 
    dplyr::ungroup() %>% 
    
    merge(mean_df, by=c('Participant_ID', 'Day'), all=T) %>%
  
    ##------------!!
    merge(segment_meta_data_df, by=c('Participant_ID', 'Day'), all=T) %>%
    dplyr::mutate(tOut=ifelse(Max_Segment==1, 0, Break_Time/(Max_Segment-1)),
                  fOut=(Max_Segment-1)*3600/T_D) %>%
    
    # tOut: The mean break time in seconds during the daily observation period. 
    # fOut The number of breaks per hour during the daily observation period.
    
    dplyr::select(
      Participant_ID,
      Day,
      Treatment,

      T_D,

      PP,
      E4_HR,
      E4_EDA,
      iWatch_HR,

      tOut,
      fOut,

      WP_Sec,
      EM_Sec,
      EA_Sec,
      PA_Sec,
      VC_Sec,
      UT_Sec,
      WB_Sec,
      NO_APP_Sec,

      T_WP,
      T_EM,
      T_EA,
      T_PA,
      T_VC,
      T_UT,
      T_WB,
      T_NO_APP,

      T_Percentage_Sum
    )
  
  # View(full_df)
  # View(mean_df)
  # print(unique(full_df[c("Applications")]))
  
  View(model_df)
  convert_to_csv(model_df, file.path(physiological_data_path, model_df_file_name))
}




#-------------------------#
#-------Main Program------#
#-------------------------#
# generate_daywise_model_data()



