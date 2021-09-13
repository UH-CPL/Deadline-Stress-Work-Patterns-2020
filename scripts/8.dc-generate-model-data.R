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
# 
# source(file.path(script_dir, 'us-common-functions.R'))




#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
generate_daywise_model_data <- function() {
  physiological_data_path <- file.path(project_dir, curated_data_dir, physiological_data_dir)
  
  full_df <- custom_read_csv(file.path(physiological_data_path, full_df_osf_file_name))
  mean_df <- custom_read_csv(file.path(physiological_data_path, qc1_normalized_mean_v1_file_name))
  
  percentage_df <- full_df %>%
    ### Activity1, Activity2, Activity3, Activities
    dplyr::select(Participant_ID,	Day, Treatment, Applications) %>%
    dplyr::filter(Treatment=='WS') %>%
    dplyr::group_by(Participant_ID, Day) %>%
    
    dplyr::summarize(DL=n(),
                     
                     DW_Sec=coalesce(sum(Applications=="Document Apps"), 0),
                     # EM_Sec=coalesce(sum(Applications=="Email"), 0),
                     # EA_Sec=coalesce(sum(Applications=="Entertaining Apps"), 0),
                     # PA_Sec=coalesce(sum(Applications=="Programming Apps"), 0),
                     # VC_Sec=coalesce(sum(Applications=="Virtual Communication Apps"), 0),
                     
                     DW=round(100*coalesce(sum(Applications=="Document Apps"), 0)/n(), 2),
                     # EM=round(100*coalesce(sum(Applications=="Email"), 0)/n(), 2),
                     # EA=round(100*coalesce(sum(Applications=="Entertaining Apps"), 0)/n(), 2),
                     # PA=round(100*coalesce(sum(Applications=="Programming Apps"), 0)/n(), 2),
                     # VC=round(100*coalesce(sum(Applications=="Virtual Communication Apps"), 0)/n(), 2)
                     
                     ) %>% 

    ungroup()
  
  # View(full_df)
  # View(mean_df)
  View(percentage_df)
  
  print(unique(full_df[c("Applications")]))
}




#-------------------------#
#-------Main Program------#
#-------------------------#
generate_daywise_model_data()



