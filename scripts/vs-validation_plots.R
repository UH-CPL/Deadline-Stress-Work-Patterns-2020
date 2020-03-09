#-------------------------#
#--------LIBRARIES--------#
#-------------------------#
library(tidyverse) 
# library(knitr) 
# library(grid) 
# library(gridExtra)
# library(ggpubr) 
# library(kableExtra) 
# library(cowplot) 
# library(gsubfn)




#-------------------------#
#-----GLOBAL VARIABLES----#
#-------------------------#
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
project_dir <- dirname(script_dir)

source(file.path(script_dir, 'us-common-functions.R'))



qc1_mean_df <- tibble()
qc1_deadline_mean_df <- tibble()


signal_name_list <- c('PP')
# signal_name_list <- c('PP', 'E4_HR', 'E4_EDA', 'iWatch_HR')


#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
read_files <- function() {
  qc1_mean_df <<- custom_read_csv(file.path(project_dir, curated_data_dir, physiological_data_dir, qc1_treatment_mean_file_name))
  # print_msg(colnames(qC1_df))  # "Participant_ID" "Day" "Treatment" "Timestamp" "Sinterface_Time" "TreatmentTime" "Raw_PP" "PP" "E4_HR" "E4_EDA" "iWatch_HR"
  # print_msg(head(qc1_df, 2))
}

get_signal_val <- function(df, day, signal_name) {
  # print(df[df$Day==day, signal_name])
  return(df[df$Day==day, signal_name])
}

generate_daywise_mean_data <- function() {
  subj_list <- unique(qc1_mean_df$Participant_ID)
  
  sapply(subj_list, function(subj) {
    qc1_mean_subj_df <- qc1_mean_df %>%
      filter(Participant_ID==subj & Treatment == 'WS')
    
    for (signal_name in signal_name_list) {
      
      day3_val = get_signal_val(qc1_mean_subj_df, 'Day3', signal_name)
      day4_val = get_signal_val(qc1_mean_subj_df, 'Day4', signal_name)
      vanilla_day_mean_val=(day3_val+day4_val)/2

      # print(paste(subj, signal_name, vanilla_day_mean_val))
      
      
      print(qc1_mean_subj_df)
      qc1_mean_subj_df <- qc1_mean_subj_df %>%
        filter(Day %in% c('Day1', 'Day2')) %>% 
        mutate(signal_name=signal_name-vanilla_day_mean_val)
      print(qc1_mean_subj_df)
    }
    
  })
  
  # qc1_deadline_mean_df <<- qc1_mean_df %>%
  #   filter(Treatment == 'WS') %>% 
  #   group_by(Participant_ID) %>% 
  #   summarise_at(vars(signal_name_list), funs(mean=get_mean, frames=get_total_frame))
}


draw_plots <- function() {
  for (signal_name in signal_name_list) {
    plot_list <- list()
  }
  
}

draw_validation_plots <- function() {
  read_files()
  generate_daywise_mean_data()
  draw_plots()
}







#-------------------------#
#-------Main Program------#
#-------------------------#
draw_validation_plots()





