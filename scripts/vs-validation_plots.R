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



qc1_deadline_mean_df <- tibble()


signal_name_list <- c('PP')
# signal_name_list <- c('PP', 'E4_HR', 'E4_EDA', 'iWatch_HR')


#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
read_files <- function() {
  qc1_deadline_mean_df <<- custom_read_csv(file.path(project_dir, curated_data_dir, physiological_data_dir, qc1_treatment_mean_file_name))
  # qc1_deadline_mean_df <<- custom_read_csv(file.path(project_dir, curated_data_dir, physiological_data_dir, qc1_deadline_mean_file_name))
  # print_msg(colnames(qc1_deadline_mean_df))
  # print_msg(head(qc1_deadline_mean_df, 2))
}



draw_plots <- function() {
  for (signal_name in signal_name_list) {
    plot_list <- list()
  }
}


draw_validation_plots <- function() {
  read_files()
  draw_plots()
}



#-------------------------#
#-------Main Program------#
#-------------------------#
draw_validation_plots()





