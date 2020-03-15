#-------------------------#
#--------LIBRARIES--------#
#-------------------------#
library(tidyverse) 
library(plyr) 
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
setwd(project_dir)

source(file.path(script_dir, 'us-common-functions.R'))



qc0_df <- tibble()
qc1_df <- tibble()


# signal_name_list <- c('PP')
signal_name_list <- c('PP', 'E4_HR', 'E4_EDA', 'iWatch_HR')


#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
read_data <- function() {
  qc0_df <<- custom_read_csv(file.path(project_dir, curated_data_dir, physiological_data_dir, qc0_final_file_name)) %>% 
    select(Participant_ID, Day, Treatment, TreatmentTime, PP, E4_HR, E4_EDA, iWatch_HR)
  
  qc1_df <<- custom_read_csv(file.path(project_dir, curated_data_dir, physiological_data_dir, qc1_file_name))
}

generate_qq_plot <- function(signal) {
  
}


draw_plots <- function() {
  for (signal in signal_list) {
    generate_qq_plot(signal)
  }
}


#-------------------------#
#-------Main Program------#
#-------------------------#
read_data()
draw_plots()





