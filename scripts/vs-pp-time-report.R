#-------------------------#
#--------LIBRARIES--------#
#-------------------------#
library(ggplot2)
library(cowplot)
library(dplyr)
library(tidyverse)
library(grid)
library(gridExtra)



#-------------------------#
#-----GLOBAL VARIABLES----#
#-------------------------#
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
project_dir <- dirname(script_dir)
setwd(project_dir)

source(file.path(script_dir, 'us-common-functions.R'))


#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
read_data <- function() {
  if (test==T) {
    full_df <<- custom_read_csv(file.path(project_dir, curated_data_dir, physiological_data_dir, 'mini_full_df.csv'))
  } else {
    full_df <<- custom_read_csv(file.path(project_dir, curated_data_dir, physiological_data_dir, full_df_file_name)) 
  }
  
  # print_msg(colnames(full_df))
}

generate_pp_time_plots <- function() {
  read_data()
  
  full_df <<- full_df %>% 
    mutate(Reduced_Activity_One=case_when(Treatment=='RB'~'RB',
                                          TRUE~Reduced_Activity_One))
  # View(full_df)
  
  if (test==T) {
    subj_list <- c('T003')
    day_list <- c('Day1')
    
  } else {
    subj_list <- unique(full_df$Participant_ID)
    subj_list <- subj_list[-length(subj_list)]
    day_list <- unique(full_df$Day)
  }
  
  for (subj in subj_list) {
    subj_df <- full_df %>% filter(Participant_ID==subj)
    plot_list <- list()
    
    for (day in day_list) {
      day_df <- subj_df %>% filter(Day==day) 
      
      plot <- ggplot(ToothGrowth, aes(x=dose, y=len)) + 
        geom_boxplot()
      
      plot_list[[length(plot_list)+1]] <- plot
    }
    
    grid_plot <- plot_grid(plot_list, ncol=2)
    save_plot(file.path(project_dir, plots_dir, pp_time_plots_dir, paste0(subj, '_', day)), grid_plot)
  }
}


#-------------------------#
#-------Main Program------#
#-------------------------#
test <<- T

generate_pp_time_plots()
# generate_pp_time_plots()




