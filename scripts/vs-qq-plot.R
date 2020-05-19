#-------------------------#
#--------LIBRARIES--------#
#-------------------------#
library(tidyverse) 
library(plyr) 
library(cowplot)




#-------------------------#
#-----GLOBAL VARIABLES----#
#-------------------------#
# script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
# project_dir <- dirname(script_dir)
# setwd(project_dir)
# 
# source(file.path(script_dir, 'us-common-functions.R'))



qc1_df <- tibble()
qc1_log_transformed_df <- tibble()


qq_plot_list <- list()
distribution_plot_list <- list()

# signal_list <- c('PP')
signal_list <- c('PP', 'E4_HR', 'E4_EDA', 'iWatch_HR')

test <- FALSE # TRUE FALSE
sample_size <- 1000

#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
read_data <- function() {
  qc1_df <<- custom_read_csv(file.path(project_dir, curated_data_dir, physiological_data_dir, qc1_file_name))
  qc1_log_transformed_df <<- custom_read_csv(file.path(project_dir, curated_data_dir, physiological_data_dir, qc1_log_transformed_file_name))
}

# get_shapiro_result <- function(data) {
#   test_val = shapiro.test(data)
#   if (test_val > 0.05) {
#     print('Distribution is normal')
#   } else {
#     print('Distribution is not normal')
#   }
# }

generate_qq_plot <- function(df, signal) {
  if (test==TRUE) {
    df <- df %>%
      select(!!signal) %>% 
      na.omit() %>% 
      slice(1:sample_size)
  }
  
  plot <- ggplot(df, aes(sample = .data[[signal]])) +
    stat_qq() +
    stat_qq_line() +
    ggtitle(signal) +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          plot.title = element_text(hjust = 0.5))

  # print(plot)
  qq_plot_list[[length(qq_plot_list)+1]] <<- plot
}


generate_distribution_plot <- function(df, signal) {
  if (test==TRUE) {
    df <- df %>%
      select(!!signal) %>% 
      na.omit() %>% 
      slice(1:sample_size)
  }
  
  plot <- ggplot(df, aes(x=.data[[signal]])) +
    geom_density(color="darkblue", fill="lightblue") +
    geom_vline(aes(xintercept=mean(.data[[signal]], na.rm=TRUE)),
               color="blue", linetype="dashed", size=1) +
    ggtitle(signal) +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          plot.title = element_text(hjust = 0.5))
  
  # print(plot)
  distribution_plot_list[[length(distribution_plot_list)+1]] <<- plot
}

draw_plots <- function(df, transformations_parameter) {
  
  qq_plot_list <<- list()
  distribution_plot_list <<- list()
  
  for (signal in signal_list) {
    generate_qq_plot(df, signal)
    generate_distribution_plot(df, signal)
    
    # generate_treatment_qq_plot(signal)
  }
  
  qq_grid_plot <- plot_grid(plotlist=qq_plot_list, align='v', ncol=2)
  distribution_grid_plot <- plot_grid(plotlist=distribution_plot_list, align='v', ncol=2)
  
  save_plot(paste0(transformations_parameter, '_qq_plot'), qq_grid_plot)
  save_plot(paste0(transformations_parameter, '_distribution_plot'), distribution_grid_plot)

}

draw_qq_plots <- function(test_input=FALSE) {
  test <<- test_input
  
  read_data()
  draw_plots(qc1_df, raw_parameter)
  draw_plots(qc1_log_transformed_df, log_tansformed_parameter)
}


#-------------------------#
#-------Main Program------#
#-------------------------#
# draw_qq_plots()





