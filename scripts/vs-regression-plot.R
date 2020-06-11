#-------------------------#
#--------LIBRARIES--------#
#-------------------------#
library(ggplot2)
library(cowplot)
library(dplyr)
library(tidyverse)
library(Hmisc)



#-------------------------#
#-----GLOBAL VARIABLES----#
#-------------------------#



#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
read_files <- function(file_type) {
  if (file_type=="Smooth") {
    file_name <- qc1_raw_mean_v1_file_name
  } else if (file_type=="Transformed") {
    file_name <- qc1_transformed_mean_v1_file_name
  } else if (file_type=="Normalized") {
    file_name <- qc1_normalized_mean_v1_file_name
  }
    
  custom_read_csv(file.path(project_dir, curated_data_dir, physiological_data_dir, file_name))
}


round_p_value <- function(p_value) {
  if (p_value < 0.00001) {
    return(0)
  }
  return(p_value)
}


draw_regression_plot <- function(df, file_type, x_col, y_col) {
  outlier_no <- 5
  if ('PP' %in% c(x_col, y_col)) {
    outlier_no <- 6
  }
  
  df <- df %>%
    filter(Treatment=='WS') %>%
    dplyr::select(Participant_ID,	Day, !!x_col, !!y_col) %>%
    na.omit() %>% 
    mutate(Diff_Signal=abs(.[[x_col]]-.[[y_col]]),
           ID=paste0(Participant_ID, '-', Day)) %>% 
    arrange(desc(Diff_Signal)) %>% 
    # mutate(Is_Outlier = ifelse(rownames(.) %in% c(seq(1, outlier_no)), "y", "n")) 
    mutate(Is_Outlier = ifelse(rownames(.) %in% c(seq(1, outlier_no)), 1, 0)) 
  
  if (remove_outlier_regression_plot==T & 'PP' %in% c(x_col, y_col)) {
    df <- df %>% 
      slice(outlier_no+1:n())
  }
  
  cor_test <- cor.test(df[[x_col]], df[[y_col]], method = "pearson")
  sample_no <- df %>% dplyr::summarize(n = dplyr::n())
  
  annot_label <- paste0("n = ", sample_no,
                        ", p = ", round_p_value(cor_test$p.value), 
                        ", r = ", specify_decimal(cor_test$estimate, 3))
  
  
  outlier_df <- df[df$Is_Outlier==1,]

  plot <- ggplot() +
    geom_point(data=df, aes(df[[x_col]], df[[y_col]]), size = 3) +
    geom_point(data=outlier_df, aes(outlier_df[[x_col]], outlier_df[[y_col]]), color='red', size = 3.5) +
    geom_text(data=outlier_df, 
              aes(outlier_df[[x_col]], outlier_df[[y_col]], label = ID),
              angle = 45,
              vjust = 2,
              # hjust = -0.5,
              size = 6
              ) +
    geom_smooth(data=df, aes(df[[x_col]], df[[y_col]]), method = "lm") +
    ggtitle(capitalize(file_type)) +
    theme_bw() + 
    xlab(x_col) +
    ylab(y_col) +
    annotate("text",
             x=max(df[[x_col]]),
             y=Inf,
             hjust=1,
             vjust=1.5,
             # angle = 45,
             label=annot_label,
             fontface = 'italic', 
             size = 8) +
    theme_bw() +
    theme(text = element_text(size=22),
          axis.text = element_text(size = 18),
          plot.title = element_text(hjust = 0.5),
          legend.position = "none",
          plot.margin = unit(c(1, 0.5, 1, 0.5), "lines"),  ##top, right, bottom, left
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black")) 
  
  plot_list[[length(plot_list)+1]] <<- plot
}

draw_regression_plots_treatment <- function(treatment, x_col, y_col) {
  plot_list <<- list()
  
  file_types <- c("Smooth", "Transformed")
  if (treatment=='WS') {
    file_types <- c(file_types, "Normalized")
  }
  
  for (file_type in file_types) {
    mean_df <- read_files(file_type)
    draw_regression_plot(mean_df, 
                         paste0(treatment, ' - ', file_type),
                         x_col,
                         y_col)
  }
  
  save_plot(paste0(tolower(x_col), '_', tolower(y_col), '_', tolower(treatment), '_regression'), 
            plot_grid(plotlist=plot_list, ncol=2), 
            30, 
            length(file_types)*6)
}


draw_regression_plots <- function() {
  # draw_regression_plots_treatment(treatment, 'E4_HR', 'iWatch_HR')
  
  # signal_name_list <- c('PP', 'E4_HR', 'E4_EDA', 'iWatch_HR')
  # 
  # for (idx_1 in 1:length(signal_name_list)) {
  #   for (idx_2 in idx_1+1:length(signal_name_list)) {
  #     if (idx_2<=length(signal_name_list)) {
  #       for (treatment in c('RB', 'WS')) {
  #         draw_regression_plots_treatment(treatment, signal_name_list[idx_1], signal_name_list[idx_2])
  #       }
  #     }
  #   }
  # }
  
  for (treatment in c('RB', 'WS')) {
    draw_regression_plots_treatment(treatment, 'E4_HR', 'iWatch_HR')
    draw_regression_plots_treatment(treatment, 'PP', 'E4_EDA')
  }
}


#-------------------------#
#-------Main Program------#
#-------------------------#
# draw_regression_plots()




