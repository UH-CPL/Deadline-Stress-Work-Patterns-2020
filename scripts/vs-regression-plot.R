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
  if (file_type=="raw") {
    file_name <- qc1_raw_mean_v1_file_name
  } else if (file_type=="transformed") {
    file_name <- qc1_transformed_mean_v1_file_name
  } else if (file_type=="normalized") {
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


draw_regression_plot <- function(df, file_type) {
  df <- df %>%
    filter(Treatment=='WS') %>%
    select(Participant_ID,	Day, E4_HR, iWatch_HR) %>%
    na.omit() %>% 
    mutate(Diff_HR=abs(E4_HR-iWatch_HR),
           ID=paste0(Participant_ID, '-', Day)) %>% 
    arrange(desc(Diff_HR)) %>% 
    # mutate(Is_Outlier = ifelse(rownames(.) %in% c(seq(1, 5)), "y", "n")) 
    mutate(Is_Outlier = ifelse(rownames(.) %in% c(seq(1, 5)), 1, 0)) 
  
  print(head(df, 7))
  
  x_col <- 'E4_HR'
  y_col <- 'iWatch_HR'
  
  cor_test <- cor.test(df[[x_col]], df[[y_col]], method = "pearson")
  sample_no <- df %>% dplyr::summarize(n = dplyr::n())
  
  annot_label <- paste0("n = ", sample_no,
                        ", p = ", round_p_value(cor_test$p.value), 
                        ", r = ", specify_decimal(cor_test$estimate, 3))
  
  
  # myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
  # sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(1, 8))
  
  plot <- df %>%
    ggplot(aes(df[[x_col]], df[[y_col]], color=df$Is_Outlier)) +
    geom_point(size = 3) +
    geom_smooth(method = "lm") +
    ggtitle(capitalize(file_type)) +
    theme_bw() + 
    xlab(x_col) +
    ylab(y_col) +
    scale_colour_gradient(
      low = "#132B43",
      high = "#bf0b0b") +
    # scale_colour_gradient2(
    #   low = "blue",
    #   high = "red") +
    # scale_colour_gradientn(colours = myPalette(100), limits=c(0, 1)) +
    # scale_color_manual(values=c("red", "blue")) +
    # scale_color_manual(values = c("y" = "red", "n" = "blue")) +
    # scale_x_continuous(limits = c(min(qc1_mean_df$N.HR), max(qc1_mean_df$N.HR))) +
    # scale_y_continuous(limits = c(min(qc1_mean_df$HR), max(qc1_mean_df$HR)),
    #                    expand = c(0.2, 0, 0.2, 0)) +
    annotate("text",
             x=max(df[[x_col]]),
             y=Inf,
             hjust=1,
             vjust=1.5,
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

draw_regression_plots <- function() {
  
  plot_list <<- list()
  file_types <- c("raw", "transformed", "normalized")
  
  for (file_type in file_types) {
    mean_df <- read_files(file_type)
    # print(head(mean_df, 2))
    draw_regression_plot(mean_df, file_type)
  }
  
  save_plot('hr_regression', plot_grid(plotlist=plot_list, ncol=2), 28, 20)
  
}


#-------------------------#
#-------Main Program------#
#-------------------------#
# draw_regression_plots()

# library(dplyr)
# df <- data.frame(x = 5:1)
# idx <- c(2, 4)
# df %>% mutate(x = ifelse(row_number(.) %in% idx, x+1, x))



