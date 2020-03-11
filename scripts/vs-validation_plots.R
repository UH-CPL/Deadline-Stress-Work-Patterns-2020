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
setwd(project_dir)

source(file.path(script_dir, 'us-common-functions.R'))



qc1_deadline_mean_df <- tibble()


signal_name_list <- c('PP')
# signal_name_list <- c('PP', 'E4_HR', 'E4_EDA', 'iWatch_HR')


#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
read_files <- function() {
  qc1_deadline_mean_df <<- custom_read_csv(file.path(project_dir, curated_data_dir, physiological_data_dir, qc1_deadline_mean_file_name))
  # print_msg(colnames(qc1_deadline_mean_df))
  print_msg(head(qc1_deadline_mean_df, 2))
}





get_label <- function(col_name) { 
  if (is_match(col_name, 'PP')) { 
    # return(expression(Delta~"ln("~bar("PP")~paste('[',''^'o','C',''^2,'])')))
    return(expression(Delta~bar("PP")~paste('[',''^'o','C',''^2,']')))
  } else if (is_match(col_name, 'iWatch_HR')) {
    # if (test_type == "tt") {
    #   return(expression(Delta~"ln("~bar("Heart Rate")~') [BPM]'))
    # }
    return(expression(Delta~bar('D-Wrist HR')~' [BPM]'))
  } else if (is_match(col_name, 'E4_HR')) {
    # if (test_type == "tt") {
    #   return(expression(Delta~"ln("~bar("Heart Rate")~') [BPM]'))
    # }
    return(expression(Delta~bar('Non-D-Wrist HR')~' [BPM]'))
  } else if (is_match(col_name, 'EDA')) {
    # if (test_type == "tt") {
    #   return(expression(Delta~"ln("~bar("EDA")~paste('[', mu, 'S])')))
    # }
    return(expression(Delta~bar('EDA')~' ['~mu~'S]')) 
  } 
  return('Unknown axis.') 
}


# get_title <- function(col_name) { 
#   if (is_match(col_name, 'PP')) { 
#     return('Perinasal Perspiration')
#   } else if (is_match(col_name, 'iWatch_HR')) {
#     return('iWatch_HR')
#   } else if (is_match(col_name, 'E4_HR')) {
#     # if (test_type == "tt") {
#     #   return(expression(Delta~"ln("~bar("Heart Rate")~') [BPM]'))
#     # }
#     return(expression(Delta~bar('Non-D-Wrist HR')~' [BPM]'))
#   } else if (is_match(col_name, 'EDA')) {
#     if (test_type == "tt") {
#       return(expression(Delta~"ln("~bar("EDA")~paste('[', mu, 'S])')))
#     }
#     return(expression(Delta~bar('EDA')~' ['~mu~'S]')) 
#   } 
#   return('Unknown axis.') 
# }



draw_plots <- function() {
  for (signal_name in signal_name_list) {
    plot_list <- list()
    
    plot_df <- qc1_deadline_mean_df %>% 
      select(Day, signal_name) %>%
      # select(Day, !!signal_name) %>%
      drop_na()
    
    label <- get_label(signal_name) 
    # title <- get_title(signal_name) 
    
    plot <- ggplot(plot_df, aes(x = Day, y = plot_df[[signal_name]])) + 
      geom_boxplot() +
      # labs(title = title, y = label) + 
      labs(y = label) + 
      theme_bw(base_size = 18) + 
      theme(axis.title.x = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x=element_text(size=16, face='bold'),
            # plot.margin = unit(c(0.5, 0.5, 0.5, plot_margin_left), "lines"),  ##top, right, bottom, left
            axis.line = element_line(colour = "black")) + 
      geom_hline(yintercept=0, linetype="dashed", color = "red", alpha = 0.6, size=1) 
      # scale_x_discrete(labels=labels_vec) + 
      # stat_summary(fun.y = mean, color = "darkred", geom = "point", shape = 3, size = 4, show_guide = FALSE) + 
      # stat_summary(fun.data = give.n, geom = "text", size = 6) +
      # scale_y_continuous(expand = c(0.15, 0, 0.15, 0)) +
      annotate("text", x=1, y=Inf, label= sign[1], vjust = 1.2, size = 10) +
      annotate("text", x=2, y=Inf, label= sign[2], vjust = 1.2, size = 10) +
      # annotate("text", x=3, y=Inf, label= sign[4], vjust = 1.2, size = 10) +
      # annotate("text", x=4, y=Inf, label= sign[7], vjust = 1.2, size = 10)
    
    print(plot)
    save_plot(paste0(signal_name, '_validation'), plot)
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





