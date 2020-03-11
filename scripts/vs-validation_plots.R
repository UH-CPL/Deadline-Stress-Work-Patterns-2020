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



qc1_deadline_mean_df <- tibble()
qc2_deadline_mean_df <- tibble()

mean_df <- tibble()
significance_df <- tibble()


# signal_name_list <- c('PP')
signal_name_list <- c('PP', 'E4_HR', 'E4_EDA', 'iWatch_HR')


#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
read_files <- function() {
  qc1_deadline_mean_df <<- custom_read_csv(file.path(project_dir, curated_data_dir, physiological_data_dir, qc1_deadline_mean_file_name))
  # print_msg(colnames(qc1_deadline_mean_df))
  # print_msg(head(qc1_deadline_mean_df, 2))
  
  
  
  #################################################
  #                DO NOT CHNAGE                  #   
  #################################################
  mean_df <<- qc1_deadline_mean_df
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
  } else if (is_match(col_name, 'E4_EDA')) {
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



conduct_test <- function(df, day, signal_name, test_type) {
  # print(df)
  
  # We calculate the p-value for this session difference
  if (test_type == "t" | test_type == "tt") {
    p_val <- t.test(df[[signal_name]])$p.value
  } else if (test_type == "w") {
    p_val <- wilcox.test(df[[signal_name]])$p.value
  }

  # We find the sign of our results (this is the '*' thing we put in our two plots way up above)
  sign <- get_significance_sign(p_val)
  # print(paste(p_val, sign))
  print(paste('----------------------', sign))

  # Then we add EVERYTHING to that tibble we made earlier to hold it for us
  significance_df <<- rbind.fill(significance_df, tibble(Signal = signal_name,
                             Day = day,
                             Test_Type = test_type,
                             p_val = p_val,
                             n = nrow(df),
                             # n = 54,
                             Significance = sign))
  # print(paste("Day:", day, "Test Type:", test_type, "p-value:", p_val, "Significance:", sign))
  # print(significance_df)
}


get_significance <- function(signal_name) {
  
  # Set a global variable to determine which test we do: 
  # 't' = t-test 
  # 'tt' = transformed t-test 
  # 'w' = Wilcoxon test 
  test_type <- "t" 
  end_str <- NA 
  
  # Firstly, get rid of missing values
  temp_mean_df <- mean_df[!is.na(mean_df[[signal_name]]), ] 
  
  if (nrow(temp_mean_df) != 0) {
    # Ensure that we always log for PP and EDA 
    # Of course to do that, we have to make sure all values are above 0 and shift up by the minimum value (plus a bit more) if not! 
    # ------------- If we do this, this should reflect on plot too!!!!!
    # if (signal_name == "PP" || signal_name == "E4_EDA") {
    #   shift_val <- 0
    #   if (min(temp_mean_df[[signal_name]]) <= 0) {
    #     shift_val <- min(temp_mean_df[[signal_name]]) + 0.001
    #   }
    #   temp_mean_df[[signal_name]] <- log(temp_mean_df[[signal_name]]) + shift_val
    # }
    
    shapiro_test_results <- shapiro.test(temp_mean_df[[signal_name]] ) 
    
    if (shapiro_test_results$p.value < 0.05) {
      test_type <- "w"
    } else {
      test_type <- "t"
    }
    
    if (signal_name == "PP" || signal_name == "E4_EDA") { 
      test_type = "tt" 
    } 
    
    
    days <- levels(factor(temp_mean_df$Day))
    
    for (day in days) {
      print(paste(signal_name, day, test_type))
      conduct_test(temp_mean_df[temp_mean_df$Day==day,], day, signal_name, test_type)
    }
    
    print(significance_df)
    convert_to_csv(significance_df, file.path(curated_data_dir, physiological_data_dir, significance_file_name))
    
  }
}



process_significance_table <- function() {
  for (signal_name in signal_name_list) {
    get_significance(signal_name)
  }
}


draw_plots <- function() {
  for (signal_name in signal_name_list) {
    plot_list <- list()
    
    plot_df <- mean_df %>% 
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
      geom_hline(yintercept=0, linetype="dashed", color = "red", alpha = 0.6, size=1) +
      # scale_x_discrete(labels=labels_vec) + 
      stat_summary(fun.y = mean, color = "darkred", geom = "point", shape = 3, size = 4, show_guide = FALSE) +
      stat_summary(fun.data = get_n, geom = "text", size = 6) +
      annotate("text", x=1, y=Inf, label= sign[1], vjust = 1.2, size = 10) +
      annotate("text", x=2, y=Inf, label= sign[2], vjust = 1.2, size = 10) +
      scale_y_continuous(expand = c(0.15, 0, 0.15, 0))
    
    print(plot)
    save_plot(paste0(signal_name, '_validation'), plot)
  }
}


draw_validation_plots <- function() {
  read_files()
  process_significance_table()
  draw_plots()
}



#-------------------------#
#-------Main Program------#
#-------------------------#
draw_validation_plots()





