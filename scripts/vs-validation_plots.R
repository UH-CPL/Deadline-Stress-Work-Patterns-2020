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
# script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
# project_dir <- dirname(script_dir)
# # setwd(project_dir)
# 
# source(file.path(script_dir, 'us-common-functions.R'))

# validation_log_file <- file.path(project_dir, log_dir, paste0('validation-log-', format(Sys.Date(), format='%m-%d-%y'), '.txt'))
# file.create(validation_log_file)



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
  qc1_deadline_mean_df <<- custom_read_csv(file.path(project_dir, curated_data_dir, physiological_data_dir, qc1_log_trans_mean_v2_file_name))
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
    return(expression(Delta~bar("ln(PP)")~paste('[',''^'o','C',''^2,']')))
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
    return(expression(Delta~bar('ln(EDA)')~' ['~mu~'S]')) 
  } 
  return('Unknown axis.') 
}



conduct_test <- function(df, day, signal_name, test_type) {
  # print(df)
  
  col_name <- paste0(day, '_Normalize')
  
  # We calculate the p-value for this session difference
  # if (test_type == "t" | test_type == "tt") {
  #   p_val <- t.test(df[[col_name]], alternative='greater')$p.value
  #   p_val_2 <- t.test(df[[day]], df$Day3_Day4_Mean, alternative='greater')$p.value
  # } else if (test_type == "w") {
  #   p_val <- wilcox.test(df[[col_name]])$p.value
  #   p_val_2 <- wilcox.test(df[[day]], df$Day3_Day4_Mean)$p.value
  # }
  
  p_val_one_sample_two_sided <- t.test(df[[col_name]])$p.value
  p_val_one_sample_greater  <- t.test(df[[col_name]], alternative='greater')$p.value
  
  # p_val_two_sample_two_sided <- t.test(df[[day]], df$Day3_Day4_Mean, paired=TRUE)$p.value
  # p_val_two_sample_greater <- t.test(df[[day]], df$Day3_Day4_Mean, paired=TRUE, alternative='greater')$p.value
  
  
  w_val_one_sample_two_sided <- wilcox.test(df[[col_name]])$p.value
  w_val_one_sample_greater  <- wilcox.test(df[[col_name]], alternative='greater')$p.value
  
  # w_val_two_sample_two_sided <- wilcox.test(df[[day]], df$Day3_Day4_Mean, paired=TRUE)$p.value
  # w_val_two_sample_greater <- wilcox.test(df[[day]], df$Day3_Day4_Mean, paired=TRUE, alternative='greater')$p.value
  
  
  
  variance_val <- var.test(df[[day]], df$Day3_Day4_Mean, data=df)$p.value
  variance_greater_val <- var.test(df[[day]], df$Day3_Day4_Mean, data=df, alternative='greater')$p.value
  variance_less_val <- var.test(df[[day]], df$Day3_Day4_Mean, data=df, alternative='less')$p.value

  # We find the sign of our results (this is the '*' thing we put in our two plots way up above)
  # sign <- get_significance_sign(p_val)
  # print(paste(p_val, sign))
  # print(paste('----------------------', sign))

  # Then we add EVERYTHING to that tibble we made earlier to hold it for us
  significance_df <<- rbind.fill(significance_df, tibble(Signal = signal_name,
                             Day = day,
                             # Test_Type = test_type,
                             n = nrow(df),
                             
                             variance_val = variance_val,
                             variance_val_sig = get_significance_sign(variance_val),
                             
                             variance_greater_val = variance_greater_val,
                             variance_greater_val_sig = get_significance_sign(variance_greater_val),
                             
                             variance_less_val = variance_less_val,
                             variance_less_val_sig = get_significance_sign(variance_less_val),
                             
                             
                             
                             
                             
                             p_val_one_sample_two_sided = p_val_one_sample_two_sided,
                             p_val_one_sample_two_sided_sig = get_significance_sign(p_val_one_sample_two_sided),
                             
                             p_val_one_sample_greater = p_val_one_sample_greater,
                             p_val_one_sample_greater_sig = get_significance_sign(p_val_one_sample_greater),
                             
                             # p_val_two_sample_two_sided = p_val_two_sample_two_sided,
                             # p_val_two_sample_two_sided_sig = get_significance_sign(p_val_two_sample_two_sided),
                             # 
                             # p_val_two_sample_greater = p_val_two_sample_greater,
                             # p_val_two_sample_greater_sig = get_significance_sign(p_val_two_sample_greater),
                             
                             
                             
                             
                             w_val_one_sample_two_sided = w_val_one_sample_two_sided,
                             w_val_one_sample_two_sided_sig = get_significance_sign(w_val_one_sample_two_sided),
                             
                             w_val_one_sample_greater = w_val_one_sample_greater,
                             w_val_one_sample_greater_sig = get_significance_sign(w_val_one_sample_greater),
                             
                             # w_val_two_sample_two_sided = w_val_two_sample_two_sided,
                             # w_val_two_sample_two_sided_sig = get_significance_sign(w_val_two_sample_two_sided),
                             # 
                             # w_val_two_sample_greater = w_val_two_sample_greater,
                             # w_val_two_sample_greater_sig = get_significance_sign(w_val_two_sample_greater),
    
                             
                             ))
  # print(paste("Day:", day, "Test Type:", test_type, "p-value:", p_val, "Significance:", sign))
  # print(significance_df)
}


get_significance <- function(signal_name) {
  
  # 't' = t-test 
  # 'tt' = transformed t-test 
  # 'w' = Wilcoxon test 
  test_type <- "t" 
  end_str <- NA 
  
  temp_mean_df <- mean_df %>% 
    filter(Signal==signal_name) %>% 
    select(Participant_ID, Treatment, Signal, Day1, Day2, Day3_Day4_Mean, Day1_Normalize, Day2_Normalize) %>% 
    na.omit()
  
  if (nrow(temp_mean_df) != 0) {
    shapiro_result_day1 <- shapiro.test(temp_mean_df$Day1_Normalize ) 
    shapiro_result_day2 <- shapiro.test(temp_mean_df$Day2_Normalize ) 
    
    # if (shapiro_test_results$p.value < 0.05) {
    #   test_type <- "w"
    # } else {
    #   test_type <- "t"
    # }
    
    if (signal_name == "PP" || signal_name == "E4_EDA") { 
      test_type = "tt" 
    } 
    
    days <- c('Day1', 'Day2')
    
    for (day in days) {
      conduct_test(temp_mean_df, day, signal_name, test_type)
    }
    
    # print(significance_df)
    convert_to_csv(significance_df, file.path(curated_data_dir, physiological_data_dir, significance_file_name))
  }
}



process_significance_table <- function() {
  for (signal_name in signal_name_list) {
    get_significance(signal_name)
  }
}

print_outliers <- function(df, signal_name, day) {
  df <- df %>% 
    filter(Day==paste0(day, '_Normalize')) %>% 
    select(Participant_ID, Value) %>% 
    arrange(desc(Value))

  # write_log_msg(paste0('---------- Outliers for ', signal_name, ' Day1'), validation_log_file)
  # write_log_msg(df, validation_log_file)
  
  print(paste0('---------- Outliers for ', signal_name, ' ', day))
  print(df)
}

get_title <- function(signal_name) {
  
  if (baseline_parameter==lowest_baseline) {
    title <- 'lowest baseline'
  } else if (baseline_parameter==corresponding_baseline) {
    title <- 'corresponding baseline'
  } else if (baseline_parameter==day3_day4_ws_mean) {
    title <- 'day3 day4 mean'
  } else if (baseline_parameter==day3_day4_ws_min) {
    title <- 'day3 day4 min'
  } 
  
  # paste(signal_name, ' - ', title)
}

get_subj <-function(df, signal_val) {
  df <- df %>% 
    filter(Value==signal_val) %>% 
    select(Participant_ID) %>% 
    pull()
  
  # print(df)
  df
}

draw_plots <- function() {
  for (signal_name in signal_name_list) {
    plot_list <- list()
    
    plot_df <- mean_df %>% 
      filter(Signal==signal_name) %>% 
      select(Participant_ID, Day1_Normalize, Day2_Normalize) %>%
      # select(Participant_ID, Day1, Day2) %>% 
      na.omit() %>% ########### CHECK ME!!!!
      gather(Day, Value, -Participant_ID) %>% 
      na.omit() ########### CHECK ME!!!!
    
    # sign <- significance_df$Significance 
    sign <- significance_df$p_val_one_sample_greater_sig 
    label <- get_label(signal_name) 
    title <- get_title(signal_name)
    
    print_outliers(plot_df, signal_name, 'Day1')
    print_outliers(plot_df, signal_name, 'Day2')
    
    
    plot <- ggplot(plot_df, aes(x = Day, y = Value)) + 
      geom_boxplot() +
      # geom_dotplot(binaxis='y', stackdir='center', dotsize=1) +
      stat_summary(geom="text", 
                   fun=quantile,
                   aes(label=sprintf("%1.3f", ..y..)),
                   # aes(label=Participant_ID),
                   # aes(label=get_subj(plot_df, ..y..)),
                   # aes(label=sprintf("%s", get_subj(plot_df, ..y..))),
                   # aes(label='T001'),
                   position=position_nudge(x=0.45), size=5.5) +
      labs(title = title, 
           y = label) +
      theme_bw(base_size = 18) + 
      theme(axis.title.x = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x=element_text(size=16, face='bold'),
            # plot.margin = unit(c(0.5, 0.5, 0.5, plot_margin_left), "lines"),  ##top, right, bottom, left
            axis.line = element_line(colour = "black"),
            plot.title = element_text(size=24, hjust = 0.5)) + 
      geom_hline(yintercept=0, linetype="dashed", color = "red", alpha = 0.6, size=1) +
      # scale_x_discrete(labels=labels_vec) + 
      stat_summary(fun = mean, color = "darkred", geom = "point", shape = 3, size = 4, show.legend = FALSE) +
      stat_summary(fun.data = get_n, geom = "text", size = 6) +
      annotate("text", x=1, y=Inf, label= sign[1], vjust = 1.2, size = 10) +
      annotate("text", x=2, y=Inf, label= sign[2], vjust = 1.2, size = 10) +
      scale_y_continuous(expand = c(0.15, 0, 0.15, 0))
    
    # print(plot)
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
# draw_validation_plots()





