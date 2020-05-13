#-------------------------#
#--------LIBRARIES--------#
#-------------------------#
library(readr)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(ggpubr) 
library(directlabels)
library(gsubfn)
library(scales)




#-------------------------#
#-----GLOBAL VARIABLES----#
#-------------------------#
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
project_dir <- dirname(script_dir)

source(file.path(script_dir, 'us-common-functions.R'))


col_list <- c('Participant_ID', 'Treatment', 'TreatmentTime', 'RR')

######################
##       ****       ##
######################
axis_type <- 'normal'
y_axis_label <- 'RR [ms]'


######################
##       ****       ##
######################
# axis_type <- 'log-based'
# y_axis_label <- bquote(paste('log'[10], '(RR [ms])'))
# y_axis_threshold <- 0

plot_list <- list()


treatment_atr <- 'all-session'
# treatment_list <- c('RB')
treatment_list <- c('RB', 'WS')





#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
#---- Removing NA values and extracting data for only the valid sessions ----#
extract_treatment_data <- function(df) {
  return(df[complete.cases(df), ] %>% filter(Treatment %in% treatment_list))
}

get_total_subj_no <- function(df) {
  # df <- extract_treatment_data(df)
  return(length(levels(factor(df$Participant_ID))))
}

get_subj_no_label <- function(subj_no) {
  return(paste("n =", subj_no))
}

replace_dots <- function(str) {
  gsubfn(".", list("." = "_", " " = "_"), tolower(str))
}


read_data <- function() {
  raw_df <<- read.csv(file.path(project_dir, curated_data_dir, physiological_data_dir, qc0_rr_file_name))[, col_list]
  filtered_df <<- read.csv(file.path(project_dir, curated_data_dir, physiological_data_dir, qc1_rr_file_name))[, col_list]
}

generate_rr_time_series_plot <- function(test=F) {
  read_data()
  
  #####################################################
  #                    *********                      #
  #####################################################
  raw_df <- extract_treatment_data(raw_df)
  filtered_df <- extract_treatment_data(filtered_df)
  #####################################################
  
  
  #---- We do not want to calculate the max of x for all sessions again & again ----#
  # non_dual_raw_df <- raw_df %>% filter(Session != 'DT')
  # max_x <- max(non_dual_raw_df$TreatmentTime)
  
  
  for(sess_idx in 1 : length(treatment_list)) {
    session_name <- treatment_list[sess_idx]
    print(session_name)
    
    session_raw_df <- raw_df %>% 
      filter(Treatment == session_name)
    
    session_filtered_df <- filtered_df %>% 
      filter(Treatment == session_name)
    
    if (test==T) {
      session_raw_df <- session_raw_df %>% 
        slice(1:10)
      
      session_filtered_df <- session_filtered_df %>% 
        slice(1:10)
    }
    
    
    
    # if (session_name == 'DT') {
    #   max_x <- max(session_raw_df$TreatmentTime)
    # }
    # print(session_name)
    # print(max_x)
    
    max_x <- max(session_raw_df$TreatmentTime)
    x_axis_label <- ''
    
    #---- PUTTING X-LABEL FOR THE LAST PLOT ONLY ----#
    if (sess_idx == length(session_list)) {
      x_axis_label <- 'Time [s]'
    }
    
    
    if (nrow(session_raw_df) != 0) {
      raw_data_plot <- ggplot(data=session_raw_df,
                              aes(x=TreatmentTime, y=RR, group=Participant_ID)) +
        geom_linerange(ymin=0, ymax=session_raw_df$RR, alpha = 0.3) +
        # geom_line(alpha = 0.7) +
        annotate("text",
                 x=Inf,
                 y=Inf,
                 hjust=1.2,
                 vjust=1.5,
                 size=4.5,
                 label=get_subj_no_label(get_total_subj_no(session_raw_df)),
                 fontface = 'italic')
      
      if (session_name != 'WS') {
        raw_data_plot <- raw_data_plot + 
          theme_bw() +
          theme(axis.line = element_line(colour = "black"))
      }
      
      raw_data_plot <- raw_data_plot + 
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.text.y.right=element_blank(),
              axis.ticks.y.right=element_blank(),
              plot.title = element_text(hjust = 0.5),
              text=element_text(size=14),
              axis.text.x=element_text(size=16),
              axis.text.y=element_text(size=12),
              legend.position='none'
        ) +
        xlim(0, max_x) +
        xlab(x_axis_label) +
        ylab(y_axis_label)
      
      
      if (axis_type=='log-based') {
        raw_data_plot <- raw_data_plot + 
          scale_y_continuous(trans='log10',
                             limits=c(
                               min(raw_df$RR) + y_axis_threshold, 
                               max(raw_df$RR)
                             ),
                             breaks=c(0.01, 1, 5, 20)) ## ****
      } else {
        raw_data_plot <- raw_data_plot + 
          scale_y_continuous(limits=c(min(raw_df$RR),
                                      max(raw_df$RR)))
        
      }
      
      if (sess_idx==1) {
        raw_data_plot <- raw_data_plot + 
          # ggtitle('Original RR sets')
          ggtitle('RR matched to E4 HR sets')
      }
      
      #---- SAVING THE PLOTS IN A LIST TO MAKE A GRID GRAPH ----#
      plot_list[[length(plot_list)+1]] <- raw_data_plot
    }
    
    if (nrow(session_filtered_df) != 0) {
      filtered_data_plot <- ggplot(data=session_filtered_df,
                                   aes(x=TreatmentTime, y=RR, group=Participant_ID)) +
        geom_linerange(ymin=0, ymax=session_filtered_df$RR, alpha = 0.3) +
        # geom_line(alpha = 0.7) +
        annotate("text", 
                 x=Inf,
                 y=Inf,
                 hjust=1.2,
                 vjust=1.5,
                 size=4.5,
                 label=get_subj_no_label(get_total_subj_no(session_filtered_df)),
                 fontface = 'italic')
      
      if (session_name != 'DT') {
        filtered_data_plot <- filtered_data_plot + 
          theme_bw() +
          theme(axis.line = element_line(colour = "black"))
      }
      
      filtered_data_plot <- filtered_data_plot + 
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.text.y.right=element_blank(),
              axis.ticks.y.right=element_blank(),
              axis.title.y.right = element_text(angle=0, vjust=0.5, face='bold'),
              plot.title = element_text(hjust=0.5),
              text=element_text(size=14),
              axis.text.x=element_text(size=16),
              axis.text.y=element_text(size=12),
              legend.position='none'
        ) +
        xlim(0, max_x) +
        xlab(x_axis_label) +
        ylab('')
      
      if (axis_type=='log-based') {
        filtered_data_plot <- filtered_data_plot + 
          scale_y_continuous(trans='log10',
                             limits=c(
                               min(raw_df$RR) + y_axis_threshold, 
                               max(raw_df$RR)
                             ),
                             breaks=c(0.01, 1, 5, 20), ## ****
                             sec.axis=sec_axis(~.+1, name=session_name))
      } else {
        filtered_data_plot <- filtered_data_plot + 
          scale_y_continuous(limits=c(min(raw_df$RR),  ## filtered_df[col_name]
                                      max(raw_df$RR)),  ## filtered_df[col_name]
                             # position='right',
                             sec.axis=sec_axis(~.+1, name=session_name))
      }
      
      if (sess_idx==1) {
        filtered_data_plot <- filtered_data_plot + 
          ggtitle('QC1 RR sets')
      }
    
      #---- SAVING THE PLOTS IN A LIST TO MAKE A GRID GRAPH ----#
      plot_list[[length(plot_list)+1]] <- filtered_data_plot
    }
  }
  
  # print(paste('plot list len: ', length(plot_list)))
  grid_plot <- do.call('grid.arrange', c(plot_list, ncol=2))
  grid_plot <- grid.arrange(grid_plot)
  # print(grid_plot)

  save_plot('rr-time-series', grid_plot)
}



draw_rr_validation_plot <- function(mean_rr_df, signal_name) {
  plot_list <- list()
  
  plot_df <- mean_rr_df %>%
    select(Participant_ID, Day1_Normalize, Day2_Normalize) %>%
    # select(Participant_ID, Day1, Day2) %>% 
    na.omit() %>% 
    gather(Day, Value, -Participant_ID)
  
  # for (day in days) {
  #   conduct_test(temp_mean_df, day, signal_name, test_type)
  # }

  # label <- get_label(signal_name) 
  # title <- get_title(signal_name)
  # 
  # print_outliers(plot_df, signal_name, 'Day1')
  # print_outliers(plot_df, signal_name, 'Day2')
  
  
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
    # labs(title = title, 
    #      y = label) +
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
    # annotate("text", x=1, y=Inf, label= sign[1], vjust = 1.2, size = 10) +
    # annotate("text", x=2, y=Inf, label= sign[2], vjust = 1.2, size = 10) +
    scale_y_continuous(expand = c(0.15, 0, 0.15, 0))
  
  # print(plot)
  save_plot('rr_validation', plot)
  # convert_to_csv(significance_df, file.path(data_dir, 'rr_significance.csv'))
}


generate_rr_mean_data <- function() {
  rr_df <- read.csv(file.path(project_dir, curated_data_dir, physiological_data_dir, qc1_rr_file_name))
  print(head(rr_df, 5))
  
  mean_rr_df <- rr_df %>% 
    select(Participant_ID, Day, Treatment, RR) %>% 
    group_by(Participant_ID, Day, Treatment) %>% 
    dplyr::summarize(NN = mean(RR, na.rm = TRUE)) %>% 
    spread(Treatment, NN) %>% 
    mutate(WS.RB = WS - RB) %>% 
    gather(Comparison, RR_Mean, -Participant_ID, -Day, -RB, -WS) %>% 
    select(Participant_ID, Day, RR_Mean) %>% 
    spread(Day, RR_Mean) %>% 
    
    mutate(Day3_Day4_Mean = case_when(
      !is.na(Day3) & !is.na(Day4)~(Day3+Day4)/2,
      !is.na(Day3)~Day3,
      !is.na(Day4)~Day4,
      TRUE~Day3)) %>%
    mutate(Day3_Day4_Min = pmin(Day3, Day4, na.rm = TRUE))
  
  if (t_test_comparison==day3_day4_ws_mean) {
    mean_rr_df <- mean_rr_df %>%
      mutate(Day1_Normalize=Day1-Day3_Day4_Mean,
             Day2_Normalize=Day2-Day3_Day4_Mean)
    
  } else if (t_test_comparison==day3_day4_ws_min) {
    mean_rr_df <- mean_rr_df %>%
      mutate(Day1_Normalize=Day1-Day3_Day4_Min,
             Day2_Normalize=Day2-Day3_Day4_Min)
  }
  
  mean_rr_df
}

generate_rr_validation_plot <- function() {
  mean_rr_df <- generate_rr_mean_data()
  draw_rr_validation_plot(mean_rr_df, 'rr')
}


#-------------------------#
#-------Main Program------#
#-------------------------#
# generate_rr_time_series_plot(test=T)
# generate_rr_time_series_plot()
# generate_rr_validation_plot()





