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

activity_color <<- c(
                     "RB" = "yellow",
                     "R" = "springgreen",
                     "W" = "blue",
                     "I" = "orange",
                     "Out" = "gray92",
                     "SP" = "red",
                     "SA" = "magenta",
                     "NA"="white"
)


#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
save_plot <- function(plot_name, plot, width=16, height=8) {
  ggsave(paste0(plot_name, '.pdf'), plot, width=width, height=height)
  ggsave(paste0(plot_name, '.pdf'), plot, device=cairo_pdf, width=width, height=height)
}

read_data <- function() {
  if (test==T) {
    full_df <<- custom_read_csv(file.path(project_dir, curated_data_dir, physiological_data_dir, 'mini_full_df.csv'))
  } else {
    full_df <<- custom_read_csv(file.path(project_dir, curated_data_dir, physiological_data_dir, full_df_file_name))
  }
  
  # full_df <<- custom_read_csv(file.path(project_dir, curated_data_dir, physiological_data_dir, 'full_df_150.csv'))

  full_df <<- full_df %>%
    mutate(Reduced_Activity_One=case_when(Treatment=='RB'~'RB',
                                          Reduced_Activity_One==''~'NA',
                                          TRUE~Reduced_Activity_One))
           # Reduced_Activity_One=factor(Reduced_Activity_One, levels=c("RB", "R", "W", "Out", "SA", "SP", "I", "NA")))

  # View(full_df)
  # print_msg(colnames(full_df))
}



generate_pp_time_plots <- function() {
  read_data()

  if (test==T) {
    subj_list <- c('T005')
    # subj_list <- c('T003', 'T005')
  } else {
    subj_list <- unique(full_df$Participant_ID)
  }

  # print(subj_list)
  # print(unique(full_df$Reduced_Activity_One))

  for (subj in subj_list) {
    subj_df <- full_df %>% filter(Participant_ID==subj)
    plot_list <- list()
    
    print(subj)

    for (day in unique(full_df$Day)) {
      day_df <- subj_df %>%
        filter(Day==day)

      rb_mean <- day_df %>%
        filter(Treatment=='RB') %>%
        dplyr::summarize(Mean = mean(Trans_PP, na.rm=TRUE))
      
      min_y=min(subj_df$Trans_PP, na.rm=T)-0.1
      max_y=max(subj_df$Trans_PP, na.rm=T)+0.1


      plot <- ggplot(day_df, aes(x=Reduced_Activity_One, y=Trans_PP, fill=Reduced_Activity_One)) +
        geom_boxplot() +
        geom_hline(yintercept=rb_mean$Mean[1], linetype="dashed", color = "red", alpha = 0.6, size=1) +
        # xlab('Activities') +
        # ylab('ln(PP)') +
        # xlab('') +
        # ylab('') +
        ggtitle(day) +
        scale_fill_manual(breaks = c("RB", "R", "W", "Out", "SA", "SP", "I", "NA"),
                          values=c(
                            "yellow",
                            "springgreen",
                            "blue",
                            "orange",
                            "gray92",
                            "red",
                            "magenta",
                            "white"
                          )) +
        scale_x_discrete(limits=c("RB", "R", "W", "Out", "SA", "SP", "I", "NA"), drop=F) +
        scale_y_continuous(limits = c(min_y, max_y),
                           breaks = round(seq(min_y, max_y, by=0.4), 1)) +
        stat_summary(fun.data = get_n, geom = "text", size = 3) +
        # scale_color_brewer(palette="Dark2") +
        # scale_y_continuous(limits = c(min(subj_df$Trans_PP, na.rm=T), 1)) +
        # scale_x_discrete(breaks=unique(day_df$Reduced_Activity_ One), drop=F) 
        
        # scale_color_manual(values = activity_color) +
        # scale_fill_manual(values = activity_color) +
        # scale_fill_manual(values = c(
        #   "yellow",
        #   "springgreen",
        #   "blue",
        #   "orange",
        #   "gray92",
        #   "red",
        #   "magenta",
        #   "white"
        # )) +
        theme_bw() +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.title = element_blank(),
          legend.position = "None",
          axis.title.x=element_blank(),
          # axis.text.x=element_blank(),
          # axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          # axis.text.y=element_blank(),
          # axis.ticks.y=element_blank(),
          plot.title = element_text(hjust = 0.5)
        )


      print(plot)
      plot_list[[length(plot_list)+1]] <- plot
    }

    
    # grid_plot <- do.call("grid.arrange", c(plot_list, ncol=2, top=textGrob(subj, gp=gpar(fontsize=20,font=3))))
    title <- ggdraw() + draw_label(paste0(subj, ": non-normalised ln(PP) per Activity"), fontface='bold')
    # grid_plot <- plot_grid(plotlist=plot_list, nrow=2)
    grid_plot <- plot_grid(title, 
                           plot_grid(plotlist=plot_list, nrow=2), 
                           ncol=1, 
                           rel_heights=c(0.07, 1))
    save_plot(file.path(project_dir, plots_dir, activity_time_box_plots_dir, subj), grid_plot)
  }
}
  

#-------------------------#
#-------Main Program------#
#-------------------------#
test <<- F
generate_pp_time_plots()




