#-------------------------#
#--------LIBRARIES--------#
#-------------------------#
library(ggplot2)




#-------------------------#
#-----GLOBAL VARIABLES----#
#-------------------------#



#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
read_files <- function() {
  qc1_deadline_mean_df <<- custom_read_csv(file.path(project_dir, curated_data_dir, physiological_data_dir, qc1_normalized_mean_v2_file_name))
  # print_msg(colnames(qc1_deadline_mean_df))
  # print_msg(head(qc1_deadline_mean_df, 2))
  
  
  
  #################################################
  #                DO NOT CHNAGE                  #   
  #################################################
  mean_df <<- qc1_deadline_mean_df
}


#-------------------------#
#-------Main Program------#
#-------------------------#
# draw_regression_plots()





