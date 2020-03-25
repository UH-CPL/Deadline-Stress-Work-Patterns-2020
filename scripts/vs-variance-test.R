#-------------------------#
#--------LIBRARIES--------#
#-------------------------#


#-------------------------#
#-----GLOBAL VARIABLES----#
#-------------------------#
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
project_dir <- dirname(script_dir)


source(file.path(script_dir, 'us-common-functions.R'))



full_df <- tibble()
variance_test_df <- tibble()


signal_name_list <- c('PP')
# signal_name_list <- c('PP', 'E4_HR', 'E4_EDA', 'iWatch_HR')




#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
read_data <- function() {
  full_df <<- custom_read_csv(file.path(project_dir, curated_data_dir, physiological_data_dir, qc1_file_name))
}


variance_test <- function(df) {
  print(head(df, 2))
}

process_variance_test <- function() {
  full_df <<- full_df %>% 
    filter(Treatment=='WS' & Mask==1) %>% 
    group_by(Participant_ID) %>% 
    do(variance_test(.))
}

conduct_variance_tests <- function() {
  read_data()
  process_variance_test()
}


#-------------------------#
#-------Main Program------#
#-------------------------#
# conduct_variance_tests()





