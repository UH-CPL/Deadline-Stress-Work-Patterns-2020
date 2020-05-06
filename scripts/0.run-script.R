#-------------------------#
#-----GLOBAL VARIABLES----#
#-------------------------#
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
source(file.path(script_dir, 'us-common-functions.R'))


##########################################################
#
# lowest_baseline="lowest_baseline"
# corresponding_baseline="corresponding_baseline"
# day3_day4_ws_mean="day3_day4_ws_mean"
# day3_day4_ws_min="day3_day4_ws_min"
#
##########################################################
baseline_parameter <- corresponding_baseline
t_test_comparison <- day3_day4_ws_min


enable_eda_smoothing <- TRUE


enable_log_transformation <- TRUE
delta_shift_val <- 0.01


discard_rate <- 5  # in %
chunk_sizes <- c(5, 10, 15)




#-------------------------------------------------------------------------------------------- 1
# source(file.path(script_dir, '1.dc-curate-and-process-data.R'))
# curate_data()



#-------------------------------------------------------------------------------------------- 2
# source(file.path(script_dir, '2.dc-process-activity-app-usage-data.R'))
# format_activity_app_usage_data()



#-------------------------------------------------------------------------------------------- 3
# source(file.path(script_dir, '3.dc-quality-control-phase-one.R'))
# process_quality_control()
# process_mean_data()



#-------------------------------------------------------------------------------------------- 4
# source(file.path(script_dir, '4.dc-generate-normalized-data.R'))
# process_normalize_data()


#-------------------------------------------------------------------------------------------- 5
# source(file.path(script_dir, '5.dc-generate-meta-data.R'))
# generate_treatment_mean_data()
# generate_daywise_mean_data()

#-------------------------------------------------------------------------------------------- 6
source(file.path(script_dir, '6.dc-process-rr-data.R'))
process_rr_data()




#-------------------------------------------------------------------------------------------- 7
# source(file.path(script_dir, 'vs-validation_plots.R'))
# draw_validation_plots()


#-------------------------------------------------------------------------------------------- 8
# source(file.path(script_dir, 'vs-variance-test.R'))
# conduct_variance_tests()


#-------------------------------------------------------------------------------------------- 9
# source(file.path(script_dir, '5.dc-generate-meta-data.R'))
# generate_ws_chunk_mean_data()

# source(file.path(script_dir, 'vs-variance-test-chunk-data.R'))
# conduct_variance_tests_chunk_data()


#-------------------------------------------------------------------------------------------- #
#                      PLEASE REMOVE THE REPEATED CODE  3 & 5                                 #
#-------------------------------------------------------------------------------------------- #







#-------------------------------------------------------------------------------------------- 10
# source(file.path(script_dir, 'vs-linear-models.Rmd'))
# conduct_linear_modeling()













