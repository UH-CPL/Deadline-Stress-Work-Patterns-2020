#-------------------------#
#-----GLOBAL VARIABLES----#
#-------------------------#
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
project_dir <- dirname(script_dir)
setwd(project_dir)

source(file.path(script_dir, 'us-common-functions.R'))




##########################################################
enable_eda_smoothing <- TRUE


# enable_log_transformation <- TRUE
# delta_shift_val <- 0.01


discard_rate_chunk_mean <- 5  # in %
chunk_sizes <- c(5, 10, 15)


##########################################################
#
# lowest_baseline="lowest_baseline"
# corresponding_baseline="corresponding_baseline"
# day3_day4_ws_mean="day3_day4_ws_mean"
# day3_day4_ws_min="day3_day4_ws_min"
#
##########################################################
baseline_parameter <- corresponding_baseline
t_test_comparison <- day3_day4_ws_mean




##########################################################
#
# log_transformation <- 'log'
# boxcox_transformation <- 'boxcox'
#
##########################################################
transformation_parameter <- log_transformation




sd_val <- 3
remove_peaks <- T
smooth_pp_signals <- F # Oiii....REMEMBER TO COMMENT OUT - Raw_Noisy_PP in script 1 & 6



#-------------------------#
#-----Function Calling----#
#-------------------------#


# #-------------------------------------------------------------------------------------------- 1
# source(file.path(script_dir, '1.dc-curate-and-process-data.R'))
# curate_data()
# 
# 
# #-------------------------------------------------------------------------------------------- 2
# source(file.path(script_dir, '2.dc-process-activity-app-usage-data.R'))
# format_activity_app_usage_data()
# 
# 
# #-------------------------------------------------------------------------------------------- 3
# source(file.path(script_dir, '3.dc-quality-control-phase-one.R'))
# process_quality_control_phase_one()
# 
# 
# #-------------------------------------------------------------------------------------------- 4
# source(file.path(script_dir, '4.dc-generate-transformed-data.R'))
# transform_data()
# 
# 
# #-------------------------------------------------------------------------------------------- 5
# source(file.path(script_dir, '5.dc-generate-normalized-data.R'))
# normalize_data()
# 
# 
# #-------------------------------------------------------------------------------------------- 6
# source(file.path(script_dir, '6.dc-merge-all-data.R'))
# merge_all_data()
# 
# 
# #---------------------------------------------------------------------------------------------- 7
# source(file.path(script_dir, '7.dc-generate-meta-data-break-activity.R'))
# generate_meta_data_break_activity()
# ### investigate_data()








#-------------------------------------------------------------------------------------------- 10.1
# source(file.path(script_dir, 'vs-regression-plot.R'))
# remove_outlier_regression_plot <- F
# draw_regression_plots()


# #-------------------------------------------------------------------------------------------- 20.3
# source(file.path(script_dir, 'vs-validation_plots.R'))
# draw_validation_plots()
# 
# 
# #-------------------------------------------------------------------------------------------- 10.2
# source(file.path(script_dir, 'vs-deadline-effect.R'))
# generate_format_table()
# 
# 
# 
# #-------------------------------------------------------------------------------------------- 10.3
# source(file.path(script_dir, 'vs-activity-pp-box-plot-report.R'))
# generate_activity_pp_box_plots()




#-------------------------------------------------------------------------------------------- 10.x
#-------------------------------------------------#
# source(file.path(script_dir, 'vs-qq-plot.R'))
# draw_qq_plots()

## draw_qq_plots(test_input=TRUE)
#-------------------------------------------------#








#-------------------------------------------------------------------------------------------- 20.1
# source(file.path(script_dir, 7.dc-process-rr-data.R'))
# gather_rr_data()
# qc1_clean_rr_data()

### Remember after QC2, Based on the bad HR data from E4, RR should be removed also
### Check multi-modal-email-study/vs-validation-plot-hrv/clean_invalid_rr() method
# remove_bad_sensor_rr_data()  



#-------------------------------------------------------------------------------------------- 20.2
# source(file.path(script_dir, 'vs-rr-plots.R'))
# generate_rr_time_series_plot()
# generate_rr_validation_plot()





#-------------------------------------------------------------------------------------------- 20.4
# source(file.path(script_dir, 'vs-variance-test.R'))
# conduct_variance_tests()


#-------------------------------------------------------------------------------------------- 20.5
# source(file.path(script_dir, '5.dc-generate-meta-data.R'))
# generate_ws_chunk_mean_data()
# 
# source(file.path(script_dir, 'vs-variance-test-chunk-data.R'))
# conduct_variance_tests_chunk_data()












