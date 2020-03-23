#-------------------------#
#-----GLOBAL VARIABLES----#
#-------------------------#
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
source(file.path(script_dir, 'us-common-functions.R'))

# curation_log_file <- file.path(log_dir, paste0('run-scripts-log-', format(Sys.Date(), format='%m-%d-%y'), '.txt'))
# file.create(curation_log_file)





#-------------------------------------------------------------------------------------------- 1
# source(file.path(script_dir, '1.dc-curate-and-process-data.R'))
# curate_data()




#-------------------------------------------------------------------------------------------- 2
# source(file.path(script_dir, '2.dc-process-activity-app-usage-data.R'))
# format_activity_app_usage_data()






#-------------------------------------------------------------------------------------------- 3
source(file.path(script_dir, '3.dc-quality-control-phase-one.R'))
process_quality_control()



##########################################################
#
# lowest_baseline="lowest_baseline"
# corresponding_baseline="corresponding_baseline"
# day3_day4_ws_mean="day3_day4_ws_mean"
#
##########################################################
process_mean_data(lowest_baseline)







#-------------------------------------------------------------------------------------------- 4
# source(file.path(script_dir, '4.dc-generate-meta-data.R'))
# function()









