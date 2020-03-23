#-------------------------#
#-----GLOBAL VARIABLES----#
#-------------------------#
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
source(file.path(script_dir, 'us-common-functions.R'))

# curation_log_file <- file.path(log_dir, paste0('run-scripts-log-', format(Sys.Date(), format='%m-%d-%y'), '.txt'))
# file.create(curation_log_file)







# source(file.path(script_dir, '1.dc-curate-and-process-data.R'))
# curate_data()




source(file.path(script_dir, '2.dc-process-activity-app-usage-data.R'))
format_activity_app_usage_data()



