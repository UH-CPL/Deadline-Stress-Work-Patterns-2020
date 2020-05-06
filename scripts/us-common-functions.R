scripts_dir <- 'scripts'
plots_dir <- 'plots'
log_dir <- 'log-files'

raw_data_dir <- 'raw-data'
curated_data_dir <- 'curated-data'

subj_data_dir <- 'subject-data'
utility_data_dir <- 'utility-data'
physiological_data_dir <- 'physiological-data'
physiological_questionnaire_data_dir <- 'physiological-questionnaire-data'
questionnaire_data_dir <- 'questionnaire-data'
performance_data_dir <- 'performance-data'
index_data_dir <- 'index-data'
final_data_dir <- 'final-data'


log_tansformed_dir <- 'log-transformed'
non_log_tansformed_dir <- 'non-log-transformed'

default_plot_width <- 12
default_plot_height <- 10

one_hour_sec <- 3600


subj_list_file_name <- 'subj_list.csv'


grp_dir <- 'Group1'
session_list <- c('Baseline', 'WorkingSession')
signal_list <- c('PP', 'E4_HR', 'E4_EDA', 'iWatch_HR')


pp_file_pattern <- '.*_pp.csv'
nr_pp_file_pattern <- '.*_nr.csv'
rb_marker_file_pattern <- '.*Baseline_sessionmarkers.csv'
ws_marker_file_pattern <- '.*WorkingSession_sessionmarkers.csv'
activity_file_pattern <- '.*Activity.csv'
mac_app_usage_file_pattern <- '.*Monitor.*log'
win_app_usage_file_pattern <- '.*MonitorLog.csv'
e4_file_pattern <- 'HR.csv|EDA.csv'
rr_file_pattern <- 'IBI.csv'
iWatch_file_pattern <- '.*iWatch.csv'



# computer_usage_pattern = '^CR$|^CW$|^Computer - Reading$|^Computer - Writing$|^C - Reading$|^C - Writing$|^C - Writing/Reading$'
computer_usage_pattern = 'CR|CW|Computer - Reading|Computer - Writing|C - Reading|C - Writing|C - Writing/Reading|Working'
# computer_usage_pattern = 'CR|CW'
Out='Break Out|Break Out - Other|Break - Out|Break Out - Tea/Coffee|Break Out -Other|Break Out - - Tea/Coffee|Out'
CR='^C - Reading$|Computer - Reading|Computer Reading CR'
CW='^C - Writing$/Reading|C - Writing|^Computer - Reading and Writing$|Computer - Writing|Computer Writing CW'
EiP='^Break In - Meal$|Break In Place - Tea/Coffee|Break In Place - Meal|Eat in Place EiP'
OB='^Break In - Other$|^Other Break OB$|Break In Place - Other'
Working='Working'
Thinking='Thinking|Thinking T'
SP='SP - Talking|SP - Looking|SmartPhone - Looking|SmartPhone - Talking|Smart Phone SP'
PR='^HC - Reading$|Hard Copy - Reading'
PW='^HC - Writing$|Hard Copy - Writing|^Board Writing$|^Board Writing BW$'	
ELD='Listening on Headphone'	
PI='H-H Interaction|Personal Interaction PI|Physical Interaction PI'	
VI='C - Talking Skype & others|Virtual Interaction VI'





###############################################################
# Baseline Parameter
###############################################################
lowest_baseline="lowest_baseline"
corresponding_baseline="corresponding_baseline"
day3_day4_ws_mean="day3_day4_ws_mean"
day3_day4_ws_min="day3_day4_ws_min"




activity_state_nasa_file_name <- 'activity_state_nasa.csv'
trait_biographic_file_name <- 'trait_biographic.csv'


physiological_questionnaire_file_name <- 'physiological_questionnaire.csv'



###############################################################
# qc0_file_name -> qc0_raw_file_name
# qc99_file_name -> qc0_final_file_name
###############################################################
qc0_raw_file_name <- 'qc0_raw_data.csv'        # all raw data - generates from dc-curate-and-process-data.R file
qc0_final_file_name <- 'qc0_final_data.csv'    # masked and manifested ontologies & activities data - generates from dc-curate-and-process-data.R file

qc1_file_name <- 'qc1_data.csv'
qc2_file_name <- 'qc2_data.csv'



qc1_normalized_file_name <- 'qc1_normalized_data.csv'
qc2_normalized_file_name <- 'qc2_normalized_data.csv'



qc1_filtered_data_file_name <- 'qc1_bad_filtered_data.csv'
qc2_filtered_data_file_name <- 'qc2_bad_filtered_data.csv'




qc1_raw_mean_v1_file_name <- 'qc1_raw_mean_v1.csv'
qc2_raw_mean_v1_file_name <- 'qc2_raw_mean_v1.csv'

qc1_raw_mean_v2_file_name <- 'qc1_raw_mean_v2.csv'
qc2_raw_mean_v2_file_name <- 'qc2_raw_mean_v2.csv'

qc1_raw_mean_v3_file_name <- 'qc1_raw_mean_v3.csv'
qc2_raw_mean_v3_file_name <- 'qc2_raw_mean_v3.csv'




qc1_log_trans_file_name <- 'qc1_log_trans_data.csv'
qc2_log_trans_file_name <- 'qc2_log_trans_data.csv'



qc1_log_trans_mean_v1_file_name <- 'qc1_log_trans_mean_v1.csv'
qc2_log_trans_mean_v1_file_name <- 'qc2_log_trans_mean_v1.csv'

qc1_log_trans_mean_v2_file_name <- 'qc1_log_trans_mean_v2.csv'
qc2_log_trans_mean_v2_file_name <- 'qc2_log_trans_mean_v2.csv'


qc1_log_trans_mean_chunk_file_name <- 'qc1_log_trans_mean.csv'
qc2_log_trans_mean_chunk_file_name <- 'qc2_log_trans_mean.csv'


## 1. Timestamp -->    Extract whatever inside []                 -->   \\[(.*)\\]
## 2. Application -->  Extract whatever after the space of []     -->   (.*)
mac_data_pattern <- '\\[(.*)\\] (.*)'

s_interface_date_format <- '%a %b %d %H:%M:%S %Y'


# qc0_ws_mean_file_name <- 'qc0_ws_mean.csv'
# qc1_ws_mean_file_name <- 'qc1_ws_mean.csv'
# qc2_ws_mean_file_name <- 'qc2_ws_mean.csv'


qc0_deadline_mean_file_name <- 'qc0_deadline_mean.csv'
qc1_deadline_mean_file_name <- 'qc1_deadline_mean.csv'
qc2_deadline_mean_file_name <- 'qc2_deadline_mean.csv'


qc0_activity_mean_file_name <- 'qc0_activity_mean.csv'
qc1_activity_mean_file_name <- 'qc1_activity_mean.csv'
qc2_activity_mean_file_name <- 'qc2_activity_mean.csv'

qc1_deadline_mean_file_name <- 'qc1_deadline_mean.csv'
qc2_deadline_mean_file_name <- 'qc2_deadline_mean.csv'


## We don't know until now how many filtering we will do :P
## Hope it doesn't exixts 99 filtering
# qc99_file_name <- 'qc0_all_data.csv'


significance_file_name <- 'significance.csv'
variance_test_file_name <- 'variance_test.csv'
variance_test_chunk_mean_file_name <- 'variance_test_chunk_mean.csv'




## 1. Timestamp -->    Extract whatever inside []                 -->   \\[(.*)\\]
## 2. Application -->  Extract whatever after the space of []     -->   (.*)
mac_data_pattern <- '\\[(.*)\\] (.*)'

s_interface_date_format <- '%a %b %d %H:%M:%S %Y'


decorator_hash <- '###########################################################'




#------------------------------------#
#-------   Common Functions   -------#
#------------------------------------#
custom_read_csv <- function(file_name) {
  return(read.csv(file_name, stringsAsFactors=F))
}

convert_to_csv <- function(df, file_path) {
  write.table(df, file = file_path, row.names=F, sep = ',')
}

save_plot <- function(plot_name, plot, width=default_plot_width, height=default_plot_height) {
  plot_path <- file.path(plots_dir, paste0(plot_name, '.png'))
  ggsave(plot_path, plot, width=width, height=height)
  
  plot_path <- file.path(plots_dir, paste0(plot_name, '.pdf'))
  ggsave(plot_path, plot, device=cairo_pdf, width=width, height=height)
}

save_rmd_plot <- function(plot_name, plot, width=default_plot_width, height=default_plot_height) {
  plot_path <- file.path(project_dir, plots_dir, paste0(plot_name, '.png'))
  ggsave(plot_path, plot, width=width, height=height)
  
  plot_path <- file.path(project_dir, plots_dir, paste0(plot_name, '.pdf'))
  ggsave(plot_path, plot, device=cairo_pdf, width=width, height=height)
}



#--------------------------#
#-------   String   -------#
#--------------------------#
print_msg <- function(msg) {
  print(msg)
  message(msg)
}

write_log_msg <- function(msg, file_name) {
  print_msg(msg)
  write(msg, file=file_name, append=TRUE)
}

is_match <- function(str, pattern) { 
  return(grepl(pattern, str)) 
} 

replace_to_underscore <- function(str) {
  gsubfn('.', list('.' = '_', ' ' = '_', '-' = '_'), tolower(str))
}

replace_to_space <- function(str) {
  gsubfn('.', list('_' = ' ', '-' = ' '), str)
}

remove_rigth_substr <- function(str, n){
  substr(str, 1, nchar(str)-n)
}

get_right_substr <- function(str, n){
  substr(str, nchar(str)-n+1, nchar(str))
}

trim <- function( x ) {
  gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}

is_null <- function(cell_val) {
  # print(cell_val)
  if (length(trim(cell_val))==0) {
    return(T)
  } else if (is.na(cell_val)) {
    return(T)
  } else if (cell_val=='NA') {
    return(T)
  }
  
  # else if (cell_val=="") {
  #   return(T)
  # }
  
  return(F)
}

#---------------------------------#
#-------   Date and Time   -------#
#---------------------------------#
convert_date <- function(date, date_format) {
  return(as.POSIXct(date, format=date_format))
}

convert_s_interface_date <- function(date) {
  convert_date(date, s_interface_date_format)
  # convert_date(paste0(substr(date, 1, 19), substr(date, 24, 29)), s_interface_date_format)
}

convert_marker_date <- function(date) {
  return(paste0(substr(date, 1, 19), substr(date, 24, 29)))
}


#--------------------------#
#--- File and Directory ---#
#--------------------------#
is_empty <- function(item) {
  return(length(item)==0)
}

get_dir_list <- function(directory) {
  return(list.dirs(path=directory, full.names=F, recursive=F))
}

get_matched_file_names <- function(directory, file_pattern) {
  return(list.files(path=directory, pattern=file_pattern, recursive=F))
}

get_matched_file_names_recursively <- function(directory, file_pattern) {
  return(list.files(path=directory, pattern=file_pattern, recursive=T))
}


#-----------------------------#
#--- Plot Helper Functions ---#
#-----------------------------#
get_n <- function(x) { 
  return(c(y=-Inf, vjust = -1, label=length(x))) 
} 

get_significance_sign <- function(p_value) { 
  if (p_value > 0.05) { 
    return(" ") 
  } else if (p_value <= 0.001) { 
    return("***") 
  } else if (p_value <= 0.01) { 
    return("**") 
  } else if (p_value <= 0.05) { 
    return("*") 
  } 
}

