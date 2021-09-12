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

signal_comparison_dir <- 'signal-comparison'
activity_plots_dir <- 'activity-plots'
activity_pp_box_plots_dir <- 'activity-pp-box-plots'


default_plot_width <- 12
default_plot_height <- 10

one_hour_sec <- 3600





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


################-------Activities-------------##################

# computer_usage_pattern = '^CR$|^CW$|^Computer - Reading$|^Computer - Writing$|^C - Reading$|^C - Writing$|^C - Writing/Reading$'
computer_usage_pattern = 'CR|CW|Computer - Reading|Computer - Writing|C - Reading|C - Writing|C - Writing/Reading|Working'
# computer_usage_pattern = 'CR|CW'
Out='Break Out|Break Out - Other|Break - Out|Break Out - Tea/Coffee|Break Out -Other|Break Out - - Tea/Coffee|Out'
CR='^C - Reading$|Computer - Reading|^Computer Reading (CR)$|^ C - Reading $'
CW='^C - Writing$/Reading|C - Writing|^Computer - Reading and Writing$|Computer - Writing|^Computer Writing (CW)$'
EiP='^Break In - Meal$|Break In Place - Tea/Coffee|Break In Place - Meal|^Eat in Place (EiP)$'
OB='^Break In - Other$|^Other Break (OB)$|Break In Place - Other'
Working='Working'
Thinking='Thinking|^Thinking (T)$'
SP='SP - Talking|^SP - Looking$|SmartPhone - Looking|SmartPhone - Talking|^Smart Phone (SP)$'
PR='^HC - Reading$|Hard Copy - Reading|^Paper Reading (PR)$'
PW='^HC - Writing$|Hard Copy - Writing|^Board Writing$|^Board Writing BW$|^Paper Writing (PW)$'	
ELD='Listening on Headphone|^Ear Listening Device (ELD)$'	
PI='H-H Interaction|Personal Interaction PI|^Physical Interaction (PI)$'	
VI='C - Talking Skype & others|^Virtual Interaction (VI)$'




################-------Applications-------------##################

DocumentsApp = '^Microsoft Word$|^Microsoft AutoUpdate$|^Microsoft PowerPoint$|^AdobeAcrobat$|^AdobeReader$|^Emacs-x86_64-10_9$|^EndNote X7$|^Microsoft Excel$|^zotero$
|^Evernote$|^Preview$|^TextEdit$|^Acrobat$|^WINWORD$|^POWERPNT$|^EXCEL$|^MendeleyDesktop$|^AcroRd32$|^Photoshop$|^lyx$|^BibDesk$|^TeXShop$'
 
Email= '^Mail$|^Microsoft Outlook$'

WebBrowsingApps='^Google Chrome$|^firefox|Safari$|^chrome$'
 
EntertainingApps ='^Photos|iTunes$|^Spotify$|^App Store$|^FindMy$'

UtilitiesApps ='^Calendar$|^Finder$|^Notes$|^explorer$'

ProgrammingApps = '^JavaApplicationStub$|^RStudio$|^Osascript$|^Terminal$|^Console$|^ATLAS.ti$|^TeamViewer$|^Idle$|^rstudio$|^pycharm$|^iTerm2$|^Mathematica$|^MATLAB$'  

VirtualCommunicationApps = '^QQ$|^Skype$|^FaceTime$|^Slack$|^WeChat$|^Messages$|^Telegram Desktop$|^Discord$'

application_usage_pattern ='Other Activities' 


Activity_pattern = '^R$|^R[+]W$|^W$|^W[+]R$'
Out_pattern = 'Out'

## 1. Timestamp -->    Extract whatever inside []                 -->   \\[(.*)\\]
## 2. Application -->  Extract whatever after the space of []     -->   (.*)
mac_data_pattern <- '\\[(.*)\\] (.*)'


s_interface_date_format <- '%a %b %d %H:%M:%S %Y'

decorator_hash <- '###########################################################'







###############################################################
# Baseline Parameter
###############################################################
lowest_baseline="lowest_baseline"
corresponding_baseline="corresponding_baseline"
day3_day4_ws_mean="day3_day4_ws_mean"
day3_day4_ws_min="day3_day4_ws_min"


raw_parameter <- 'raw'
log_transformation <- 'log'
boxcox_transformation <- 'boxcox'




subj_list_file_name <- 'subj_list.csv'
baseline_curated_time_file_name <- 'baseline_curated_time.csv'


###############################################################
# qc0_file_name -> qc0_raw_file_name
# qc99_file_name -> qc0_final_file_name
###############################################################
qc0_raw_file_name <- 'qc0_raw_data.csv'        # all raw data - generates from dc-curate-and-process-data.R file
qc0_final_file_name <- 'qc0_final_data.csv'    # masked and manifested ontologies & activities data - generates from dc-curate-and-process-data.R file

qc1_file_name <- 'qc1_data.csv'
qc1_filtered_data_file_name <- 'qc1_bad_filtered_data.csv'

qc1_raw_mean_v1_file_name <- 'qc1_raw_mean_v1.csv'
qc1_raw_mean_v2_file_name <- 'qc1_raw_mean_v2.csv'


qc1_transformed_file_name <- 'qc1_transformed_data.csv'

qc1_transformed_mean_v1_file_name <- 'qc1_transformed_mean_v1.csv'
qc1_transformed_mean_v2_file_name <- 'qc1_transformed_mean_v2.csv'


qc1_normalized_mean_v1_file_name <- 'qc1_normalized_mean_v1.csv'
qc1_normalized_mean_v2_file_name <- 'qc1_normalized_mean_v2.csv'


####????####
qc1_mean_chunk_file_name <- 'qc1_mean_chunk.csv'




segment_df_file_name <- 'segment_df.csv'
segment_meta_data_df_file_name <- 'segment_meta_data_df.csv'





full_df_file_name <- 'full_df.csv'

full_df_osf_file_name <- 'full_df_osf.csv'




# qc1_lm_file_name <- 'qc1_lm_data.csv'
# qc1_lm_mean_v1_file_name <- 'qc1_lm_mean_v1.csv'
# qc1_lm_mean_v2_file_name <- 'qc1_lm_mean_v2.csv'


# qc1_log_trans_file_name <- 'qc1_log_trans_data.csv'







significance_file_name <- 'significance.csv'
variance_test_file_name <- 'variance_test.csv'
variance_test_chunk_mean_file_name <- 'variance_test_chunk_mean.csv'





qc0_rr_file_name <- 'qc0_rr_data.csv' 
qc1_rr_file_name <- 'qc1_rr_data.csv' 

qc1_rr_mean_v1_file_name <- 'qc1_rr_mean_v1.csv'
qc1_rr_mean_v2_file_name <- 'qc1_rr_mean_v2.csv'

significance_rr_file_name <- 'significance_rr.csv'





activity_state_nasa_file_name <- 'activity_state_nasa.csv'
trait_biographic_file_name <- 'trait_biographic.csv'

physiological_questionnaire_file_name <- 'physiological_questionnaire.csv'





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

specify_decimal <- function(x, k) {
  trimws(format(round(x, k), nsmall=k))
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

get_gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}






#-----------------------------#
#--- Deadline Stress Study ---#
#-----------------------------#
get_shift_val <- function(df, signal) {
  shift_val <- 0 
  
  if (min(df[[signal]], na.rm = TRUE) <= 0) {
    shift_val <- abs(min(df[[signal]], na.rm = TRUE)) + delta_shift_val
  }
  
  # print(paste0(signal, ' - ', shift_val))
  shift_val
}




generate_daywise_mean_data <- function(mean_df, output_v2_file_name) {
  daywise_mean_df <- mean_df %>%
    gather(Signal, Mean_Value, -Participant_ID, -Day, -Treatment) %>% 
    spread(Day, Mean_Value) %>%
    mutate(Day3_Day4_Mean = case_when(
      !is.na(Day3) & !is.na(Day4)~(Day3+Day4)/2,
      !is.na(Day3)~Day3,
      !is.na(Day4)~Day4,
      TRUE~Day3)) %>%  # it's creating problem for NA. Anyhow Day3 or Day4 is NA, so default NA
    mutate(Day3_Day4_Min = pmin(Day3, Day4, na.rm = TRUE)) %>% 
    mutate(Four_Day_Min = pmin(Day1, Day2, Day3, Day4, na.rm = TRUE))
  
  if (t_test_comparison==day3_day4_ws_mean) {
    daywise_mean_df <- daywise_mean_df %>%
      mutate(Day1_Normalize=Day1-Day3_Day4_Mean,
             Day2_Normalize=Day2-Day3_Day4_Mean)
    
  } else if (t_test_comparison==day3_day4_ws_min) {
    daywise_mean_df <- daywise_mean_df %>%
      mutate(Day1_Normalize=Day1-Day3_Day4_Min,
             Day2_Normalize=Day2-Day3_Day4_Min)
  }
  
  convert_to_csv(daywise_mean_df, file.path(project_dir, curated_data_dir, physiological_data_dir, output_v2_file_name))
}

generate_treatment_mean_data <- function(df) {
  mean_df <- df %>%
    dplyr::select(Participant_ID,	Day, Treatment, Mask, PP, E4_HR, E4_EDA, iWatch_HR) %>%
    group_by(Participant_ID,	Day, Treatment) %>%
    filter(Mask==1) %>%
    summarize_all(mean, na.rm=T) %>%
    ungroup() %>% 
    dplyr::select(-Mask)
  
  return(mean_df)
}


generate_mean_data <- function(input_file_name, output_v1_file_name, output_v2_file_name) {
  physiological_data_dir_path <- file.path(project_dir, curated_data_dir, physiological_data_dir)
  
  df <- custom_read_csv(file.path(physiological_data_dir_path, input_file_name))
  mean_df <- generate_treatment_mean_data(df)
  convert_to_csv(mean_df, file.path(physiological_data_dir_path, output_v1_file_name))
  
  generate_daywise_mean_data(mean_df, output_v2_file_name)
}







