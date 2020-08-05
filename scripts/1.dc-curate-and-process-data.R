#-------------------------#
#--------LIBRARIES--------#
#-------------------------#
# library(XLConnect)
# library(scales)
# library(ggplot2)

library(plyr)      ## for func rbind.fill()
library(readr)
library(magrittr)  ## for func set_colnames()
library(gsubfn)    ## for func read.pattern()
library(zoo)       ## for func na.locf()
library(data.table)
library(dplyr)     ## load it after loading all other libraries


# require(xlsx)
# library(readxl)
# library(lubridate)







#-------------------------#
#-----GLOBAL VARIABLES----#
#-------------------------#
# script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
# source(file.path(script_dir, 'us-common-functions.R'))

# source(file.path(script_dir, 'us-filter-pp.R'))
# source(file.path(script_dir, 'us-down-sample-pp.R'))




source(file.path(script_dir, 'us-downsample-pp.R'))
source(file.path(script_dir, 'us-denoise-pp.R'))


curation_log_file <- file.path(log_dir, paste0('curation-log-', format(Sys.Date(), format='%m-%d-%y'), '.txt'))
file.create(curation_log_file)


all_subj_df <- tibble()
win_app_usage_df <- tibble()


decay_f <- NULL

#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
get_session_abbr <- function(session_name) {
  if (session_name=='Baseline') {
    return('RB')
  } else if (session_name=='WorkingSession') {
    return('WS')
  }
}






get_e4_data <- function(day_dir, e4_file_name) {
  # e4_df <- custom_read_csv(file.path(day_dir, e4_file_name), col_names=F, col_types = cols())
  e4_df <- read_csv(file.path(day_dir, e4_file_name), col_names=F, col_types = cols())
  
  ## 1st row indicates the start time
  timestamp <- as.numeric(e4_df[1, 1]) 
  
  ## 2nd row indicates the rate of the signal
  rate <- as.numeric(e4_df[2, 1]) 
  
  ## The actual signal starts from 3rd row
  e4_df <- e4_df[-(1:2), ]
  
  
  timestamp_vector <- c(timestamp)
  if (rate==1) { 
    ## Sample rate is 1 - just add a CovertedTime timestamp
    for (i in 1:nrow(e4_df)) { 
      timestamp_vector <- c(timestamp_vector, timestamp + i) 
    } 
    
  } else { 
    ## Sample rate is not 1 - take mean for every `rate` values, then add a CovertedTime timestamp 
    for (i in 1:round(nrow(e4_df)/rate)) { 
      e4_df[i, ] <- sum(e4_df[(rate * (i - 1) + 1):(rate * i), ])/rate 
      timestamp_vector <- c(timestamp_vector, timestamp + i) 
    }
    
    ## list is one element too large after the loop, so we delete the final element 
    # timestamp_vector <- timestamp_vector[-length(timestamp_vector)] 
    # message(nrow(e4_df))
    e4_df <- e4_df[1:round(nrow(e4_df)/rate), ] 
    # message(nrow(e4_df))
  } 
  
  ## list is one element too large after the loop, so we delete the final element 
  timestamp_vector <- timestamp_vector[-length(timestamp_vector)] 
  
  colnames(e4_df) <- c(sub('.csv', '', sub('.*/', '', e4_file_name)))
  e4_df$Timestamp <- as.POSIXct(timestamp_vector, origin='1970-01-01', tz='America/Chicago') # timestamp with Houston timezone 
  e4_df <- data.frame(e4_df)
  
  # message(str(e4_df))
  # message(str(merged_df))
  
  
  ## TIMEZONE BUG FIX 
  # if (!subj_name %in% bad_eda_subj_list) {
  #   if (as.numeric(e4_df$CovertedTime[1] - merged_df$CovertedTime[1]) > 1) { 
  #     e4_df$CovertedTime <- e4_df$CovertedTime - 2 * one_hour_sec 
  #   } 
  # }
  
}






read_downsampled_pp <- function(nr_pp_file_name) {
  downsampled_pp_df <- custom_read_csv(file.path(curated_data_dir, subj_data_dir, nr_pp_file_name))
  downsampled_pp_df$Timestamp <- as.POSIXct(downsampled_pp_df$Timestamp)
  
  return(downsampled_pp_df)
}

reduce_noise_and_downsample_newFFT <- function(session_dir, pp_file_name, session_name) {
  
  print(session_name)
  if(session_name=="Baseline") {
    decay_f <<- 1/6
  }
  if(session_name=="WorkingSession") {
    # decay_f <<- 1/150
    decay_f <<- 1/15
  }
  #print(decay_f)
  pp_df <- custom_read_csv(file.path(session_dir, pp_file_name))
  names(pp_df) <- c("Frame",	"Time",	"Timestamp", "PP")
 
  # pp_df$Timestamp <- as.POSIXct(strptime(pp_df$Timestamp, format=s_interface_date_format)) 
  pp_df$Timestamp <- convert_s_interface_date(convert_marker_date(pp_df$Timestamp))
  
  
  if((session_name=="WorkingSession") & (remove_peaks==T)) {
    mean <- mean(pp_df$PP, na.rm=TRUE)
    sd <- sd(pp_df$PP, na.rm=TRUE)
    
    # print(head(pp_df, 2))
    # print(paste(mean, sd))
    
    outliers <- pp_df %>%
      filter(pp_df$PP<mean-sd_val*sd | pp_df$PP>mean+sd_val*sd) %>%
      select(PP) %>%
      unlist()
    
    # print(head(pp_df, 2))
    # pp_df[pp_df$PP %in% as.list(outliers), 'PP'] <- NA
    # print(head(pp_df, 2))
    # print(11111111111111111)
    
    pp_df <- pp_df %>% 
      mutate(Raw_Noisy_PP=PP) %>% 
      filter(!(PP %in% as.list(outliers)))
    
    # print(22222222222222222)
    # print(head(pp_df, 2))
    # mutate(ReportEmail=case_when(Treatment %in% c('ST', 'DT')~1,
    #                              TRUE~NA))
  } else {
    pp_df <- pp_df %>%
      mutate(Raw_Noisy_PP=PP)
  }
  

  #pp_df$NR_PP <- remove_noise(pp_df$PP)
  pp_df$NR_PP <- remove_noise(pp_df$PP, removeImpluse = T, lowpassDecayFreq = decay_f, samplePerSecond = 7)

  #downsampled_pp_df <- downsample_using_mean(pp_df, c('PP', 'NR_PP'))
  downsampled_pp_df <- downsample_using_mean(pp_df, c('PP', 'NR_PP'))
  
  convert_to_csv(downsampled_pp_df, file.path(curated_data_dir, subj_data_dir, paste0(substr(pp_file_name, 1, nchar(pp_file_name)-7), '_pp_nr.csv')))
  
  return(downsampled_pp_df)
}


reduce_noise_and_downsample <- function(session_dir, pp_file_name) {

  pp_df <- custom_read_csv(file.path(session_dir, pp_file_name))
  names(pp_df) <- c("Frame",	"Time",	"Timestamp", "PP")
  # pp_df$Timestamp <- as.POSIXct(strptime(pp_df$Timestamp, format=s_interface_date_format)) 
  pp_df$Timestamp <- convert_s_interface_date(convert_marker_date(pp_df$Timestamp))
  
  pp_df$NR_PP <- remove_noise(pp_df$PP)
  
  downsampled_pp_df <- downsample_using_mean(pp_df, c('PP', 'NR_PP'))
  
  convert_to_csv(downsampled_pp_df, file.path(curated_data_dir, subj_data_dir, paste0(substr(pp_file_name, 1, nchar(pp_file_name)-7), '_pp_nr.csv')))
  
  return(downsampled_pp_df)
}


get_downsampled_pp <- function(subj_name, day_serial, session_name) {
  session_dir <- file.path(raw_data_dir, grp_dir, subj_name, day_serial, session_name)
  
  pp_file_name <- get_matched_file_names(session_dir, pp_file_pattern)
  nr_pp_file_name <- get_matched_file_names(file.path(curated_data_dir, subj_data_dir), paste0('.*', subj_name, '_', day_serial, '_', session_name, nr_pp_file_pattern))
  
  if(!is_empty(pp_file_name)) {
    if(is_empty(nr_pp_file_name) | smooth_pp_signals) {
      ##### write_log_msg('--- noise reduced pp file NOT found ---', curation_log_file)
      ##### downsampled_pp_df <- reduce_noise_and_downsample(session_dir, pp_file_name)
      downsampled_pp_df <- reduce_noise_and_downsample_newFFT(session_dir, pp_file_name, session_name)
    } else {
      # write_log_msg('--- noise reduced pp file found ---', curation_log_file)
      downsampled_pp_df <- read_downsampled_pp(nr_pp_file_name)
      downsampled_pp_df$Timestamp <- as.POSIXct(downsampled_pp_df$Timestamp)
    }
  }
  
  ## This is when we need to update the *pp_nr.csv file
  # downsampled_pp_df <- reduce_noise_and_downsample(session_dir, pp_file_name)
  
  downsampled_pp_df <- downsampled_pp_df %>% 
    dplyr::mutate(Participant_ID=subj_name,
           Day=day_serial,
           Ontologies=NA,
           Treatment=get_session_abbr(session_name)) %>%
    mutate_if(is.logical, as.character)
  
  # write_log_msg(class(downsampled_pp_df$Ontologies), curation_log_file)
  
  #################################################################
  ## Here, from working session marker, add the timestamps at the beginning and the end (if needed)
  #################################################################
  
  return(downsampled_pp_df)
}

curate_rb_session_data <- function(subj_name, day_serial) {
  session_name <- 'Baseline'
  session_dir <- file.path(raw_data_dir, grp_dir, subj_name, day_serial, session_name)
  
  ###################################################
  ##       Find out exact 4-5min session           ##
  ###################################################
  rb_marker_file_name <- get_matched_file_names(session_dir, rb_marker_file_pattern)
  rb_marker_df <- custom_read_csv(file.path(session_dir, rb_marker_file_name))
  
  rb_start_time <- convert_s_interface_date(convert_marker_date(rb_marker_df$startTimestamp[1]))
  rb_end_time <- convert_s_interface_date(convert_marker_date(rb_marker_df$EndTimestamp[1]))
      
  # write_log_msg(paste0('Baseline start time: ', rb_start_time), curation_log_file)
  # write_log_msg(paste0('Baseline end time: ', rb_end_time), curation_log_file)
  
  downsampled_pp_df <- get_downsampled_pp(subj_name, day_serial, session_name) %>% 
    filter(rb_start_time <= Timestamp & Timestamp <= rb_end_time)
  # downsampled_pp_df$Ontologies <- NA
  
  
  
  return(downsampled_pp_df)
}










## Using For Loop
add_ontologies <- function(ontologies_df, pp_df) {
  # write_log_msg(paste(ontologies_df$startTimestamp, " - ", ontologies_df$EndTimestamp, ': ', ontologies_df$Ontologies), curation_log_file)
  
  if("Ontologies" %in% colnames(ontologies_df)) {
    ont_start_time <- convert_s_interface_date(convert_marker_date(ontologies_df$startTimestamp))
    ont_end_time <- convert_s_interface_date(convert_marker_date(ontologies_df$EndTimestamp))
    
    pp_df[pp_df$Timestamp >= ont_start_time & pp_df$Timestamp <= ont_end_time, ]$Ontologies <- ontologies_df$Ontologies
    
    # write_log_msg(paste(ont_start_time, " - ", ont_end_time, ': ', ontologies_df$Ontologies), curation_log_file)
    # write_log_msg(paste('nrow: ', nrow(pp_df)), curation_log_file)
    
    #################################################################################
    ## CHECK - PROBLEM OF LOGICAL/CHAR VECTOR
    # pp_df <- pp_df %>%
    #   mutate(Ontologies=case_when(Timestamp>=ont_start_time & Timestamp<=ont_end_time~ontologies_df$Ontologies,
    #                               TRUE~Ontologies))
    #################################################################################
  }
  
  return(pp_df)
}


## Using Apply
# add_ontologies <- function(ontologies_df, pp_df) {
#   # write_log_msg(paste(ontologies_df[['startTimestamp']], " - ", ontologies_df[['EndTimestamp']], ': ', ontologies_df[['Ontologies']]), curation_log_file)
# 
#   # if("Ontologies" %in% colnames(ontologies_df)) {
#     ont_start_time <- convert_s_interface_date(ontologies_df[['startTimestamp']])
#     ont_end_time <- convert_s_interface_date(ontologies_df[['EndTimestamp']])
# 
#     # write_log_msg(paste(ont_start_time, " - ", ont_end_time, ': ', ontologies_df[['Ontologies']]), curation_log_file)
# 
#     pp_df[ont_start_time <= pp_df$Timestamp & pp_df$Timestamp <= ont_end_time, ]$Ontologies <- ontologies_df[['Ontologies']]
#     write_log_msg(paste('nrow: ', nrow(pp_df)), curation_log_file)
#     
#     
#     #################################################################################
#     ## CHECK - DPLYR PROBLEM
#     # pp_df <- pp_df %>%
#     #   mutate(Ontologies=case_when(Timestamp >= ont_start_time & Timestamp <= ont_end_time~ontologies_df$Ontologies))
#     #################################################################################
#   # }
# 
#   return(pp_df)
# }

get_session_marker_ontologies <- function(ontologies_df, pp_df) {
  # pp_df$Ontologies <- NA
  for(i in rownames(ontologies_df)) {
    pp_df <- add_ontologies(ontologies_df[i, ], pp_df)
  }
  
  
  # apply(ontologies_df, 1, function(ontologies_row) {
  #   pp_df <- add_ontologies(ontologies_row, pp_df)
  # })
  
  return(pp_df)
}

get_activity_tracker_ontologies <- function(activity_df, pp_df) {
  ## 1. Removing last row which contains "Undefined"
  ## 2. Pasting all columns except the first column
  ## 3. Setting column names
  ## 4. Mutate
  ##    a) Removing the ending commas
  ##    b) Converting to specific date format
  activity_df <- activity_df %>%
    slice(1:(n()-1)) %>% 
    unite(Ontologies, colnames(activity_df)[-1], sep = ", ", remove = TRUE) %>% 
    set_colnames(c("Timestamp", "Ontologies")) %>%
    mutate(Ontologies=gsub("(, )*$", "", Ontologies),
           Timestamp=convert_s_interface_date(strptime(substr(Timestamp, 1, 24), 
                                                       format='%a %b %d %Y %H:%M:%S')))
  
  pp_df <- pp_df %>% 
    dplyr::select(-Ontologies) %>%
    ##############################################################################################
    merge(activity_df, by='Timestamp', all.x=T)  ## CHECK!!! - all vs. all.x
    ##############################################################################################
  
  return(pp_df)
}

get_ontologies <- function(subj_name, day_serial, session_name, downsampled_pp_df) {
  day_dir <- file.path(raw_data_dir, grp_dir, subj_name, day_serial)
  session_dir <- file.path(day_dir, session_name)
  
  activity_file_name <- get_matched_file_names_recursively(day_dir, activity_file_pattern)
  # write_log_msg(paste0('Activity File Name:', activity_file_name), curation_log_file)
  
  
  ## We will use the activity tracker ontology if the file exists
  ## Otherwise, we will use the manual session marker file
  if(!is_empty(activity_file_name)) {
    # write_log_msg('Activity Tracker File - Found', curation_log_file)
    activity_df <- custom_read_csv(file.path(day_dir, activity_file_name))
    ws_df <- get_activity_tracker_ontologies(activity_df, downsampled_pp_df)
    
  } else {
    # write_log_msg('Activity Tracker File - Not Found \n Looking for session marker file', curation_log_file)
    ws_marker_file_name <- get_matched_file_names(session_dir, ws_marker_file_pattern)
    ws_marker_df <- custom_read_csv(file.path(session_dir, ws_marker_file_name))
    
    # downsampled_pp_df$Ontologies <- NA
    ws_df <- get_session_marker_ontologies(ws_marker_df, downsampled_pp_df)
  }
  
  
  return(ws_df)
}

get_app_usage_data <- function(subj_name, day_serial, ws_df) {
  day_dir <- file.path(raw_data_dir, grp_dir, subj_name, day_serial)
  mac_activity_file_name <- get_matched_file_names_recursively(day_dir, mac_app_usage_file_pattern)
  win_activity_file_name <- get_matched_file_names_recursively(day_dir, win_app_usage_file_pattern)
  
  if(!is_empty(mac_activity_file_name)) {
    # write_log_msg('Mac app usage file - Found', curation_log_file)
    # ws_df <- get_mac_app_usage_data(mac_activity_df, ws_df)
    
    
    
    ws_df <- read.pattern(file.path(day_dir, mac_activity_file_name), pattern=mac_data_pattern) %>% 
      set_colnames(c('Timestamp', 'Application')) %>% 
      mutate_if(is.factor, as.character) %>%
      mutate(Timestamp=convert_s_interface_date(strptime(Timestamp, format='%Y-%m-%d %H:%M:%S'))) %>%
      ##############################################################################################
    merge(ws_df, by='Timestamp', all=T)      ## CHECK!!! - all vs. all.x
    # merge(ws_df, by='Timestamp', all.y=T)  ## CHECK!!! - all vs. all.x
    ##############################################################################################
    
    # write_log_msg(levels(factor(ws_df$Application)), curation_log_file)
    
  } else if (!is_empty(win_activity_file_name)) {
    # write_log_msg('Windows app usage file - Found', curation_log_file)
    # ws_df <- get_win_app_usage_data(win_activity_df, ws_df)
    
    
    # print(as.numeric(ws_df$Timestamp))
    
    
    ws_df <- custom_read_csv(file.path(day_dir, win_activity_file_name)) %>% 
      filter(Activity=='Application') %>% 
      mutate(Application=Details,
             Timestamp=convert_s_interface_date(convert_marker_date(Timestamp))) %>%
      dplyr::select(Timestamp, Application) %>%
      ##############################################################################################
      merge(ws_df, by='Timestamp', all=T)      ## CHECK!!! - all vs. all.x
    
    #print(head(ws_df))
    # merge(ws_df, by='Timestamp', all.y=T)  ## CHECK!!! - all vs. all.x
    ##############################################################################################
    
    # win_app_usage_df <<- rbind.fill(win_app_usage_df, ws_df)
    
    
    
    # temp_win_activity_df <- win_activity_df %>%
    #   group_by(Activity, Details) %>%
    #   summarise(TotalRows=n()) %>%
    #   mutate(Participant_ID=subj_name,
    #          Day=day_serial) %>%
    #   select(Participant_ID, Day, everything())
    # 
    # win_app_usage_df <<- rbind.fill(win_app_usage_df, temp_win_activity_df)
    
    
    
    
    # print(str(ws_df))
    # print(as.numeric(ws_df$Timestamp))
    # write_log_msg(levels(factor(win_activity_df$Activity)), curation_log_file)
    # write_log_msg(levels(factor(win_activity_df$Details)), curation_log_file)
    
    
    
    # write_log_msg(levels(factor(temp_win_activity_df$Activity)), curation_log_file)
    # write_log_msg(levels(factor(temp_win_activity_df$Details)), curation_log_file)
    # print(str(temp_win_activity_df))
    # View(temp_win_activity_df)
    
    
    
  } else {
    # write_log_msg('Mac/Win app usage file - Not Found', curation_log_file)
    return(ws_df)
  }
  
  ws_df <- ws_df %>% 
    # mutate(Application_QC1=Application)
    mutate(Application_QC1=na.locf(Application))
  
  return(ws_df)
}


curate_ws_session_data <- function(subj_name, day_serial, rb_df) {
  session_name <- 'WorkingSession'
  
  
  ###################################################
  ## Get 1-fps pp signal
  ###################################################
  downsampled_pp_df <- get_downsampled_pp(subj_name, day_serial, session_name)
  
  
  
  ###################################################
  ## Find out the ontologies
  ###################################################
  ws_df <- get_ontologies(subj_name, day_serial, session_name, downsampled_pp_df)
  # print(str(ws_df))
  
  
  ###################################################
  ## Find out the app monitor log
  ###################################################
  ws_df <- get_app_usage_data(subj_name, day_serial, ws_df)
  # print(str(ws_df))
  # print(nrow(ws_df))
  # print(levels(factor(ws_df$Application)))
  
  
  ## rbind.fill bind two data frames that don't have the same set of columns
  full_day_df <- rbind.fill(rb_df, ws_df)
  # print(levels(factor(full_day_df$Application)))
  
  return(full_day_df)
}


get_decay <- function(signal, treatment) {
  if (signal %in% c('EDA')) {
    if (treatment == 'RB') {
      return(1/6)
    } 
    
    return(1/150)
    
  } else if (signal %in% c('HR')) {
    if (treatment == 'RB') {
      return(1/2)
    } 
    
    return(1/75)
    
  } else if (signal %in% c('iWatch_HR')) {
    if (treatment == 'RB') {
      return(1)
    } 
    
    return(1/20)
  }
}

get_smooth_signal_for_session <- function(df, treatment, signal) {
  session_df <- df %>% 
    filter(Treatment==treatment) %>%
    dplyr::select(-Treatment) %>%
    na.omit()
  
  # print('------------------------------------ 1')
  if (nrow(session_df)>2) {
    # if (signal=='iWatch_HR') {
    #   print(nrow(session_df))
    #   print(head(session_df, 2))
    # }
    session_df[[signal]] <- remove_noise(session_df[[paste0('Raw_', signal)]], 
                                         removeImpluse = T, 
                                         lowpassDecayFreq = get_decay(signal, treatment), 
                                         samplePerSecond = 1)
  }
  # print('------------------------------------ 2')
  
  session_df
}

generate_smooth_signal <- function(df, signal) {
  raw_signal_name <- paste0('Raw_', signal)
  df[[raw_signal_name]] <- df[[signal]]
  
  df <- df %>% 
    filter(Treatment %in% c('RB', 'WS')) %>% 
    dplyr::select(Timestamp, Treatment, !!raw_signal_name)
  
  smooth_df <- tibble()
  treatments <- unique(df$Treatment)
  
  for (treatment in treatments) {
    smooth_df <- rbind.fill(smooth_df, get_smooth_signal_for_session(df, treatment, signal)) 
  }
  
  final_smooth_df <<- final_smooth_df %>%
    merge(smooth_df, by='Timestamp', all=T)
}

smooth_signal <- function(df) {
  # print(head(df, 2))
  
  wrist_signal_list <- c('EDA', 'HR', 'iWatch_HR')
  final_smooth_df <<- df
  
  for (signal in wrist_signal_list) {
    if (signal %in% colnames(df)) {
      # print(signal)
      # print(colnames(df))
      final_smooth_df <<- final_smooth_df %>% 
        dplyr::select(-!!signal)
    }
  }
  
  for (signal in wrist_signal_list) {
    if (signal %in% colnames(df)) {
      write_log_msg(paste0('smoothing...', signal), curation_log_file)
      generate_smooth_signal(df, signal)
    }
  }

  final_smooth_df
}


merge_e4_data <- function(subj_name, day_serial, full_day_df) {
  day_dir <- file.path(raw_data_dir, grp_dir, subj_name, day_serial)
  
  ## Two files for HR and EDA
  # downsampled_e4_file_list <- get_matched_file_names(file.path(curated_data_dir, subj_data_dir), e4_file_pattern)
  subj_day_info <- paste0('Group1_', subj_name, '_', day_serial, '_')
  downsampled_e4_file_list <- get_matched_file_names(file.path(curated_data_dir, subj_data_dir), paste0(subj_day_info, 'HR', '|', subj_day_info, 'EDA'))
  
  
  if(is_empty(downsampled_e4_file_list)) {
    ## Two files for HR and EDA
    e4_file_list <- get_matched_file_names_recursively(day_dir, e4_file_pattern)
    
    for(e4_file_name in e4_file_list) {
      e4_df <- get_e4_data(day_dir, e4_file_name)
      convert_to_csv(e4_df, file.path(curated_data_dir, subj_data_dir, paste0('Group1_', subj_name, '_', day_serial, '_', sub('.csv', '', sub('.*/', '', e4_file_name)), '.csv')))
      
      ##############################################################################################
      full_day_df <- merge(full_day_df, e4_df, by='Timestamp', all=T)   ## CHECK!!! - all vs. all.x
      ##############################################################################################
    }
    
  } else {
    for(e4_file_name in downsampled_e4_file_list) {
      e4_df <- custom_read_csv(file.path(curated_data_dir, subj_data_dir, e4_file_name)) %>%
        mutate(Timestamp=as.POSIXct(Timestamp))
      # print(str(e4_df))

      ##############################################################################################
      full_day_df <- merge(full_day_df, e4_df, by='Timestamp', all=T)   ## CHECK!!! - all vs. all.x
      ##############################################################################################
    }
  }
  
  full_day_df
}


merge_iwatch_data <- function(subj_name, day_serial, full_day_df) {
  day_dir <- file.path(raw_data_dir, grp_dir, subj_name, day_serial)
  iWatch_file_name <- get_matched_file_names_recursively(day_dir, iWatch_file_pattern)
  
  if(!is_empty(iWatch_file_name) & (subj_name != "T015") & (subj_name != "T017")) {
    
    # print(paste("All", subj_name))
    # print(head(custom_read_csv(file.path(day_dir, iWatch_file_name))), 2)
    
    full_day_df <- custom_read_csv(file.path(day_dir, iWatch_file_name)) %>% 
      dplyr::rename(Timestamp=Time,
             iWatch_HR=HeartRate) %>% 
      mutate(Timestamp=convert_s_interface_date(strptime(Timestamp, format='%Y-%m-%d %H:%M:%S')) - 5 * one_hour_sec) %>%
      # mutate(Timestamp=Timestamp - 5 * one_hour_sec) %>% 
      ##############################################################################################
      merge(full_day_df, by='Timestamp', all=T) ## CHECK!!! - all vs. all.x
    ##############################################################################################
    
    
    # print(str(full_day_df))
    
    ##############################################################################################
    # full_day_df <- merge(full_day_df, iWatch_df, by='Timestamp', all=T)   ## CHECK!!! - all vs. all.x
    ##############################################################################################
  }
  if(!is_empty(iWatch_file_name) & (subj_name == "T015" | subj_name == "T017")) {
    print(paste("Daylight",subj_name))
    full_day_df <- custom_read_csv(file.path(day_dir, iWatch_file_name)) %>% 
      dplyr::rename(Timestamp=Time,
             iWatch_HR=HeartRate) %>% 
      mutate(Timestamp=convert_s_interface_date(strptime(Timestamp, format='%Y-%m-%d %H:%M:%S')) - 6 * one_hour_sec) %>%
      # mutate(Timestamp=Timestamp - 5 * one_hour_sec) %>% 
      ##############################################################################################
    merge(full_day_df, by='Timestamp', all=T) ## CHECK!!! - all vs. all.x
    ##############################################################################################
    
    
    # print(str(full_day_df))
    
    ##############################################################################################
    # full_day_df <- merge(full_day_df, iWatch_df, by='Timestamp', all=T)   ## CHECK!!! - all vs. all.x
    ##############################################################################################
  }
  
  return(full_day_df)
}

curate_baseline_time_for_row <- function(rb_row, df) {
  # subj <- rb_row$Subject_ID
  # day <- rb_row$Day
  # start_time <- rb_row$StartTime
  # end_time <- rb_row$EndTime
  # 
  # print(paste(typeof(subj), typeof(day), typeof(start_time), typeof(end_time)))
  # print(paste(subj, day, start_time, end_time))
  
  # print(head(df, 2))
  
  # temp_df <- df %>% 
  #   filter(Participant_ID==rb_row$Subject_ID & 
  #              Day==rb_row$Day &
  #              Treatment=="RB" &
  #              (TreatmentTime<=rb_row$StartTime |
  #              TreatmentTime>=rb_row$EndTime)
  #          )
  # 
  # print(head(temp_df$TreatmentTime, 2))
  
  
  df <- df %>%
    filter(!(Participant_ID==rb_row$Subject_ID &
               Day==rb_row$Day &
               Treatment=='RB' &
               (BaseTreatmentTime<=rb_row$StartTime | BaseTreatmentTime>=rb_row$EndTime)))
  
  df
}

curate_baseline_time <- function(df) {
  rb_curated_time_df <- custom_read_csv(file.path(curated_data_dir, utility_data_dir, baseline_curated_time_file_name))
  # print(rb_curated_time_df)
  
  for(i in rownames(rb_curated_time_df)) {
    df <- curate_baseline_time_for_row(rb_curated_time_df[i, ], df)
  }
  
  df
}


refactor_and_export_all_subj_data <- function(all_subj_df) {
  #################################################################################
  #################################################################################
  # all_subj_df <- all_subj_df %>%
  #   mutate(Ontologies=case_when(
  #     is.na(Ontologies) & Treatment=='WS' & Participant_ID=='T001'~'Working',
  #     is.na(Ontologies) & Treatment=='WS' & Participant_ID=='T003'~'C - Writing/Reading',
  #     TRUE~Ontologies))
  
  # all_subj_df <- all_subj_df %>%
  #   mutate(Ontologies=case_when(
  #   Ontologies==NA & Treatment=='WS' & Participant_ID=='T001'~'Working',
  #   Ontologies==NA & Treatment=='WS' & Participant_ID=='T003'~'C - Writing/Reading'))
  
  
  all_subj_df <- all_subj_df %>%
    dplyr::mutate(Ontologies=case_when(
      #is.na(Ontologies) & Treatment=='WS' & Participant_ID=='T001'~'Working',
      is.na(Ontologies) & Treatment=='WS' & Participant_ID=='T003'~'C - Writing/Reading',
      TRUE~Ontologies))
  
  # # # # This is for T001 & T003, for whom we only noted the break times
  # # # all_subj_df[is.na(all_subj_df$Ontologies) & all_subj_df$Treatment=='WS' & all_subj_df$Participant_ID=='T001', ]$Ontologies <- 'Working'
  # # # all_subj_df[is.na(all_subj_df$Ontologies) & all_subj_df$Treatment=='WS' & all_subj_df$Participant_ID=='T003', ]$Ontologies <- 'C - Writing/Reading'
  
  
  #----------------------------------- T005 - Day5 -------------------------------------------#
  all_subj_df <- all_subj_df[!(all_subj_df$Participant_ID=="T005" & all_subj_df$Day=="Day4"), ]
  all_subj_df$Day[all_subj_df$Day=="Day5"] <- "Day4"
  
  #############################################################################################
  #############################################################################################
  

  # all_subj_df <- curate_baseline_time(all_subj_df)
  # View(all_subj_df)
  
  # print(head(all_subj_df, 2))
  all_subj_df <- all_subj_df %>%
    dplyr::rename(Sinterface_Time=Time,
           Activities=Ontologies,
           # Raw_Noisy_PP=PP_RAW_NOISE,
           Raw_PP=PP,
           PP=NR_PP,
           E4_HR=HR,
           Raw_E4_HR=Raw_HR,
           E4_EDA=EDA,
           Raw_E4_EDA=Raw_EDA,
           ) %>% 
    
    
  # Raw_EDA=EDA,
  # Raw_HR=HR,
  # Raw_iWatch_HR=iWatch_HR
    
    ## Calculating relative treatment time
    group_by(Participant_ID, Day, Treatment) %>% 
    arrange(Timestamp) %>%
    dplyr::mutate(BaseTreatmentTime=as.numeric(Timestamp)-as.numeric(head(Timestamp, 1))) %>%
    
    do(curate_baseline_time(.)) %>% 
    
    group_by(Participant_ID, Day, Treatment) %>% 
    arrange(BaseTreatmentTime) %>%
    dplyr::mutate(TreatmentTime=BaseTreatmentTime-head(BaseTreatmentTime, 1)) %>%
    
    dplyr::select(Participant_ID,
           Day,
           Treatment,
           Timestamp,
           Sinterface_Time,
           BaseTreatmentTime,
           TreatmentTime,
           
           # Raw_Noisy_PP,
           Raw_PP,
           PP,
           
           Raw_E4_EDA,
           E4_EDA,
           
           Raw_E4_HR,
           E4_HR,
           
           Raw_iWatch_HR,
           iWatch_HR,
           
           Activities,
           Application,
           Application_QC1
    ) %>%
    
  ###################################################################################
  ## Note: This is very important to understand this code
  ## We were some missing WS data.
  ## So, we merge all data, and based on WS start and end time we get all WS data
  ## Here, we are removing NA Treatments - caused for merging all
  drop_na(Treatment) 
  ###################################################################################
  
  
  # If we don't want eda smoothing, we will remove the smoothened column and rename the raw value to E4_EDA
  if(!enable_eda_smoothing) {
    all_subj_df <- all_subj_df %>%
      dplyr::select(-E4_EDA) %>% 
      dplyr::rename(E4_EDA=Raw_E4_EDA)
  }
  
  
  View(all_subj_df)
  # write_log_msg(levels(factor(all_subj_df$Application)), curation_log_file)
  write_log_msg(paste0('Total relative time mismatch row: ', nrow(all_subj_df[all_subj_df$Sinterface_Time != all_subj_df$TreatmentTime, ])), curation_log_file)
  
  convert_to_csv(all_subj_df, file.path(curated_data_dir, physiological_data_dir, qc0_raw_file_name))
}


curate_data <- function() {
  all_subj_df <<- tibble()
  
  # subj_list <- get_dir_list(file.path(raw_data_dir, grp_dir))
  subj_list <- custom_read_csv(file.path(curated_data_dir, utility_data_dir, subj_list_file_name))$Subject
  # print(subj_list)
  
  # sapply(subj_list, function(subj_name) {
  # sapply(subj_list[2], function(subj_name) {
  sapply(c('T003', 'T005'), function(subj_name) {
  # sapply(c('T005'), function(subj_name) {

    subj_dir <- file.path(raw_data_dir, grp_dir, subj_name)
    day_list <- get_dir_list(subj_dir)
    
    sapply(day_list, function(day_serial) {
    # sapply(day_list[3], function(day_serial) {
      tryCatch({
        write_log_msg(paste0('\n----------\n', subj_name, '-', day_serial, "\n----------"), curation_log_file)
        
        write_log_msg('Processing.....Resting Baseline', curation_log_file)
        rb_df <- curate_rb_session_data(subj_name, day_serial)
        
        write_log_msg('Processing.....Working Session', curation_log_file)
        full_day_df <- curate_ws_session_data(subj_name, day_serial, rb_df)
        
        write_log_msg('Merging.....e4 data', curation_log_file)
        full_day_df <- merge_e4_data(subj_name, day_serial, full_day_df)
        
        write_log_msg('Merging.....iwatch data', curation_log_file)
        full_day_df <- merge_iwatch_data(subj_name, day_serial, full_day_df)
        
        write_log_msg('Smoothing.....signals', curation_log_file)
        full_day_df <- smooth_signal(full_day_df)
        
        write_log_msg('Fixing.....missing working session data', curation_log_file)
        ## Here get the info from ws start and end time
        ## Check for the in this time period which rows has NA session
        ## Replace those treatements by ws
        # full_day_df <-  add_missing_ws_session_data(subj_name, day_serial, full_day_df)
        
        
        write_log_msg('Merging.....all subj data', curation_log_file)
        all_subj_df <<- rbind.fill(all_subj_df, full_day_df)
        
      },
      error=function(err) {
        write_log_msg(paste0('\n', decorator_hash, '\n', subj_name, '-', day_serial, ': ERROR!'), curation_log_file)
        write_log_msg(paste0(err, decorator_hash), curation_log_file)
      })
    })
  })
  
  # convert_to_csv(win_app_usage_df, file.path(curated_data_dir, physiological_data_dir, 'win_app_usage_df.csv'))
  # convert_to_csv(win_app_usage_df, file.path(curated_data_dir, physiological_data_dir, 'win_app_usage_row_num_df.csv'))
  
  write_log_msg('Finally...refactoring all subj data', curation_log_file)
  refactor_and_export_all_subj_data(all_subj_df)
}






#-------------------------#
#-------Main Program------#
#-------------------------#
# curate_data()


