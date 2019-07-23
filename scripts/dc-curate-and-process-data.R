#-------------------------#
#--------LIBRARIES--------#
#-------------------------#
# library(XLConnect)
# library(scales)
# library(ggplot2)
library(dplyr)
library(readr)
# 
# require(xlsx)
# library(readxl)
# library(lubridate)







#-------------------------#
#-----GLOBAL VARIABLES----#
#-------------------------#
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
source(file.path(script_dir, 'us-common-functions.R'))
source(file.path(script_dir, 'us-filter-pp.R'))
source(file.path(script_dir, 'us-down-sample-pp.R'))


current_dir <- dirname(script_dir)
setwd(current_dir)

curation_log_file <- file.path(log_dir, paste0('curation-log-', format(Sys.Date(), format='%m-%d-%y'), '.txt'))
file.create(curation_log_file)


all_subj_df <- tibble()


#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
refactor_and_export_all_subj_data <- function() {
  
  ## VERY BAD CODING!!!
  ## This is for T001 & T003, for whom we only noted the break times
  # all_subj_df[is.na(all_subj_df$Ontologies) & all_subj_df$Treatment=='WS' & all_subj_df$Participant_ID=='T001', ]$Ontologies <<- 'Working'
  # all_subj_df[is.na(all_subj_df$Ontologies) & all_subj_df$Treatment=='WS' & all_subj_df$Participant_ID=='T003', ]$Ontologies <<- 'C - Writing/Reading'
  
  
  
  all_subj_df <<- all_subj_df %>%
    select(Participant_ID,
           Day,
           Treatment,
           Timestamp,
           Time,
           Ontologies,
           PP,
           NR_PP,
           HR,
           EDA
    )
  
  convert_to_csv(all_subj_df, file.path(curated_data_dir, physiological_data_dir, 'quantitative_df_qc0.csv'))
}





reduce_noise_and_downsample <- function(session_dir, pp_file_name) {
  pp_df <- read.csv(file.path(session_dir, pp_file_name))
  names(pp_df) <- c("Frame",	"Time",	"Timestamp", "PP")
  pp_df$Timestamp <- as.POSIXct(strptime(pp_df$Timestamp, format=s_interface_date_format)) 
  
  pp_df$NR_PP <- remove_noise(pp_df$PP)
  downsampled_pp_df <- downsample_using_mean(pp_df, c('PP', 'NR_PP'))
  
  convert_to_csv(downsampled_pp_df, file.path(curated_data_dir, subj_data_dir, paste0(substr(pp_file_name, 1, nchar(pp_file_name)-7), '_pp_nr.csv')))
  
  return(downsampled_pp_df)
}


get_downsampled_pp <- function(nr_pp_file_name) {
  downsampled_pp_df <- read.csv(file.path(curated_data_dir, subj_data_dir, nr_pp_file_name))
  downsampled_pp_df$Timestamp <- as.POSIXct(downsampled_pp_df$Timestamp)
  
  return(downsampled_pp_df)
}

add_ontologies <- function(ontologies_df, pp_df) {
  # write_log_msg(paste(ontologies_df$startTimestamp, " - ", ontologies_df$EndTimestamp, ': ', ontologies_df$Ontologies), curation_log_file)
  
  if("Ontologies" %in% colnames(ontologies_df)) {
    ont_start_time <- convert_s_interface_date(ontologies_df$startTimestamp)
    ont_end_time <- convert_s_interface_date(ontologies_df$EndTimestamp)
    
    pp_df[pp_df$Timestamp >= ont_start_time & pp_df$Timestamp <= ont_end_time, ]$Ontologies <- ontologies_df$Ontologies
    
    #################################################################################
    ## CHECK - DPLYR PROBLEM
    # pp_df <- pp_df %>%
    #   mutate(Ontologies=case_when(Timestamp >= ont_start_time & Timestamp <= ont_end_time~ontologies_df$Ontologies))
    #################################################################################
  }
  
  return(pp_df)
}

get_ontologies <- function(ontologies_df, pp_df) {
  ## Check for loop if apply is complex
  # pp_df <- apply(ontologies_df, 1, add_ontologies, pp_df)
  
  for(i in rownames(ontologies_df)) {
    pp_df <- add_ontologies(ontologies_df[i, ], pp_df)
  }
  
  
  ## This is for T001 & T003, for whom we only noted the break times
  # pp_df[is.na(pp_df$Ontologies) & pp_df$Participant_ID=='T001', ]$Ontologies <- 'Working'
  # pp_df[is.na(pp_df$Ontologies) & pp_df$Participant_ID=='T003', ]$Ontologies <- 'C - Writing/Reading'
  
  
  #################################################################################
  ## CHECK - DPLYR PROBLEM
  # message(class(pp_df$Ontologies))
  # pp_df <- pp_df %>%
  #   mutate(Ontologies=case_when(
  #     is.na(Ontologies) & Participant_ID=='T001'~'Working',
  #     is.na(Ontologies) & Participant_ID=='T003'~'C - Writing/Reading'))
  #################################################################################
  
  
  # pp_df <- pp_df %>%
  #   mutate(Ontologies=case_when(
  #     Ontologies==NA & Participant_ID=='T001'~'Working',
  #     Ontologies==NA & Participant_ID=='T003'~'C - Writing/Reading'))
  
  return(pp_df)
}

get_session_abbr <- function(session_name) {
  if (session_name=='Baseline') {
    return('RB')
  } else if (session_name=='WorkingSession') {
    return('WS')
  }
}


get_e4_data <- function(day_dir, e4_file_name) {
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
    message(nrow(e4_df))
    e4_df <- e4_df[1:round(nrow(e4_df)/rate), ] 
    message(nrow(e4_df))
  } 
  
  ## list is one element too large after the loop, so we delete the final element 
  timestamp_vector <- timestamp_vector[-length(timestamp_vector)] 
  
  colnames(e4_df) <- c(sub('.csv', '', sub('.*/', '', e4_file_name)) )
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

curate_session_data <- function(subj_name, day_serial, session_name) {
  day_dir <- file.path(raw_data_dir, grp_dir, subj_name, day_serial)
  session_dir <- file.path(day_dir, session_name)
  
  pp_file_name <- get_matched_file_names(session_dir, pp_file_pattern)
  nr_pp_file_name <- get_matched_file_names(file.path(curated_data_dir, subj_data_dir), paste0('.*', subj_name, '.*', day_serial, '.*', session_name, nr_pp_file_pattern))
  
  marker_file_name <- get_matched_file_names(session_dir, marker_file_pattern)
  marker_df <- read_csv(file.path(session_dir, marker_file_name))
  
  if(!is_empty(pp_file_name)) {
    if(is_empty(nr_pp_file_name)) {
      # write_log_msg('--- noise reduced pp file NOT found ---', curation_log_file)
      downsampled_pp_df <- reduce_noise_and_downsample(session_dir, pp_file_name)
    } else {
      # write_log_msg('--- noise reduced pp file found ---', curation_log_file)
      downsampled_pp_df <- get_downsampled_pp(nr_pp_file_name)
      downsampled_pp_df$Timestamp <- as.POSIXct(downsampled_pp_df$Timestamp)
    }
  }
  
  # downsampled_pp_df <- reduce_noise_and_downsample(subj_name, day_serial, session_name)
  
  
  downsampled_pp_df <- downsampled_pp_df %>% 
    mutate(Participant_ID=subj_name,
           Day=day_serial,
           Treatment=get_session_abbr(session_name),
           Ontologies=NA) %>%
    mutate_if(is.logical, as.character)
  # 
  # write_log_msg(class(downsampled_pp_df$Ontologies), curation_log_file)
  
  ###################################################
  ## Now do specific works for spedific session
  ## Baseline - find out exact 4-5min session
  ## Working Session - Find out the activity log
  ###################################################
  if (session_name=='Baseline') { ## Baseline
    ###################################################
    ## Find out exact 4-5min session
    ###################################################
    
    
    
    
    rb_pp_df <<- downsampled_pp_df
    
  } else { ## WorkingSession
    ## Find out the ontologies
    ws_pp_df <- get_ontologies(marker_df, downsampled_pp_df)
    
    ###################################################
    ## Find out the activity log
    ###################################################
    
    
    
    ## We will merge all signal data in merged_data
    write_log_msg(str(rb_pp_df), curation_log_file)
    write_log_msg(str(ws_pp_df), curation_log_file)
    merged_df <- rbind(rb_pp_df, ws_pp_df)
    rb_pp_df <<- data.frame() ## initializing again
    
    
    
    
    ###################################################
    ## Now add e4 data
    ###################################################
    e4_file_list <- get_matched_file_names_recursively(day_dir, e4_file_pattern)
    for(e4_file_name in e4_file_list) {
      e4_df <- get_e4_data(day_dir, e4_file_name)
      merged_df <- merge(merged_df, e4_df, by='Timestamp', all.x=T)
    }
    
    
    
    
    
    
    ###################################################
    ## Now add iWatch data
    ###################################################
    
    
    
    
    
    all_subj_df <<- rbind(all_subj_df, merged_df)
  }
}

curate_data <- function() {
  # subj_list <- get_dir_list(file.path(raw_data_dir, grp_dir))
  subj_list <- read.csv(file.path(curated_data_dir, utility_data_dir, subj_list_file_name))$Subject
  
  # sapply(subj_list, function(subj_name) {
  sapply(subj_list[1], function(subj_name) {
    
    subj_dir <- file.path(raw_data_dir, grp_dir, subj_name)
    day_list <- get_dir_list(subj_dir)
    
    sapply(day_list, function(day_serial) {
    # sapply(day_list[1], function(day_serial) {
      day_dir <- file.path(raw_data_dir, grp_dir, subj_name, day_serial)
      sapply(session_list, function(session_name) {
        tryCatch({
          write_log_msg(paste0('Strating Processing...', subj_name, '-', day_serial, '-', session_name), curation_log_file)
          curate_session_data(subj_name, day_serial, session_name)
          write_log_msg(paste0(subj_name, '-', day_serial, '-', session_name, ': SUCCESSFUL'), curation_log_file)
        },
        error=function(err) {
          write_log_msg('----------------------------------------------------------', curation_log_file)
          write_log_msg(paste0(subj_name, '-', day_serial, '-', session_name, ': ERROR!'), curation_log_file)
          write_log_msg(paste0(err, '\n'), curation_log_file)
        })
      })
    })
  })
  
  
  refactor_and_export_all_subj_data()
}






#-------------------------#
#-------Main Program------#
#-------------------------#
curate_data()

