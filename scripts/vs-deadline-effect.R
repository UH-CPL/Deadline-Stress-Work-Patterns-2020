#-------------------------#
#--------LIBRARIES--------#
#-------------------------#
library(data.table)
library(dplyr)
library(formattable)
library(tidyr) 
library(ggplot2)
library(htmltools)
library(webshot) 


customGreen0 = "#DeF7E9"
customGreen = "#71CA97"


#-------------------------#
#-----GLOBAL VARIABLES----#
#-------------------------#
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
project_dir <- dirname(script_dir)
setwd(project_dir)

source(file.path(script_dir, 'us-common-functions.R'))



#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
export_formattable <- function(f, file, width = "100%", height = NULL, background = "white", delay = 0.2) {
  w <- as.htmlwidget(f, width = width, height = height)
  path <- html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot(url,
          file = file,
          selector = ".formattable_widget",
          delay = delay)
}

generate_format_table <- function() {
  df <- custom_read_csv(file.path(project_dir, curated_data_dir, physiological_data_dir, qc1_log_trans_mean_v2_file_name)) %>% 
    select(Participant_ID, Treatment, Signal, Day1_Normalize, Day2_Normalize) %>% 
    rename(Day1=Day1_Normalize,
           Day2=Day2_Normalize) %>% 
    gather(Day, Normalize_Value, -Participant_ID, -Signal, -Treatment) %>% 
    spread(Signal, Normalize_Value) %>% 
    select(Participant_ID, Treatment, Day, PP, E4_EDA, E4_HR, iWatch_HR) 
    
  
  print(head(df, 2))
  
  improvement_formatter <- formatter("span",
                                     style = x ~ style(font.weight = "bold",
                                                       color = ifelse(x > 0, customGreen, ifelse(x < 0, customRed, "black"))),
                                     x ~ icontext(ifelse(x>0, "arrow-up", "arrow-down"), x)
  )


  format_table <- formattable(df, align =c("l", "c", "c", "c", "c", "c", "c"), list(
    `Participant_ID` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
    `Treatment` = formatter("span", style = ~ style(color = "grey")),
    `Day` = formatter("span", style = ~ style(color = "grey")),
    `PP`= improvement_formatter,
    `E4_EDA`= improvement_formatter,
    `E4_HR`= improvement_formatter,
    `iWatch_HR`= improvement_formatter
  ))
  
  export_formattable(format_table, file.path(project_dir, plots_dir, 'deadline_effect.png'))
}



#-------------------------#
#-------Main Program------#
#-------------------------#
generate_format_table()





