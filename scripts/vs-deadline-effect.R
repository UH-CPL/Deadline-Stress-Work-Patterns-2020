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
customRed = "#ff7f7f"

#-------------------------#
#-----GLOBAL VARIABLES----#
#-------------------------#



#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
improvement_formatter <- formatter("span",
                                   style = x ~ style(font.weight = "bold",
                                                     color = ifelse(x > 0, customGreen, ifelse(x < 0, customRed, "black"))),
                                   x ~ icontext(ifelse(x>0, "arrow-up", "arrow-down"), x)
)


export_formattable <- function(f, file, width = "100%", height = NULL, background = "white", delay = 0.2) {
  w <- as.htmlwidget(f, width = width, height = height)
  path <- html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot(url,
          file = file,
          selector = ".formattable_widget",
          delay = delay)
}

generate_specific_format_table <- function(input_file, treatment, output_file_parameter) {
  df <- custom_read_csv(file.path(project_dir, curated_data_dir, physiological_data_dir, input_file)) %>%
    filter(Treatment==treatment) %>%
    select(Participant_ID, Treatment, Signal, Day1_Normalize, Day2_Normalize) %>%
    dplyr::rename(Day1=Day1_Normalize,
                  Day2=Day2_Normalize) %>%
    gather(Day, Normalize_Value, -Participant_ID, -Signal, -Treatment) %>%
    spread(Signal, Normalize_Value) %>%
    select(Participant_ID, Treatment, Day, PP, E4_EDA, E4_HR, iWatch_HR)
  
  print(head(df, 2))
  
  format_table <- formattable(df, align =c("l", "c", "c", "c", "c", "c", "c"), list(
    `Participant_ID` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
    `Treatment` = formatter("span", style = ~ style(color = "grey")),
    `Day` = formatter("span", style = ~ style(color = "grey")),
    `PP`= improvement_formatter,
    `E4_EDA`= improvement_formatter,
    `E4_HR`= improvement_formatter,
    `iWatch_HR`= improvement_formatter
  ))
  
  export_formattable(format_table, file.path(project_dir, 
                                             plots_dir, 
                                             paste0('deadline_effect_', output_file_parameter,'_', tolower(treatment), '.png')))
}


generate_format_table <- function() {
  # generate_specific_format_table(input_file=qc1_raw_mean_v2_file_name,
  #                                treatment='WS',
  #                                output_file_parameter='raw')
  
  generate_specific_format_table(input_file=qc1_log_trans_mean_v2_file_name,
                                 treatment='WS',
                                 output_file_parameter='log_transformed')
  
  
  
  generate_specific_format_table(input_file=qc1_raw_mean_v2_file_name,
                                 treatment='RB',
                                 output_file_parameter='raw')
  
  generate_specific_format_table(input_file=qc1_lm_mean_v2_file_name,
                                 treatment='RB',
                                 output_file_parameter='lm')
}




#-------------------------#
#-------Main Program------#
#-------------------------#
# generate_format_table()










# generate_format_table_ws <- function() {
#   df <- custom_read_csv(file.path(project_dir, curated_data_dir, physiological_data_dir, qc1_log_trans_mean_v2_file_name)) %>% 
#     filter(Treatment=='WS') %>%
#     select(Participant_ID, Treatment, Signal, Day1_Normalize, Day2_Normalize) %>% 
#     dplyr::rename(Day1=Day1_Normalize,
#            Day2=Day2_Normalize) %>% 
#     gather(Day, Normalize_Value, -Participant_ID, -Signal, -Treatment) %>% 
#     spread(Signal, Normalize_Value) %>% 
#     select(Participant_ID, Treatment, Day, PP, E4_EDA, E4_HR, iWatch_HR) 
#   
#   print(head(df, 2))
#   
#   format_table <- formattable(df, align =c("l", "c", "c", "c", "c", "c", "c"), list(
#     `Participant_ID` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
#     `Treatment` = formatter("span", style = ~ style(color = "grey")),
#     `Day` = formatter("span", style = ~ style(color = "grey")),
#     `PP`= improvement_formatter,
#     `E4_EDA`= improvement_formatter,
#     `E4_HR`= improvement_formatter,
#     `iWatch_HR`= improvement_formatter
#   ))
#   
#   export_formattable(format_table, file.path(project_dir, plots_dir, 'deadline_effect_ws.png'))
# }



#####################################################################
#------------------------- DONT DELETE -----------------------------#
#####################################################################
# generate_format_table_rb <- function() {
#   # df <- custom_read_csv(file.path(project_dir, curated_data_dir, physiological_data_dir, qc1_raw_mean_v1_file_name)) %>%
#   #   filter(Treatment=='RB') %>% 
#   #   
#   #   gather(Signal, Mean_Value, -Participant_ID, -Day, -Treatment) %>% 
#   #   spread(Day, Mean_Value) %>%
#   #   mutate(Day3_Day4_Mean = case_when(
#   #     !is.na(Day3) & !is.na(Day4)~(Day3+Day4)/2,
#   #     !is.na(Day3)~Day3,
#   #     !is.na(Day4)~Day4,
#   #     TRUE~Day3)) %>%
#   #   mutate(Day3_Day4_Min = pmin(Day3, Day4, na.rm = TRUE))
#   #   
#   #   if (t_test_comparison==day3_day4_ws_mean) {
#   #     df <- df %>%
#   #       mutate(Day1_Normalize=Day1-Day3_Day4_Mean,
#   #              Day2_Normalize=Day2-Day3_Day4_Mean)
#   #     
#   #   } else if (t_test_comparison==day3_day4_ws_min) {
#   #     df <- df %>%
#   #       mutate(Day1_Normalize=Day1-Day3_Day4_Min,
#   #              Day2_Normalize=Day2-Day3_Day4_Min)
#   #   }
#   
#   df <- custom_read_csv(file.path(project_dir, curated_data_dir, physiological_data_dir, qc1_raw_mean_v2_file_name)) %>%
#     filter(Treatment=='RB') %>% 
#     select(Participant_ID, Treatment, Signal, Day1_Normalize, Day2_Normalize) %>% 
#     dplyr::rename(Day1=Day1_Normalize,
#            Day2=Day2_Normalize) %>%
#     gather(Day, Normalize_Value, -Participant_ID, -Signal, -Treatment) %>%
#     spread(Signal, Normalize_Value) %>% 
#     select(Participant_ID, Treatment, Day, PP, E4_EDA, E4_HR, iWatch_HR)
#   
#   print(head(df, 2))
#   
#   format_table <- formattable(df, align =c("l", "c", "c", "c", "c", "c", "c"), list(
#     `Participant_ID` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
#     `Treatment` = formatter("span", style = ~ style(color = "grey")),
#     `Day` = formatter("span", style = ~ style(color = "grey")),
#     `PP`= improvement_formatter,
#     `E4_EDA`= improvement_formatter,
#     `E4_HR`= improvement_formatter,
#     `iWatch_HR`= improvement_formatter
#   ))
#   
#   export_formattable(format_table, file.path(project_dir, plots_dir, 'deadline_effect_rb.png'))
# }





