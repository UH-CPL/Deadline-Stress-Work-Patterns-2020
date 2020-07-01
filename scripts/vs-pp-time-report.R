#-------------------------#
#--------LIBRARIES--------#
#-------------------------#
library(ggplot2)
library(cowplot)
library(dplyr)
library(tidyverse)



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
generate_pp_time_plots <- function() {
  
}


#-------------------------#
#-------Main Program------#
#-------------------------#
generate_pp_time_plots()




