# List of useful packages
pkg <- c("readr", "tidyr", "dplyr", 
         "ggplot2", "knitr", "rmarkdown", 
         "ggpubr", "kableExtra", "directlabels", "XLConnect")

# Check if packages are not installed and assign the
# names of the uninstalled packages to the variable new.pkg
# new.pkg <- pkg
new.pkg <- pkg[!(pkg %in% installed.packages())]

# If there are any packages in the list that aren't installed, install them
if (length(new.pkg)) {
  install.packages(new.pkg, repos = "http://cran.rstudio.com", dependencies=T)
}




# check installed packages
installed.packages()

# list all packages where an update is available
old.packages()

# update all available packages
# update.packages()

# update, without prompts for permission/clarification
# update.packages(ask = FALSE)

# update only a specific package use install.packages()
# install.packages("plotly")

