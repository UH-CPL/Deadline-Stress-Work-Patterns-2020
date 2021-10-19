# Deadline-Stress-Work-Patterns
This repository contains the R scripts to curate, ensure quality control, and validate the raw data collected
via the S-Interface and other tools in the "Office Tasks 2020" project.


## Getting Started

#### Prerequisites
- R and RStudio
- Required packages

#### Installing R Packages
Packages are available on CRAN and can be installed using a simple call to `install.packages()`:

    install.packages('PackageName')
	
	
## Script Set
##### Please run the following scripts sequentially
**Data Curation (dc)** 

	- 0.dc-all-script.R
		- This is the only script to run to curate, control the quality, transform and get the final data for modeling. This script calls all the following scripts sequentially.
			- 1.dc-curate-and-process-data.R
			- 2.dc-process-activity-app-usage-data.R
			- 3.dc-quality-control-phase-one.R
			- 4.dc-generate-transformed-data.R
			- 5.dc-generate-normalized-data.R
			- 6.dc-merge-all-data.R
			- 7.dc-generate-meta-data-break-activity.R
			- 8.dc-generate-model-data.R
	
**Validation Scripts (vs)**

	- vs-supplementary-activity-signal.Rmd
		- Generate plots for activity and app usage
	- vs-modeling-analysis.R
	- 



##### Note: Please do not run any script after this
-------------------------------------------------------------------------------------------------------------
**Utility Scripts (us)**

	- us-common-functions.R
	    - Useful functions that are called from almost all scripts
	- us-filter-pp.R
	    - Removes noise from PP signals. It is called from 1.dc-curate-and-process-data
	- us-down-sample-pp.R
	    - Downsamples data to 1 fps. It is called from 1.dc-curate-and-process-data
	    
	    
**Data Curation (dc)**

	- 1.dc-curate-and-process-data.R
	- 2.dc-process-activity-app-usage-data.R
	- 3.dc-quality-control-phase-one.R
	- 4.dc-generate-transformed-data.R
	- 5.dc-generate-normalized-data.R
	- 6.dc-merge-all-data.R
	- 7.dc-generate-meta-data-break-activity.R
	- 8.dc-generate-model-data.R
