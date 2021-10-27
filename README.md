# Computer-centered deadline behaviors and stress patterns
This repository contains the R scripts to curate, ensure quality control, normalize, and generate final model data from the raw data collected
via the S-Interface and other tools in the "Deadline Stress and Work Patterns - 2020" project.


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
    - The only script to run to curate, control the quality, transform and get the final data for modeling. 
    - Download raw_and_noise_removed_df.csv from [OSF Deadline Stress-Work Patterns](https://osf.io/46x7w) and put it on the "all-subj-data" folder under "curated-data" folder. 
    - This script calls all the following scripts sequentially:
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
	    - Generates plots for activity and app usage
	- vs-modeling-sanalysis.R
	    - Checks collinearity and performs linear modeling
	- vs-final-model-visualization.rmd
	    - Visualizes the significant predictors of the model



##### Note: Please do not run any script after this
-------------------------------------------------------------------------------------------------------------
**Utility Scripts (us)**

	- us-common-functions.R
	    - Useful functions that are called from almost all scripts.
	- us-filter-pp.R
	    - Removes noise from PP signals. It is called from 1.dc-curate-and-process-data.
	- us-down-sample-pp.R
	    - Downsamples data to 1 fps. It is called from 1.dc-curate-and-process-data.
	    
	    
**Data Curation (dc)**

	- 1.dc-curate-and-process-data.R
	    - For each participant the script does the following:
	    	- Reads the original perinasal perspiration signal data, removes noise, downsamples to 1 frame per second (fps)
		- Reads the E4 and iWatch signal files, downsamples them, removes noise, and merges them with the PP signal

	- 2.dc-process-activity-app-usage-data.R
	    - Processes and finalizes the participant's activity data
	    - Processes and finalizes the app usage data

	- 3.dc-quality-control-phase-one.R
	    - Performs filtering on physiological data, to remove the signals with invalid range
	
	- 4.dc-generate-transformed-data.R
	    - Performs log transformation on all modalities of the physiological channel
	
	- 5.dc-generate-normalized-data.R
	    - Generates normalized data in respect of the Resting Baseline session
	
	- 6.dc-merge-all-data.R
	    - Gathers and merges the physiological, psychometrics, activity data for all participants
	    
	- 7.dc-generate-meta-data-break-activity.R
	    - Generates data for total breaks, time of the break, frequency of the breaks etc.
	
	- 8.dc-generate-model-data.R
	    - Generates the working session mean data to perform the linear modeling
