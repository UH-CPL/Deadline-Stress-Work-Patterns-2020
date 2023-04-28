# Sympathetic Activation in Deadlines
This repository contains the R scripts to curate, ensure quality control, normalize, and generate final model results from the raw data collected
via the S-Interface and other tools in the context of a naturalistic study on deadlines. The study was funded by NSF (grant #1704682) and appeared in the paper **Sympathetic Activation in Deadlines of Deskbound Research - A Study in the Wild**, published in CHI 2023.


## Getting Started

#### Prerequisites
- R and RStudio
- Required packages

#### Installing R Packages
Packages are available on CRAN and can be installed using a simple call to `install.packages()`:

    install.packages('PackageName')
	
	
## Script Set
##### Please run the following scripts sequentially

The **Data Curation (dc)** scrips will process the raw data and create the model data. The **Modeling Scripts (ms)** will perform the multy linear regression and produce the visualizations.

- 0.dc-all-script.R
    - The only script to run to curate, control the quality, transform and get the final data for modeling. 
    Download raw_and_noise_removed_df.csv from OSF [Sympathetic Activation in Deadlines](https://osf.io/46x7w/) put it on the "all-subj-data" folder under "curated-data" folder.
    - This script calls all the following scripts sequentially:
    	- 2.dc-process-activity-app-usage-data.R
    	- 3.dc-quality-control-phase-one.R
    	- 4.dc-generate-transformed-data.R
    	- 5.dc-generate-normalized-data.R
    	- 6.dc-merge-all-data.R
    	- 7.dc-generate-meta-data-break-activity.R
    	- 8.dc-generate-model-data.R
    	- ms-descriptive.rmd
    	- ms-model-visualization.rmd


##### Note: Please do not run any script after this
-------------------------------------------------------------------------------------------------------------
**Details of Utility Scripts (us)**

The **Utility Scripts (us)** are used to define the common functions like read-write files, and are being called from the **Data Curation (dc)** scripts.  

	- us-common-functions.R
	    - Useful functions that are called from almost all scripts.
	- us-filter-pp.R
	    - Removes noise from PP signals. It is called from 1.dc-curate-and-process-data.
	- us-down-sample-pp.R
	    - Downsamples data to 1 fps. It is called from 1.dc-curate-and-process-data.
	    
	    
**Details of Data Curation (dc) & Modeling Scripts (ms)**

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
	
	- ms-descriptive.rmd
	    - Produces the exploratory data visualization
	
	- ms-model-visualization.rmd
	    - Checks collinearity and performs linear modeling and Visualizes the significant predictors of the model
