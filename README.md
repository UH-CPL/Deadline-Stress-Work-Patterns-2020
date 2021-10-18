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



##### Note: Please do not run any script after this
-------------------------------------------------------------------------------------------------------------
**Utility Scripts (us)**

	- us-common-functions.R
	- us-down-sample-pp.R
	- us-filter-pp.R
	
	
**Data Curation (dc)**

	- 1.dc-curate-and-process-data.R
	- 2.dc-process-activity-app-usage-data.R
	- 3.dc-quality-control-phase-one.R
	- 4.dc-generate-transformed-data.R
	- 5.dc-generate-normalized-data.R
	- 6.dc-merge-all-data.R
	- 7.dc-generate-meta-data-break-activity.R
	- 8.dc-generate-model-data.R
