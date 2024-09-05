## Introduction

This repository provides replication materials for tables and plots in the paper:

> Mastrosavvas, A. (2024) “The Geography of Partisan Homophily in the 2020 US Presidential Election”, *Applied Geography*, 171, 103371, Available at: [https://doi.org/10.1016/j.apgeog.2024.103371](https://doi.org/10.1016/j.apgeog.2024.103371).

Date: 2024-09-24 

## Overview of scripts
* The R scripts numbered 0-5 perform all the prerequisite data collection and pre-processing steps, and produce the tables and plots:
  
  * `0a_create_folders.R` creates the file structure of the repository.
  * `0b_get_data.R` downloads some of the larger required datasets using publicly accessible links. 
  * `0c_prep_voting_data.R` pre-processes data on 2020 US presidential election results.
  * `0d_prep_lkup_data.R` produces ZCTA lookup tables.
  * `1_prep_int_data.R` pre-processes social connectedness and geographic distance data.
  * `2_get_valid_df_wtmx.R` produces weights matrices.
  * `3_get_main_sf_and_lisa.R` produces final geometries and calculates LISA measures.
  * `4_get_maps.R` produces map plots.
  * `5_get_charts_tables.R` produces charts and tables.

* To reproduce the results in the paper, the scripts need to be run in order i.e. from 0 to 5. 

* Note that, depending on system specifications, some scripts may take several hours to run. Alternatively, specific files produced in the pipeline are available on request.
 
## Before running

* Open the R project file `REPL-Geography-of-Partisan-Hom-US.Rproj` in an R session before running R scripts.
  
* Each R script checks your system for any missing R package dependencies and installs them. The relevant code can be found at the top of each script. Ensure that you are satisfied with installing the relevant packages if not already installed before running the script.

* Download data on precinct-level 2020 US presidential election results from the Harvard Dataverse. First, navigate to the dataset [2020 Precinct-Level Election Results](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/K7760H) produced by the Voting and Election Science Team of the University of Florida and Wichita State University. Then, click on "Access Dataset" dropdown menu and select the option "Download ZIP" to download the required master .zip file. Once downloaded, extract the contents of the master .zip file under the folder `rawdata/voting` and delete the files ky_2020.zip and nj_2020.zip.
  
* Download data on ZCTA-level average download speeds from the Inter-university Consortium for Political and Social Research (ICPSR) data archive. First, [login](https://www.icpsr.umich.edu/web/pages/) using an institutional or other account. Second, navigate to the dataset [National Neighborhood Data Archive (NaNDA): Broadband Internet Availability, Speed, and Adoption by Census Tract and ZIP Code Tabulation Area, United States, 2014-2020 (ICPSR 38567)](https://www.icpsr.umich.edu/web/ICPSR/studies/38567/publications). Then, click on the "Download" dropdown menu and select the option "R" to download the required .rda file. Ensure that the file is saved within the repository as follows: `./rawdata/broadband/38567-0002-Data.rda`.

* The tables and plots appearing in the paper are available under the `output' folder. If just accessing this data, there is no need to run the scripts.
 
 ## Attribution statements

* The lookup table `rawdata/lookups/geocorr2022_2329704436.csv` has been retrieved using the publicly available [Geocorr](https://mcdc.missouri.edu/applications/geocorr.html) application supported by the Missouri Census Data Center.

 ## Storage

 It is advised that at least 50GB of storage is made available.

 ## System and version information

 Platform: aarch64-apple-darwin20 (64-bit) 
 
 Version: R version 4.2.2
 
 
