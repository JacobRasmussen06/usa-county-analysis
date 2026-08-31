#############################################################################
#
# 00_run_project.R
#
# Purpose: reproduce the project section of this U.S County Analysis Project
#
# Running this script will run all 11 following scripts, however it will not run the app
# please run app.R in the app/ directory to locally host the app if you desire. 
#
#############################################################################

library(here)

message("Running 01_download_and_clean_data.R") # Note: this is by far the most expensive and time consuming download, the author strongly recommends, especially if you are on a laptop, close all other running windows while running this script
source(here("scripts", "01_download_and_clean_data.R"))
message("Running 02_feature_engineering.R")
source(here("scripts", "02_feature_engineering.R"))
message("Running 03_merging_data.R")
source(here("scripts", "03_merging_data.R"))
message("Running 04_eda.R")
source(here("scripts", "04_eda.R"))
message("Running 05_pca.R")
source(here("scripts", "05_pca.R"))
message("Running 06_pca_clustering.R")
source(here("scripts", "06_pca_clustering.R"))
message("Running 07_hierarchical_clustering.R")
source(here("scripts", "07_hierarchical_clustering.R"))
message("Running 08_gmm_clustering.R")
source(here("scripts", "08_gmm_clustering.R"))
message("Running 09_similarity.R")
source(here("scripts", "09_similarity.R"))
message("Running 10_uniqueness.R")
source(here("scripts", "10_uniqueness.R"))
message("Running 11_dashboard_dataset.R")
source(here("scripts", "11_dashboard_dataset.R"))

message("Project has successfully ran!")

#############################################################################
# End of Script
#############################################################################