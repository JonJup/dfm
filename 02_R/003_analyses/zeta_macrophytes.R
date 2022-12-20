### ---------------------------------------------- ###
### --- compute zeta diversity for macrophytes --- ### 
### ---------------------------------------------- ###

# -------------------------------
# date written: 10.03.22
# date last modified: 14.12.22 
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: Compute zeta diversity for macrophytes 
# Notes: 
# last changes: Adapt to Macrophytes incl. Bryophytes 
# -------------------------------


##load packages
library(data.table)
library(dplyr)
library(tidyr)
library(zetadiv)


## save setup to file 
data <- readRDS("01_data/003_macrophytes/002_combined_data/03_2022-12-12_no_rare_taxa.rds")
called_by <- "macrophytes"

### log files ---- 
sink(file = paste0("02_R/999_log_files/zeta","_", called_by, "_", Sys.Date(), "_", "log.txt"))
rstudioapi::documentPath()
sessionInfo()
sink(file = NULL)
### -------------- 

source("02_R/003_analyses/zeta.R")
zeta

saveRDS(zeta, paste0("01_data/004_results/macrophytes_zeta_", Sys.Date(), ".rds"))

