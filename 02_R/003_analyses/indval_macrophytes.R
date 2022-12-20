### -------------------------------------- ###
### --- compute indval for macrophytes --- ### 
### -------------------------------------- ###

# -------------------------------
# date written: 10.03.22
# date last modified: 14.12.22 
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: Compute indval for macrophytes 
# last changes: compute for macrophytes including bryophytes 

# -------------------------------

library(indicspecies)
library(data.table)
library(rstudioapi)

data <- readRDS("01_data/003_macrophytes/002_combined_data/03_2022-12-12_no_rare_taxa.rds")

called_by <- taxon <- "macrophytes"

### log files ---- 
sink(file = paste0("02_R/999_log_files/","indval_", called_by, "_", Sys.Date(), "_", "log.txt"))
rstudioapi::documentPath()
sessionInfo()
sink(file = NULL)
### --------------


source("02_R/003_analyses/indval.R")
results_indval

saveRDS(results_indval, paste0("01_data/004_results/macrophytes_indval_", Sys.Date(), ".rds"))
