### -------------------------------- ###
### --- compute nmds for diatoms --- ### 
### -------------------------------- ###

# -------------------------------
# date written: 21.11.2022
# date last modified: 21.11.2022
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: Compute nmds for diatoms 
# Notes: 
# -------------------------------

library(data.table)
library(parallelDist)
library(vegan)

data <- readRDS("data/diatoms/combined_data/03_2022-06-20_no_rare_taxa.rds")

called_by <- taxon <- "diatoms"

### log files ---- 
sink(file = paste0("R/analyses/log_files/anosim","_", called_by, "_", Sys.Date(), "_", "log.txt"))
rstudioapi::documentPath()
sessionInfo()
sink(file = NULL)
### --------------

source("R/analyses/anosim.R")
anosim_result

saveRDS(anosim_result, paste0("data/results/diatoms_anosim_", Sys.Date(), ".rds"))
