### -------------------------------------- ###
### --- compute anosim for macrophytes --- ### 
### -------------------------------------- ###

# -------------------------------
# date written: 10.03.22
# date last modified: 12.12.22 
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: Compute anosim for macrophytes 
# Notes: 
# -------------------------------

library(data.table)
library(vegan)

data <- readRDS("data/macrophytes/combined_data/03_2022-12-12_no_rare_taxa.rds")

called_by <- taxon <- "macrophytes"

### log files ---- 
sink(file = paste0("R/analyses/log_files/anosim","_", called_by, "_", Sys.Date(), "_", "log.txt"))
rstudioapi::documentPath()
sessionInfo()
sink(file = NULL)
### --------------


source("R/analyses/anosim.R")
anosim_result

saveRDS(anosim_result, paste0("data/results/macrophytes_anosim_", Sys.Date(), ".rds"))
