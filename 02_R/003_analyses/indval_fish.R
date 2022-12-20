### ------------------------------- ###
### --- compute indval for fish --- ### 
### ------------------------------- ###

# -------------------------------
# date written: 10.03.22
# date last modified: 15.06.22 
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: Compute indval for fish 
# Notes: 
# -------------------------------

library(indicspecies)
library(data.table)

data <- readRDS("data/fish/combined_data/03_2022-06-15_no_rare_taxa.rds")

called_by <- taxon <- "fish"

### log files ---- 
sink(file = paste0("R/analyses/log_files/indval","_", called_by, "_", Sys.Date(), "_", "log.txt"))
rstudioapi::documentPath()
sessionInfo()
sink(file = NULL)
### --------------


source("R/analyses/indval.R")
results_indval

saveRDS(results_indval, paste0("data/results/fish_indval_", Sys.Date(), ".rds"))
beepr::beep()