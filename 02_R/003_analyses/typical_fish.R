### -------------------------------- ###
### --- compute typical for fish --- ### 
### -------------------------------- ###

# -------------------------------
# date written: 10.03.22
# date last modified: 10.03.22 
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: Compute typical communities for fish 
# Notes: 
# -------------------------------

library(data.table)
library(vegan)
library(dplyr)
library(DoE.base)

data <- readRDS("data/fish/combined_data/03_2022-06-15_no_rare_taxa.rds")

called_by <- taxon <- "fish"

### log files ---- 
sink(file = paste0("R/analyses/log_files/typical","_", called_by, "_", Sys.Date(), "_", "log.txt"))
rstudioapi::documentPath()
sessionInfo()
sink(file = NULL)
### --------------


source("R/analyses/typical.R")
res_lst2

saveRDS(res_lst2, paste0("data/results/fish_typical_", Sys.Date(), ".rds"))
beepr::beep()