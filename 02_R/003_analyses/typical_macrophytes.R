### --------------------------------------- ###
### --- compute typical for macrophytes --- ### 
### --------------------------------------- ###

# -------------------------------
# date written: 10.03.22
# date last modified: 14.12.22 
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: Compute typical communities for macrophytes 
# last changes: compute for macropyhtes including bryphytes
# -------------------------------


# setup ----------------------------------------------------------------------------
library(data.table)
library(vegan)
library(dplyr)
library(DoE.base)

called_by <- taxon <- "macrophytes"
### log files ---- 
sink(file = paste0("02_R/999_log_files/typical","_", called_by, "_", Sys.Date(), "_", "log.txt"))
rstudioapi::documentPath()
sessionInfo()
sink(file = NULL)
### --------------
rm(called_by)

# load data -------------------------------------------------------------------------
data <- readRDS("01_data/003_macrophytes/002_combined_data/03_2022-12-12_no_rare_taxa.rds")

# compute similarity between typical assemblages ------------------------------------
source("02_R/003_analyses/typical.R")
res_lst2

saveRDS(res_lst2, paste0("01_data/004_results/macrophytes_typical_", Sys.Date(), ".rds"))
