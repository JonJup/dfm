### ------------------------------------------ ###
### --- compute zeta diversity for diatoms --- ### 
### ------------------------------------------ ###

# -------------------------------
# date written: 10.03.22
# date last modified: 16.06.22 
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: Compute zeta diversity for diatoms 
# Notes: 
# -------------------------------

library(pacman)
p_load(data.table, rstudioapi, zetadiv)

data <- readRDS("data/diatoms/combined_data/03_2022-06-15_no_rare_taxa.rds")

called_by <- "diatom"

### log files ---- 
sink(file = paste0("R/analyses/log_files/zeta","_", called_by, "_", Sys.Date(), "_", "log.txt"))
rstudioapi::documentPath()
sessionInfo()
sink(file = NULL)
### --------------


source("R/analyses/zeta.R")
zeta

saveRDS(zeta, paste0("data/results/diatom_zeta_", Sys.Date(), ".rds"))
