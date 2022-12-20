### --------------------------------------- ###
### --- compute zeta diversity for fish --- ### 
### --------------------------------------- ###

# -------------------------------
# date written: 08.03.22
# date last modified: 08.03.22 
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: Compute zeta diversity for fish 
# Notes: 
# -------------------------------

library(pacman)
p_load(data.table, rstudioapi, zetadiv)

data <- readRDS("data/fish/combined_data/03_2022-06-15_no_rare_taxa.rds")

called_by <- "fish"

### log files ---- 
sink(file = paste0("R/analyses/log_files/zeta","_", called_by, "_", Sys.Date(), "_", "log.txt"))
documentPath()
sessionInfo()
sink(file = NULL)
### --------------


source("R/analyses/zeta.R")
zeta

saveRDS(zeta, paste0("data/results/fish_zeta_", Sys.Date(), ".rds"))
