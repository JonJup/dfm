## -- compute anosim for fish 


# created: 10.03.22
# modified: 15.06.22 
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: Compute ANOSIM for fish 


# setup -----------------------------------------------------------------------------
library(data.table)
library(vegan)

data <- readRDS("data/fish/combined_data/03_2022-06-15_no_rare_taxa.rds")

called_by <- taxon <- "fish"

#- create log files 
sink(file = paste0("R/analyses/log_files/anosim","_", called_by, "_", Sys.Date(), "_", "log.txt"))
rstudioapi::documentPath()
sessionInfo()
sink(file = NULL)

# - call ANOSIM script 
source("R/analyses/anosim.R")
anosim_result

saveRDS(anosim_result, paste0("data/results/fish_anosim_", Sys.Date(), ".rds"))
