### ------------------------------------------ ###
### --- analyze indicators for macrophytes --- ### 
### ------------------------------------------ ###

# -------------------------------
# date written: 12.12.22
# date last modified: 12.12.22 
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: Inspect indicator species for macrophytes 
# Notes: 
# -------------------------------

library(dplyr)
library(data.table)
library(rstudioapi)

data <- readRDS("data/macrophytes/combined_data/03_2022-06-20_no_rare_taxa.rds")

called_by <- taxon <- "macrophytes"

### log files ---- 
sink(file = paste0("R/analyses/log_files/indval_communities_inspect","_", called_by, "_", Sys.Date(), "_", "log.txt"))
documentPath()
sessionInfo()
sink(file = NULL)
### --------------

# load data -------------------------------------------------------------------------
brt <- readRDS("data/results/indicators/macrophytes_species_brt.rds")
bgr <- readRDS("data/results/indicators/macrophytes_species_bgr.rds")
ife <- readRDS("data/results/indicators/macrophytes_species_ife.rds")
enz <- readRDS("data/results/indicators/macrophytes_species_enz.rds")
few <- readRDS("data/results/indicators/macrophytes_species_few.rds")

# basic data ------------------------------------------------------------------------
table(brt$type)
table(bgr$type)

write.csv(brt, "data/results/indicators/macrophytes_species_brt.csv")
