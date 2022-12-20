### --------------------------- ###
### --- Infos on data sets  --- ### 
### --------------------------- ###

# -------------------------------
# written : 17.10.22
# modified: 17.10.22
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: Get summary information on data sets
# -------------------------------


# setup -----------------------------------------------------------------------------

library(pacman)

p_load(data.table)


# load data -------------------------------------------------------------------------

fish <- readRDS("data/fish/combined_data/01_2022-06-15_combined_data_aggregated.rds")
diat <- readRDS("data/diatoms/combined_data/01_2022-06-15_combined_data_aggregated.rds")
maph <- readRDS("data/macrophytes/combined_data/01_2022-06-17_combined_data_aggregated.rds")

# numbers of sites ------------------------------------------------------------------
uniqueN(fish$gr_sample_id)
uniqueN(diat$gr_sample_id)
uniqueN(maph$gr_sample_id)

