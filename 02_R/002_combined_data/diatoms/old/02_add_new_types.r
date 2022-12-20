### ------------------------------------------ ###
### --- ADD NEW Typologies to Diatoms      --- ### 
### ------------------------------------------ ###

# -------------------------------
# date written: 10.03.22
# date last modified: 10.03.22
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: Add European Ecozones and FEOW to Macrophyte data  
# Notes: 
# -------------------------------

# load data -------------------------------------------

data <- readRDS("data/diatoms/combined_data/01_2022-03-10_combined_data_aggregated.rds")

# add typology systems  -------------------------------------------------------------

# - add FEOW 
source("R/misc/add_feow.R")

# - environmental zones 
source("R/misc/add_environmental_zones.R")

data[, geometry := NULL]

# save to file  ---------------------------------------------------------------------
saveRDS(data, paste0("data/diatoms/combined_data/02_",Sys.Date(),"_w_new_types.rds"))
rm(list = ls())

