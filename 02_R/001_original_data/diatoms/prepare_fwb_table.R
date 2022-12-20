### —————————————————————————————— ###
### Prepare FWB table for analyses ### 
### —————————————————————————————— ###

# ———————————————————————————————————
# date first written: 30-11-21
# date last modified: 30-11-21
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: The table S2 from Kahlert et al (2020). https://doi.org/10.1111/fwb.13490
# is used to harmonize diatom taxonomy. In this script, I prepare the table. 
# ————————————————


# setup -----------------------------------------------------------------------------
pacman::p_load(data.table, dplyr, readxl)

# load data -------------------------------------------------------------------------
dia <- read_excel("data/diatoms/fwb13490-sup-0003-TableS2.xlsx", skip = 1)

# prepare table  --------------------------------------------------------------------
dia <- dia[,1:2]
names(dia) <- c("taxon_new", "taxon_old")

# save to file  ---------------------------------------------------------------------
saveRDS(object = dia, file = "data/diatoms/fwb_table.rds")

