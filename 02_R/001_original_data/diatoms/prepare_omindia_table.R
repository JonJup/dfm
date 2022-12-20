### —————————————————————————————————— ###
### Prepare Omnidia table for analyses ### 
### —————————————————————————————————— ###

# ———————————————————————————————————
# date first written: 30-11-21
# date last modified: 30-11-21
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: The taxon list from the Omnidia software is used to harmonize diatom taxonomy. 
# In this script, I prepare the table. 
# ————————————————

# setup -----------------------------------------------------------------------------
pacman::p_load(readxl, dplyr, data.table, stringr, magrittr)

# load data -------------------------------------------------------------------------
dat <- read_excel("data/diatoms/OMNIDIAFILE.xls", skip = 1)

# prepare data  ---------------------------------------------------------------------
dat2 <- select(dat, "CODE", "DENOM", "SYNO")
dat2 %<>% rename(code = CODE, taxon = DENOM, new = SYNO)

# save to file  ---------------------------------------------------------------------
saveRDS(dat2, "data/diatoms/omn_table.rds")

