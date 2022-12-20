### -------------------------------------------------------- ###
### -------- Mean states at least disturbed FECs ----------- ### 
### -------------------------------------------------------- ###

 
# date created: 06.05.22
# date last modified: 06.05.22
# Project: Evaluating European Broad River Types for Macroinvertebrates
# Purpose: Compute the mean states for least disturbed FECs 


# setup -----------------------------------------------------------------------------
library(pacman)
p_load(sf,
       ggplot2,
       data.table,
       mapview,
       dplyr,
       rstudioapi,
       tidyr)

x<-getActiveDocumentContext()
sink(file = paste0("R/03_identification_of_least_impaired_sites/log_files/002","_", Sys.Date(), "_", "log.txt"))
Sys.Date()
x$path
sessionInfo()
sink(file = NULL)
rm(x)

# load data -------------------------------------------------------------------------
fec <- st_read("data/lemm_least_impacted.gpkg")

