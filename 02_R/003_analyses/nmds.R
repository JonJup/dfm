### -------------------- ###
### --- compute nmds --- ### 
### -------------------- ###

# -------------------------------
# date written: 21.11.22
# date last modified: 21.11.22
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: Compute NMDS
# Notes: 
# -------------------------------

distance <- lapply(data, function(x) parallelDist(as.matrix(x[,-c(1:7)]), method = "binary", threads = 7))
nmds <- lapply(distance, function(x) metaMDS(comm = x))
               