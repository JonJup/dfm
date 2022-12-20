### ----------------------------------------- ###
### --- derive indicators for macrophytes --- ### 
### ----------------------------------------- ###

# -------------------------------
# date written: 12.12.22
# date last modified: 12.12.22 
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: Derive indicator species for macrophytes 
# Notes: 
# -------------------------------

library(indicspecies)
library(data.table)
library(rstudioapi)
library(tidyr)
library(dplyr)
library(stringr)

data <- readRDS("data/macrophytes/combined_data/03_2022-06-20_no_rare_taxa.rds")

called_by <- taxon <- "macrophytes"

### log files ---- 
sink(file = paste0("R/analyses/log_files/indval","_", called_by, "_", Sys.Date(), "_", "log.txt"))
documentPath()
sessionInfo()
sink(file = NULL)
### --------------

# analysis --------------------------------------------------------------------------


ind1 <- function(x, typology, permutations = 999) {
        multipatt(x[, -c(1:7)],
                  cluster = unlist(x[, (typology), with = FALSE]),
                  func = "IndVal.g", # correct indval for unequal group sizes
                  duleg = TRUE,  # TRUE = do not consider site groups 
                  print.perm = TRUE, # TRUE = print every 100th permutation to console
                  control = how(nperm = permutations)  # Number of permutations
                  )
}
ind2 <- 
        function(y){
                out <- y$sign
                taxa <- rownames(y$str)
                setDT(out)
                out[, taxon := taxa]
                out <- out[p.value <=0.05]
                out <- tidyr::pivot_longer(out, cols = !c(stat, p.value, index, taxon), names_to = "type", values_to = "indic")
                out <- dplyr::filter(out, indic == 1)
                out <- dplyr::select(out, taxon, type, stat)
                out <- mutate(out, stat = round(stat, 2))
                out <- mutate(out, type = str_remove(out$type, "^s\\."))
                return(out)
        }
## - species level brt 
x <- ind1(x = data[[1]], typology = "brt", permutations = 999) 
x <- ind2(x)
saveRDS(x, "data/results/indicators/macrophytes_species_brt.rds")
write.csv(x, "data/results/indicators/macrophytes_species_brt.csv")
x <- ind1(x = data[[1]], typology = "ife", permutations = 999) 
x <- ind2(x)
saveRDS(x, "data/results/indicators/macrophytes_species_ife.rds")
write.csv(x, "data/results/indicators/macrophytes_species_ife.csv")
x <- ind1(x = data[[1]], typology = "bgr", permutations = 999) 
x <- ind2(x)
saveRDS(x, "data/results/indicators/macrophytes_species_bgr.rds")
write.csv(x, "data/results/indicators/macrophytes_species_bgr.csv")
x <- ind1(x = data[[1]], typology = "few", permutations = 999) 
x <- ind2(x)
saveRDS(x, "data/results/indicators/macrophytes_species_few.rds")
write.csv(x, "data/results/indicators/macrophytes_species_few.csv")
x <- ind1(x = data[[1]], typology = "enz", permutations = 999) 
x <- ind2(x)
saveRDS(x, "data/results/indicators/macrophytes_species_enz.rds")
write.csv(x, "data/results/indicators/macrophytes_species_enz.csv")
