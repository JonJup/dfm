## -- Inspect results typical

# created:  21.02.22
# modified: 20.12.22
# project : Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# purpose : Inspect the results of the typical analysis. 

# setup -----------------------------------------------------------------------------
library(pacman)
p_load(rstudioapi, 
       data.table,
       ggplot2,
       magrittr,
       dplyr,
       wesanderson,
       RColorBrewer)

### log files ---- 
sink(file = paste0("02_R/999_log_files/inspect_results_typical_", Sys.Date(), "_", "log.txt"))
documentPath()
sessionInfo()
sink(file = NULL)
### --------------


# functions -------------------------------------------------------------------------

# load data -------------------------------------------------------------------------
fish <- readRDS("01_data/004_results/fish_typical_2022-06-15.rds")
diat <- readRDS("01_data/004_results/diatoms_typical_2022-06-16.rds")
maph <- readRDS("01_data/004_results/macrophytes_typical_2022-12-14.rds")
null <- readRDS("01_data/004_results/all_typical_null_model2022-12-14.rds")

# prepare data ----------------------------------------------------------------------
# - color palettes 
pal <- wes_palette("Darjeeling1")[c(1,2,3,4,5,6)]
myPalette <- colorRampPalette(rev(brewer.pal(11, "RdBu")))


fish$taxon = "fish"
diat$taxon = "diatom"
maph$taxon = "macrophyte"

data <- rbindlist(list(diat, fish, maph, null), use.names = TRUE)

data$typology_system <- factor(data$typology_system, levels = c("brt", "bgr", "enz", "few",
                                                                "ife", "null_model1_type",
                                                                "null_model2_type", 
                                                                "null_model3_type",
                                                                "null_model4_type"))

data[taxonomic_level == "species" & taxon == "diatom", median(jaccard_similarity), by = "typology_system"]




