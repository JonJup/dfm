## -- Inspect results zeta

# created:  21.02.22
# modified: 20.12.22
# project : Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# purpose : Inspect the results of the zeta analysis. 

# setup -----------------------------------------------------------------------------
library(pacman)
p_load(rstudioapi, 
       data.table,
       ggplot2,
       magrittr,
       dplyr,
       wesanderson,
       RColorBrewer)
# - color palettes 
pal <- wes_palette("Darjeeling1")[c(1,2,3,4,5)]
myPalette <- colorRampPalette(rev(brewer.pal(11, "RdBu")))

### log files ---- 
sink(file = paste0("02_R/999_log_files/inspect_results_zeta_", Sys.Date(), "_", "log.txt"))
documentPath()
sessionInfo()
sink(file = NULL)
### --------------

# load data -------------------------------------------------------------------------

fish <- readRDS("01_data/004_results/fish_zeta_2022-06-15.rds")
diat <- readRDS("01_data/004_results/diatom_zeta_2022-06-16.rds")
maph <- readRDS("01_data/004_results/macrophytes_zeta_2022-12-14.rds")
null <- readRDS("01_data/004_results/all_zeta_null_model.rds")

fish$taxon <- "fish"
diat$taxon <- "diatom"
maph$taxon <- "macrophyte"

null[taxon == "mph", taxon := "macrophyte"]
null[taxon == "fsh", taxon := "fish"]
null[taxon == "dia", taxon := "diatom"]

data <- rbindlist(list(fish, diat, maph, null))
rm(fish, diat, maph, null)

#data.u <- unique(data, by = c("typology", "taxon", "taxonomic_resolution"))

# mean values  ----------------------------------------------------------------------
data |>   
        mutate(resolution = factor(taxonomic_resolution, levels = c(1,2,3,4))) |> 
        filter(resolution != 4) |>  
        ggplot(aes(x = resolution, y = auc, group = taxon)) + 
        geom_line(aes(col = taxon)) +
        facet_wrap(.~typology)


data |> 
        filter(taxon == "macrophyte") |> 
        ggplot(aes(y = auc, x = taxonomic_resolution)) + 
        geom_jitter(width = .1, height = 0, alpha = 0.2)

data[taxon == "macrophyte" & taxonomic_resolution == 1, median(auc), by = c("typology")]
data[taxon == "fish" & taxonomic_resolution == 1, median(auc), by = c("typology")]
data[taxon == "macrophyte", median(auc), by = c("typology")]

data[!stringr::str_detect(typology, "null") & taxonomic_resolution == 1, median(auc), by = "taxon"]
data[ taxonomic_resolution == 1, median(auc), by = "typology"]
