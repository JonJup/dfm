## -- Inspect results ISA

# created:  21.02.22
# modified: 19.12.22
# project : Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# purpose : Inspect the results of the ISA analysis. 

# setup -----------------------------------------------------------------------------
library(pacman)
p_load(rstudioapi, 
       data.table,
       ggplot2,
       magrittr,
       dplyr,
       wesanderson,
       RColorBrewer,
       stringr)
# - color palettes 
pal <- wes_palette("Darjeeling1")[c(1,2,3,4,5)]
myPalette <- colorRampPalette(rev(brewer.pal(11, "RdBu")))

### log files ---- 
sink(file = paste0("02_R/999_log_files/inspect_results_ISA_", Sys.Date(), "_", "log.txt"))
documentPath()
sessionInfo()
sink(file = NULL)
### --------------


# functions -------------------------------------------------------------------------

# load data -------------------------------------------------------------------------
fish   <- readRDS("01_data/004_results/fish_indval_2022-06-15.rds")
fish.n <- readRDS("01_data/004_results/fish_indval_null.rds")
diat   <- readRDS("01_data/004_results/diatoms_indval_2022-06-16.rds")
diat.n <- readRDS("01_data/004_results/diatom_indval_null.rds")
macr   <- readRDS("01_data/004_results/macrophytes_indval_2022-12-14.rds")
macr.n <- readRDS("01_data/004_results/macrophytes_indval_null.rds")

diat$taxon <- "diatom"
diat.n$taxon <- "diatom"
macr$taxon <- "macrophyte"
macr.n$taxon <- "macrophyte"

fish %<>% rename(taxonomic.resolution = "taxoonomic.resolution")
diat %<>% rename(taxonomic.resolution = "taxoonomic.resolution")
macr %<>% rename(taxonomic.resolution = "taxoonomic.resolution")

# adjust number of indicators to total number of taxa
data <- rbindlist(list(fish,
                       fish.n,
                       diat,
                       diat.n,
                       macr,
                       macr.n),
                  fill = TRUE)
data[taxon == "diatom" & taxonomic.resolution == "species", n.indicator.adj := n.indicator/1110]
data[taxon == "diatom" & taxonomic.resolution == "genus", n.indicator.adj := n.indicator/176]
data[taxon == "diatom" & taxonomic.resolution == "family", n.indicator.adj := n.indicator/60]
data[taxon == "fish" & taxonomic.resolution == "species", n.indicator.adj := n.indicator/105]
data[taxon == "fish" & taxonomic.resolution == "genus", n.indicator.adj := n.indicator/69]
data[taxon == "fish" & taxonomic.resolution == "family", n.indicator.adj := n.indicator/21]
data[taxon == "macrophyte" & taxonomic.resolution == "species", n.indicator.adj := n.indicator/299]
data[taxon == "macrophyte" & taxonomic.resolution == "genus", n.indicator.adj := n.indicator/131]
data[taxon == "macrophyte" & taxonomic.resolution == "family", n.indicator.adj := n.indicator/67]

# inspect results  ----------------------------------------------------------------------
data |> 
        filter(taxonomic.resolution == "species") |> 
        filter(!str_detect(typology, "null")) |> 
        group_by(taxon) |> 
        mutate(a = mean(n.indicator.adj)) |> 
        ggplot(aes(x = taxon, y = a)) + geom_point()
        
data |>   
        mutate(resolution = factor(taxonomic.resolution, levels = c("species", "genus", "family", "order"))) |> 
        filter(resolution != "order") |>  
        ggplot(aes(x = resolution, y = mean_p , group = taxon)) + 
        geom_line(aes(col = taxon)) + 
        facet_wrap(~typology)
data |>   
        mutate(resolution = factor(taxonomic.resolution, levels = c("species", "genus", "family", "order"))) |> 
        filter(resolution != "order") |> 
        ggplot(aes(x = resolution, y = n.indicator.adj , group = taxon)) + 
        geom_line(aes(col = taxon)) +
        facet_wrap(~typology)

data[taxon == "macrophyte" & taxonomic.resolution == "species"]

data[taxonomic.resolution == "species" & taxon == "macrophyte", .(typology, mean_p)]
data[taxonomic.resolution == "species", mean(n.indicator.adj), by = "typology"]
data[taxonomic.resolution == "species" & !str_detect(typology, "null"), mean(n.indicator.adj), by = "taxon"]

data[taxonomic.resolution == "species", mean(mean_p), by = "typology"]
data[taxonomic.resolution == "species" & !str_detect(typology, "null"), mean(mean_p), by = "taxon"]
