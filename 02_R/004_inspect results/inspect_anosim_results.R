## -- Inspect results ANOSIM

# created:  21.02.22
# modified: 19.12.22
# project : Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# purpose : Inspect the results of the ANOSIM analysis. 

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
sink(file = paste0("02_R/999_log_files/inspect_results_anosim_", Sys.Date(), "_", "log.txt"))
documentPath()
sessionInfo()
sink(file = NULL)
### --------------

# load data -------------------------------------------------------------------------
fish <- readRDS("01_data/004_results/fish_anosim_2022-06-15.rds")
diat <- readRDS("01_data/004_results/diatoms_anosim_2022-06-15.rds")
maph <- readRDS("01_data/004_results/macrophytes_anosim_2022-12-12.rds")
null <- readRDS("01_data/004_results/all_anosim_null_multiple_nulls.rds")

data <- rbindlist(list(fish, diat, maph, null))


# mean values  ----------------------------------------------------------------------
data |>   
        filter() |> 
        mutate(resolution = factor(taxonomic.resolution, levels = c("species", "genus", "family", "order"))) |> 
        ggplot(aes(x = resolution, y = statistic , group = taxon)) + 
        geom_line(aes(col = taxon)) +
        facet_wrap(.~typology)

data |>  
        dplyr::filter(taxonomic.resolution == "species") |> 
        dplyr::filter(taxon == "macrophytes") |> 
        select(typology, statistic)
