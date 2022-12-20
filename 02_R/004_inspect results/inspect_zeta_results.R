## -- Inspect results zeta

# created:  21.02.22
# modified: 21.02.22
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
sink(file = paste0("R/log_files/inspect_results_zeta_", Sys.Date(), "_", "log.txt"))
documentPath()
sessionInfo()
sink(file = NULL)
### --------------


# functions -------------------------------------------------------------------------

# load data -------------------------------------------------------------------------

fish <- readRDS("data/results/fish_zeta_2022-06-15.rds")
diat <- readRDS("data/results/diatom_zeta_2022-06-16.rds")
maph <- readRDS("data/results/macrophytes_zeta_2022-06-17.rds")
null <- readRDS("data/results/all_zeta_null_model.rds")

fish$taxon <- "fish"
diat$taxon <- "diatom"
maph$taxon <- "macrophyte"

data <- rbindlist(list(fish, diat, maph, null))
rm(fish, diat, maph, null)

data <- unique(data, by = c("typology", "taxon", "taxonomic_resolution"))

# mean values  ----------------------------------------------------------------------
data |>   
        mutate(resolution = factor(taxonomic_resolution, levels = c(1,2,3,4))) |> 
        filter(resolution != 4) |>  
        ggplot(aes(x = resolution, y = auc, group = taxon)) + 
        geom_line(aes(col = taxon)) +
        facet_wrap(.~typology)


data |> 
        filter(taxon == "macrophyte") |> 
        mutate(resolution = factor(taxonomic_level, levels = c("species", "genus", "family", "order"))) |>         
        ggplot(aes(y = total, x = resolution)) + 
        geom_jitter(width = .1, height = 0, alpha = 0.2)



cs %<>% mutate(difference = within_type - between_type)
cs_dia <- filter(cs, taxon == "diatom" & resolution == "genus", typology != "null_model_type")
cs_fis <- filter(cs, taxon == "fish" & resolution == "genus", typology != "null_model_type")
cs_mac <- filter(cs, taxon == "macrophytes" & resolution == "genus", typology != "null_model_type")

cs_dia1 <- arrange(cs_dia, between_type)
cs_dia2 <- arrange(cs_dia, desc(within_type))
cs_dia3 <- arrange(cs_dia, difference)
cs_fis1 <- arrange(cs_fis, between_type)
cs_fis2 <- arrange(cs_fis, desc(within_type))
cs_fis3 <- arrange(cs_fis, difference)
cs_mac1 <- arrange(cs_mac, between_type)
cs_mac2 <- arrange(cs_mac, desc(within_type))
cs_mac3 <- arrange(cs_mac, difference)

cs_dia1 %<>% mutate(type = factor(type, levels = unique(cs_dia1$type)))
cs_dia2 %<>% mutate(type = factor(type, levels = unique(cs_dia2$type)))
cs_dia3 %<>% mutate(type = factor(type, levels = unique(cs_dia3$type)))
cs_fis1 %<>% mutate(type = factor(type, levels = unique(cs_fis1$type)))
cs_fis2 %<>% mutate(type = factor(type, levels = unique(cs_fis2$type)))
cs_fis3 %<>% mutate(type = factor(type, levels = unique(cs_fis3$type)))
cs_mac1 %<>% mutate(type = factor(type, levels = unique(cs_mac1$type)))
cs_mac2 %<>% mutate(type = factor(type, levels = unique(cs_mac2$type)))
cs_mac3 %<>% mutate(type = factor(type, levels = unique(cs_mac3$type)))


cs_plot(cs_dia1) + facet_wrap(.~typology, scales="free") + ggtitle("diatoms")
