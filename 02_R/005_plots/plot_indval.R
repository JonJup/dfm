## -- plot indval results 

# created: 15.06.22
# modified: 19.12.22
# project : Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# purpose : visualize the results of the Indval analysis


# setup -----------------------------------------------------------------------------

library(pacman)
p_load(ggplot2, dplyr, data.table, magrittr, wesanderson)

### log files ---- 
sink(file = paste0("02_R/999_log_files/indval_plot_", Sys.Date(), "_", "log.txt"))
rstudioapi::documentPath()
sessionInfo()
sink(file = NULL)
### --------------

# functions  ------------------------------------------------------------------------

plot_fun <- function(resolution){
        
        vlinevar <- data[typology == "null_model" & taxonomic.resolution == resolution]
        vlinevar %<>% unique(by = c("typology", "taxonomic.resolution", "taxon"))
        
        out_plot <- 
                data |>  
                filter(typology != "null_model", taxonomic.resolution == resolution) |> 
                unique(by = c("typology", "taxonomic.resolution", "taxon")) |> 
                ggplot(aes(y=n.indicator.adj, x=typology)) + 
                geom_col(aes(fill = typology), alpha = .8) +
                geom_hline(aes(yintercept = n.indicator.adj), data = vlinevar, lty = "dashed", linewidth = 1) + 
                xlab(NULL) + 
                ylab("Relative number of Indicators") + 
                facet_grid(.~taxon) + 
                scale_color_manual(values = pal, guide = "none") +
                scale_fill_manual(values = pal) + 
                theme(panel.background = element_rect(fill = "white"),
                      panel.grid.major.x = element_line(colour = "grey89"),
                      axis.ticks = element_blank(),
                      axis.text.x = element_blank(), #element_text(color = pal), # size = 14, lineheight = .9
                      legend.key = element_blank(),
                      panel.border = element_blank(),
                      panel.spacing.y = unit(0,"line")) 
        
        return(out_plot)
        
}
plot_fun2 <- function(resolution){
        
        vlinevar <- data[typology == "null_model" & taxonomic.resolution == resolution]
        vlinevar %<>% unique(by = c("typology", "taxonomic.resolution", "taxon"))
        
        out_plot <- 
                data |>  
                filter(typology != "null_model", taxonomic.resolution == resolution) |> 
                unique(by = c("typology", "taxonomic.resolution", "taxon")) |> 
                ggplot(aes(y=mean_p, x=typology)) + 
                geom_col(aes(fill = typology), alpha = .8) +
                geom_hline(aes(yintercept = mean_p), data = vlinevar, lty = "dashed", size = 1) + 
                xlab(NULL) + 
                ylab("Mean p-value") + 
                facet_grid(.~taxon) + 
                scale_color_manual(values = pal, guide = "none") +
                scale_fill_manual(values = pal) + 
                theme(panel.background = element_rect(fill = "white"),
                      panel.grid.major.x = element_line(colour = "grey89"),
                      axis.ticks = element_blank(),
                      axis.text.x = element_blank(), #element_text(color = pal), # size = 14, lineheight = .9
                      legend.key = element_blank(),
                      panel.border = element_blank(),
                      panel.spacing.y = unit(0,"line")) 
        
        return(out_plot)
        
}

# load data -------------------------------------------------------------------------

fish   <- readRDS("01_data/004_results/fish_indval_2022-06-15.rds")
fish.n <- readRDS("01_data/004_results/fish_indval_null.rds")
diat   <- readRDS("01_data/004_results/diatoms_indval_2022-06-16.rds")
diat.n <- readRDS("01_data/004_results/diatom_indval_null.rds")
macr   <- readRDS("01_data/004_results/macrophytes_indval_2022-12-14.rds")
macr.n <- readRDS("01_data/004_results/macrophytes_indval_null.rds")


# prepare data ----------------------------------------------------------------------
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

# - define color palette from wesanderson package 
pal <- wes_palette("Darjeeling1")[c(1,2,3,4,5)]
data <- data[!typology %in% c("null_model2", "null_model3", "null_model4")]
data[typology == "null_model1", typology := "null_model"]
data[, typology := factor(typology, levels = c("brt", "bgr", "enz", "feow", "ife", "null_model"))]

res<- data[taxonomic.resolution == "species" & taxon == "diatom"]
res[, median(mean_p), by = "typology"]

# create plots  ---------------------------------------------------------------------

for (i in c("species", "genus", "family")){
        plot_fun(i); ggsave(paste0("04_fig/002_results/003_identicator/indval_n_",i,"_", Sys.Date(),".tiff"))  
}
for (i in c("species", "genus", "family")){
        plot_fun2(i); ggsave(paste0("04_fig/002_results/003_identicator/indval_p_",i,"_", Sys.Date(),".tiff"))  
}


