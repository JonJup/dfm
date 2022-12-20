## -- plot zeta results 

# created: 15.06.22
# modified: 16.12.22
# project : Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# purpose : visualize the results of the zeta diversity analysis
# last change: add bryphytes to macrophytes 

# setup -----------------------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(data.table)
library(ggdist)
library(ggrepel)
library(magrittr)
library(wesanderson)
## not relevant for paper
library(viridis)

### log files ---- 
sink(file = paste0("02_R/999_log_files/zeta_plot_", Sys.Date(), "_", "log.txt"))
rstudioapi::documentPath()
sessionInfo()
sink(file = NULL)
### --------------


# functions -------------------------------------------------------------------------

plot_fun <- function(res){
        
        vlinevar <- 
                data[typology == "null_model_type" & taxonomic_resolution == res] %>% 
                unique(by = c("typology", "taxonomic_resolution", "taxon","type"))
        vlinevar[, auc := median(auc), by = c("typology", "taxon", "taxonomic_resolution")]
        vlinevar %<>% unique(by =  c("typology", "taxon", "taxonomic_resolution"))
        out_plot <- 
                data |>  
                filter(typology != "null_model_type", taxonomic_resolution == res) |> 
                unique(by = c("taxonomic_resolution", "taxon","typology", "type")) |> 
                ggplot(aes(y=auc, x=typology)) + 
                geom_boxplot(aes(fill = typology), alpha = .8, size = .8) +
                geom_hline(aes(yintercept = auc), data = vlinevar, lty = "dashed", size = 1) + 
                xlab(NULL) + 
                ylab("Area Under Curve") + 
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

fish <- readRDS("01_data/004_results/fish_zeta_2022-06-15.rds")
diat <- readRDS("01_data/004_results/diatom_zeta_2022-06-16.rds")
maph <- readRDS("01_data/004_results/macrophytes_zeta_2022-12-14.rds")
null <- readRDS("01_data/004_results/all_zeta_null_model.rds")

unique(null$taxon)
null[taxon == "dia", taxon := "diatom"]
null[taxon == "fsh", taxon := "fish"]
null[taxon == "mph", taxon := "macrophyte"]

fish$taxon <- "fish"
diat$taxon <- "diatom"
maph$taxon <- "macrophyte"

data <- rbindlist(list(fish, diat, maph, null))
rm(fish, diat, maph, null)
# single curves ---------------------------------------------------------------------

mean_line <-
        filter(data,taxonomic_resolution == 1 & order != 1) |> 
        group_by(typology, order) |> 
        summarise(mean_zeta = mean(zeta_diversity))

data |> 
        filter( taxonomic_resolution == 1) |> 
        filter(order != 1) |> 
        ggplot(aes(x = order, y = zeta_diversity, group = type)) + 
        #geom_point() + 
        geom_line(alpha = 0.4, aes(col = auc)) + 
        facet_wrap(taxon~.) + 
        scale_color_viridis()

rm(mean_line)
# overall plot ----------------------------------------------------------------------

data <- data[! typology %in% c("null_model2_type", "null_model3_type", "null_model4_type")]
data[typology == "null_model1_type", typology := "null_model_type"]

data <- 
        data |> 
        unique(, by = c("taxonomic_resolution", "type", "typology", "taxon")) |>  
        mutate(typology = factor(typology, levels = c("brt", "bgr", "enz", "few", "ife", "null_model_type")))

pal <- wes_palette("Darjeeling1")[c(1,2,3,4,5)]

plot_sp <- plot_fun(1)
plot_ge <- plot_fun(2)
plot_fa <- plot_fun(3)
plot_or <- plot_fun(4)


# save to file ----------------------------------------------------------------------
ggsave(plot = plot_sp, 
       filename = "04_fig/002_results/005_zeta/221212_zeta_species.png")



## There is series of AUCζ for macrophytes that all are two. Whats up with that? 
data |> 
        filter(
                       taxonomic_resolution == 1 & 
                       taxon == "macrophyte") |> 
        arrange(auc)

## Same samples i guess? check 


# get data --------------------------------------------------------------------------

data[, median_auc := median(auc), by = c("typology", "taxon", "taxonomic_resolution")]
summary_table <- 
        data |> 
        group_by(by = typology, taxon, taxonomic_resolution) |> 
        summarise(median_auc := median(auc))
View(filter(summary_table, taxonomic_resolution == 1))

data[taxon == "macrophyte" & typology == "brt" & taxonomic_resolution == 1, ] |> arrange(auc)

# MODELS ----------------------------------------------------------------------------
lm.fish <- lm(auc ~ typology, data = filter(data, taxonomic_resolution == 1 & taxon == "fish"))
lm.diat <- lm(auc ~ typology, data = filter(data, taxonomic_resolution == 1 & taxon == "diatom"))
lm.maph <- lm(auc ~ typology, data = filter(data, taxonomic_resolution == 1 & taxon == "macrophyte"))

summary(lm.fish)
plot(lm.fish)
ggplot(filter(data, taxonomic_resolution == 1 & taxon == "fish"), aes(x = auc, y = typology)) + 
        geom_boxplot() + 
        geom_jitter(width = 0) + 
        geom_label_repel(aes(label = type), max.overlaps = 20)

ggplot(filter(data, taxonomic_resolution == 1 & taxon == "fish"), aes(y = auc, x = typology)) + 
        #geom_boxplot() + 
        geom_point() + 
        geom_text_repel(aes(label = type), max.overlaps = 10, direction = "x", force = 2) + 
        facet_wrap(.~typology, scale = "free")


summary(lm.diat)
plot(lm.diat)
ggplot(filter(data, taxonomic_resolution == 1 & taxon == "diatom"), aes(x = auc, y = typology)) + 
        geom_boxplot() + 
        geom_jitter(width = 0)
ggplot(filter(data, taxonomic_resolution == 1 & taxon == "diatom"), aes(y = auc, x = typology)) + 
        #geom_boxplot() + 
        geom_point() + 
        geom_text_repel(aes(label = type), max.overlaps = 10, direction = "x", force = 2) + 
        facet_wrap(.~typology, scale = "free")



summary(lm.maph)
plot(lm.maph)
ggplot(filter(data, taxonomic_resolution == 1 & taxon == "macrophyte"), aes(x = auc, y = typology)) + 
        geom_boxplot() + 
        geom_jitter(width = 0)
ggplot(filter(data, taxonomic_resolution == 1 & taxon == "macrophyte"), aes(y = auc, x = typology)) + 
        #geom_boxplot() + 
        geom_point() + 
        geom_text_repel(aes(label = type), max.overlaps = 10, direction = "x", force = 2) + 
        facet_wrap(.~typology, scale = "free")
