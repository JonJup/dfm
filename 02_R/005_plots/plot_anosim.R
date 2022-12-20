## -- plot anosim results 

# created: 15.06.22
# modified: 16.12.22
# project : Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# purpose : visualize the results of the ANOSIM analysis
# last change: add multiple null models to plot

# setup -----------------------------------------------------------------------------
library(pacman)
p_load(data.table, dplyr, ggplot2, magrittr, wesanderson, stringr)


### log files ----      
sink(file = paste0("02_R/005_plots/log_files/anosim_", Sys.Date(), "_", "log.txt"))
rstudioapi::documentPath()
sessionInfo()
sink(file = NULL)
### --------------

# functions -------------------------------------------------------------------------
plot_out <- function(res, tax = "all"){
        
        if (tax == "all") tax = c("diatoms", "fish", "macrophytes")
        
        #vlinevar <- data[str_detect(typology, c("null_model")) & taxonomic.resolution == res & taxon %in% tax]
        vlinevar <- data[typology == "null_model1_type"& taxonomic.resolution == res & taxon %in% tax]
        vlinevar %<>% unique(by = c("typology", "taxonomic.resolution", "taxon"))  
        
        out_plot <- 
                data |>  
                filter(
                        !typology %in% c("null_model1_type", "null_model2_type", "null_model3_type", "null_model4_type"),
                       taxonomic.resolution == res & taxon %in% tax) |> 
                unique(by = c("typology", "taxonomic.resolution", "taxon")) |> 
                ggplot(aes(y=statistic, x=typology)) + 
                geom_col(aes(fill = typology), alpha = .8) +
                geom_hline(aes(yintercept = statistic), data = vlinevar, lty = "dashed", size = 1) + 
                xlab(NULL) + 
                ylab("ANOSIM R") + 
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

fish <- readRDS("01_data/004_results/fish_anosim_2022-06-15.rds")
diat <- readRDS("01_data/004_results/diatoms_anosim_2022-06-15.rds")
maph <- readRDS("01_data/004_results/macrophytes_anosim_2022-12-12.rds")
null <- readRDS("01_data/004_results/all_anosim_null_multiple_nulls.rds")

null[taxon == "fishes", taxon := "fish"]

data <- rbindlist(list(fish, diat, maph, null))

# inspect data ----------------------------------------------------------------------

# data[taxon == "diatoms"] |> 
#         mutate(taxonomic.resolution = factor(taxonomic.resolution, levels = c("species", "genus", "family", "order"))) |> 
#         ggplot(aes(x = statistic, y = typology, col = taxonomic.resolution)) + 
#         geom_point(size = 5) 
#         #facet_wrap(.~taxonomic.resolution)
# data[taxon == "fish"] |> 
#         mutate(taxonomic.resolution = factor(taxonomic.resolution, levels = c("species", "genus", "family", "order"))) |> 
#         ggplot(aes(x = statistic, y = typology)) + 
#         geom_point() + 
#         facet_wrap(.~taxonomic.resolution)
# data[taxon == "macrophytes"] |> 
#         mutate(taxonomic.resolution = factor(taxonomic.resolution, levels = c("species", "genus", "family", "order"))) |> 
#         ggplot(aes(x = statistic, y = typology)) + 
#         geom_point() + 
#         facet_wrap(.~taxonomic.resolution)


# plot diatoms ----------------------------------------------------------------------
pal <- wes_palette("Darjeeling1")[c(1,2,3,4,5)]

plot_out(res = "species"); ggsave(filename = paste0("04_fig/002_results/001_anosim/anosim_all_species", Sys.Date(), ".tiff"))
plot_out(res = "genus")  ; ggsave(filename = paste0("04_fig/002_results/001_anosim/anosim_all_genus", Sys.Date(), ".tiff"))
plot_out(res = "family") ; ggsave(filename = paste0("04_fig/002_results/001_anosim/anosim_all_family", Sys.Date(), ".tiff"))


data[taxonomic.resolution=="species"]



# old -------------------------------------------------------------------------------


# vlinevar <- data[typology == "null_model_type" & taxonomic.resolution == "species"]
# vlinevar %<>% unique(by = c("typology", "taxonomic.resolution", "taxon"))
# 
# out_plot <- 
#         data |>  
#         filter(typology != "null_model_type", taxonomic.resolution == "species") |> 
#         unique(by = c("typology", "taxonomic.resolution", "taxon")) |> 
#         ggplot(aes(y=statistic, x=typology)) + 
#         geom_col(aes(fill = typology), alpha = .8) +
#         geom_hline(aes(yintercept = statistic), data = vlinevar, lty = "dashed", size = 1) + 
#         xlab(NULL) + 
#         ylab("ANOSIM R") + 
#         facet_grid(.~taxon) + 
#         scale_color_manual(values = pal, guide = "none") +
#         scale_fill_manual(values = pal) + 
#         theme(panel.background = element_rect(fill = "white"),
#               panel.grid.major.x = element_line(colour = "grey89"),
#               axis.ticks = element_blank(),
#               axis.text.x = element_blank(), #element_text(color = pal), # size = 14, lineheight = .9
#               legend.key = element_blank(),
#               panel.border = element_blank(),
#               panel.spacing.y = unit(0,"line")) 
# 
# # diatoms at species level 
# vlinevar <- data[typology == "null_model_type" & taxonomic.resolution == "genus" & taxon == "diatoms"]
# vlinevar %<>% unique(by = c("typology", "taxonomic.resolution", "taxon"))
# dia_gen_plot <- 
#         data |>  
#         filter(typology != "null_model_type", taxonomic.resolution == "genus" & taxon == "diatoms") |> 
#         unique(by = c("typology", "taxonomic.resolution")) |> 
#         ggplot(aes(y=statistic, x=typology)) + 
#         geom_col(aes(fill = typology), alpha = .8) +
#         geom_hline(aes(yintercept = statistic), data = vlinevar, lty = "dashed", size = 1) + 
#         xlab(NULL) + 
#         ylab("ANOSIM R") + 
#         scale_color_manual(values = pal, guide = "none") +
#         scale_fill_manual(values = pal) + 
#         theme(panel.background = element_rect(fill = "white"),
#               panel.grid.major.x = element_line(colour = "grey89"),
#               axis.ticks = element_blank(),
#               axis.text.x = element_blank(), #element_text(color = pal), # size = 14, lineheight = .9
#               legend.key = element_blank(),
#               panel.border = element_blank(),
#               panel.spacing.y = unit(0,"line")) 


# 
# 
# # - table for paper 
# tfp <- 
#         data[taxon == "diatoms", c("statistic", "taxonomic.resolution", "typology")] |> 
#         tidyr::pivot_wider(id_cols = typology, 
#                            names_from = taxonomic.resolution, 
#                            values_from = statistic)
# fwrite(tfp, "anosim_diatom.csv")
# tfp <- 
#         data[taxon == "fish", c("statistic", "taxonomic.resolution", "typology")] |> 
#         tidyr::pivot_wider(id_cols = typology, 
#                            names_from = taxonomic.resolution, 
#                            values_from = statistic)
# fwrite(tfp, "anosim_fish.csv")
# tfp <- 
#         data[taxon == "macrophytes", c("statistic", "taxonomic.resolution", "typology")] |> 
#         tidyr::pivot_wider(id_cols = typology, 
#                            names_from = taxonomic.resolution, 
#                            values_from = statistic)
# fwrite(tfp, "data/results/tables//anosim_macrophytes.csv")