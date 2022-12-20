### --------------------------------- ###
### --- Individual data set check --- ### 
### --------------------------------- ###

# -------------------------------
# date written: 03.05.22
# date last modified: 03.05.22
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: Post Clean Check for macrophytes data from Czech CHMI
# -------------------------------

# setup -----------------------------------------------
library(adespatial)
library(betapart)
library(cowplot)
library(data.table)
library(dplyr)
library(ggplot2)
library(magrittr)
library(mapview)
library(plotly)
library(sf)
library(tidyr)
library(vegan)

# load data -------------------------------------------
data <- readRDS("data/macrophytes/combined_data/01_2022-05-03_combined_data_aggregated.rds")
unique(data$data.set) |> sort()
data <- data[data.set == "chezch_chmi_macrophytes"]

data_name <- "macrophytes_czech"

## drop sites with only one observation
drop_id <- c(
        "site_00155_date_00168_czech_chmi_macrophytes",
        "site_00256_date_00285_czech_chmi_macrophytes",
        "site_00345_date_00354_czech_chmi_macrophytes",
        "site_00414_date_00186_czech_chmi_macrophytes",
        "site_00444_date_00418_czech_chmi_macrophytes",
        "site_00445_date_00418_czech_chmi_macrophytes",
        "site_00453_date_00109_czech_chmi_macrophytes"
)

data <- data[!gr_sample_id %in% drop_id]

rowSums(data2[,-c(1:2)])
rowSums(data3[,-c(1:2)])
min(rowSums(data4[,-c(1:2)]))

# prepare data ----------------------------------------
data2 <- data |> 
        filter(!is.na(species)) |> 
        mutate(abundance = 1) |>
        pivot_wider(id_cols = c("gr_sample_id", "brt12"), names_from = species, values_from = abundance, values_fill = 0)

data3 <- data |> 
        filter(!is.na(genus)) |> 
        mutate(abundance = 1) |>
        unique(by = c("gr_sample_id", "genus")) |> 
        pivot_wider(id_cols = c("gr_sample_id", "brt12"), names_from = genus, values_from = abundance, values_fill = 0)

data4 <- data |> 
        filter(!is.na(family)) |> 
        mutate(abundance = 1) |>
        unique(by = c("gr_sample_id", "family")) |> 
        pivot_wider(id_cols = c("gr_sample_id", "brt12"), names_from = family, values_from = abundance, values_fill = 0)

richness2 <- data.frame(id = data2$gr_sample_id, 
                        richness = data2 |> select(!c("gr_sample_id", "brt12")) |> 
                                rowSums()  )
hist(richness2$richness)
richness2 |> filter(richness < 2)


data_list <- list(data2, data3, data4)

vegdist(data4[,-c(1:2)])

bc_list <- lapply(data_list, function(x) betapart.core(x[, -c(1:2)]))
bc_list <- lapply(bc_list, function(x) beta.pair(x, index.family = "jaccard"))

# analyze --------------------------------------------
nmds.t_a <- lapply(bc_list, function(x) metaMDS(x$beta.jtu))
nmds.n_a <- lapply(bc_list, function(x) metaMDS(x$beta.jne))
nmds.j_a <- lapply(bc_list, function(x) metaMDS(x$beta.jac))

nmds.t <- lapply(nmds.t_a, function(x) data.table(id = data2$gr_sample_id, brt = data2$brt12,x = x$points[,1], y = x$points[,2])) 
nmds.n <- lapply(nmds.n_a, function(x) data.table(id = data2$gr_sample_id, brt = data2$brt12,x = x$points[,1], y = x$points[,2])) 
nmds.j <- lapply(nmds.j_a, function(x) data.table(id = data2$gr_sample_id, brt = data2$brt12,x = x$points[,1], y = x$points[,2])) 
hull.t <- lapply(nmds.t, function(x) slice(group_by(x, brt), chull(x,y)))
hull.n <- lapply(nmds.n, function(x) slice(group_by(x, brt), chull(x,y)))
hull.j <- lapply(nmds.j, function(x) slice(group_by(x, brt), chull(x,y)))

for (i in 1:3){
        if (i == 1){
                p1 <- list()
                p2 <- list()
        } 
        i.title <- switch(i, "1" = "species", "2" = "genus", "3" = "family")        
        i.x <- nmds.t[[i]]  
        p1[[i]] <- 
                i.x |> 
                #mutate(outlier = ifelse(!is.na(outlier), id, NA)) |> 
                ggplot(aes(x, y)) + 
                geom_point(aes(col = brt)) + 
                geom_polygon(data = hull.t[[i]], alpha = 0.5, aes ( fill = brt)) + 
                ggtitle(i.title) + 
                ##geom_point(data = filter(i.x, !is.na(outlier)), aes(col = brt), size = 4) + 
                guides(colour = guide_legend(ncol = 2))
        
        p2[[i]] <- 
                i.x |> 
                #mutate(outlier = ifelse(!is.na(outlier), id, NA)) |> 
                ggplot(aes(x, y)) + 
                geom_point(aes(col = brt)) + 
                ggtitle(i.title) + 
                guides(colour = guide_legend(ncol = 2))
        
        
        ## extract legend in first round 
        if (i == 1){
                p1_legend <- cowplot::get_legend(p1[[1]])
                p2_legend <- cowplot::get_legend(p2[[1]])
        }
        
        p1[[i]] <- p1[[i]] + theme(legend.position = "none")
        p2[[i]] <- p2[[i]] + theme(legend.position = "none")
        
        rm(list = ls()[grepl(pattern = "^i\\.", x = ls())])
        if (i == 3) {
                p1[[4]] <- p1_legend
                outt  <- cowplot::plot_grid(plotlist = p1)
        }
        if (i == 3) {
                p2[[4]] <- p2_legend
                outt2 <- cowplot::plot_grid(plotlist = p2)
        }
}
for (i in 1:3){
        if (i == 1){
                p1 <- list()
                p2 <- list()
        } 
        i.title <- switch(i, "1" = "species", "2" = "genus", "3" = "family")        
        i.x <- nmds.n[[i]]  
        p1[[i]] <- 
                i.x |> 
                #mutate(outlier = ifelse(!is.na(outlier), id, NA)) |> 
                ggplot(aes(x, y)) + 
                geom_point(aes(col = brt)) + 
                geom_polygon(data = hull.n[[i]], alpha = 0.5, aes ( fill = brt)) + 
                ggtitle(i.title) + 
                #geom_point(data = filter(i.x, !is.na(outlier)), aes(col = brt), size = 4) + 
                guides(colour = guide_legend(ncol = 2))
        
        p2[[i]] <- 
                i.x |> 
                #mutate(outlier = ifelse(!is.na(outlier), id, NA)) |> 
                ggplot(aes(x, y)) + 
                geom_point(aes(col = brt)) + 
                ggtitle(i.title) + 
                guides(colour = guide_legend(ncol = 2))
        
        
        ## extract legend in first round 
        if (i == 1){
                p1_legend <- cowplot::get_legend(p1[[1]])
                p2_legend <- cowplot::get_legend(p2[[1]])
        }
        
        p1[[i]] <- p1[[i]] + theme(legend.position = "none")
        p2[[i]] <- p2[[i]] + theme(legend.position = "none")
        
        rm(list = ls()[grepl(pattern = "^i\\.", x = ls())])
        if (i == 3) {
                p1[[4]] <- p1_legend
                outn  <- cowplot::plot_grid(plotlist = p1)
        }
        if (i == 3) {
                p2[[4]] <- p2_legend
                outn2 <- cowplot::plot_grid(plotlist = p2)
        }
}
for (i in 1:3){
        if (i == 1){
                p1 <- list()
                p2 <- list()
        } 
        i.title <- switch(i, "1" = "species", "2" = "genus", "3" = "family")        
        i.x <- nmds.j[[i]]  
        p1[[i]] <- 
                i.x |> 
                #mutate(outlier = ifelse(!is.na(outlier), id, NA)) |> 
                ggplot(aes(x, y)) + 
                geom_point(aes(col = brt)) + 
                geom_polygon(data = hull.j[[i]], alpha = 0.5, aes ( fill = brt)) + 
                ggtitle(i.title) + 
                #geom_point(data = filter(i.x, !is.na(outlier)), aes(col = brt), size = 4) + 
                guides(colour = guide_legend(ncol = 2))
        
        p2[[i]] <- 
                i.x |> 
                #mutate(outlier = ifelse(!is.na(outlier), id, NA)) |> 
                ggplot(aes(x, y)) + 
                geom_point(aes(col = brt)) + 
                ggtitle(i.title) + 
                guides(colour = guide_legend(ncol = 2))
        
        
        ## extract legend in first round 
        if (i == 1){
                p1_legend <- cowplot::get_legend(p1[[1]])
                p2_legend <- cowplot::get_legend(p2[[1]])
        }
        
        p1[[i]] <- p1[[i]] + theme(legend.position = "none")
        p2[[i]] <- p2[[i]] + theme(legend.position = "none")
        
        rm(list = ls()[grepl(pattern = "^i\\.", x = ls())])
        if (i == 3) {
                p1[[4]] <- p1_legend
                outj  <- cowplot::plot_grid(plotlist = p1)
        }
        if (i == 3) {
                p2[[4]] <- p2_legend
                outj2 <- cowplot::plot_grid(plotlist = p2)
        }
}
beepr::beep()

#nmds.j[[1]][x < -0.6]

ggsave(paste0("fig/post_clean_check/", data_name, "_nmds_turnover.png"), plot = outt, height = 17, width = 17, units = "cm")
ggsave(paste0("fig/post_clean_check/", data_name, "_nmds_nestedness.png"), plot = outn, height = 17, width = 17, units = "cm")
ggsave(paste0("fig/post_clean_check/", data_name, "_nmds_total.png"), plot = outj, height = 17, width = 17, units = "cm")
ggsave(paste0("fig/post_clean_check/", data_name, "_nmds_turnover2.png"), plot = outt2, height = 17, width = 17, units = "cm")
ggsave(paste0("fig/post_clean_check/", data_name, "_nmds_nestedness2.png"), plot = outn2, height = 17, width = 17, units = "cm")
ggsave(paste0("fig/post_clean_check/", data_name, "_nmds_total2.png"), plot = outj2, height = 17, width = 17, units = "cm")


anosim.spe <- lapply(bc_list[[1]], function(x) anosim(x, grouping = data2$brt12))
anosim.gen <- lapply(bc_list[[2]], function(x) anosim(x, grouping = data2$brt12))
anosim.fam <- lapply(bc_list[[3]], function(x) anosim(x, grouping = data2$brt12))

anosim_data <- data.frame(
        stat = c(
                unlist(purrr::transpose(anosim.spe)$statistic),
                unlist(purrr::transpose(anosim.gen)$statistic),
                unlist(purrr::transpose(anosim.fam)$statistic)
        ),
        metric = rep(c("turnover", "nestedness", "jaccard"), times = 3),
        taxonomic_resolution = rep(c("species", "genus", "family"), each = 3)
        
)
ggplot(anosim_data, aes(x = metric, y = stat, fill = taxonomic_resolution)) +
        geom_jitter(shape = 21, size = 4, width = 0.1, height = 0)
ggsave(paste0("fig/post_clean_check/", data_name, "_anosim.png"), height = 17, width = 17, units = "cm")





