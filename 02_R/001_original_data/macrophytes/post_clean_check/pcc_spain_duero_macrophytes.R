### --------------------------------- ###
### --- Individual data set check --- ### 
### --------------------------------- ###

# -------------------------------
# date written: 04.03.22
# date last modified: 04.03.22
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: Post Clean Check for macrophytes data from Germany Saxony
# Notes: 
# -------------------------------

# setup -----------------------------------------------
library(adespatial)
library(tidyr)
library(dplyr)
library(vegan)
library(betapart)
library(ggplot2)
library(plotly)
library(sf)
library(mapview)
library(cowplot)
library(data.table)
library(magrittr)

# load data -------------------------------------------
data <- readRDS("data/macrophytes/combined_data/01_2022-03-04_combined_data_aggregated.rds")
data <- rbindlist(data)
unique(data$data.set) |> sort()
data <- data[data.set == "spain_duero_macrophytes"]

data_name <- "macrophyte_spain_duero"

## drop sites with only one observation
drop_id <- c(
        "site_00006_date_00002_spain_duero_macrophytes",
        "site_00059_date_00097_spain_duero_macrophytes",
        "site_00067_date_00065_spain_duero_macrophytes",
        "site_00107_date_00156_spain_duero_macrophytes",
        "site_00115_date_00038_spain_duero_macrophytes",
        "site_00128_date_00163_spain_duero_macrophytes",
        "site_00132_date_00163_spain_duero_macrophytes",
        "site_00133_date_00047_spain_duero_macrophytes",
        "site_00186_date_00235_spain_duero_macrophytes",
        "site_00219_date_00061_spain_duero_macrophytes",
        "site_00234_date_00074_spain_duero_macrophytes",
        "site_00244_date_00090_spain_duero_macrophytes",
        "site_00260_date_00008_spain_duero_macrophytes",
        "site_00261_date_00235_spain_duero_macrophytes",
        "site_00262_date_00008_spain_duero_macrophytes",
        "site_00264_date_00097_spain_duero_macrophytes",
        "site_00266_date_00097_spain_duero_macrophytes",
        "site_00278_date_00102_spain_duero_macrophytes",
        "site_00292_date_00102_spain_duero_macrophytes",
        "site_00296_date_00053_spain_duero_macrophytes",
        "site_00302_date_00043_spain_duero_macrophytes",
        "site_00303_date_00235_spain_duero_macrophytes",
        "site_00305_date_00092_spain_duero_macrophytes",
        "site_00314_date_00090_spain_duero_macrophytes",
        "site_00339_date_00092_spain_duero_macrophytes",
        "site_00340_date_00002_spain_duero_macrophytes",
        "site_00394_date_00083_spain_duero_macrophytes",
        "site_00412_date_00092_spain_duero_macrophytes",
        "site_00418_date_00047_spain_duero_macrophytes",
        "site_00033_date_00058_spain_duero_macrophytes",
        "site_00097_date_00144_spain_duero_macrophytes",
        "site_00187_date_00237_spain_duero_macrophytes",
        "site_00007_date_00505_spain_duero_macrophytes",
        "site_00472_date_00621_spain_duero_macrophytes",
        "site_00139_date_00624_spain_duero_macrophytes",
        "site_00477_date_00631_spain_duero_macrophytes",
        "site_00202_date_00675_spain_duero_macrophytes",
        "site_00208_date_00681_spain_duero_macrophytes",
        "site_00342_date_00806_spain_duero_macrophytes",
        "site_00535_date_00818_spain_duero_macrophytes",
        "site_00414_date_00862_spain_duero_macrophytes",
        "site_00017_date_00027_spain_duero_macrophytes",
        "site_00007_date_00046_spain_duero_macrophytes",
        "site_00068_date_00109_spain_duero_macrophytes",
        "site_00094_date_00141_spain_duero_macrophytes",
        "site_00095_date_00072_spain_duero_macrophytes",
        "site_00102_date_00150_spain_duero_macrophytes",
        "site_00129_date_00178_spain_duero_macrophytes",
        "site_00144_date_00194_spain_duero_macrophytes",
        "site_00155_date_00207_spain_duero_macrophytes",
        "site_00179_date_00230_spain_duero_macrophytes",
        "site_00187_date_00236_spain_duero_macrophytes",
        "site_00193_date_00241_spain_duero_macrophytes",
        "site_00194_date_00243_spain_duero_macrophytes",
        "site_00202_date_00255_spain_duero_macrophytes",
        "site_00253_date_00319_spain_duero_macrophytes",
        "site_00267_date_00339_spain_duero_macrophytes",
        "site_00268_date_00253_spain_duero_macrophytes",
        "site_00307_date_00093_spain_duero_macrophytes",
        "site_00310_date_00390_spain_duero_macrophytes",
        "site_00329_date_00141_spain_duero_macrophytes",
        "site_00139_date_00190_spain_duero_macrophytes",
        "site_00194_date_00244_spain_duero_macrophytes",
        "site_00023_date_00501_spain_duero_macrophytes",
        "site_00436_date_00533_spain_duero_macrophytes",
        "site_00442_date_00550_spain_duero_macrophytes",
        "site_00068_date_00552_spain_duero_macrophytes",
        "site_00448_date_00559_spain_duero_macrophytes",
        "site_00449_date_00560_spain_duero_macrophytes",
        "site_00095_date_00590_spain_duero_macrophytes",
        "site_00109_date_00600_spain_duero_macrophytes",
        "site_00466_date_00605_spain_duero_macrophytes",
        "site_00117_date_00609_spain_duero_macrophytes",
        "site_00144_date_00626_spain_duero_macrophytes",
        "site_00267_date_00729_spain_duero_macrophytes",
        "site_00520_date_00760_spain_duero_macrophytes",
        "site_00548_date_00586_spain_duero_macrophytes", 
        "site_00375_date_00053_spain_duero_macrophytes", 
        "site_00336_date_00411_spain_duero_macrophytes", 
        "site_00429_date_00520_spain_duero_macrophytes"
        
)

data <- data[!gr_sample_id %in% drop_id]

# prepare data ----------------------------------------
data2 <- data |> 
        filter(!is.na(species)) |> 
        mutate(abundance = 1) |>
        pivot_wider(id_cols = c("gr_sample_id", "brt12", "season"), names_from = species, values_from = abundance, values_fill = 0)

data3 <- data |> 
        filter(!is.na(genus)) |> 
        mutate(abundance = 1) |>
        unique(by = c("gr_sample_id", "genus")) |> 
        pivot_wider(id_cols = c("gr_sample_id", "brt12", "season"), names_from = genus, values_from = abundance, values_fill = 0)

data4 <- data |> 
        filter(!is.na(family)) |> 
        mutate(abundance = 1) |>
        unique(by = c("gr_sample_id", "family")) |> 
        pivot_wider(id_cols = c("gr_sample_id", "brt12", "season"), names_from = family, values_from = abundance, values_fill = 0)

richness2 <- data.frame(id = data2$gr_sample_id, 
                        richness = data2 |> select(!c("gr_sample_id", "brt12", "season")) |> 
                                rowSums()  )
hist(richness2$richness)
richness2 |> filter(richness < 2)


data_list <- list(data2, data3, data4)

bc_list <- lapply(data_list, function(x) betapart.core(x[, -c(1:3)]))
bc_list <- lapply(bc_list, function(x) beta.pair(x, index.family = "jaccard"))

# ### SCBD and LCBD 
# 
# cotri <- lapply(data_list, function(x) beta.div(Y = x[, -c(1:3)], method = "jaccard"))
# lcbd <- data.frame (lcbd = unlist(purrr::transpose(cotri)$LCBD),
#                     id   = data2$gr_sample_id,
#                     level = rep(c("species", "genus", "family"), each = nrow(data2))
# )
# 
# is_outlier <- function(x) {
#         return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
# }
# 
# lcbd |> 
#         group_by(level) %>%
#         mutate(outlier = ifelse(is_outlier(lcbd), id, NA)) %>%
#         ggplot(aes(y = lcbd, x = level)) + 
#         geom_boxplot() +
#         geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.02) 
# 
# ggsave(paste0("fig/post_clean_check/",data_name,"_lcbd.png"), height = 17, width = 17, units = "cm")
# 
# out_id <- unique(lcbd$id[which(is_outlier(lcbd$lcbd))])

# analyze --------------------------------------------
nmds.t_a <- lapply(bc_list, function(x) metaMDS(x$beta.jtu))
nmds.n_a <- lapply(bc_list, function(x) metaMDS(x$beta.jne))
nmds.j_a <- lapply(bc_list, function(x) metaMDS(x$beta.jac))

nmds.t <- lapply(nmds.t_a, function(x) data.table(id = data2$gr_sample_id, brt = data2$brt12, season = data2$season,x = x$points[,1], y = x$points[,2])) 
nmds.n <- lapply(nmds.n_a, function(x) data.table(id = data2$gr_sample_id, brt = data2$brt12, season = data2$season,x = x$points[,1], y = x$points[,2])) 
nmds.j <- lapply(nmds.j_a, function(x) data.table(id = data2$gr_sample_id, brt = data2$brt12, season = data2$season,x = x$points[,1], y = x$points[,2])) 
hull.t <- lapply(nmds.t, function(x) slice(group_by(x, brt), chull(x,y)))
hull.n <- lapply(nmds.n, function(x) slice(group_by(x, brt), chull(x,y)))
hull.j <- lapply(nmds.j, function(x) slice(group_by(x, brt), chull(x,y)))

# nmds.t %<>% lapply(function(x) x[id %in% out_id, outlier := T])
# nmds.n %<>% lapply(function(x) x[id %in% out_id, outlier := T])
# nmds.j %<>% lapply(function(x) x[id %in% out_id, outlier := T])


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

nmds.j[[2]][x < -0.5]

ggsave(paste0("fig/post_clean_check/", data_name, "_nmds_turnover.png"), plot = outt, height = 17, width = 17, units = "cm")
ggsave(paste0("fig/post_clean_check/", data_name, "_nmds_nestedness.png"), plot = outn, height = 17, width = 17, units = "cm")
ggsave(paste0("fig/post_clean_check/", data_name, "_nmds_total.png"), plot = outj, height = 17, width = 17, units = "cm")
ggsave(paste0("fig/post_clean_check/", data_name, "_nmds_turnover2.png"), plot = outt2, height = 17, width = 17, units = "cm")
ggsave(paste0("fig/post_clean_check/", data_name, "_nmds_nestedness2.png"), plot = outn2, height = 17, width = 17, units = "cm")
ggsave(paste0("fig/post_clean_check/", data_name, "_nmds_total2.png"), plot = outj2, height = 17, width = 17, units = "cm")

nmds.j[[1]][x > .5]

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
        geom_point(shape = 21, size = 4)
ggsave(paste0("fig/post_clean_check/", data_name, "_anosim.png"), height = 17, width = 17, units = "cm")





