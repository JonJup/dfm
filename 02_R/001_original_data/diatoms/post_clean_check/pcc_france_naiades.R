### --------------------------------- ###
### --- Individual data set check --- ### 
### --------------------------------- ###

# -------------------------------
# date written: 01.03.22
# date last modified: 01.03.22
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: Post Clean Check for diatom data from Naiades
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

data <- readRDS("data/diatoms/combined_data/01_2022-03-02_combined_data_aggregated.rds")
data <- rbindlist(data)
unique(data$data.set)
data <- data[data.set == "france_naiades_diatoms"]

data_name <- "diatom_france_naiades"

data <- data[!gr_sample_id %in% c(
        "site_00367_date_00200_france_naiades_diatoms", "site_02656_date_00506_france_naiades_diatoms",
        "site_02659_date_00345_france_naiades_diatoms", "site_07256_date_01614_france_naiades_diatoms",
        "site_07340_date_01668_france_naiades_diatoms", "site_07347_date_01663_france_naiades_diatoms",
        "site_08261_date_01025_france_naiades_diatoms", "site_00187_date_00050_france_naiades_diatoms",
        "site_00189_date_00057_france_naiades_diatoms", "site_00191_date_00057_france_naiades_diatoms",
        "site_00205_date_00057_france_naiades_diatoms", "site_00227_date_00120_france_naiades_diatoms",
        "site_00232_date_00120_france_naiades_diatoms", "site_00233_date_00120_france_naiades_diatoms",
        "site_00298_date_00120_france_naiades_diatoms", "site_00300_date_00120_france_naiades_diatoms",
        "site_00507_date_00260_france_naiades_diatoms", "site_01472_date_00322_france_naiades_diatoms",
        "site_02047_date_00686_france_naiades_diatoms", "site_02048_date_00883_france_naiades_diatoms",
        "site_02167_date_00451_france_naiades_diatoms", "site_02593_date_00503_france_naiades_diatoms",
        "site_02649_date_00022_france_naiades_diatoms", "site_02659_date_00182_france_naiades_diatoms",
        "site_02671_date_00028_france_naiades_diatoms", "site_02688_date_00253_france_naiades_diatoms",
        "site_02807_date_00710_france_naiades_diatoms", "site_02810_date_00107_france_naiades_diatoms",
        "site_02813_date_00040_france_naiades_diatoms", "site_02847_date_00010_france_naiades_diatoms",
        "site_02940_date_00917_france_naiades_diatoms", "site_02942_date_00149_france_naiades_diatoms",
        "site_02953_date_00317_france_naiades_diatoms", "site_02955_date_00917_france_naiades_diatoms",
        "site_02971_date_00267_france_naiades_diatoms", "site_03010_date_00018_france_naiades_diatoms",
        "site_03018_date_00030_france_naiades_diatoms", "site_03038_date_00342_france_naiades_diatoms",
        "site_03255_date_00287_france_naiades_diatoms", "site_04245_date_00329_france_naiades_diatoms",
        "site_04430_date_00073_france_naiades_diatoms", "site_04583_date_00022_france_naiades_diatoms",
        "site_04832_date_00012_france_naiades_diatoms", "site_04874_date_00012_france_naiades_diatoms",
        "site_04888_date_00612_france_naiades_diatoms", "site_06197_date_00339_france_naiades_diatoms",
        "site_06228_date_00321_france_naiades_diatoms", "site_06270_date_00012_france_naiades_diatoms",
        "site_06650_date_00458_france_naiades_diatoms", "site_06887_date_00213_france_naiades_diatoms",
        "site_07358_date_00038_france_naiades_diatoms", "site_07834_date_00034_france_naiades_diatoms",
        "site_07859_date_00043_france_naiades_diatoms", "site_07905_date_00528_france_naiades_diatoms",
        "site_00227_date_00121_france_naiades_diatoms", "site_00299_date_00069_france_naiades_diatoms",
        "site_01971_date_00839_france_naiades_diatoms", "site_02714_date_00655_france_naiades_diatoms",
        "site_02764_date_01022_france_naiades_diatoms", "site_02918_date_01015_france_naiades_diatoms",
        "site_04942_date_00922_france_naiades_diatoms", "site_05302_date_01241_france_naiades_diatoms",
        "site_05638_date_00695_france_naiades_diatoms", "site_05870_date_00417_france_naiades_diatoms",
        "site_06032_date_00922_france_naiades_diatoms", "site_06072_date_00751_france_naiades_diatoms",
        "site_06163_date_00864_france_naiades_diatoms", "site_06191_date_01104_france_naiades_diatoms",
        "site_06762_date_01003_france_naiades_diatoms", "site_07076_date_00238_france_naiades_diatoms",
        "site_07111_date_00383_france_naiades_diatoms", "site_08401_date_00925_france_naiades_diatoms"
)]

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
richness2 |> filter(richness < 10|richness>50) |> pull(id)


data_list <- list(data2, data3, data4)

bc_list <- lapply(data_list, function(x) betapart.core(x[, -c(1:3)]))
bc_list <- lapply(bc_list, function(x) beta.pair(x, index.family = "jaccard"))

### SCBD and LCBD 

## not computed 

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


ggsave(paste0("fig/post_clean_check/", data_name, "_nmds_turnover.png"), plot = outt, height = 17, width = 17, units = "cm")
ggsave(paste0("fig/post_clean_check/", data_name, "_nmds_nestedness.png"), plot = outn, height = 17, width = 17, units = "cm")
ggsave(paste0("fig/post_clean_check/", data_name, "_nmds_total.png"), plot = outj, height = 17, width = 17, units = "cm")
ggsave(paste0("fig/post_clean_check/", data_name, "_nmds_turnover2.png"), plot = outt2, height = 17, width = 17, units = "cm")
ggsave(paste0("fig/post_clean_check/", data_name, "_nmds_nestedness2.png"), plot = outn2, height = 17, width = 17, units = "cm")
ggsave(paste0("fig/post_clean_check/", data_name, "_nmds_total2.png"), plot = outj2, height = 17, width = 17, units = "cm")

#data[gr_sample_id %in% out_id]

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





