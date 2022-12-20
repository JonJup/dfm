### --------------------------------- ###
### --- Individual data set check --- ### 
### --------------------------------- ###

# -------------------------------
# date written: 25.02.22
# date last modified: 25.02.22
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: Post Clean Check for diatom data from janne soininen 
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

data <- readRDS("data/diatoms/original_data/finland_jenny/2022-01-20_final_aggregated.rds")

data_name <- "diatom_jjm"

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


data_list <- list(data2, data3, data4)

bc_list <- lapply(data_list, function(x) betapart.core(x[, -c(1:3)]))
bc_list <- lapply(bc_list, function(x) beta.pair(x, index.family = "jaccard"))

### SCBD and LCBD 

cotri <- lapply(data_list, function(x) beta.div(Y = x[, -c(1:3)], method = "jaccard"))
lcbd <- data.frame (lcbd = unlist(purrr::transpose(cotri)$LCBD),
                    id   = data2$gr_sample_id,
                    level = rep(c("species", "genus", "family"), each = nrow(data2))
)

is_outlier <- function(x) {
        return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

lcbd |> 
        group_by(level) %>%
        mutate(outlier = ifelse(is_outlier(lcbd), id, NA)) %>%
        ggplot(aes(y = lcbd, x = level)) + 
        geom_boxplot() +
        geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.02) 

ggsave(paste0("fig/post_clean_check/",data_name,"_lcbd.png"))

out_id <- lcbd$id[which(is_outlier(lcbd$lcbd))]


# analyze --------------------------------------------
nmds.t <- lapply(bc_list, function(x) metaMDS(x$beta.jtu))
nmds.n <- lapply(bc_list, function(x) metaMDS(x$beta.jne))
nmds.j <- lapply(bc_list, function(x) metaMDS(x$beta.jac))

nmds.t <- lapply(nmds.t, function(x) data.table(id = data2$gr_sample_id, brt = data2$brt12, season = data2$season,x = x$points[,1], y = x$points[,2])) 
nmds.n <- lapply(nmds.n, function(x) data.table(id = data2$gr_sample_id, brt = data2$brt12, season = data2$season,x = x$points[,1], y = x$points[,2])) 
nmds.j <- lapply(nmds.j, function(x) data.table(id = data2$gr_sample_id, brt = data2$brt12, season = data2$season,x = x$points[,1], y = x$points[,2])) 
hull.t <- lapply(nmds.t, function(x) slice(group_by(x, brt), chull(x,y)))
hull.n <- lapply(nmds.n, function(x) slice(group_by(x, brt), chull(x,y)))
hull.j <- lapply(nmds.j, function(x) slice(group_by(x, brt), chull(x,y)))

nmds.t %<>% lapply(function(x) x[id %in% out_id, outlier := T])
nmds.n %<>% lapply(function(x) x[id %in% out_id, outlier := T])
nmds.j %<>% lapply(function(x) x[id %in% out_id, outlier := T])


for (i in 1:3){
        if (i == 1) p <- list()
       i.title <- switch(i, "1" = "species", "2" = "genus", "3" = "family")        
       i.x <- nmds.t[[i]]  
       p[[i]] <- 
               i.x |> 
               mutate(outlier = ifelse(!is.na(outlier), id, NA)) |> 
               ggplot(aes(x, y)) + 
               geom_point(aes(col = brt)) + 
               geom_polygon(data = hull.t[[i]], alpha = 0.5, aes ( fill = brt)) + 
               ggtitle(i.title) + 
               geom_point(data = filter(i.x, !is.na(outlier)), aes(col = brt), size = 4)
       
       rm(list = ls()[grepl(pattern = "^i\\.", x = ls())])
       if (i == 3) outt <- cowplot::plot_grid(plotlist = p)
}
for (i in 1:3){
        if (i == 1) p <- list()
       i.title <- switch(i, "1" = "species", "2" = "genus", "3" = "family")        
       i.x <- nmds.n[[i]]  
       p[[i]] <- 
               i.x |> 
               mutate(outlier = ifelse(!is.na(outlier), id, NA)) |> 
               ggplot(aes(x, y)) + 
               geom_point(aes(col = brt)) + 
               geom_polygon(data = hull.n[[i]], alpha = 0.5, aes ( fill = brt)) + 
               ggtitle(i.title) + 
               geom_point(data = filter(i.x, !is.na(outlier)), aes(col = brt), size = 4)
       rm(list = ls()[grepl(pattern = "^i\\.", x = ls())])
       if (i == 3) outn <- cowplot::plot_grid(plotlist = p)
}
for (i in 1:3){
        if (i == 1) p <- list()
       i.title <- switch(i, "1" = "species", "2" = "genus", "3" = "family")        
       i.x <- nmds.j[[i]]  
       p[[i]] <- 
               i.x |> 
               mutate(outlier = ifelse(!is.na(outlier), id, NA)) |> 
               ggplot(aes(x, y)) + 
               geom_point(aes(col = brt)) + 
               geom_polygon(data = hull.j[[i]], alpha = 0.5, aes ( fill = brt)) + 
               ggtitle(i.title) + 
               geom_point(data = filter(i.x, !is.na(outlier)), aes(col = brt), size = 4)
       rm(list = ls()[grepl(pattern = "^i\\.", x = ls())])
       if (i == 3) outj <- cowplot::plot_grid(plotlist = p)
}

outt
ggsave(paste0("fig/post_clean_check/", data_name, "nmds_turnover.png"))
outn 
ggsave(paste0("fig/post_clean_check/", data_name, "nmds_turnover.nestedness.png"))
outj
ggsave(paste0("fig/post_clean_check/", data_name, "nmds_total.png"))

data[gr_sample_id %in% out_id]

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
ggsave(paste0("fig/post_clean_check/", data_name, "_anosim.png"))


                    
                    




# sites <- unique(data, by = "gr_sample_id")
# sites[, lcbd := contributionstobeta$LCBD]
# sites[, lcbd_p := contributionstobeta$p.LCBD]
# sites <- st_as_sf(sites, coords = c("x.coord", "y.coord"), crs = sites$EPSG[1])
# mapview(sites, zcol = "lcbd_p")
# ggplot(sites, aes(x = brt12, y = lcbd)) + geom_point() + geom_violin(alpha = 0.3)
# ggplot(sites, aes(x = richness, y = lcbd)) + geom_point() + geom_smooth()

# 
# ## indicator taxa 
# library(indicspecies)
# 
# indic <- indicspecies::multipatt(x = data2[,-c(1:3)], 
#                         cluster = data2$brt12, 
#                         duleg = TRUE)
# indic$sign |> 
#         filter(p.value < 0.05)

# save data -------------------------------------------