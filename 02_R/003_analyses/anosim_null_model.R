## -- ANOSIM for null model 

library(data.table)
library(vegan)
library(rstudioapi)

diat <- readRDS("data/diatoms/combined_data/03_2022-12-12_no_rare_taxa.rds")
fish <- readRDS("data/fish/combined_data/03_2022-12-12_no_rare_taxa.rds")
macr <- readRDS("data/macrophytes/combined_data/03_2022-12-12_no_rare_taxa.rds")

sink(file = paste0("R/analyses/log_files/anosim","_null_model_", Sys.Date(), "_", "log.txt"))
documentPath()
sessionInfo()
sink(file = NULL)


data <- list(diat, fish, macr)

for (i in 1:3){
        i.tabl <- table(data[[i]][[1]]$null_model1_type) 
        if (any(i.tabl < 20)){ 
                drop_id <- names(i.tabl)[which(i.tabl < 20)]
                data[[i]]    <- lapply(data[[i]], function(x) x[!null_model1_type %in% drop_id])
                rm(drop_id)
        }
        i.tabl <- table(data[[i]][[1]]$null_model2_type) 
        if (any(i.tabl < 20)){ 
                drop_id <- names(i.tabl)[which(i.tabl < 20)]
                data[[i]]    <- lapply(data[[i]], function(x) x[!null_model2_type %in% drop_id])
                rm(drop_id)
        }
        i.tabl <- table(data[[i]][[1]]$null_model3_type) 
        if (any(i.tabl < 20)){ 
                drop_id <- names(i.tabl)[which(i.tabl < 20)]
                data[[i]]    <- lapply(data[[i]], function(x) x[!null_model3_type %in% drop_id])
                rm(drop_id)
        }
        i.tabl <- table(data[[i]][[1]]$null_model4_type) 
        if (any(i.tabl < 20)){ 
                drop_id <- names(i.tabl)[which(i.tabl < 20)]
                data[[i]]    <- lapply(data[[i]], function(x) x[!null_model4_type %in% drop_id])
                rm(drop_id)
        }
}


# - anosim.diat1 is a list. The elements are the ANOSIMs for all four taxonomic groups (in
# - the order species, genus, family, order) for the first null model. anosim.diat2
# - contains the same data but for the second null model

anosim.diat1 <- lapply(data[[1]], function(x) anosim(x[,-c(1:10)], distance = "jaccard", grouping = x$null_model1_type, parallel = 10))
anosim.diat2 <- lapply(data[[1]], function(x) anosim(x[,-c(1:10)], distance = "jaccard", grouping = x$null_model2_type, parallel = 10))
anosim.diat3 <- lapply(data[[1]], function(x) anosim(x[,-c(1:10)], distance = "jaccard", grouping = x$null_model3_type, parallel = 10))
anosim.diat4 <- lapply(data[[1]], function(x) anosim(x[,-c(1:10)], distance = "jaccard", grouping = x$null_model4_type, parallel = 10))
anosim.fish1 <- lapply(data[[2]], function(x) anosim(x[,-c(1:10)], distance = "jaccard", grouping = x$null_model1_type, parallel = 10))
anosim.fish2 <- lapply(data[[2]], function(x) anosim(x[,-c(1:10)], distance = "jaccard", grouping = x$null_model2_type, parallel = 10))
anosim.fish3 <- lapply(data[[2]], function(x) anosim(x[,-c(1:10)], distance = "jaccard", grouping = x$null_model3_type, parallel = 10))
anosim.fish4 <- lapply(data[[2]], function(x) anosim(x[,-c(1:10)], distance = "jaccard", grouping = x$null_model4_type, parallel = 10))


# - The macrophyte data returned an error in this setup and will be handeled seperately below. 

anosim.fun1 <- function(x, typology, taxon){
        y <- purrr::transpose(x)
        z <- data.table(
                statistic = unlist(y$statistic),
                p.value   = unlist(y$signif),
                taxonomic.resolution = c("species", "genus", "family", "order"),
                typology = typology,
                taxon
        )
        z
}

a.d.1 <- anosim.fun1(anosim.diat1, taxon = "diatoms", typology = "null_model1_type")
a.d.2 <- anosim.fun1(anosim.diat2, taxon = "diatoms", typology = "null_model2_type")
a.d.3 <- anosim.fun1(anosim.diat3, taxon = "diatoms", typology = "null_model3_type")
a.d.4 <- anosim.fun1(anosim.diat4, taxon = "diatoms", typology = "null_model4_type")
a.f.1 <- anosim.fun1(anosim.fish1, taxon = "fishes",  typology = "null_model1_type")
a.f.2 <- anosim.fun1(anosim.fish2, taxon = "fishes",  typology = "null_model2_type")
a.f.3 <- anosim.fun1(anosim.fish3, taxon = "fishes",  typology = "null_model3_type")
a.f.4 <- anosim.fun1(anosim.fish4, taxon = "fishes",  typology = "null_model4_type")

f.out <- rbindlist(list(a.f.1, a.f.2, a.f.3, a.f.4))
d.out <- rbindlist(list(a.d.1, a.d.2, a.d.3, a.d.4))

ggplot(f.out, aes(x = typology, y = statistic, col = taxonomic.resolution)) + geom_point()
ggplot(d.out, aes(x = typology, y = statistic, col = taxonomic.resolution)) + geom_point()


# - Error negative distances for macrophytes? Whats up with that? 
# - compute distance matrix in separate function call 
library(parallelDist)
dist1 <- lapply(data[[3]], function(x) parallelDist(as.matrix(x[,-c(1:10)]), method = "binary", parallel = 10))

anosim.macr1.1 <- anosim(x = dist1[[1]], grouping = data[[3]][[1]]$null_model1_type, parallel = 10)
anosim.macr1.2 <- anosim(x = dist1[[1]], grouping = data[[3]][[1]]$null_model2_type, parallel = 10)
anosim.macr1.3 <- anosim(x = dist1[[1]], grouping = data[[3]][[1]]$null_model3_type, parallel = 10)
anosim.macr1.4 <- anosim(x = dist1[[1]], grouping = data[[3]][[1]]$null_model4_type, parallel = 10)

anosim.macr2.1 <- anosim(x = dist1[[2]], grouping = data[[3]][[2]]$null_model1_type, parallel = 10)
anosim.macr2.2 <- anosim(x = dist1[[2]], grouping = data[[3]][[2]]$null_model2_type, parallel = 10)
anosim.macr2.3 <- anosim(x = dist1[[2]], grouping = data[[3]][[2]]$null_model3_type, parallel = 10)
anosim.macr2.4 <- anosim(x = dist1[[2]], grouping = data[[3]][[2]]$null_model4_type, parallel = 10)

anosim.macr3.1 <- anosim(x = dist1[[3]], grouping = data[[3]][[3]]$null_model1_type, parallel = 10)
anosim.macr3.2 <- anosim(x = dist1[[3]], grouping = data[[3]][[3]]$null_model2_type, parallel = 10)
anosim.macr3.3 <- anosim(x = dist1[[3]], grouping = data[[3]][[3]]$null_model3_type, parallel = 10)
anosim.macr3.4 <- anosim(x = dist1[[3]], grouping = data[[3]][[3]]$null_model4_type, parallel = 10)

anosim.macr4.1 <- anosim(x = dist1[[4]], grouping = data[[3]][[4]]$null_model1_type, parallel = 10)
anosim.macr4.2 <- anosim(x = dist1[[4]], grouping = data[[3]][[4]]$null_model2_type, parallel = 10)
anosim.macr4.3 <- anosim(x = dist1[[4]], grouping = data[[3]][[4]]$null_model3_type, parallel = 10)
anosim.macr4.4 <- anosim(x = dist1[[4]], grouping = data[[3]][[4]]$null_model4_type, parallel = 10)

anosim.macr1 <- list(anosim.macr1.1, anosim.macr1.2, anosim.macr1.3, anosim.macr1.4)
anosim.macr2 <- list(anosim.macr2.1, anosim.macr2.2, anosim.macr2.3, anosim.macr2.4)
anosim.macr3 <- list(anosim.macr3.1, anosim.macr3.2, anosim.macr3.3, anosim.macr3.4)
anosim.macr4 <- list(anosim.macr4.1, anosim.macr4.2, anosim.macr4.3, anosim.macr4.4)

anosim.fun1 <- function(x, taxon, res){
        y <- purrr::transpose(x)
        z <- data.table(
                statistic = unlist(y$statistic),
                p.value   = unlist(y$signif),
                taxonomic.resolution = res,
                typology = c("null_model1_type", "null_model2_type", "null_model3_type", "null_model4_type"),
                taxon = taxon
        )
        z
}
a.m.1 <- anosim.fun1(anosim.macr1, taxon = "macrophytes", res = "species")
a.m.2 <- anosim.fun1(anosim.macr2, taxon = "macrophytes", res = "genus")
a.m.3 <- anosim.fun1(anosim.macr3, taxon = "macrophytes", res = "family")
a.m.4 <- anosim.fun1(anosim.macr4, taxon = "macrophytes", res = "order")

m.out <- rbindlist(list(a.m.1, a.m.2, a.m.3, a.m.4))
ggplot(m.out, aes(x = typology, y = statistic, col = taxonomic.resolution)) + geom_point()

anosim_result <- 
        rbindlist(
                list(
                d.out, f.out, m.out
                )
        )

saveRDS(anosim_result, "data/results/all_anosim_null_multiple_nulls.rds")
