### ------------------------------------------ ###
### --- compute indval for the null models --- ### 
### ------------------------------------------ ###

# -------------------------------
# date written: 10.03.22
# date last modified: 14.12.22 
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: Compute indval for null models 
# last changes: compute for new null models (now 4 different ones)
# setup ----------------------------------------------------------------------------
library(pacman)
p_load(indicspecies, data.table, rstudioapi)

### log files ---- 
sink(file = paste0("02_R/999_log_files/indval","_", "null_model", "_", Sys.Date(), "_", "log.txt"))
documentPath()
sessionInfo()
sink(file = NULL)
### ---- 


# load data -------------------------------------------------------------------------
diat <- readRDS("01_data/001_diatoms/002_combined_data/03_2022-12-12_no_rare_taxa.rds")
fish <- readRDS("01_data/002_fish/002_combined_data/03_2022-12-12_no_rare_taxa.rds")
macr <- readRDS("01_data/003_macrophytes/002_combined_data/03_2022-12-12_no_rare_taxa.rds")


# prepare data ----------------------------------------------------------------------
indval_fun1 <- 
        function(data, typology){
                x1 <- lapply(data,
                             function(x)
                                     multipatt(
                                             x[, -c(1:7)],
                                             cluster = unlist(x[,(typology), with = FALSE]),
                                             func = "IndVal.g",
                                             duleg = TRUE
                                     ))    
                
        }

indval_fun2 <- 
        function(y){
                out <- 
                        y |> 
                        lapply(function(x) x$sign)|> 
                        lapply(function(x) setDT(x))|> 
                        lapply(function(x) x[p.value <=0.05])|> 
                        lapply(function(x) nrow(x))     |> 
                        unlist()
                return(out)
        }

indval_fun3 <- 
        function(t){
                out <- 
                        t |> 
                        lapply(function(x) x$sign)|> 
                        lapply(function(x) mean(x$p.value, na.rm = TRUE)) |> 
                        unlist()
                
        }

# analysis --------------------------------------------------------------------------

# - create one copy for each typology system. This is so we can drop rare types individually 
data1 <- list(diat, fish, macr)
data2 <- list(diat, fish, macr)
data3 <- list(diat, fish, macr)
data4 <- list(diat, fish, macr)

# - for each taxon and typology system remove types with less than 20 samples
# - loop over taxa
for (i in 1:3){
        i.tabl1 <- table(data1[[i]][[1]]$null_model1_type)
        i.tabl2 <- table(data2[[i]][[1]]$null_model2_type)
        i.tabl3 <- table(data3[[i]][[1]]$null_model3_type)
        i.tabl4 <- table(data4[[i]][[1]]$null_model4_type)
        if (any(i.tabl1 < 20)){
                drop_id   <- names(i.tabl1)[which(i.tabl1 < 20)]
                data1[[i]] <- lapply(data1[[i]], function(x) x[!null_model1_type %in% drop_id])
                rm(drop_id)
        }
        if (any(i.tabl2 < 20)){
                drop_id <- names(i.tabl2)[which(i.tabl2 < 20)]
                data2[[i]]    <- lapply(data2[[i]], function(x) x[!null_model2_type %in% drop_id])
                rm(drop_id)
        }
        if (any(i.tabl3 < 20)){
                drop_id <- names(i.tabl3)[which(i.tabl3 < 20)]
                data3[[i]]    <- lapply(data3[[i]], function(x) x[!null_model3_type %in% drop_id])
                rm(drop_id)
        }
        if (any(i.tabl4 < 20)){
                drop_id <- names(i.tabl4)[which(i.tabl4 < 20)]
                data4[[i]]    <- lapply(data4[[i]], function(x) x[!null_model4_type %in% drop_id])
                rm(drop_id)
        }
        rm(i.tabl1, i.tabl2, i.tabl3, i.tabl4)
}

iv1.diat1 <- indval_fun1(data = data1[[1]], typology = "null_model1_type")
iv1.diat2 <- indval_fun1(data = data2[[1]], typology = "null_model2_type")
iv1.diat3 <- indval_fun1(data = data3[[1]], typology = "null_model3_type")
iv1.diat4 <- indval_fun1(data = data4[[1]], typology = "null_model4_type")

iv1.fish1 <- indval_fun1(data = data1[[2]], typology = "null_model1_type")
iv1.fish2 <- indval_fun1(data = data2[[2]], typology = "null_model2_type")
iv1.fish3 <- indval_fun1(data = data3[[2]], typology = "null_model3_type")
iv1.fish4 <- indval_fun1(data = data4[[2]], typology = "null_model4_type")

iv1.macr1 <- indval_fun1(data = data1[[3]], typology = "null_model1_type")
iv1.macr2 <- indval_fun1(data = data2[[3]], typology = "null_model2_type")
iv1.macr3 <- indval_fun1(data = data3[[3]], typology = "null_model3_type")
iv1.macr4 <- indval_fun1(data = data4[[3]], typology = "null_model4_type")

iv2.diat1 <- indval_fun2(iv1.diat1)
iv2.diat2 <- indval_fun2(iv1.diat2)
iv2.diat3 <- indval_fun2(iv1.diat3)
iv2.diat4 <- indval_fun2(iv1.diat4)
iv2.fish1 <- indval_fun2(iv1.fish1)
iv2.fish2 <- indval_fun2(iv1.fish2)
iv2.fish3 <- indval_fun2(iv1.fish3)
iv2.fish4 <- indval_fun2(iv1.fish4)
iv2.macr1 <- indval_fun2(iv1.macr1)
iv2.macr2 <- indval_fun2(iv1.macr2)
iv2.macr3 <- indval_fun2(iv1.macr3)
iv2.macr4 <- indval_fun2(iv1.macr4)

iv3.diat1 <- indval_fun3(iv1.diat1)
iv3.diat2 <- indval_fun3(iv1.diat2)
iv3.diat3 <- indval_fun3(iv1.diat3)
iv3.diat4 <- indval_fun3(iv1.diat4)
iv3.fish1 <- indval_fun3(iv1.fish1)
iv3.fish2 <- indval_fun3(iv1.fish2)
iv3.fish3 <- indval_fun3(iv1.fish3)
iv3.fish4 <- indval_fun3(iv1.fish4)
iv3.macr1 <- indval_fun3(iv1.macr1)
iv3.macr2 <- indval_fun3(iv1.macr2)
iv3.macr3 <- indval_fun3(iv1.macr3)
iv3.macr4 <- indval_fun3(iv1.macr4)


# - compile results
results_diatom <- 
        data.table(
                taxon = "diatom", 
                typology = rep(paste0("null_model", 1:4), each = 4),
                taxonomic.resolution = c("species", "genus", "family", "order"),
                n.indicator = c(iv2.diat1,iv2.diat2,iv2.diat3,iv2.diat4),
                mean_p = c(iv3.diat1, iv3.diat2, iv3.diat3, iv3.diat4)
        )
results_fish <- 
        data.table(
                taxon = "fish", 
                typology = rep(paste0("null_model", 1:4), each = 4),
                taxonomic.resolution = c("species", "genus", "family", "order"),
                n.indicator = c(iv2.fish1,iv2.fish2,iv2.fish3,iv2.fish4),
                mean_p = c(iv3.fish1, iv3.fish2, iv3.fish3, iv3.fish4)
        )
results_macrophytes <- 
        data.table(
                taxon = "macrophytes", 
                typology = rep(paste0("null_model", 1:4), each = 4),
                taxonomic.resolution = c("species", "genus", "family", "order"),
                n.indicator = c(iv2.macr1,iv2.macr2,iv2.macr3,iv2.macr4),
                mean_p = c(iv3.macr1, iv3.macr2, iv3.macr3, iv3.macr4)
        )

# save to file ---------------------------------------------------------------------
saveRDS(results_diatom     , "01_data/004_results/diatom_indval_null.rds")
saveRDS(results_fish       , "01_data/004_results/fish_indval_null.rds")
saveRDS(results_macrophytes, "01_data/004_results/macrophytes_indval_null.rds")

