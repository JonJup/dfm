### ---------------------------------------------- ###
### --- compute zeta diversity for Null models --- ### 
### ---------------------------------------------- ###

# -------------------------------
# date written: 10.03.22
# date last modified: 16.12.22 
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: Compute zeta diversity for null models 
# Notes: 
# last changes: Adapt to multiple null models
# -------------------------------


# setup -----------------------------------------------------------------------------
library(pacman)
p_load(data.table, rstudioapi, zetadiv, stringr)

sink(file = paste0("02_R/999_log_files/zeta","_null_model_", Sys.Date(), "_", "log.txt"))
documentPath()
sessionInfo()
sink(file = NULL)

# load data -------------------------------------------------------------------------
diat <- readRDS("01_data/001_diatoms/002_combined_data/03_2022-12-12_no_rare_taxa.rds")
fish <- readRDS("01_data/002_fish/002_combined_data/03_2022-12-12_no_rare_taxa.rds")
macr <- readRDS("01_data/003_macrophytes/002_combined_data/03_2022-12-12_no_rare_taxa.rds")

# prepare data ----------------------------------------------------------------------

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
        rm(i.tabl1, i.tabl2, i.tabl3, i.tabl4, i)
}

# - Create a list of the four lists 
meta.list <- list(data1, data2, data3, data4)

# - from aach table drop unwanted columns, i.e., typology systems and NA
drop_list <- c("gr_sample_id", "brt", "ife", "bgr", "few", "enz", "null_model1_type", "null_model2_type", "null_model3_type","null_model4_type", "NA")

# - clean environment 
rm(data1, data2, data3, data4, diat, fish, macr)

# - 
i = k = l = 1
# - loop over null models 
for (i in 1:4){
        # - loop over taxa 
        for (k in 1:3){
                k.list <- vector(mode = "list", length = 4)
                k.tax = switch(k,
                               "dia",
                               "fsh",
                               "mph")
                
                # - loop over taxonomic resolutions
                for (l in 1:4){
                        l.drop_id <- 
                                which(
                                names(
                                        meta.list [[i]] [[k]] [[l]]
                                        ) 
                                %in% 
                                        drop_list
                                )
                        
                        
                        l.obj <- copy(meta.list [[i]] [[k]][[l]])
                        k.list[[l]] <- l.obj[,(l.drop_id) := NULL]
                        
                        # assign(x =  paste0(
                        #                        "sxs_", 
                        #                        l.tax,
                        #                        "_",
                        #                        i,"_",l
                        #                ),
                        #        value = l.obj)
                        rm(l)
                        rm(list = ls()[grepl(x = ls(), pattern = "^l\\.")])
                        } # END of LOOP over taxonomic resolutions
                assign(x =  paste0(
                                       "sxs_",
                                       k.tax,
                                       "_",
                                       i
                               ),
                       value = k.list)
                rm(k)
                rm(list = ls()[grepl(x = ls(), pattern = "^k\\.")])
        }# END OF LOOP OVER TAXA
        rm(i)
}# END OF LOOP OVER NULL MODELS
rm(drop_list)

# - loop variables 
# orders in zeta decline 
v.orders <- 10

# - taxa
sxs <- ls()[grepl("^sxs", ls())]

#sxs <- list(sxs_dia, sxs_fsh, sxs_mph)

# - loop over taxa 
tax = i = k = 1

# - move to later in loop 
# - extract types 
types <- data[[tax]][[1]][["null_model_type"]]
unique_types <- unique(types)


# - loop over elements of sxs 
for (i in seq_along(sxs)) {
        
        # - get basic information on this run
        i.data <- get(sxs[i])
        i.taxon <-  str_remove(sxs[i], "sxs_") |> str_remove("_.*$")
        i.tax.number <- ifelse(i.taxon == "dia", 1, ifelse(i.taxon == "fsh", 2, 3))
        i.nullmodel <- str_remove(sxs[i], paste0("sxs_", i.taxon, "_"))
        
        # - extract types 
        # -- get relevant entry out of the meta list  
        i.meta.list <- meta.list[[as.numeric(i.nullmodel)]] [[i.tax.number]]
        # -- extract null model type vector 
        i.names <- names(i.meta.list[[1]])
        i.type <- i.meta.list[[1]][, get(i.names[6 + as.numeric(i.nullmodel)])]
        i.unique.type <- unique(i.type)
        # - loop over types 
        for (l in seq_along(i.unique.type)){
                # - loop over taxonomic resolutions
                for (k in 1:4){
                        k.x <- 
                                Zeta.decline.ex(
                                        i.data[[k]][which(i.type == i.unique.type[l]),],
                                        orders = 1:v.orders
                                )   
                        k.x2 <- k.x$zeta.val
                        k.x2 <- k.x2 / k.x2[1]
                        k.auc.var <- c()
                        for (auc in 1:(v.orders-1)){
                                k.auc.var[auc] <- (k.x2[auc] + k.x2[auc + 1])/2
                        }
                        k.auc.var <- sum(k.auc.var)
                        ls.rs[[length(ls.rs) + 1]] <- data.table(
                                order = k.x$zeta.order,
                                zeta_diversity = k.x2,
                                auc            = k.auc.var,
                                taxonomic_resolution = k,
                                type = i.unique.type[l], 
                                typology = paste0("null_model",i.nullmodel,"_type"),
                                taxon = i.taxon
                        )
                        rm(list = ls()[grepl(pattern = "^k\\.", x = ls())])   
                        rm(k)
                } 
                rm(list = ls()[grepl(pattern = "^l\\.", x = ls())])   
                rm(l)   
        }
        rm(list = ls()[grepl(pattern = "^i\\.", x = ls())])   
        rm(i)
}

zeta <- rbindlist(ls.rs)

# save to file ----------------------------------------------------------------------

saveRDS(zeta, "01_data/004_results/all_zeta_null_model.rds")


