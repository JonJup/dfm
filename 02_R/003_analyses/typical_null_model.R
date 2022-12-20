### --------------------------------------- ###
### --- compute typical for null models --- ### 
### --------------------------------------- ###

# -------------------------------
# date written: 10.03.22
# date last modified: 14.12.22 
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: Compute typical communities for macrophytes 
# last changes: compute for new null models 
# -------------------------------

library(conflicted)
library(data.table)
library(vegan)
library(dplyr)
library(DoE.base)
library(rstudioapi)
library(magrittr)

sink(file = paste0("02_R/999_log_files/typical","_null_model_", Sys.Date(), "_", "log.txt"))
sessionInfo()
sink(file = NULL)

# load data -------------------------------------------------------------------------
diat <- readRDS("01_data/001_diatoms//002_combined_data/03_2022-12-12_no_rare_taxa.rds")
fish <- readRDS("01_data/002_fish/002_combined_data/03_2022-12-12_no_rare_taxa.rds")
macr <- readRDS("01_data/003_macrophytes/002_combined_data/03_2022-12-12_no_rare_taxa.rds")

# prepare analysis ------------------------------------------------------------------

# - combine data sets in one list
data <- list(diat, fish, macr)

# # - drop null model types for which less than 20 observations are available
# for (i in 1:3){
#         i.tabl <- table(data[[i]][[1]]$null_model_type) 
#         if (any(i.tabl < 20)){ 
#                 drop_id <- names(i.tabl)[which(i.tabl < 20)]
#                 data[[i]]    <- lapply(data[[i]], function(x) x[!null_model_type %in% drop_id])
#                 rm(drop_id)
#         }
#         rm(i.tabl)
# }

# - cutoffs for what is considered a tyical taxon 
cut_off_typical <- list(spe = 0.33, gen = 0.5, fam = 0.66, ord = 0.8)
# - prepare list for results 
res_lst <- vector(mode = "list", length = 728)

# - loop over null models
for (t in paste0("null_model",1:4,"_type")){
        # - loop over taxa
        for (j in 1:3){
                
                # - how many samples from taxon j do we have for each type from typology t? 
                j.n.sample.per.type <- 
                        table(data[[j]] [[1]] [[t]]) |> 
                        data.frame() |> 
                        setDT()
                
                # - for which types do we have less than 20 samples? 
                j.rare.types <- which(j.n.sample.per.type$Freq < 20)
                
                # - drop these types from j.n.sample.per.type
                j.n.sample.per.type <- j.n.sample.per.type[!Var1 %in% j.rare.types]
                
                # - drop the rare types from the data set of the jth taxon.
                j.data <- lapply(data[[j]], function(x) x[!get(t) %in% j.rare.types])
                
                # - for the jth taxon sum the number of remaining occurrences for each
                # - species, genus, family, and order in the null model t
                j.sums <- lapply(j.data, function(x) x[, lapply(.SD, sum), by = t, .SDcols = 11:ncol(x)]) 
                
                # - rename column of j.n.sample.per.type to name of typology system t         
                names(j.n.sample.per.type)[1] <- t
                
                # - the type variable is currently a factor. Turn into integer. 
                j.n.sample.per.type %<>% mutate(focal_type = as.integer(as.character(get(t)))) %>% dplyr::select(focal_type, Freq)
                
                # - rename typology variable in j.sums to enable join 
                j.sums %<>% lapply(function(x) rename(x, "focal_type" = starts_with("null_model")))
                
                # - join the number of samples per type to the number of occurences in each type per focal taxonomic level
                j.joind <- 
                        j.sums |> 
                        lapply(left_join, j.n.sample.per.type, by = "focal_type") 
               
                # - divide occurrences by the number of samples  
                j.joind %<>% 
                        lapply( 
                               function(x) x[, lapply(.SD, function(x) x/Freq), .SDcols = 2:(ncol(x)-1)]
                               )
                
                # - which taxa are more common than their cutoffs? 
                # - loop over taxonomic levels
                for (k in 1:4) {
                        #- loop over types
                        for (i in 1:nrow(j.n.sample.per.type)) {
                                i.id <- which(j.joind[[k]][i,] > cut_off_typical[[k]])
                                i.typical <- names(j.joind[[k]])[i.id]
                                i.typical <- data.table(
                                        taxonomic_resolution = c("species", "genus", "family", "order")[k],
                                        typology_system = t,
                                        type = j.n.sample.per.type[i,1],
                                        typical_taxa = i.typical,
                                        taxon = c("diatom", "fish", "macrophyte")[j]
                                )
                                i.typical <- i.typical[!is.na(typical_taxa)]
                                print(paste(t, j, k, i, nrow(i.typical)))
                                res_lst[[length(res_lst)+ 1]] <- i.typical
                                rm(list = ls()[grepl(pattern = "^i\\.", x = ls())])
                                rm(i)
                        }
                        rm(k)
                }
                rm(list = ls()[grepl(pattern = "^j\\.", x = ls())])
                rm(j)
        }    
        rm(list = ls()[grepl(pattern = "^t\\.", x = ls())])
        rm(t)
}
    
res_lst <- rbindlist(res_lst)
res_lst %<>% rename(type = type.focal_type)
res_lst2 <- list()
###  ——— check similarity ——— ### 
# - loop over taxa
for (i in 1:3) {
        # - loop over taxonomic levels
        print(paste(i, "--"))
        for (l in c("species", "genus", "family", "order")) {
                print(paste(i, "--", l, "--"))
                for (t in paste0("null_model", 1:4, "_type")) {
                        
                        print(paste(i, "--", l, "--", t, "--"))
                        i.res_lst <-
                                res_lst[taxon == c("diatom", "fish", "macrophyte")[i] &
                                                typology_system == t]
                        # - create vector with unique types of focal typology system
                        i.types <- unique(i.res_lst$type)
                        # - create a factorial design matrix from the types
                        i.factorial <-
                                setDT(fac.design(
                                        nlevels = c(length(i.types), length(i.types)),
                                        randomize = F
                                ))
                        # - drop rows with equal factor levels
                        i.factorial <- i.factorial[A != B]
                        # - rename factor levels to types of focal typology system
                        i.factorial[, A := i.types[A]]
                        i.factorial[, B := i.types[B]]
                        # - rename columns of factorial matrix
                        names(i.factorial) <- c("type1", "type2")
                        i.factorial[, taxonomic_level := l]
                        i.factorial[, typology_system := t]
                        i.factorial[, taxon := c("diatom", "fish", "macrophyte")[i]]
                        i.factorial[, c("total", "shared", "only1", "only2") := 0]
                        for (k in 1:nrow(i.factorial)) {
                                print(paste(i, "--", l, "--", t, "--", k))
                                ## extract typical taxa
                                k.1 <-
                                        res_lst[taxon == c("diatom",
                                                           "fish",
                                                           "macrophyte")[i] &
                                                        typology_system == t &
                                                        taxonomic_resolution == l &
                                                        type == i.factorial$type1[k], typical_taxa]
                                k.2 <-
                                        res_lst[taxon == c("diatom",
                                                           "fish",
                                                           "macrophyte")[i] &
                                                        typology_system == t &
                                                        taxonomic_resolution == l &
                                                        type == i.factorial$type2[k], typical_taxa]
                                
                                ## combine
                                k.comb <- append(k.1, k.2) |> unique()
                                ## which are in type1
                                k.B <- k.comb %in% k.1
                                ## which are in type2
                                k.C <- k.comb %in% k.2
                                
                                k.A <- sum(k.B & k.C)
                                
                                k.D <-
                                        (k.comb %in% k.1) & (!k.comb %in% k.2)
                                k.D <- sum(k.D)
                                k.E <-
                                        (!k.comb %in% k.1) & (k.comb %in% k.2)
                                k.E <- sum(k.E)
                                i.factorial$total[k]  <- length(k.comb)
                                i.factorial$shared[k] <- k.A
                                i.factorial$only1[k]  <- k.D
                                i.factorial$only2[k]  <- k.E
                                
                                rm(list = ls()[grepl(pattern = "^k\\.",
                                                     x = ls())])
                                rm(list = ls()[grepl(pattern = "^K\\.",
                                                     x = ls())])
                        }
                        res_lst2[[length(res_lst2) + 1]] <-
                                i.factorial
                }
        }
}

res_lst2 <- rbindlist(res_lst2)
res_lst2[, jaccard_similarity := shared/(total)]
saveRDS(res_lst2, paste0("01_data/004_results/all_typical_null_model", Sys.Date(), ".rds"))
