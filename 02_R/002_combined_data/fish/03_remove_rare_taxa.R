### --- Drop rare Taxa    --- ###
### --- Fish              --- ### 


# date written: 31.01.2022
# date last modified: 20.03.2022
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: Remove rare fish taxa from data


# setup -----------------------------------------------
library(pacman)
p_load(data.table, dplyr, tidyr)

# load data -------------------------------------------
data <- readRDS("data/fish/combined_data/02_2022-12-12_w_null_model.rds")

# prepare data ----------------------------------------
data <- rename(data, brt = brt12)

# - get one row per sample 
sites <- unique(data, by = "gr_sample_id")



# - count samples within each type of each typology 
table.lst <- list(
        brt = table(sites$brt), 
        ife = table(sites$ife), 
        bgr = table(sites$bgr), 
        few = table(sites$few), 
        enz = table(sites$enz) 
                  )
# - drop types with less than 20 samples

while (any(unlist(table.lst)<20)){
        
        if (any(table.lst$brt <20)){
                rare_types <- names(which(table.lst$brt<20))        
                data <- data[! brt %in% rare_types]
        }
        if (any(table.lst$ife<20)){
                rare_types <- names(which(table.lst$ife<20))        
                data <- data[! ife %in% rare_types]
        }
        if (any(table.lst$bgr<20)){
                rare_types <- names(which(table.lst$bgr<20))        
                data <- data[! bgr %in% rare_types]
        }
        if (any(table.lst$few<20)){
                rare_types <- names(which(table.lst$few<20))        
                data <- data[! few %in% rare_types]
        }
        if (any(table.lst$enz<20)){
                rare_types <- names(which(table.lst$enz<20))        
                data <- data[! enz %in% rare_types]
        }
        sites <- unique(data, by = "gr_sample_id")
        table.lst <- list(
                brt = table(sites$brt), 
                ife = table(sites$ife), 
                bgr = table(sites$bgr), 
                few = table(sites$few), 
                enz = table(sites$enz) 
        )
        
}

data[, uniqueN(gr_sample_id), by = "data.set"]

# - add a "presence" variable that is always 1. Zeros will be automatically added in the
# - next step (pivot_wider).

data[, presence := 1]

data$species |> uniqueN()
data$genus |> uniqueN()
data$family |> uniqueN()
data$order |> uniqueN()

piv <- function(x) {
        ub <- c(x, "gr_sample_id")
        data |> 
                unique(by=ub) |>
                select(!geometry) |> 
                pivot_wider(id_cols = c("gr_sample_id", "brt", "ife", "bgr", 
                                        "few", "enz", "null_model1_type", 
                                        "null_model2_type", "null_model3_type", 
                                        "null_model4_type"), 
                            names_from = x, 
                            values_from = "presence", 
                            values_fill = 0) -> 
                out
        return(out)        
}


## pivot wider to site X taxon format

data.spe <- piv("species") |> setDT() |> {\(x) x[, "NA" := NULL]}()
data.gen <- piv("genus")   |> setDT() |> {\(x) x[, "NA" := NULL]}()
# - family and order currently have no entry with missing data thus the last step throws a warning
data.fam <- piv("family")  |> setDT() |> {\(x) x[, "NA" := NULL]}()
data.ord <- piv("order")   |> setDT() |> {\(x) x[, "NA" := NULL]}()

data.ls <- list(data.spe, data.gen, data.fam, data.ord)

# - what are the three most common species 
data.spe |> select(-c(1:10)) |> apply(2,sum) |> sort(decreasing = T) |> {\(x) x[1:3]}()
# - how many singletons 
data.spe |> select(-c(1:10)) |> apply(2,sum) |> {\(x) x == 1}() |> sum()
# - what is the mean number of occurrences? And the SD? 
data.spe |> select(-c(1:10)) |> apply(2,sum) |> mean()
data.spe |> select(-c(1:10)) |> apply(2,sum) |> sd()
# - what is the mean species richness? And the SD? 
data.spe |> select(-c(1:10)) |> apply(1,sum) |> mean()
data.spe |> select(-c(1:10)) |> apply(1,sum) |> sd()

occurrence_frequencies <- lapply(data.ls, function(x) apply(x[,-c(1:10)],2,sum))

# - I wont use one percent of taxa since this biases against taxa from rare types. 
# - Instead I remove singletons. 
occurrence_threshold <- 1 #lapply(data.ls, function(x) round(0.01 * nrow(x)))

data2 <- list()
# - identify and remove rare taxa 
for (i in seq_along(data.ls)){
        i.rare      <- which(occurrence_frequencies[[i]] <= occurrence_threshold)
        i.remove <- names(i.rare)
        i.remove <- unique(i.remove)
        data2[[i]] <- data.ls[[i]][, (i.remove) := NULL]
        rm(list = ls()[grep(pattern = "^i\\.", x = ls())])
}

# analyze --------------------------------------------

## Remaining number of taxa species
data2[[1]] |> ncol() |> {\(x) x - 10}()
## Remaining number of taxa genera
data2[[2]] |> ncol() |> {\(x) x - 10}()
## Remaining number of taxa families
data2[[3]] |> ncol() |> {\(x) x - 10}()
## Remaining number of taxa orders
data2[[4]] |> ncol() |> {\(x) x - 10}()

data2[[1]] <- data2[[1]][which(rowSums(data2[[1]][, -c(1:10)]) > 1), ]
data2[[2]] <- data2[[2]][which(rowSums(data2[[1]][, -c(1:10)]) > 1), ]
data2[[3]] <- data2[[3]][which(rowSums(data2[[1]][, -c(1:10)]) > 1), ]
data2[[4]] <- data2[[4]][which(rowSums(data2[[1]][, -c(1:10)]) > 1), ]

# save data -------------------------------------------
saveRDS(data2, file = paste0("data/fish/combined_data/03_",Sys.Date(),"_no_rare_taxa.rds"))
