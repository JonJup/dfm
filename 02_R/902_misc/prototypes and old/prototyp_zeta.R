### ------------------------- ###
### --- Test Zeta Diversity --- ### 
### ------------------------- ###

# -------------------------------
# date written: 28.02.22
# date last modified: 28.02.22
# Project: DFM  
# Purpose: Test zeta computations
# Notes: 
# -------------------------------

# TEST 
# BOX 
# Dont Repeat Yourself  
# AUTOMATE 
# DOCUMENT 
# NO BORKEN WINDOWS 
# Design by Contract 
# Fail fast/ defensive programming 

# setup -----------------------------------------------
library(data.table)
library(dplyr)
library(tidyr)
# load data -------------------------------------------

data <- readRDS("data/diatoms/combined_data/01_2022-01-25_combined_data_aggregated.rds")

# prepare data ----------------------------------------

sxs <- data |> 
        rbindlist() |> 
        select(!geometry) |> 
        filter(!is.na(species)) |> 
        mutate(abundance = 1) |> 
        pivot_wider(id_cols = c("gr_sample_id", "brt12"), names_from = species, values_from = abundance, values_fill = 0)

type <- sxs$brt12

sxs <- sxs[, -c(1,2)]

test <- sum(sxs[1,] == 1 & sxs[2,] == 1)





sxs.test <- sxs[1:100, ]

o01 <- Zeta.order.ex(data.spec = sxs.test, order = 1)
o02 <- Zeta.order.ex(data.spec = sxs.test, order = 2)
o03 <- Zeta.order.ex(data.spec = sxs.test, order = 3)
o04 <- Zeta.order.ex(data.spec = sxs.test, order = 4)
o05 <- Zeta.order.ex(data.spec = sxs.test, order = 5)
o06 <- Zeta.order.ex(data.spec = sxs.test, order = 6)
o07 <- Zeta.order.ex(data.spec = sxs.test, order = 7)
o08 <- Zeta.order.ex(data.spec = sxs.test, order = 8)
o09 <- Zeta.order.ex(data.spec = sxs.test, order = 9)
o10 <- Zeta.order.ex(data.spec = sxs.test, order = 10)

d.test <- Zeta.decline.ex(sxs.test, orders = 1:20)

RT1 <- which(type == "RT01") 

RT1_decline <- Zeta.decline.ex(sxs[RT1,])
RT2_decline <- Zeta.decline.ex(sxs[ which(type == "RT02"),  ])
RT3_decline <- Zeta.decline.ex(sxs[ which(type == "RT03") ,])
RT4_decline <- Zeta.decline.ex(sxs[ which(type == "RT04") ,])
RT5_decline <- Zeta.decline.ex(sxs[ which(type == "RT05") ,])
RT6_decline <- Zeta.decline.ex(sxs[ which(type == "RT06") ,])
RT7_decline <- Zeta.decline.ex(sxs[ which(type == "RT07") ,])

r_sample <- sample(1:nrow(sxs), 100)
sxs_r_sample <- sxs[r_sample, ]
r_sample_decline <- Zeta.decline.ex(sxs_r_sample)


test.list <- list(
        RT1_decline ,
        RT2_decline ,
        RT3_decline ,
        RT4_decline ,
        RT5_decline ,
        RT6_decline ,
        RT7_decline,
        r_sample_decline
)

test.list2 <- purrr::transpose(test.list)

test.list |> lapply(function(x) x$zeta.val/x$zeta.val[1]) |> unlist()

decline <- data.frame(
        order = rep(1:10, times = 8),
        zeta  = test.list |> lapply(function(x) x$zeta.val/x$zeta.val[1]) |> unlist(),
        type  = rep(c(paste0("RT", 1:7), "random"), each = 10)
)

library(ggplot2)

decline |> 
        ggplot(aes(x = order, y = zeta, col = type, fill = type)) + 
        geom_line() #+ 
        geom_point() 

        
auc_zeta <- function(x, type_arg) {
        x <- filter(x, type == type_arg)
        x <- c(x$zeta)
        y <- 0 
        for (i in 1:(length(x)-1)){
                y <- y + (x[i] + x[i + 1])/2
                
        }
        y
}        
        
lapply(c(paste0("RT", 1:7), "random"), function(x) auc_zeta(decline, x))        
        
        

# old -------------------------------------------------------------------------------

## random eval level 2 

# nrow_id <- 1:nrow(sxs)
# 
# order2 <- list()
# 
# for (i in 1:10){
#         
#         ## pick a random point for evaluation 
#         i.id <- sample(nrow_id,1)
#         i.nrow_id <- nrow_id[-which(nrow_id == i.id)]
#         ## pick 50 points for comparison 
#         i.id2 <- sample(i.nrow_id,50)
#         
#         i.zeta <- vector(mode = "integer", length = 10)
#         
#         for (k in 1:10){
#                 i.zeta[k] <- sum(sxs[i.id,] == 1 & sxs[i.id2[k],] == 1)   
#         }
#         
#         order2[[i]] <- mean(i.zeta)
#         
# }
