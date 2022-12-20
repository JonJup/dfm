### ------------------------- ###
### --- Diatom Traits     --- ### 
### ------------------------- ###

# -------------------------------
# date written: 24.02.22
# date last modified: 25.02.22
# Project: GETREAL
# Purpose: Analyses of Diatom Traits 
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

### BRT20 types 


# R-01 Very large rivers
# R-02 Lowland, siliceous, medium-large
# R-03 Lowland, siliceous, very small-small 
# R-04 Lowland, calcareous or mixed, medium-large 
# R-05 Lowland, calcareous or mixed, very small-small 
# R-06a Lowland, organic and siliceous, very small-small 
# R-06b Lowland, organic and siliceous, medium-large 
# R-07 Lowland, organic and calcareous/mixed 
# R-08 Mid-altitude, siliceous, medium-large 
# R-09 Mid-altitude, siliceous, very small-small 
# R-10 Mid-altitude, calcareous or mixed, medium-large 
# R-11 Mid-altitude, calcareous or mixed, very small-small 
# R-12a Mid-altitude, organic and siliceous, very small-small 
# R-12b Mid-altitude, organic and siliceous, medium-large 
# R-13 Mid-altitude, organic and calcareous/mixed 
# R-14 Highland (all Europe), siliceous, incl. organic (humic) 
# R-15 Highland (all Europe), calcareous/mixed 
# R-16 Glacial rivers (all Europe) 
# R-17 Mediterranean, lowland, medium-Large, perennial 
# R-18 Mediterranean, mid altitude, medium-large, perennial 
# R-19 Mediterranean, very small-small, perennial 
# R-20 Mediterranean, temporary/intermittent streams 



# setup -----------------------------------------------
library(magrittr)     # 2.02
library(data.table)   # 1.14.2
library(ggplot2)      # 3.3.5 
library(ggdist)       # 3.1.0
library(mFD)          # 1.0.1 
box::use(
        dplyr[rename, case_when, mutate, select], # 1.0.8 
        s = sf,                    # 1.0-6 
        colorspace[lighten]
        )



# load data -------------------------------------------

# diatom traits 
dt <- readRDS("data/diatoms/220224_dfm_traits.rds")

# prepare data ----------------------------------------

# fix shape variable - shape has very strange entires. I wont use it for analyses 
table(dt$Shape)

# check which traits can be collapse to more informative one column traits

dt%<>%rename(size_class = "Size class")
dt%<>%rename(adnate = "Adnate")
dt%<>%rename(pedunculate = "Pedunculate (stalk or pad attached to substrate)")
dt%<>%rename(pad = "Pad (attached to substrate)")
dt%<>%rename(hpg = "High profile guild")
dt%<>%rename(lpg = "Low profile guild")
dt%<>%rename(mg = "Motile guild")
dt%<>%rename(pl = "Planktonic")
dt%<>%rename(stalk2 = "Stalk (attached to substrate)")

dt[, size_class := lapply(.SD, as.numeric), .SDcols = 5]
dt[, adnate := lapply(.SD, as.numeric), .SDcols = 10]
dt[, pedunculate := lapply(.SD, as.numeric), .SDcols = 11]
dt[, pad := lapply(.SD, as.numeric), .SDcols = 12]
dt[, stalk := lapply(.SD, as.numeric), .SDcols = 13]
dt[, hpg := lapply(.SD, as.numeric), .SDcols = 23]
dt[, lpg := lapply(.SD, as.numeric), .SDcols = 24]
dt[, mg := lapply(.SD, as.numeric), .SDcols = 25]
dt[, pl := lapply(.SD, as.numeric), .SDcols = 26]
dt[, Mobile := lapply(.SD, as.numeric), .SDcols = 9]
dt[, Pioneer := lapply(.SD, as.numeric), .SDcols = which(names(dt) == "Pioneer")]
dt[, pedunculate := lapply(.SD, as.numeric), .SDcols = which(names(dt) == "pedunculate")]
dt[, pad := lapply(.SD, as.numeric), .SDcols = which(names(dt) == "pad")]
dt[, stalk2 := lapply(.SD, as.numeric), .SDcols = which(names(dt) == "stalk2")]
dt[, Colonial := lapply(.SD, as.numeric), .SDcols = which(names(dt) == "Colonial")]

which(names(dt) == "Colonial")


dt[, check1 := adnate + pedunculate + pad + stalk]
table(dt$check1)
# cant collapse 
dt[, check1 := hpg + lpg + mg + pl]
table(dt$check1)
# can collapse 
dt[, guild := case_when(hpg == 1 ~ "high",
                        lpg == 1 ~ "low",
                        mg == 1 ~ "motile",
                        pl == 1 ~ "planktonic")]



# add brt20  ------------------------------------------------------------------------

## load typology data incl. brt20 
at <- readRDS("data/all_typologies.rds")

## extract unique samples 
sites <- unique(dt, by = c("gr_sample_id"))
## turn spatial 
sites <- s$st_as_sf(sites)
## transform CRS of site to that of typology 
sites <- s$st_transform(sites, crs = s$st_crs(at))
## nearest river for each sample 
nn <- s$st_nearest_feature(sites, at)
## reoder typology data with nearest river 
nn <- at[nn, ]
## compute distance for each object to nearest river 
distances <- s$st_distance(sites, y = nn, by_element = TRUE)
## add brt20 to sites 
sites <- mutate(sites, brt20 = nn$brt20)
## drop all variables except sample id and brt20 type
sites <- select(sites, gr_sample_id, brt20) |> setDT()
# drop geometry
sites[, geometry := NULL]
## join brt20 type to data 
dt <- sites[dt, on = "gr_sample_id"]

## aggregate types to getreal aggregates 
dt[, brt20_aggregate := brt20]
dt[brt20 %in% c("RT6", "RT8"), brt20_aggregate := "RT6_8"]
dt[brt20 %in% c("RT14", "RT15", "RT16"), brt20_aggregate := "RT14_15_16"]

## drop types without typical assembalges 
dt <- dt[!brt20 %in% c("RT1", "RT3", "RT4", "RT5", "RT7", "RT11", "RT12", "RT13", "RT17", "RT19", "RT20")]
dt[, brt20f := factor(brt20_aggregate)]
dt[, brt20n := as.numeric(brt20f)]
dt[,brt20_aggregate := factor(brt20_aggregate, levels = c("RT2", "RT6_8", "RT9", "RT10","RT14_15_16", "RT18"))]

# plot function --------------------------------------------

trait_plot_brt <- function(data, trait, least.impaired = FALSE, maxX = NULL){
        
        
        
        if(least.impaired){
                data <- data[least.impacted == TRUE]
                }
        
        trait_column <- which(names(data) == trait)
        names(data)[trait_column] <- "focal_trait"
        data[, geometry := NULL]
        
        data[, median_trait := mean(focal_trait, na.rm = TRUE), by = c("gr_sample_id")]
        
        ## determine plot title 
        title <- trait
        if (least.impaired){
                title <- paste(title, "least impaired sites")
        } else {
                title <- paste(title, "all sites")
        }
        
        
        
        plot_out <-
                data |>
                ggplot() +
                stat_halfeye(
                        aes(
                                x = focal_trait,
                                y = brt20_aggregate,
                                col = brt20_aggregate,
                                fill = after_scale(lighten(color, 0.5))
                        ),
                        #alpha = 0.9,
                        point_size = 4,
                        scale = 0.5,
                        point_interval = "mean_qi"
                ) +
                theme(legend.position = "none") +
                ylab("broad river type") +
                xlab(trait) + 
                ggtitle(title)
        if (!is.null(maxX)){
                plot_out <- plot_out + xlim(0,maxX)
        }
        return(plot_out)
}

# trait_plot_brt_binary <- function(data, trait, least.impaired = FALSE){
#         if(least.impaired){
#                 data <- data[least.impacted == TRUE]
#         }
#         
#         trait_column <- which(names(data) == trait)
#         names(data)[trait_column] <- "focal_trait"
#         data[, geometry := NULL]
#         
#         data[, median_trait := mean(focal_trait, na.rm = TRUE), by = c("gr_sample_id")]
#         
#         ## determine plot title 
#         title <- trait
#         if (least.impaired){
#                 title <- paste(title, "least impaired sites")
#         } else {
#                 title <- paste(title, "all sites")
#         }
#         
#         plot_out <- 
#                 ggplot(data, 
#                        aes(y = median_trait, x = brt20_aggregate, fill = brt20_aggregate)) + 
#                 geom_violin(draw_quantiles = 0.5)
#         plot_out        
# }

# create trait plots ----------------------------------------------------------------
trait_plot_brt(dt, "hpg") # slight pattern 
trait_plot_brt(dt, "lpg") # slight pattern
trait_plot_brt(dt, "pl")  # slight pattern
trait_plot_brt(dt, "mg")  # slight pattern
trait_plot_brt(dt, "size_class") # slight pattern 
trait_plot_brt(dt, "length", maxX = 75) # very slight pattern 
trait_plot_brt(dt, "width", maxX = 25) # slight pattern 
trait_plot_brt(dt, "thickness", maxX = 5) # no pattenr
trait_plot_brt(dt, "adnate") # no pattern 
trait_plot_brt(dt, "Mobile") # no pattern 
trait_plot_brt(dt, "Pioneer") # no pattern 
trait_plot_brt(dt, "pedunculate") # no pattern 
trait_plot_brt(dt, "pad") # no pattern 
trait_plot_brt(dt, "Stalk (attached to substrate)") # no pattern 
trait_plot_brt(dt, "Colonial") # no pattern 

# compute diversity  ----------------------------------------------------------------



# save data -------------------------------------------
### ------------------------- ###
### --- Diatom Traits     --- ### 
### ------------------------- ###

# -------------------------------
# date written: 24.02.22
# date last modified: 25.02.22
# Project: GETREAL
# Purpose: Analyses of Diatom Traits 
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

### BRT20 types 


# R-01 Very large rivers
# R-02 Lowland, siliceous, medium-large
# R-03 Lowland, siliceous, very small-small 
# R-04 Lowland, calcareous or mixed, medium-large 
# R-05 Lowland, calcareous or mixed, very small-small 
# R-06a Lowland, organic and siliceous, very small-small 
# R-06b Lowland, organic and siliceous, medium-large 
# R-07 Lowland, organic and calcareous/mixed 
# R-08 Mid-altitude, siliceous, medium-large 
# R-09 Mid-altitude, siliceous, very small-small 
# R-10 Mid-altitude, calcareous or mixed, medium-large 
# R-11 Mid-altitude, calcareous or mixed, very small-small 
# R-12a Mid-altitude, organic and siliceous, very small-small 
# R-12b Mid-altitude, organic and siliceous, medium-large 
# R-13 Mid-altitude, organic and calcareous/mixed 
# R-14 Highland (all Europe), siliceous, incl. organic (humic) 
# R-15 Highland (all Europe), calcareous/mixed 
# R-16 Glacial rivers (all Europe) 
# R-17 Mediterranean, lowland, medium-Large, perennial 
# R-18 Mediterranean, mid altitude, medium-large, perennial 
# R-19 Mediterranean, very small-small, perennial 
# R-20 Mediterranean, temporary/intermittent streams 



# setup -----------------------------------------------
library(magrittr)     # 2.02
library(data.table)   # 1.14.2
library(ggplot2)      # 3.3.5 
library(ggdist)       # 3.1.0
box::use(
        dplyr[rename, case_when, mutate, select], # 1.0.8 
        s = sf,                    # 1.0-6 
        colorspace[lighten]
        )



# load data -------------------------------------------

# diatom traits 
dt <- readRDS("data/diatoms/220224_dfm_traits.rds")

# prepare data ----------------------------------------

# fix shape variable - shape has very strange entires. I wont use it for analyses 
table(dt$Shape)

# check which traits can be collapse to more informative one column traits

dt%<>%rename(size_class = "Size class")
dt%<>%rename(adnate = "Adnate")
dt%<>%rename(pedunculate = "Pedunculate (stalk or pad attached to substrate)")
dt%<>%rename(pad = "Pad (attached to substrate)")
dt%<>%rename(hpg = "High profile guild")
dt%<>%rename(lpg = "Low profile guild")
dt%<>%rename(mg = "Motile guild")
dt%<>%rename(pl = "Planktonic")
dt%<>%rename(stalk2 = "Stalk (attached to substrate)")

dt[, size_class := lapply(.SD, as.numeric), .SDcols = 5]
dt[, adnate := lapply(.SD, as.numeric), .SDcols = 10]
dt[, pedunculate := lapply(.SD, as.numeric), .SDcols = 11]
dt[, pad := lapply(.SD, as.numeric), .SDcols = 12]
dt[, stalk := lapply(.SD, as.numeric), .SDcols = 13]
dt[, hpg := lapply(.SD, as.numeric), .SDcols = 23]
dt[, lpg := lapply(.SD, as.numeric), .SDcols = 24]
dt[, mg := lapply(.SD, as.numeric), .SDcols = 25]
dt[, pl := lapply(.SD, as.numeric), .SDcols = 26]
dt[, Mobile := lapply(.SD, as.numeric), .SDcols = 9]
dt[, Pioneer := lapply(.SD, as.numeric), .SDcols = which(names(dt) == "Pioneer")]
dt[, pedunculate := lapply(.SD, as.numeric), .SDcols = which(names(dt) == "pedunculate")]
dt[, pad := lapply(.SD, as.numeric), .SDcols = which(names(dt) == "pad")]
dt[, stalk2 := lapply(.SD, as.numeric), .SDcols = which(names(dt) == "stalk2")]
dt[, Colonial := lapply(.SD, as.numeric), .SDcols = which(names(dt) == "Colonial")]

which(names(dt) == "Colonial")


dt[, check1 := adnate + pedunculate + pad + stalk]
table(dt$check1)
# cant collapse 
dt[, check1 := hpg + lpg + mg + pl]
table(dt$check1)
# can collapse 
dt[, guild := case_when(hpg == 1 ~ "high",
                        lpg == 1 ~ "low",
                        mg == 1 ~ "motile",
                        pl == 1 ~ "planktonic")]



# add brt20  ------------------------------------------------------------------------

## load typology data incl. brt20 
at <- readRDS("data/all_typologies.rds")

## extract unique samples 
sites <- unique(dt, by = c("gr_sample_id"))
## turn spatial 
sites <- s$st_as_sf(sites)
## transform CRS of site to that of typology 
sites <- s$st_transform(sites, crs = s$st_crs(at))
## nearest river for each sample 
nn <- s$st_nearest_feature(sites, at)
## reoder typology data with nearest river 
nn <- at[nn, ]
## compute distance for each object to nearest river 
distances <- s$st_distance(sites, y = nn, by_element = TRUE)
## add brt20 to sites 
sites <- mutate(sites, brt20 = nn$brt20)
## drop all variables except sample id and brt20 type
sites <- select(sites, gr_sample_id, brt20) |> setDT()
# drop geometry
sites[, geometry := NULL]
## join brt20 type to data 
dt <- sites[dt, on = "gr_sample_id"]

## aggregate types to getreal aggregates 
dt[, brt20_aggregate := brt20]
dt[brt20 %in% c("RT6", "RT8"), brt20_aggregate := "RT6_8"]
dt[brt20 %in% c("RT14", "RT15", "RT16"), brt20_aggregate := "RT14_15_16"]

## drop types without typical assembalges 
dt <- dt[!brt20 %in% c("RT1", "RT3", "RT4", "RT5", "RT7", "RT11", "RT12", "RT13", "RT17", "RT19", "RT20")]
dt[, brt20f := factor(brt20_aggregate)]
dt[, brt20n := as.numeric(brt20f)]
dt[,brt20_aggregate := factor(brt20_aggregate, levels = c("RT2", "RT6_8", "RT9", "RT10","RT14_15_16", "RT18"))]

# plot function --------------------------------------------

trait_plot_brt <- function(data, trait, least.impaired = FALSE, maxX = NULL){
        
        
        
        if(least.impaired){
                data <- data[least.impacted == TRUE]
                }
        
        trait_column <- which(names(data) == trait)
        names(data)[trait_column] <- "focal_trait"
        data[, geometry := NULL]
        
        data[, median_trait := mean(focal_trait, na.rm = TRUE), by = c("gr_sample_id")]
        
        ## determine plot title 
        title <- trait
        if (least.impaired){
                title <- paste(title, "least impaired sites")
        } else {
                title <- paste(title, "all sites")
        }
        
        
        
        plot_out <-
                data |>
                ggplot() +
                stat_halfeye(
                        aes(
                                x = focal_trait,
                                y = brt20_aggregate,
                                col = brt20_aggregate,
                                fill = after_scale(lighten(color, 0.5))
                        ),
                        #alpha = 0.9,
                        point_size = 4,
                        scale = 0.5,
                        point_interval = "mean_qi"
                ) +
                theme(legend.position = "none") +
                ylab("broad river type") +
                xlab(trait) + 
                ggtitle(title)
        if (!is.null(maxX)){
                plot_out <- plot_out + xlim(0,maxX)
        }
        return(plot_out)
}

# trait_plot_brt_binary <- function(data, trait, least.impaired = FALSE){
#         if(least.impaired){
#                 data <- data[least.impacted == TRUE]
#         }
#         
#         trait_column <- which(names(data) == trait)
#         names(data)[trait_column] <- "focal_trait"
#         data[, geometry := NULL]
#         
#         data[, median_trait := mean(focal_trait, na.rm = TRUE), by = c("gr_sample_id")]
#         
#         ## determine plot title 
#         title <- trait
#         if (least.impaired){
#                 title <- paste(title, "least impaired sites")
#         } else {
#                 title <- paste(title, "all sites")
#         }
#         
#         plot_out <- 
#                 ggplot(data, 
#                        aes(y = median_trait, x = brt20_aggregate, fill = brt20_aggregate)) + 
#                 geom_violin(draw_quantiles = 0.5)
#         plot_out        
# }

# create trait plots ----------------------------------------------------------------
trait_plot_brt(dt, "hpg") # slight pattern 
trait_plot_brt(dt, "lpg") # slight pattern
trait_plot_brt(dt, "pl")  # slight pattern
trait_plot_brt(dt, "mg")  # slight pattern
trait_plot_brt(dt, "size_class") # slight pattern 
trait_plot_brt(dt, "length", maxX = 75) # very slight pattern 
trait_plot_brt(dt, "width", maxX = 25) # slight pattern 
trait_plot_brt(dt, "thickness", maxX = 5) # no pattenr
trait_plot_brt(dt, "adnate") # no pattern 
trait_plot_brt(dt, "Mobile") # no pattern 
trait_plot_brt(dt, "Pioneer") # no pattern 
trait_plot_brt(dt, "pedunculate") # no pattern 
trait_plot_brt(dt, "pad") # no pattern 
trait_plot_brt(dt, "stalk2") # no pattern 
trait_plot_brt(dt, "Colonial") # no pattern 

# compute diversity  ----------------------------------------------------------------

## prepare data 
bio_data <- 
        select(dt, gr_sample_id, lowest.taxon) |> 
        mutate(abundance = 1) |> 
        tidyr::pivot_wider(
                id_cols = gr_sample_id, 
                names_from = lowest.taxon, 
                values_from = abundance, 
                values_fill = 0
        ) |> 
        select(!gr_sample_id)


dt[, test2 := ]

trait_data <- 
        select(dt, guild, size_class, Pioneer,  lowest.taxon)  |> 
        tidyr::pivot_longer(cols = !lowest.taxon, names_to = "trait", values_to = "modality") |> 
        unique(by = c("lowest.taxon", "trait")) |> 
        tidyr::pivot_wider(id_cols = lowest.taxon, 
                           names_from = trait, 
                           values_from = modality)

plot(trait_data$length, trait_data$biovolume)
plot(trait_data$biovolume)
plot(trait_data$thickness)

trait_data[which(trait_data$length > 500), ]


## standardize traits 
trait_data <- as.data.frame(trait_data)
rownames(trait_data) <- trait_data$lowest.taxon
trait_data <- trait_data[,-1]

tr_cat_arg <- data.frame(trait_name = names(trait_data), 
                         trait_type = c("Q", "Q", "Q"),
                         trait_weight = c(1,1,1),
                         fuzzy_name = c(NA, NA, NA) 
                         )



dist <- funct.dist(sp_tr = trait_data, 
           tr_cat = tr_cat_arg, 
           metric = "euclidean", 
           scale_euclid = "center")

raq <- alpha.fd.hill(asb_sp_w = as.matrix(bio_data), 
                     sp_dist = dist, 
                     q = 1, 
                     tau = "max")
raq$asb_FD_Hill
test <- metaMDS(dist)
plot(test)
# save data -------------------------------------------