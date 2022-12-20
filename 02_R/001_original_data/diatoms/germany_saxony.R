# ————————————————————————————————————————————————— #
# ——— Clean diatoms data from Germany: Saxony ————— # 
# ————————————————————————————————————————————————— #

# ———————————————————————————————————
# date first written: 22-01-13
# date last modified: 10.06.22
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: In this script I create a harmonized spatial data set from the raw data provided 
# from Germany: Saxony. 
# Temporal aggregation: NO
# CRS: ETRS89 / UTM zone 33N -> EPSG: 25833
# ————————————————

# setup -----------------------------------------------------------------------------
#devtools::install_github("https://github.com/jonjup/jjmisc")
library(data.table)
library(dplyr)
library(fs)
library(lubridate)
library(mapview)
library(magrittr)
library(sf)
library(stringr)
library(stringdist)
library(readr)
library(readxl)
library(tidyr)

source("R/functions/harmonize diatoms.R")
source("R/functions/add_typologies.R")

# load data -------------------------------------------------------------------------

## directory with all biological data 
bio_wd <- "data/diatoms/original_data/germany_saxony/raw/Artenliste-Diatomeen_BWPL_2021/"
file_list <- dir_ls(bio_wd)

# read in data 
n <- length(file_list)
for (i in 1:n){
        if (i == 1) save_list <- list()
        i.data <- fread(file_list[i])
        #- subset and rename 
        i.data <- i.data[, .(original_site_name = MKZ,
                             taxon = Artbezeichnung,
                             date = Probe,
                             EPSG = 25833,
                             x.coord = Ostwert.ETRS,
                             y.coord = Nordwert.ETRS,
                             abundance = absolute.Abundanz)]
        #- fix date 
        i.data[,date := str_remove(date, ".*/")]
        i.data[,date := ymd(date)]
        save_list[[length(save_list) + 1]] <- i.data
        rm(i.data)
}

data <- rbindlist(save_list)

rm(i,n,file_list, bio_wd, save_list)

# dia1 <- readRDS("data/diatoms/fwb_table.rds")
# dia2 <- readRDS("data/diatoms/omn_table.rds")

# prepare data ----------------------------------------------------------------------

#- add season and year
data[,c("year", "season") := .(year(date), 
                               case_when(month(date) %in% c(12,1,2)  ~ "winter",
                                         month(date) %in% c(3,4,5)   ~ "spring",
                                         month(date) %in% c(6,7,8)   ~ "summer",
                                         month(date) %in% c(9,10,11) ~ "autumn"))]
#- check on map 
sites <- unique(data, by = "original_site_name")
sites2 <- st_as_sf(sites, coords = c("x.coord", "y.coord"), crs = sites$EPSG[1])
mapview(sites2)


# harmonize taxonomy ----------------------------------------------------------------

taxontable <- readRDS("data/diatoms/2022-05-17_taxontable_diatoms.rds")

data <- data[!taxon %in% c("Pennales")]

TU <- sort(unique(data$taxon))
TU <- setdiff(TU, taxontable$original_name)

# finish data  ----------------------------------------------------------------------


data <- rename(data, original_name = taxon)
setDT(data)
data2 <- taxontable[data, on = "original_name"]  

## check
sort(unique(data2$phylum))
sort(unique(data2$class))

## add site and date ids
data2[, site_id := .GRP, by = "original_site_name"]
data2[, date_id := .GRP, by = "date"]

## add leading zeros
data2[, site_id := case_when(
        nchar(trunc(site_id)) == 1 ~ paste0("0000", site_id),
        nchar(trunc(site_id)) == 2 ~ paste0("000", site_id),
        nchar(trunc(site_id)) == 3 ~ paste0("00", site_id),
        nchar(trunc(site_id)) == 4 ~ paste0("0", site_id),
        nchar(trunc(site_id)) == 5 ~ paste0(site_id))]
data2[, date_id := case_when(
        nchar(trunc(date_id)) == 1 ~ paste0("0000", date_id),
        nchar(trunc(date_id)) == 2 ~ paste0("000",  date_id),
        nchar(trunc(date_id)) == 3 ~ paste0("00",   date_id),
        nchar(trunc(date_id)) == 4 ~ paste0("0",    date_id),
        nchar(trunc(date_id)) == 5 ~ paste0(        date_id))]

## add gr_sample_id
data2[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_germany_saxony_diatoms")]


## reshape data
data3 <- data2[, list(
        gr_sample_id,
        original_site_name,
        date,
        year,
        season,
        site_id,
        date_id,
        original_name,
        species,
        genus,
        family,
        order,
        class,
        phylum,
        kingdom,
        abundance,
        x.coord,
        y.coord,
        EPSG,
        data.set = "germany_saxony_diatom"
)]

## combine entries of same taxon
data3[, lowest.taxon := ifelse(!is.na(species), species,
                               ifelse(!is.na(genus), genus,
                                      ifelse(!is.na(family), family,
                                             ifelse(!is.na(order), order,
                                                    ifelse(!is.na(class), class,
                                                           ifelse(!is.na(phylum), phylum, kingdom))))))]

data3[, abundance := as.numeric(abundance)]
data3[, abundance := sum(abundance), by = c("gr_sample_id", "lowest.taxon")]
data3 <- unique(data3, by = c("gr_sample_id", "lowest.taxon"))
data4 <- copy(data3)
typologies <- readRDS("data/all_typologies.rds")
data5 <- add_typologies(data4)

## visual checks
sites <- unique(data5, by = "site_id") |> st_as_sf(coords = c("x.coord", "y.coord"), crs = data5$EPSG[1])

mapview(sites, zcol = "brt12")
mapview(sites, zcol = "ife")
mapview(sites, zcol = "bgr")
mapview(sites, zcol = "least.impacted")

# - subset to least impacted catchments 
data6 <- data5[least.impacted == TRUE]

sites <- unique(data6, by = "site_id") |> st_as_sf(coords = c("x.coord", "y.coord"), crs = data5$EPSG[1])

## look for sites with different ID but same coordinates 
distances  <- st_distance(sites)
distances2 <- as.matrix(distances)
diag(distances2) <- 999
(duplicate_sites <- which(distances2 < units::as_units(1, "m")))

# - drop sites with very few species 
data6[, richness := uniqueN(lowest.taxon), by = "gr_sample_id"]
data7 <- data6[richness > 10]

# - drop sites far removed from ECRINS river network 
data8 <- data7[distance < 300]
rl <- readRDS("data/diatoms/original_data/germany_saxony/2022-01-25_remove_list.rds")
data8 <- data8[!site_id %in% rl]
# - visually check the assignment of sites 

rt <- 
        data8 |> 
        unique(by = "site_id") |> 
        st_as_sf(coords = c("x.coord", "y.coord"), 
                 crs = data5$EPSG[1])

plot_typology <- st_crop(typologies, st_transform(sites, crs = st_crs(typologies)))
options(warn = -1)
updated_type <- data.table(site_id = rt$site_id)
for (i in 1:nrow(rt)) {
        
        i.rt <- rt[i,]
        buff_dist <- 1000
        i.plot_typology <-
                st_crop(plot_typology,
                        st_buffer(st_transform(i.rt, crs = st_crs(typologies)), dist =  buff_dist))
        while(nrow(i.plot_typology) == 0){
                buff_dist <- buff_dist + 1000
                i.plot_typology <-
                        st_crop(plot_typology,
                                st_buffer(st_transform(i.rt, crs = st_crs(typologies)), dist =  buff_dist))
        }
        x <-
                mapview(i.plot_typology,
                        zcol = "brt",
                        map.type = "OpenStreetMap.DE") + mapview(i.rt,
                                                                 popup = c("water_body"),
                                                                 color = "red")
        print(x)
        #i.bool <- "n"
        i.bool <- readline(paste(i, "/", nrow(rt), ":"))
        if (i.bool == "break") {
                break()
        } else if (i.bool == "n") {
                updated_type[site_id == i.rt$site_id, new_type := "drop"]
        } else if (i.bool == "c") {
                i.towhat <- readline("change to:")
                updated_type[site_id == i.rt$site_id, new_type := i.towhat]
        } else {
                updated_type[site_id == i.rt$site_id, new_type := i.rt$brt12]
        }
        
        rm(list = ls()[grepl("i\\.", ls())])
}

#- save the remove list. 
saveRDS(updated_type, paste0("data/diatoms/original_data/germany_saxony/", Sys.Date(), "_updated_type.rds"))

#- drop remove sites 
data9 <- left_join(data8, 
                   updated_type, 
                   by = "site_id")
data9 <- data9[new_type != "drop"]
data9[, brt12 := NULL]
data9 <- rename(data9, brt12 = new_type)

agg <- data9 |> unique(by = "gr_sample_id")
unique(table(agg$site_id))
data10 <- data9[month(date) %in% 5:9]
saveRDS(data10, paste0("data/diatoms/original_data/germany_saxony/",Sys.Date(),"_final_aggregated.rds"))
#data10 <- readRDS("data/diatoms/original_data/spain_goma/2022-01-25_final_aggregated.rds")

# time span
summary(data10$year)
# all sites and samples
uniqueN(data5$site_id)
uniqueN(data5$gr_sample_id)
# least impacted sites and samples
uniqueN(data6$site_id)
uniqueN(data6$gr_sample_id)
# no sites with <10 taxa 
uniqueN(data7$site_id)
uniqueN(data7$gr_sample_id)
# only close sites 
uniqueN(data8$site_id)
uniqueN(data8$gr_sample_id)
# no sites that can not definitively be assigned to a river segment
uniqueN(data9$site_id)
uniqueN(data9$gr_sample_id)
uniqueN(data10$gr_sample_id)
# mean richness: 
unique(data10, by = "gr_sample_id") |> pull(richness) |> mean()