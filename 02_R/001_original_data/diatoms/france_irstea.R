# ——————————————————————————————————————————————— #
# ——— Clean Diatom data from IRSTEA - FRANCE  ——— # 
# ——————————————————————————————————————————————— #

# ———————————————————————————————————
# created: 21-12-20
# modified: 15.06.22
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: In this script I create a harmonized spatial data set of diatom from the 
#          raw data from IRSETA - FRANCE.   
# CRS: Lambert-93 transformation projected system; EPSG: 2154
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
library(readxl)
library(tidyr)

source("R/functions/harmonize diatoms.R")
source("R/functions/add_typologies.R")
# load data -------------------------------------------------------------------------
## directory with all biological data 
bio_wd <- "data/diatoms/original_data/french_irstea/raw/French_data_for_Jupke.RData"

load(bio_wd)

# dia1 <- readRDS("data/diatoms/fwb_table.rds")
# dia2 <- readRDS("data/diatoms/omn_table.rds")
# non_diatom_algae <- readRDS("data/diatoms/2021-12-08_non_diatom_algae.rds")


# prepare data  ---------------------------------------------------------------------

## check the three elements in "French_data_for_Jupke.RData": comm, env, taxa

## Communities - each column is one taxon each row one site. Elements are abundances and 
## rownames are site ids. Taxons are given as Omnidia codes. 
str(comm)

## Translation table from taxoncode to Omnindia code. 
## Aso includes Passy guildes 
str(taxa)

## Environmental variables incl. coordinates.  
str(env)

## env does not contain site ids. If comm and env have the same number of rows (TRUE) then I presume the order matches. 
## I will add an ID column 
nrow(env) == nrow(comm)

env$ID = 1:nrow(env)

## reshape comm 
data <- 
        comm |> 
        mutate(ID = 1:n()) |> 
        pivot_longer(cols = !ID, names_to = "taxon", values_to = "abundance") |> 
        filter(abundance != 0) |> 
        left_join(taxa, by = "taxon") |> 
        select(ID, taxon = Nom_complet, abundance) |> 
        mutate(taxon = as.character(taxon)) |> 
        mutate(taxon = str_trim(taxon)) |> 
        left_join(env, by = "ID") |> 
        select(ID, taxon, abundance, x.coord = X, y.coord = Y, date = date_opecont) |> 
        mutate(date = ymd(date), 
               EPSG = 2154, 
               data.set = "france_irstea_diatoms")

# taxonomic harmonization -----------------------------------------------------------

taxontable <- readRDS("data/diatoms/2022-05-17_taxontable_diatoms.rds")
setDT(data)
data <- data[!taxon %in% c("Centric Diatoms Diatomées centriques indifférenciées", 
                           "Centriques indifférenciées in TDI3 Kelly",
                           "Diatomée anormale Abnormal diatom valve (unidentified) or sum of deformities abundance",
                           "Diatomées non identifiées vue connectives"
                           )]

TU <- sort(unique(data$taxon))
(TU <- setdiff(TU, taxontable$original_name))

# - join 
data <- rename(data, original_name = taxon)
setDT(data)
data2 <- taxontable[data, on = "original_name"]  

## check
sort(unique(data2$phylum))
sort(unique(data2$class))

## add site and date ids
data2%<>%rename(original_site_name = ID)
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
data2[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_france_irstea_diatoms")]

data2[,c("year", "season") := .(year(date), 
                                case_when(month(date) %in% c(12,1,2)  ~ "winter",
                                          month(date) %in% c(3,4,5)   ~ "spring",
                                          month(date) %in% c(6,7,8)   ~ "summer",
                                          month(date) %in% c(9,10,11) ~ "autumn"))]

data2 <- data2[!is.na(original_name)]

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
        data.set
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
# sites <- unique(data5, by = "gr_sample_id") |> st_as_sf(coords = c("x.coord", "y.coord"), crs = data5$EPSG[1])
# 
# mapview(sites, zcol = "brt12")
# mapview(sites, zcol = "ife")
# mapview(sites, zcol = "bgr")
# mapview(sites, zcol = "least.impacted")

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

# - check duplicate sites with NAIADES 
naiades_data <- readRDS("data/diatoms/original_data/french_naiades/2022-06-10_final_aggregated.rds")
naiades_sites <- unique(naiades_data, by = "site_id") |> st_as_sf(coords = c("x.coord", "y.coord"), crs = naiades_data$EPSG[1])
irstea_sites <- unique(data8, by = "site_id")|> st_as_sf(coords = c("x.coord", "y.coord"), crs = data8$EPSG[1])


nn <- st_nearest_feature(irstea_sites, naiades_sites)
distances <- st_distance(irstea_sites, naiades_sites[nn,],by_element = TRUE)
dropID <- which(distances < units::as_units(1, "m"))

data8 <- data8[-dropID, ]

# - visually check the assignment of sites 

rl <- readRDS("data/diatoms/original_data/french_irstea/2022-01-21_remove_list.rds")
data8 <- data8[!site_id %in% rl]
# cl <- readRDS("data/diatoms/original_data/french_irstea/2022-01-21_connection_list.rds")
# 
# rt <- 
#         data8 |> 
#         filter(!site_id %in% cl$site_id) |> 
#         unique(by = "site_id") |> 
#         st_as_sf(coords = c("x.coord", "y.coord"), 
#                  crs = data5$EPSG[1])
# 
# plot_typology <- st_crop(typologies, st_transform(sites, crs = st_crs(typologies)))
# options(warn = -1)
# updated_type <- data.table(site_id = rt$site_id)

# for (i in 1:nrow(rt)) {
#         i.rt <- rt[i,]
#         i.plot_typology <-
#                 st_crop(plot_typology,
#                         st_buffer(st_transform(i.rt, crs = st_crs(typologies)), dist =  2000))
#         x <-
#                 mapview(i.plot_typology,
#                         zcol = "brt",
#                         map.type = "OpenStreetMap.DE") + mapview(i.rt,
#                                                                  popup = c("water_body"),
#                                                                  color = "red")
#         print(x)
#         #i.bool <- "n"
#         i.bool <- readline(paste(i, "/", nrow(rt), ":"))
#         if (i.bool == "break") {
#                 break()
#         } else if (i.bool == "n") {
#                 updated_type[site_id == i.rt$site_id, new_type := "drop"]
#         } else if (i.bool == "c") {
#                 i.towhat <- readline("change to:")
#                 updated_type[site_id == i.rt$site_id, new_type := i.towhat]
#         } else {
#                 updated_type[site_id == i.rt$site_id, new_type := i.rt$brt12]
#         }
#         
#         rm(list = ls()[grepl("i\\.", ls())])
# }

# utc <- bind_rows(cl, updated_type)
# - save the remove list. 
saveRDS(utc, paste0("data/diatoms/original_data/french_irstea/", Sys.Date(), "_updated_list_combi.rds"))
utc <- readRDS("data/diatoms/original_data/french_irstea/2022-06-10_updated_list_combi.rds")
utc[is.na(new_type), new_type := brt12]
utc[, brt12 := NULL]
#- drop remove sites 
data9 <- left_join(data8, 
                   utc, 
                   by = "site_id")

#- drop "drop" rows determined in for-loop
data9 <- data9[new_type != "drop"]
data9[, brt12 := NULL]
data9 <- rename(data9, brt12 = new_type)

# temporal aggregation --------------------------------------------------------------
agg <- data9 |> unique(by = "gr_sample_id")
unique(table(agg$site_id))
# no 
data10 <- data9[month(date) %in% 5:9]
#source("R/functions/newest_sample.R")
saveRDS(data10, paste0("data/diatoms/original_data/french_irstea/",Sys.Date(),"_final_aggregated.rds"))
#data10 <- readRDS("data/diatoms/original_data/french_naiades/2022-01-21_final_aggregated.rds")

# statistics -------------------------------------------------------------------------
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
unique(data8, by = "gr_sample_id") |> pull(richness) |> mean()
