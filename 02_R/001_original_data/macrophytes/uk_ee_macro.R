# —————————————————————————————————————————————————————————— #
# ——— Clean macrophyte data from UK Environment Explorer ——— # 
# —————————————————————————————————————————————————————————— #

# ———————————————————————————————————
# date first written: 29-11-21
# date last modified: 14.06.22
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: In this script I create a harmonized spatial data set of macrophytes from the 
# raw data provided by the Environment Exporer, UK. 
# CRS: OSGB 1936 / British National Grid - United Kingdom Ordnance Survey; EPSG: 27700
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
library(readxl)

source("R/functions/add_typologies.R")
source("R/functions/dfm.R")

# load data -------------------------------------------------------------------------
## directory with all biological data 
bio_wd <- "data/macrophytes/original_data/uk_ee/raw/MACP_OPEN_DATA_TAXA_2021-11-12.csv"
sit_wd <- "data/macrophytes/original_data/uk_ee/raw/MACP_OPEN_DATA_SITE_2021-11-12.csv"

bio <- fread(bio_wd)
sit <- fread(sit_wd)

taxontable <- readRDS("data/macrophytes/2022-06-14_taxontable_macrophytes.rds")
typologies <- readRDS("data/all_typologies.rds")

rm(bio_wd, sit_wd)
# prepare data ----------------------------------------------------------------------

## sampling method 
table(bio$SAMPLE_METHOD_DESCRIPTION)
bio <- bio[SAMPLE_METHOD_DESCRIPTION != "MACROPHYTE: ECN Survey"]

table(bio$SAMPLE_TYPE_DESCRIPTION)
table(bio$ANALYSIS_TYPE_DESCRIPTION)

bio <- bio[ANALYSIS_TYPE_DESCRIPTION == "FIELD ANALYSIS"]

bio <- bio[,c("SITE_ID", "SAMPLE_DATE", "TAXON_NAME")]
names(bio) <- c("original_site_name", "date", "taxon")
sit <- sit[, c("SITE_ID", "FULL_EASTING", "FULL_NORTHING")]
names(sit) <- c("original_site_name", "x.coord", "y.coord")
data <- sit[bio, on = "original_site_name"]

## add season 
data[,c("year", "season") := .(year(date), 
                               case_when(month(date) %in% c(12,1,2)  ~ "winter",
                                         month(date) %in% c(3,4,5)   ~ "spring",
                                         month(date) %in% c(6,7,8)   ~ "summer",
                                         month(date) %in% c(9,10,11) ~ "autumn"))]
## remove sites without coordinates
data <- data[!is.na(x.coord)]
data <- data[!is.na(y.coord)]
data <- data[!is.na(taxon)]

data[, EPSG := 27700]
sites <- unique(data, by = "original_site_name")
sites2 <- st_as_sf(sites, coords = c("x.coord", "y.coord"), crs = data$EPSG[1])
mapview(sites2)

## —— Inspect taxa ———————————————————————————————————————————————————————————————————————
data <- 
        data[!taxon %in% c("Algae", 
                           "Blue-green algal scum / pelts", 
                           "Diatoms" ,
                           "Dreissena bugensis",
                           "Fern",                                                     
                           "Filamentous green algae" ,
                           "Gelatinous algae (lake macrophyte surveys only)",
                           "Lamperta",
                           "Monocotyledon",
                           "Moss" ,
                           "Pacifastacus leniusculus",
                           "Pennate diatoms",
                           "Porifera",
                           "Sponge",
                           "Spongilla"   ,                                             
                           "Spongillidae" ,
                           "Terrestrial grass",                                        
                           "Terrestrial herb",
                           "Unidentified colony"  ,                                    
                           "Unidentified filament",                                    
                           "Unidentified other" ,                                      
                           "Unidentified pennate diatoms",                             
                           "Unidentified unbranched filamentous non-slimy green algae"
                           )
             ]


(TU <- unique(data$taxon) |> sort())
(TU <- setdiff(TU,taxontable$original_name))

# taxontable <- update_taxonomy2(TU)
# 
# taxontable[clean == FALSE]
# taxontable <- fill_taxon_table(o = "Glyceria declinata x G. fluitans",
#                                s = "Glyceria declinata", 
#                                g = "Glyceria", 
#                                f = "Poaceae",
#                                or = "Poales",
#                                c = "Liliopsida", 
#                                p = "Tracheophyta", 
#                                k = "Plantae")
# taxontable <- fill_taxon_table(o = "Ranunculus circinatus x R. penicillatus var. calcareus",
#                                s = "Ranunculus circinatus", 
#                                g = "Ranunculus", 
#                                f = "Ranunculaceae",
#                                or = "Ranunculales",
#                                c = "Magnoliopsida", 
#                                p = "Tracheophyta", 
#                                k = "Plantae")
# taxontable <- fill_taxon_table(o = "Urtica",
#                                g = "Urtica", 
#                                f = "Urticaceae",
#                                or = "Rosales",
#                                c = "Magnoliopsida", 
#                                p = "Tracheophyta", 
#                                k = "Plantae")
# taxontable[original_name == "Urtica"]
# taxontable <- taxontable[original_name != "Monocotyledon"]
# taxontable[, clean := TRUE]
# 
# saveRDS(taxontable, paste0("data/macrophytes/",Sys.Date(),"_taxontable_macrophytes.rds"))

names(data)[which(names(data) == "taxon")] <- "original_name"
data2 <- taxontable[data, on = "original_name"]

sort(unique(data2$kingdom))

data2[kingdom == "Chromista", unique(original_name)]
data2 <- data2[kingdom == "Plantae"]
sort(unique(data2$phylum))
sort(unique(data2$class))
sort(unique(data2$subclass))
sort(unique(data2$order))

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
data2[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_uk_ee_macrophytes")]

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
        subclass,
        class,
        phylum,
        kingdom,
        abundance = NA,
        x.coord,
        y.coord,
        EPSG,
        data.set = "uk_ee_macrophytes"
)]

## combine entries of same taxon 
data3[, lowest.taxon := ifelse(!is.na(species), species,
                               ifelse(!is.na(genus), genus,
                                      ifelse(!is.na(family), family,
                                             ifelse(!is.na(order), order,
                                                    ifelse(!is.na(subclass), subclass,
                                                           ifelse(!is.na(class), class,
                                                                  ifelse(!is.na(phylum), phylum, kingdom)))))))]

#data3[, abundance := sum(abundance), by = c("gr_sample_id", "lowest.taxon")]
data3 <- unique(data3, by = c("gr_sample_id", "lowest.taxon"))
data4 <- copy(data3)
data5 <- add_typologies(data4)

# - visual checks 
sites <- unique(data5, by = "gr_sample_id") |> st_as_sf(coords = c("x.coord", "y.coord"), crs = data5$EPSG[1])

mapview(sites, zcol = "brt12")
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
data7 <- data6#[richness > 10]

# - drop sites far removed from ECRINS river network 
data8 <- data7[distance < 300]
rl <- readRDS("data/macrophytes/original_data/uk_ee/2022-01-27_remove_list.rds")
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

for (i in 1:nrow(rt)){
        i.rt <- rt[i, ]
        i.plot_typology <- st_crop(plot_typology, st_buffer(st_transform(i.rt, crs = st_crs(typologies)), dist =  2000))
        x <- mapview::mapview(i.plot_typology, 
                              zcol = "brt"#,
                              #map.type = "OpenStreetMap.DE"
        ) + mapview::mapview(i.rt, color = "red")
        print(x)
        i.bool <- "n"
        i.bool <- readline(paste(i, ":"))
        if (i.bool == "break"){
                break()
        }
        if (i.bool == "n"){
                # remove_list[length(remove_list) + 1] <- i.rt$site_id 
                updated_type[site_id == i.rt$site_id, new_type := "drop"]
        } else if (i.bool == "c"){
                i.towhat <- readline("change to:")
                updated_type[site_id == i.rt$site_id, new_type := i.towhat]
        } else {
                updated_type[site_id == i.rt$site_id, new_type := i.rt$brt12]
        }
        
        rm(list = ls()[grepl("i\\.", ls())])
}

#- save the remove list. 
saveRDS(updated_type, paste0("data/macrophytes/original_data/uk_ee/", Sys.Date(), "_updated_type.rds"))

data9 <- left_join(data8, 
                      updated_type, 
                      by = "site_id")

#- drop "drop" rows determined in for-loop      
data9 <- data9[new_type != "drop"]
data9[, brt12 := NULL]
data9 <- rename(data9, brt12 = new_type)

data9 <- data9[species != ""]
data9$brt12 |> unique()

# temporal aggregation --------------------------------------------------------------
agg <- data9 |> unique(by = "gr_sample_id")
unique(table(agg$site_id))
# - yes 
source("R/functions/newest_sample.R")
data10 <- data9[month(date) %in% 5:9]
data10 <- newest_sample(data10, season_available = FALSE)
saveRDS(data10, paste0("data/macrophytes/original_data/uk_ee/",Sys.Date(),"_final_aggregated.rds"))

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
unique(data10, by = "gr_sample_id") |> pull(richness) |> mean()