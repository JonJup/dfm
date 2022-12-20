# ————————————————————————————————————————————————————————— #
# ——— Clean Diatom data from Germany - Landau data  ——————— # 
# ————————————————————————————————————————————————————————— #

# ———————————————————————————————————
# date first written: 22-01-12
# date last modified: 11.06.22
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: In this script I create a harmonized spatial data set of diatom from the 
# raw data from Germany - Landau data 
# CRS: 31463 -- DHDN / 3-degree Gauss zone 3 
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
# load data ----------------------------------------------------------------------
bio_wd <- "data/diatoms/original_data/germany_landau/raw/diatom_samples.csv"
sit_wd <- "data/diatoms/original_data/germany_landau/raw/diatom_sites.csv"

bio <- fread(bio_wd) 
sit <- fread(sit_wd)

# dia1 <- readRDS("data/diatoms/fwb_table.rds")
# dia2 <- readRDS("data/diatoms/omn_table.rds")
# non_diatom_algae <- readRDS("data/diatoms/2021-12-08_non_diatom_algae.rds")
# prepare data ----------------------------------------------------------------------

# 02. Data Cleaning -----------------------------------------------------

#remove rows with zero abundance 
bio = bio[iz_n != 0]

## drop columns  
bio = bio[, c("date", "taxon", "site_id", "iz_n") ]

# In samples the sites with TH in their site_id seem to have mistakes. All TH ..
# sites in the samples data go like TH_TH_xxxx while those in the sites data go
# TH_xxxx. Hence I remove the first TH 
bio$site_id = str_replace_all(bio$site_id, "TH_TH_", "TH_")
sit = sit[, c("site_id", "stream", "site_name", "geom")]

#fix site coordinates 
#Geometry type PostgreSQL columns They can be converted with sf see
#https://github.com/r-dbi/RPostgres/issues/114 This converts the geom column
#which holds Postgres Geom codes to xy coordinates and also returns the
#projection.
coord <- st_as_sfc(
        structure(
                sit$geom, 
                class = "WKB"
        ),
        EWKB = TRUE
)
coord2 = st_coordinates(coord) %>% data.frame()
sites2  <- bind_cols(
        sit,
        coord2,
        EPSG = rep(31463, nrow(sit))
) 
sites2 = sites2[,-c("geom")]
# join data sets 
data = left_join(bio, sites2) %>% setDT
data <- rename(data, original_site_name = site_id)
data <- rename(data, abundance = iz_n)
data <- rename(data, x.coord = X)
data <- rename(data, y.coord = Y)

# fix date column
data[,c("date","year", "month") := list(ymd(date), year(date), month(date))]
data[,"season" := ifelse(month %in% c(12,1,2), "winter", ifelse(month %in% c(3,4,5), "spring", ifelse(month %in% c(6,7,8), "summer", "autumn")))]

# check on map 
data <- data[!is.na(x.coord)]
sites <- unique(data, by = c("original_site_name"))
sites <- st_as_sf(sites, coords = c("x.coord", "y.coord"), crs = data$EPSG[1])
mapview(sites)

# harmonize taxa --------------------------------------------------------------------
taxontable <- readRDS("data/diatoms/2022-05-17_taxontable_diatoms.rds")

TU <- sort(unique(data$taxon))
TU <- setdiff(TU, taxontable$original_name)

## what is the most similar in in taxontable 
strdist_id  <- amatch(TU, taxontable$original_name, maxDist = 100000)
strdist_tbl <- data.table(taxontable_name = taxontable$original_name[strdist_id], TU) |> 
        filter(!is.na(taxontable_name)) 

for (i in 1:nrow(strdist_tbl)){
        
        i.1 <- pull(strdist_tbl[i,1])
        i.2 <- pull(strdist_tbl[i,2])
        
        print(i.1)
        print(i.2)
        
        i.bool <- readline("match?:")
        
        if (i.bool == "y"){
                taxontable <- append_to_tt(i.1,i.2)
        } else if (i.bool == "n") {
                next()
        }
        rm(list = ls()[grepl("^i", ls())])
}
TU <- unique(data$taxon) |> sort()
TU <- setdiff(TU, taxontable$original_name)

for (i in seq_along(TU)) {
        i.tu  <- TU[i]
        i.det <- str_detect(dia1$taxon_old, i.tu)
        if (any(i.det)) {
                i.id <- which(i.det)
                print(paste("name:", i.tu))
                print(paste("suggestions:", dia2$taxon[i.id]))
                i.rl <- readline()
        } else {
                i.id <- amatch(i.tu, dia1$taxon_old, maxDist = 100000) 
                if (is.null(i.id))
                        next()
                print(paste("name:", i.tu))
                print(paste("suggestions:", dia1$taxon_old[i.id]))
                i.rl <- readline()
        }        
        if (i.rl != "break") {
                i.id <- i.id[as.numeric(i.rl)]
                ## is it a synonym?
                print(paste("Final name: ", dia1$taxon_new[i.id]))
                i.final <- readline()
                ## check that against fwb
                taxontable <- add_entry_tt(i.final)
        }
        rm(list = ls()[grepl(pattern = "^i\\.", x = ls())])
}

TU <- unique(data$taxon) |> sort()
TU <- setdiff(TU, taxontable$original_name)

for (i in seq_along(TU)) {
        i.tu  <- TU[i]
        i.det <- str_detect(dia2$taxon, i.tu)
        if (any(i.det)) {
                i.id <- which(i.det)
                print(paste("name:", i.tu))
                print(paste("suggestions:", dia2$taxon[i.id]))
                i.rl <- readline()
        } else {
                i.id <- amatch(i.tu, dia2$taxon, maxDist = 100000) 
                if (is.null(i.id))
                        next()
                print(paste("name:", i.tu))
                print(paste("suggestions:", dia2$taxon[i.id]))
                i.rl <- readline()
        }        
        if (i.rl != "break") {
                i.id <- i.id[as.numeric(i.rl)]
                ## is it a synonym?
                if (!is.na(dia2$new[i.id])) {
                        print(paste("new code:",
                                    dia2$new[i.id]))
                        ## enter new code
                        i.rl2 <- readline()
                        i.id <- which(dia2$code == i.rl2)
                }
                print(paste("Final name: ", dia2$taxon[i.id]))
                i.final <- readline()
                ## check that against fwb
                if (check_fwb(i.final)) {
                        i.final <- get_fwb(i.final)
                }
                taxontable <- add_entry_tt(i.final)
        }
        rm(list = ls()[grepl(pattern = "^i\\.", x = ls())])
}
taxontable <- append_to_tt("Pinnularia dubitabilis", "Pinnularia borealis var. rectangularis")

TU <- unique(data$taxon) |> sort()
TU <- setdiff(TU, taxontable$original_name)


# saveRDS(taxontable, paste0("data/diatoms/",Sys.Date(),"_taxontable_diatoms.rds"))

# join 
data <- rename(data, original_name = taxon)
setDT(data)
data2 <- taxontable[data, on = "original_name"]  

## check
sort(unique(data2$phylum))
sort(unique(data2$class))

## add site and date ids
data2[, c("x.coord_round", "y.coord_round") := .(round(x.coord, 5),
                                                round(y.coord, 5))]

data2[, site_id := .GRP, by = c("x.coord_round", "y.coord_round")]
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
data2[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_germany_landau_diatoms")]


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
        data.set = "germany_landau_diatom"
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
sites <- unique(data5, by = "gr_sample_id") |> st_as_sf(coords = c("x.coord", "y.coord"), crs = data5$EPSG[1])
# mapview(sites, zcol = "brt12")
# mapview(sites, zcol = "ife")
# mapview(sites, zcol = "bgr")
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
rl <- readRDS("data/diatoms/original_data/germany_landau/2022-01-19_remove_list.rds")
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
        i.plot_typology <-
                st_crop(plot_typology,
                        st_buffer(st_transform(i.rt, crs = st_crs(typologies)), dist =  2000))
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
saveRDS(updated_type, paste0("data/diatoms/original_data/germany_landau/", Sys.Date(), "_updated_type.rds"))

#- join updated types to data 8
data9 <- left_join(data8, 
                   updated_type, 
                   by = "site_id")

#- drop "drop" rows determined in for-loop
data9 <- data9[new_type != "drop"]
data9[, brt12 := NULL]
data9 <- rename(data9, brt12 = new_type)

# temporal aggregation --------------------------------------------------------------
agg <- data9 |> unique(by = "gr_sample_id")
unique(table(agg$site_id))
# - yes
data10 <- data9[month(date) %in% 5:9]
source("R/functions/newest_sample.R")
data10 <- newest_sample(data10, season_available = FALSE)

saveRDS(data10, paste0("data/diatoms/original_data/germany_landau/",Sys.Date(),"_final_aggregated.rds"))
# data10 <- readRDS("data/diatoms/original_data/germany_landau/2022-01-19_final_aggregated.rds")

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





