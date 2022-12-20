# ————————————————————————————————————————— #
# ——— Clean fish data from Germany: Saarland ——— # 
# ————————————————————————————————————————— #

# ———————————————————————————————————
# date first written: 21-12-08
# date last modified: 22-01-26
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: In this script I create a harmonized spatial data set from the raw data  
# from Germany: Saarland. 
# Temporal aggregation:  yes
# CRS: DHDN / 3-degree Gauss-Kruger zone 2 ; EPSG: 31466
# ————————————————


# setup -----------------------------------------------------------------------------
#devtools::install_github("https://github.com/jonjup/jjmisc")
library(data.table)
library(dplyr)
library(jjmisc)
library(lubridate)
library(mapview)
library(sf)
library(stringr)
library(readxl)

# load data -------------------------------------------------------------------------
bio_wd  <- "data/fish/original_data/germany_Saarland/raw/Daten_SAL_2013-2018.xlsx"

bio     <- read_excel(bio_wd, sheet = 3) |> setDT()
sit     <- read_excel(bio_wd, sheet = 4) |> setDT()

# prepare data ----------------------------------------------------------------------

bio2 <- 
        data.table(
                original_site_name = bio$`Messst-Nr`,
                date = dmy(bio$`Datum Probenahme`), 
                taxon = bio$`Taxon (latein)`,
                abundance = bio$IZ_n
        )

sit2 <- 
        data.table(
                original_site_name = sit$`Messst-Nr.`,
                x.coord = sit$`R-Wert`,
                y.coord = sit$`H-Wert`,
                EPSG    = 31466
        )

data <- sit2[bio2, on = "original_site_name"]

## add season 
data[,c("year", "season") := .(year(date), 
                               case_when(month(date) %in% c(12,1,2)  ~ "winter",
                                         month(date) %in% c(3,4,5)   ~ "spring",
                                         month(date) %in% c(6,7,8)   ~ "summer",
                                         month(date) %in% c(9,10,11) ~ "autumn"))]
## remove sites without coordinates
data <- data[!is.na(x.coord)]
data <- data[!is.na(taxon)]
data <- data[taxon != ""]

sites <- unique(data, by = "original_site_name")
sites2 <- st_as_sf(sites, coords = c("x.coord", "y.coord"), crs = data$EPSG[1])
mapview(sites2)


## —— Inspect taxa ———————————————————————————————————————————————————————————————————————
taxontable <- readRDS("data/fish/2022-01-12_taxontable_fish.rds")
(TU <- unique(data$taxon) |> sort())
TU <- setdiff(TU,taxontable$original_name)

# taxontable <- update_taxonomy2(TU)
# taxontable[clean == FALSE]
# saveRDS(taxontable, paste0("data/fish/",Sys.Date(),"_taxontable_fish.rds"))

names(data)[which(names(data) == "taxon")] <- "original_name"
data2 <- taxontable[data, on = "original_name"]

sort(unique(data2$kingdom))
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
data2[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_germany_saarland_fish")]

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
        abundance,
        x.coord,
        y.coord,
        EPSG,
        data.set = "germany_saarland_fish"
)]

## combine entries of same taxon 
data3[, lowest.taxon := ifelse(!is.na(species), species,
                               ifelse(!is.na(genus), genus,
                                      ifelse(!is.na(family), family,
                                             ifelse(!is.na(order), order,
                                                    ifelse(!is.na(subclass), subclass,
                                                           ifelse(!is.na(class), class,
                                                                  ifelse(!is.na(phylum), phylum, kingdom)))))))]

data3[, abundance := as.numeric(abundance)]
data3[, abundance := sum(abundance), by = c("gr_sample_id", "lowest.taxon")]
data3 <- unique(data3, by = c("gr_sample_id", "lowest.taxon"))
data4 <- copy(data3)
typologies <- readRDS("data/all_typologies.rds")
data5 <- add_typologies(data4)

## visual checks 
sites <- unique(data5, by = "gr_sample_id") |> st_as_sf(coords = c("x.coord", "y.coord"), crs = data5$EPSG[1])

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
data7 <- data6[richness > 3]

# - drop sites far removed from ECRINS river network 
data8 <- data7[distance < 300]
rt <- 
        data8 |> 
        unique(by = "site_id") |> 
        st_as_sf(coords = c("x.coord", "y.coord"), 
                 crs = data5$EPSG[1])

plot_typology <- st_crop(typologies, st_transform(sites, crs = st_crs(typologies)))

remove_list <- c()

options(warn = -1)

for (i in 1:nrow(rt)){
        i.rt <- rt[i, ]
        i.plot_typology <- st_crop(plot_typology, 
                                   st_buffer(
                                           st_transform(i.rt, 
                                                        crs = st_crs(typologies)
                                           ), 
                                           dist =  2000)
        )
        i.x <- 
                mapview(i.plot_typology, zcol = "brt12") + 
                mapview(i.rt, color = "red")
        print(i.x)
        i.bool <- "n"
        i.bool <- readline(paste(i,"/",nrow(rt), ":"))
        if (i.bool == "n"){
                remove_list[length(remove_list) + 1] <- i.rt$site_id  
        }
        if (i.bool == "c"){
                i.towhat <- readline("change to:")
                data8[site_id == i.rt$site_id, brt12 := i.towhat]
        }
        rm(list = ls()[grepl("i\\.", ls())])
}

#- save the remove list. 
saveRDS(remove_list, paste0("data/fish/original_data/germany_hesse/", Sys.Date(), "_remove_list.rds"))
remove_list <- readRDS("data/fish/original_data/france_naiades/2022-01-26_remove_list.rds")

#- drop remove sites 
data9 <- data8[!site_id %in% remove_list]


# temporal aggregation --------------------------------------------------------------
agg <- data9 |> unique(by = "gr_sample_id")
unique(table(agg$site_id))
# yes 
source("R/functions/newest_sample.R")
data10 <- newest_sample(data9)

saveRDS(data10, paste0("data/fish/original_data/germany_hesse/",Sys.Date(),"_final_aggregated.rds"))
data10 <- readRDS("data/diatoms/original_data/french_naiades/2022-01-21_final_aggregated.rds")

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