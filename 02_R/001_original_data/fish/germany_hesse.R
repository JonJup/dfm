# ———————————————————————————————————————————— #
# ——— Clean fish data from Germany - Hesse ——— # 
# ———————————————————————————————————————————— #

# ———————————————————————————————————
# date first written: 21-11-17
# date last modified: 09.06.22
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: In this script I create a harmonized spatial data set from the raw data  
# from Germany Hesse. 
# Temporal aggregation: yes 
# EPSG: Pulkovo 1942(83) / 3-degree Gauss-Kruger zone 3 (E-N); EPSG: 5673 
# ————————————————


# setup -----------------------------------------------------------------------------
#devtools::install_github("https://github.com/jonjup/jjmisc")
library(data.table)
library(dplyr)
library(lubridate)
library(mapview)
library(sf)
library(stringr)
library(readxl)
source("R/functions/add_typologies.R")


# load data -------------------------------------------------------------------------
bio_wd  <- "data/fish/original_data/germany_hesse/raw/fisch_anzahl.xlsx"
sit_wd  <- "data/fish/original_data/germany_hesse/raw/Messstellen.xlsx"

bio     <- read_excel(bio_wd) |> setDT()
sit     <- read_excel(sit_wd) |> setDT()

taxontable <- readRDS("data/fish/2022-04-27_taxontable_fish.rds")
typologies <- readRDS("data/all_typologies.rds")

# prepare data ----------------------------------------------------------------------
data <- sit[bio, on = "MST_ID"]

data2 <- 
        data.table(
                original_site_name = data$MST_ID,
                x.coord = data$RW_GK,
                y.coord = data$HW_GK,
                date = ymd(data$PROBENAHMEDATUM), 
                taxon = data$TAXON_NAME,
                abundance = data$ANZAHL_GESAMT,
                EPSG = 5673 
        )


## add season 
data2[,c("year", "season") := .(year(date), 
                               case_when(month(date) %in% c(12,1,2)  ~ "winter",
                                         month(date) %in% c(3,4,5)   ~ "spring",
                                         month(date) %in% c(6,7,8)   ~ "summer",
                                         month(date) %in% c(9,10,11) ~ "autumn"))]


sites <- unique(data2, by = "original_site_name")
st_as_sf(sites, coords = c("x.coord", "y.coord"), crs = 5673) |> mapview::mapview()


## remove sites without coordinates
data2 <- data2[!is.na(x.coord)]
data2 <- data2[!is.na(taxon)]
data2 <- data2[taxon != ""]

## —— Inspect taxa ———————————————————————————————————————————————————————————————————————
(TU <- unique(data2$taxon) |> sort())

translation_table <- data.table(taxon = TU,  sci = character(length(TU)))
translation_table[1, "sci"] <- "Anguilla anguilla"
translation_table[2, "sci"] <- "Thymallus thymallus"
translation_table[3, "sci"] <- "Salmo salar"
translation_table[4, "sci"] <- "Salmo trutta"
translation_table[5, "sci"] <- "Lampetra planeri"
translation_table[6, "sci"] <- "Salvelinus fontinalis"
translation_table[7, "sci"] <- "Barbus barbus"
translation_table[8, "sci"] <- "Perca fluviatilis"
translation_table[9, "sci"] <- "Rhodeus amarus"
translation_table[10, "sci"] <- "Pseudorasbora parva"
translation_table[11, "sci"] <- "Abramis brama"
translation_table[12, "sci"] <- "Squalius cephalus"
translation_table[13, "sci"] <- "Gasterosteus aculeatus"
translation_table[14, "sci"] <- "Gasterosteus aculeatus"
translation_table[15, "sci"] <- "Phoxinus phoxinus"
translation_table[16, "sci"] <- "Neogobius fluviatilis"
translation_table[17, "sci"] <- "Rutilus pigus"
translation_table[18, "sci"] <- "Carassius gibelio"
translation_table[19, "sci"] <- "Carassius gibelio"
translation_table[20, "sci"] <- "Ctenopharyngodon idella"
translation_table[21, "sci"] <- "Cottus gobio"
translation_table[22, "sci"] <- "Gobio gobio"
translation_table[23, "sci"] <- "Blicca bjoerkna"
translation_table[24, "sci"] <- "Leuciscus leuciscus"
translation_table[25, "sci"] <- "Esox lucius"
translation_table[26, "sci"] <- "Hucho hucho"
translation_table[27, "sci"] <- "Carassius carassius"
translation_table[28, "sci"] <- "Cyprinus carpio"
translation_table[29, "sci"] <- "Gymnocephalus cernua"
translation_table[30, "sci"] <- "Leuciscus idus"
translation_table[31, "sci"] <- "Proterorhinus marmoratus"
translation_table[32, "sci"] <- "Salmo trutta"
translation_table[33, "sci"] <- "Petromyzon marinus"
translation_table[34, "sci"] <- "Leucaspius delineatus"
translation_table[35, "sci"] <- "Chondrostoma nasus"
translation_table[36, "sci"] <- "Neogobio kessleri"
translation_table[37, "sci"] <- "Lota lota"
translation_table[38, "sci"] <- "Leuciscus aspius"
translation_table[39, "sci"] <- "Oncorhynchus mykiss"
translation_table[40, "sci"] <- "Rutilus rutilus"
translation_table[41, "sci"] <- "Scardinius erythrophthalmus"
translation_table[42, "sci"] <- "Misgurnus fossilis"
translation_table[43, "sci"] <- "Tinca tinca"
translation_table[44, "sci"] <- "Barbatula barbatula"
translation_table[45, "sci"] <- "Alburnoides bipunctatus"
translation_table[46, "sci"] <- "Neogobius melanostomus"
translation_table[47, "sci"] <- "Salmo trutta"
translation_table[48, "sci"] <- "Lepomis gibbosus"
translation_table[49, "sci"] <- "Cottus perifretum"
translation_table[50, "sci"] <- "Cobitis taenia"
translation_table[51, "sci"] <- "Alburnus alburnus"
translation_table[52, "sci"] <- "Romanogobio albipinnatus"
translation_table[53, "sci"] <- "Silurus glanis"
translation_table[54, "sci"] <- "Vimba vimba"
translation_table[55, "sci"] <- "Sander lucioperca"
translation_table[56, "sci"] <- "Pungitius pungitius"

data2 <- translation_table[data2, on = "taxon"]
(TU <- unique(data2$sci) |> sort())


TU <- setdiff(TU,
              taxontable$original_name)

# taxontable <- update_taxonomy2(TU)
# taxontable[clean == FALSE]
# taxontable <- taxontable[original_name != "Schmerle"]
# taxontable[, clean := TRUE]
# saveRDS(taxontable, paste0("data/fish/",Sys.Date(),"_taxontable_fish.rds"))



names(data2)[which(names(data2) == "sci")] <- "original_name"
data2 <- taxontable[data2, on = "original_name"]
data2[, taxon := NULL]


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
data2[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_", "germany_hesse_fish")]

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
        data.set = "germany_hesse_fish"
)]

## combine entries of same taxon 
data3[, lowest.taxon := ifelse(!is.na(species), species,
                               ifelse(!is.na(genus), genus,
                                      ifelse(!is.na(family), family,
                                             ifelse(!is.na(order), order,
                                                    ifelse(!is.na(subclass), subclass,
                                                           ifelse(!is.na(class), class,
                                                                  ifelse(!is.na(phylum), phylum, kingdom)))))))]

data3[, abundance := sum(abundance), by = c("gr_sample_id", "lowest.taxon")]
data3 <- unique(data3, by = c("gr_sample_id", "lowest.taxon"))
data4 <- copy(data3)
data5 <- add_typologies(data4)

## visual checks 
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
data7 <- data6[richness > 2]

# - drop sites far removed from ECRINS river network 
data8 <- data7[distance < 300]
remove_list <- readRDS("data/fish/original_data/germany_hesse/2022-01-26_remove_list.rds")
data8 <- data8[!site_id %in% remove_list]
rt <- 
        data8 |> 
        unique(by = "site_id") |> 
        st_as_sf(coords = c("x.coord", "y.coord"), 
                 crs = data5$EPSG[1])

plot_typology <- st_crop(typologies, st_transform(sites, crs = st_crs(typologies)))

options(warn = -1)

updated_type <- data.table(site_id = rt$site_id)

for (i in 1:nrow(rt)){
        i.percent <- i/nrow(rt) * 100
        i.rt <- rt[i, ]
        i.plot_typology <- st_crop(plot_typology, st_buffer(st_transform(i.rt, crs = st_crs(typologies)), dist =  2000))
        x <- mapview(i.plot_typology, zcol = "brt") + mapview(i.rt, popup = "waterbody", color = "red")
        print(x)
        i.bool <- "n"
        i.bool <- readline(paste0(i,"/", nrow(rt)))
        if (i.bool == "break")
                break()
        if (i.bool == "n"){
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
saveRDS(updated_type, paste0("data/fish/original_data/germany_hesse/", Sys.Date(), "_updated_type.rds"))
#remove_list <- readRDS("data/fish/original_data/france_naiades/2022-01-26_remove_list.rds")

data9 <- left_join(data8, 
                   updated_type, 
                   by = "site_id")
any(is.na(data9$new_type))

#- drop "drop" rows determined in for-loop      
data9 <- data9[new_type != "drop"]
data9[, brt12 := NULL]
data9 <- rename(data9, brt12 = new_type)

# temporal aggregation --------------------------------------------------------------
agg <- data9 |> unique(by = "gr_sample_id")
unique(table(agg$site_id))
# - yes 
data9[, month := month(date)]
data9 <- data9[month %in% 5:9]

source("R/functions/newest_sample.R")
data10 <- newest_sample(data9, season_available = FALSE)
saveRDS(data10, paste0("data/fish/original_data/germany_hesse/",Sys.Date(),"_final_aggregated.rds"))
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
unique(data10, by = "gr_sample_id") |> pull(richness) |> mean()