# —————————————————————————————————————————— #
# ——— Clean macrophyte data from Naiades ——— # 
# —————————————————————————————————————————— #

# ———————————————————————————————————
# date first written: 10-11-21
# date last modified: 12.06.22
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: In this script I create a harmonized spatial data set from the raw data provided 
# from Naiades. 
# Temporal aggregation: yes 
# CRS: RGF93 / Lambert-93 -- France; EPSG: 2154
# ————————————————


# setup -----------------------------------------------------------------------------
library(data.table)
library(dplyr)
library(lubridate)
library(mapview)
library(sf)

source("R/functions/add_typologies.R")
source("R/functions/dfm.R")
# load data -------------------------------------------------------------------------
bio_wd  <- "data/macrophytes/original_data/france_naiades/raw/Naiades_Export_France_Entiere_HB/fauneflore.csv"
site_wd <- "data/macrophytes/original_data/france_naiades/raw/Naiades_Export_France_Entiere_HB/stations.csv"

bio     <- fread(bio_wd) 
sites   <- fread(site_wd)

taxontable <- readRDS("data/macrophytes/2022-06-12_taxontable_macrophytes.rds")
typologies <- readRDS("data/all_typologies.rds")
# prepare data ----------------------------------------------------------------------

## The variable "LbSupport" from fauna table tells us what kind of observation is in the
## row. There are diatoms, fishes, macroinvertebrates, macrophytes and phytoplancton.
unique(bio$LbSupport)

## We subset fauna_table to macroinvertebrate observations. 
bio <- bio[LbSupport == "Macrophytes"]

## What "Type de mesure du taxon répertorié" are in the data 
table(bio$MnTypTaxRep)

## Meaning of codes (taken from https://mdm.sandre.eaufrance.fr/node/297741)
## NbrTax   - Dénombrement de taxon - Dénombrement absolu d’un taxon évalué sous la forme : Présence/Absence
## DensTax  - Densité par taxon - Dénombrement absolu associé à un taxon donné ramené à une unité de volume ou de surface.
## IndAbTax - Indice d’abondance de taxon - Nombre indiquant la surface couverte par le taxon végétal concerné et déterminé selon les préconisatons de la méthode appliquée.
## RecTax   - Pourcentage de recouvrement du taxon - Pourcentage de recouvrement de l’ensemble des individus appartenant à ce même taxon, par rapport à la surface de référence du point de prélèvement.

## only abundances lead to a really small data set (4 sites - two impaired)
#bio <- bio[MnTypTaxRep == "NbrTax"]

## Therefore, we will instead keep all four metrics and transform them to PA

## Join macrophyte data with station data in sites. The latter contains station
## coordinates.
bio  <- sites[bio, on = "CdStationMesureEauxSurface"]
data <- bio[,
            list(
                    "original_site_name" = CdStationMesureEauxSurface,
                    "date"               = ymd(DateDebutOperationPrelBio),
                    "taxon"              = NomLatinAppelTaxon,
                    "x.coord"            = CoordXStationMesureEauxSurface,
                    "y.coord"            = CoordYStationMesureEauxSurface,
                    "EPSG"               = LibelleProjection,
                    "abundance"          = RsTaxRep,
                    "data.set"           = "france_naiades_macrophytes"
            )
]

## transform to PA 
data[abundance != 0, abundance := 1]

## add season 
data[,c("year", "season") := .(year(date), 
                               case_when(month(date) %in% c(12,1,2)  ~ "winter",
                                         month(date) %in% c(3,4,5)   ~ "spring",
                                         month(date) %in% c(6,7,8)   ~ "summer",
                                         month(date) %in% c(9,10,11) ~ "autumn"))]
## remove sites without coordinates
data <- data[!is.na(x.coord)]

## check EPSG 
table(data$EPSG)

## Replace CRS name with EPSG code 
data[, EPSG :=2154]
data[, EPSG := as.numeric(EPSG)]

## drop invertebrate taxa 
data <- data[!taxon %in% c("Agapetus" ,
                           "Besdolus imhoffi", 
                           "Chloroperla susemicheli",
                           "Hemianax" ,
                           "Hydroptilidae", 
                           "Marthamea vitripennis"  ,  
                           "Oxyethira" ,
                           "Protonemura praecox", 
                           "Rhabdiopteryx thienemanni",
                           "Sphaerotylus" ,
                           "Synagapetus", 
                           "Taeniopteryx hubaulti"    ,
                           "Taeniopteryx kuehtreiberi",
                           "Taeniopteryx schoenemundi")]


## —— Inspect taxa ———————————————————————————————————————————————————————————————————————
(TU <- unique(data$taxon) |> sort())

## Which taxa where not in the taxon_table?
(TU <- unique(data$taxon) |> sort())
(TU <- setdiff(TU,taxontable$original_name))

# - taxontable completeted in scirpt: fixtax_france_naiades_macrophytes

names(data)[which(names(data) == "taxon")] <- "original_name"
data2 <- taxontable[data, on = "original_name"]

sort(unique(data2$kingdom))
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
data2[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_france_naiades_macrophytes")]

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
        data.set
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
data7 <- data6#data6[richness > 10]

# - drop sites far removed from ECRINS river network 
data8 <- data7[distance < 300]
rl <- readRDS("data/macrophytes/original_data/france_naiades/2022-01-27_remove_list.rds")
data8 <- data8[!site_id %in% rl]
# - visually check the assignment of sites 
rt <- 
        data8 |> 
        unique(by = "site_id") |> 
        st_as_sf(coords = c("x.coord", "y.coord"), 
                 crs = data5$EPSG[1])

plot_typology <- st_crop(typologies, st_transform(sites, crs = st_crs(typologies)))
updated_type  <- data.table(site_id = rt$site_id)
options(warn = -1)
for (i in 1:nrow(rt)){
        i.rt <- rt[i, ]
        i.plot_typology <- st_crop(plot_typology, st_buffer(st_transform(i.rt, crs = st_crs(typologies)), dist =  2000))
        if (nrow(i.plot_typology) == 0){
                x <- mapview(i.rt, color = "red")
        } else {
                x <- mapview(i.plot_typology, zcol = "brt") + mapview(i.rt, popup = c("water_body"), color = "red")
        }
        print(x)
        #i.bool <- "n"
        i.bool <- readline(paste(i, "/", nrow(rt), ":"))
        if (i.bool == "break"){
                break()
        } else if (i.bool == "n"){
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
# saveRDS(remove_list, paste0("data/macrophytes/original_data/france_naiades/", Sys.Date(), "_remove_list.rds"))
# remove_list <- readRDS("data/macrophytes/original_data/french_naiades/2022-01-21_final_aggregated.rds")

saveRDS(updated_type, paste0("data/macrophytes/original_data/france_naiades/", Sys.Date(), "_updated_type.rds"))

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
saveRDS(data10, paste0("data/macrophytes/original_data/france_naiades/",Sys.Date(),"_final_aggregated.rds"))
# data10 <- readRDS("data/macrophytes/original_data/french_naiades/2022-01-21_final_aggregated.rds")

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
