# ———————————————————————————————————— #
# ——— Clean fish data from Naiades ——— # 
# ———————————————————————————————————— #

# ———————————————————————————————————
# date first written: 21-11-10
# date last modified: 09.06.22
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: In this script I create a harmonized spatial data set from the raw data provided 
# from Naiades. 
# Temporal aggregation: yes 
# ————————————————


# setup -----------------------------------------------------------------------------
library(data.table)
library(dplyr)
library(lubridate)
library(mapview)
library(sf)
source("R/functions/add_typologies.R")
# load data -------------------------------------------------------------------------
bio_wd <- "data/fish/original_data/france_naiades/raw/fauneflore.csv"
site_wd <- "data/fish/original_data/france_naiades/raw/stations.csv"

bio     <- fread(bio_wd) 
sites   <- fread(site_wd)

taxontable <- readRDS("data/fish/2022-04-27_taxontable_fish.rds")
typologies <- readRDS("data/all_typologies.rds")

# prepare data ----------------------------------------------------------------------

## The variable "LbSupport" from fauna table tells us what kind of observation is in the
## row. There are diatoms, fishes, macroinvertebrates, macrophytes and phytoplancton.
unique(bio$LbSupport)

## We subset fauna_table to macroinvertebrate observations. 
bio <- bio[LbSupport == "Poissons" & MnTypTaxRep == "NbrTax"]

## check results 
unique(bio$LbSupport)

## Join fish data with station data in sites. The latter contains station
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
     "data.set"           = "naiades"
     )
]
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

## The data with CRS: RGFG95 / UTM 22 (EPSG: 2972) are from French Guyana. They are removed. 
data <- data[EPSG != "RGFG95 / UTM 22"]

## check 
table(data$EPSG)

## Replace CRS name with EPSG code 
data[, EPSG :=2154]
data[, EPSG := as.numeric(EPSG)]

## —— Inspect taxa ———————————————————————————————————————————————————————————————————————
(TU <- unique(data$taxon) |> sort())

## Both species form the genus Atherina include (Hepsetia) as alternative genus.  
## The latter is a synonym according to gbif (https://www.gbif.org/species/2411970). 

data[taxon == "Atherina (Hepsetia) presbyter", taxon := "Atherina presbyter"]
data[taxon == "Atherina (Hepsetia) boyeri", taxon := "Atherina boyeri"]

## There are subspecies for Carassius auratus and Salmo turtta. For our analysis, species
## is the lowest taxonomic level considered. Thus we omit the subspeces name.

data[taxon %in% c("Carassius auratus auratus","Carassius auratus gibelio"), 
     taxon := "Carassius auratus"]
data[taxon %in% c("Salmo trutta fario","Salmo trutta lacustris","Salmo trutta trutta"), 
     taxon := "Salmo trutta"]

## There are two entries with faulty formatting: 
## "Hybride brÃ¨me-gardon" 
## "Hybrides de cyprinidÃ©s"  
## Both seem to be hybries. We omit them. 
data <- data[!taxon %in% c("Hybride brÃ¨me-gardon","Hybrides de cyprinidÃ©s" )]

## There are several decapod species in this data set. They are omitted         
data <- data[!taxon %in% c("Astacidea", 
                   "Astacus astacus",
                   "Astacus leptodactylus",
                   "Austropotamobius pallipes",
                   "Eriocheir sinensis",
                   "Orconectes immunis",
                   "Orconectes limosus",
                   "Pacifastacus leniusculus",
                   "Procambarus clarkii",
                   "Procambarus fallax"
                   )]

## Which taxa where not in the taxon_table?
(TU <- unique(data$taxon) |> sort())
TU <- setdiff(TU,
              taxontable$original_name)


#taxontable <- update_taxonomy2(TU)

#taxontable[clean == FALSE]
#taxontable <- fill_taxon_table("Phoxinus", g = "Phoxinus", f = "Leuciscidae", or = "Cypriniformes", c = "Actinopterygii", p = "Chordata", k = "Animalia")
#taxontable[clean == TRUE]
#saveRDS(taxontable, paste0("data/fish/",Sys.Date(),"_taxontable_fish.rds"))


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
data2[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_naiades")]

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

data3[, abundance := sum(abundance), by = c("gr_sample_id", "lowest.taxon")]
data3 <- unique(data3, by = c("gr_sample_id", "lowest.taxon"))
data4 <- copy(data3)
data5 <- add_typologies(data4)

## visual checks 
# sites <- unique(data5, by = "gr_sample_id") |> st_as_sf(coords = c("x.coord", "y.coord"), crs = data5$EPSG[1])
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
data7 <- data6[richness > 2]

# - drop sites far removed from ECRINS river network 
data8 <- data7[distance < 300]
remove_list <- readRDS("data/fish/original_data/france_naiades/2022-01-26_remove_list.rds")
data8 <- data8[!site_id %in% remove_list]
rt <- 
        data8 |> 
        unique(by = "site_id") |> 
        st_as_sf(coords = c("x.coord", "y.coord"), 
                 crs = data5$EPSG[1])

plot_typology <- st_crop(typologies, st_transform(sites, crs = st_crs(typologies)))
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
saveRDS(updated_type, paste0("data/fish/original_data/france_naiades/", Sys.Date(), "_updated_type.rds"))
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
# yes 
data9[, month := month(date)]
data9 <- data9[month %in% 5:9]
source("R/functions/newest_sample.R")
data10 <- newest_sample(data9, season_available = FALSE)
data10 <- data10[year > 2004]
saveRDS(data10, paste0("data/fish/original_data/france_naiades/",Sys.Date(),"_final_aggregated.rds"))
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