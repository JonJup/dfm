# ———————————————————————————————————————————————— #
# ——— Clean Diatom data from Spain, Ebro Basin ——— # 
# ———————————————————————————————————————————————— #

# ———————————————————————————————————
# date first written: 22-01-19
# date last modified: 11.06.22
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: In this script I create a harmonized spatial data set of diatoms from the 
# raw data from the Ebro Hydrografic Confederation  
# CRS: ETRS89 / UTM zone 30N ——— 25830
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
bio_wd <- "data/diatoms/original_data/spain_ebro/raw/WCASF_20220119082845.xlsx"

bio <- read_excel(bio_wd)

# dia1 <- readRDS("data/diatoms/fwb_table.rds")
# dia2 <- readRDS("data/diatoms/omn_table.rds")

# prepare data  ---------------------------------------------------------------------

# - to data.table
setDT(bio)
names(bio) <- c("original_site_name1", "water_body", "original_site_name2", "water_body2", "x.coord", "y.coord", "misc1", "misc2", "misc3", "sample_id", "date", "misc4", "parameter", "taxon", "misc5", "abundance", "misc6")
bio2 <- select(bio, !starts_with("misc"))
bio2 <- bio2[parameter == "Nº de valvas"]

# - the original_site_name1 works, 2 does not. 
all(pull(bio2[, uniqueN(x.coord), by = "original_site_name1"], V1) == 1)
all(pull(bio2[, uniqueN(x.coord), by = "original_site_name2"], V1) == 1)

bio2[, c("original_site_name2", "water_body2") := NULL]
#- multiple samples per site. Compare this to gr_sample_id later. 
bio2[, uniqueN(sample_id), by = "original_site_name1"]

# - add EPSG 
bio2[, EPSG := 25830]

sites <- unique(bio2, by = "original_site_name1")
sites2 <- st_as_sf(sites, coords = c("x.coord", "y.coord"), crs =  25830)
mapview(sites2)

data <- bio2
data <- data[taxon != "--"]
data[,date := dmy(date)]
#- add season and year
data[,c("year", "season") := .(year(date), 
                               case_when(month(date) %in% c(12,1,2)  ~ "winter",
                                         month(date) %in% c(3,4,5)   ~ "spring",
                                         month(date) %in% c(6,7,8)   ~ "summer",
                                         month(date) %in% c(9,10,11) ~ "autumn"))]


rm(sites, sites2, bio2, bio, bio_wd); gc()
# taxonomic harmonization -----------------------------------------------------------

taxontable <- readRDS("data/diatoms/2022-05-17_taxontable_diatoms.rds")

TU <- unique(data$taxon)|> sort()
TU <- setdiff(TU, taxontable$original_name)

## what is the most similar in in taxontable 
strdist_id  <- amatch(TU, taxontable$original_name, maxDist = 100000)
strdist_tbl <- 
        data.table(taxontable_name = taxontable$original_name[strdist_id], TU) |> 
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

TU <- unique(data$taxon) |> sort()
TU <- setdiff(TU, taxontable$original_name)

taxontable <- new_entry(ori = "Achnanthidium petersenii", fix = "Achnanthidium petersenii")
taxontable <- new_genus(ori = "Delicatophycus delicatulus", fix = "Delicatophycus delicatulus", spe = "Delicatophycus delicatulus", gen = "Delicatophycus", fam = "Cymbellaceae", ord = "Cymbellales", cla = "Bacillariophyceae")
taxontable <- new_genus(ori = "Dorofeyukea kotschyi", fix = "Dorofeyukea kotschyi", spe = "Dorofeyukea kotschyi", gen = "Dorofeyukea", fam = "Stauroneidaceae", ord = "Naviculales", cla = "Bacillariophyceae")
taxontable <- new_entry(ori = "Gomphonella olivacea", fix = "Gomphonella olivacea")
taxontable <- new_entry(ori = "Haslea duerrenbergiana", fix = "Haslea duerrenbergiana")
taxontable <- new_entry(ori = "Lindavia comta", fix = "Lindavia comta")
taxontable <- new_entry(ori = "Nitzschia tenuis var. sigmoidea", fix = "Nitzschia pura-linearis Complex")
taxontable <- new_entry(ori = "Paraplaconeis minor", fix = "Paraplaconeis minor")
taxontable <- new_entry(ori = "PROSCHKINIA sp.", fix = "Proschkinia", spe = NA, gen = "Proschkinia")
taxontable <- new_entry(ori = "Punctastriata subconstricta", fix = "Pseudostaurosira subconstricta")
taxontable <- new_entry(ori = "Sellaphora raederae", fix = "Sellaphora raederae")
taxontable <- new_entry(ori = "Staurosirella neopinnata", fix = "Staurosirella neopinnata")
taxontable <- new_entry(ori = "Ulnaria sp.", fix = "Ulnaria", spe = NA, gen = "Ulnaria")

taxontable

TU <- unique(data$taxon) |> sort()
TU <- setdiff(TU, taxontable$original_name)

check_taxon_table(taxontable)

saveRDS(taxontable, paste0("data/diatoms/",Sys.Date(),"_taxontable_diatoms.rds"))

rm(strdist_id, strdist_tbl, TU, dia1, dia2);gc()

# Join data ------------------------------------------------------------------------
data <- rename(data, original_name = taxon)
data2 <- taxontable[data, on = "original_name"]  

data2%<>%rename("original_site_name" = "original_site_name1")

data2[, site_id := .GRP, by = "original_site_name"]
data2[, date_id := .GRP, by = "date"]

all (pull( data2[,uniqueN(x.coord), by = "site_id"], V1) == 1)

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
data2[,gr_sample_id := paste0(sample_id,"_spain_ebro_diatoms")]

# - check that gr_sample_id matches sample_id
data2[, sample_id_per_gr := uniqueN(sample_id), by = "gr_sample_id"]
data2[sample_id_per_gr > 1 ] |> View()
# -> doesnt made changes above to fix this. Instead of the usual gr_sample_id method we
# simiply use the sample_id provided by the original data.set

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
        data.set = "spain_ebro_diatom",
        water_body
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

# - visual checks
# sites <- unique(data5, by = "site_id") |> st_as_sf(coords = c("x.coord", "y.coord"), crs = data5$EPSG[1])
# mapview(sites, zcol = "brt12")
# mapview(sites, zcol = "ife")
# mapview(sites, zcol = "bgr")
# mapview(sites, zcol = "least.impacted")

# - subset to least impacted catchments 
data6 <- data5[least.impacted == TRUE]
sites <- unique(data6, by = "site_id") |> st_as_sf(coords = c("x.coord", "y.coord"), crs = data5$EPSG[1])

# - look for sites with different ID but same coordinates 
distances  <- st_distance(sites)
distances2 <- as.matrix(distances)
diag(distances2) <- 999
(duplicate_sites <- which(distances2 < units::as_units(1, "m")))

# - drop sites with very few species 
data6[, richness := uniqueN(lowest.taxon), by = "gr_sample_id"]
data7 <- data6[richness > 10]

# - drop sites far removed from ECRINS river network 
data8 <- data7[distance < 300]

rl <- readRDS("data/diatoms/original_data/spain_ebro/2022-01-19_remove_list.rds")
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
saveRDS(updated_type, paste0("data/diatoms/original_data/spain_ebro/", Sys.Date(), "_updated_type.rds"))

# - drop remove sites 
data9 <- left_join(data8, 
                   updated_type, 
                   by = "site_id")
# - drop "drop" rows determined in for-loop
data9 <- data9[new_type != "drop"]
data9[, brt12 := NULL]
data9 <- rename(data9, brt12 = new_type)


# temporal aggregation --------------------------------------------------------------
agg <- data9 |> unique(by = "gr_sample_id")
unique(table(agg$site_id))
# - yes
data10 <- data9[lubridate::month(date) %in% 5:9]
source("R/functions/newest_sample.R")
data10 <- newest_sample(data10, season_available = FALSE)

saveRDS(data10, paste0("data/diatoms/original_data/spain_ebro/",Sys.Date(),"_final_aggregated.rds"))
# data10 <- readRDS("data/diatoms/original_data/spain_ebro/2022-01-18_final_aggregated.rds")

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
data9[, uniqueN(lowest.taxon), by = "gr_sample_id"] |> 
        pull(V1) |> 
        mean()



# temporal aggregation --------------------------------------------------------------
agg <- data5 |> unique(by = "gr_sample_id")
unique(table(agg$site_id))

# statistics -------------------------------------------------------------------------
# time span
summary(data5$year)
# number of sites
uniqueN(data5$site_id)
# number of samples
uniqueN(data5$gr_sample_id)
# most recent
data5[, uniqueN(gr_sample_id)]
# reference condition
data5[least.impacted == TRUE, uniqueN(site_id)]
data5[least.impacted == TRUE, uniqueN(gr_sample_id)]




