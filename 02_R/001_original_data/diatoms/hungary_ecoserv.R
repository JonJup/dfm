# ——————————————————————————————————————————————— #
# ——— Clean Diatom data from Ecoserv - Hungary——— # 
# ——————————————————————————————————————————————— #

# ———————————————————————————————————
# date first written: 17-12-21
# date last modified: 11.06.22
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: In this script I create a harmonized spatial data set of diatom from the 
# raw data from Ecoserv - Hungary.  
# CRS: 
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
source("R/functions/add_typologies.R")
source("R/functions/harmonize diatoms.R")
# load data -------------------------------------------------------------------------
## directory with all biological data 
bio_wd <- "data/diatoms/original_data/hungary_ecosurv/raw/2415PhybeRiversExport2.xls"
sit_wd <- "data/diatoms/original_data/hungary_ecosurv/raw/tb_Sampling_locations_all.csv"
dat_wd <- "data/diatoms/original_data/hungary_ecosurv/raw/tb_Sampling_Event_PhytoBenton.csv"
bio <- read_excel(bio_wd) |> setDT()
sit <- fread(sit_wd)
dates <- fread(dat_wd) 

# dia1 <- readRDS("data/diatoms/fwb_table.rds")
# dia2 <- readRDS("data/diatoms/omn_table.rds")
# non_diatom_algae <- readRDS("data/diatoms/2021-12-08_non_diatom_algae.rds")

# prepare data ----------------------------------------------------------------------
sampling_id1 <- names(bio)[-c(1:2)] |> str_split(pattern = "[:digit:].*", simplify = TRUE) |> {\(x) x[,1]}()
sampling_id2 <- names(bio)[-c(1:2)] |> str_extract(pattern = "[:digit:].*")
sampling_id3 <- paste(sampling_id1, sampling_id2, sep = "_")

names(bio)[-c(1:2)] <- sampling_id3

names(bio)[-c(1:2)] %in% dates$PHYBENT.sampling.Number


dates2 <- 
        dates |> 
        select(
                original_site_name = "PHYBENT.sampling.site.azon", 
                x.coord            = PHYBENT.eovy,
                y.coord            = PHYBENT.eovx, 
                join_id            = PHYBENT.sampling.Number,
                date               = PHYBENT.sampling.date
        ) |> 
        mutate(date = mdy_hms(date))

data <- 
        bio |> 
        select(!PhyBENT_Taxoncode) |> 
        pivot_longer(cols = !PhyBENT_TaxonName, names_to = "join_id", values_to = "abundance") |> 
        filter(abundance != 0) |> 
        right_join(
                dates2,
                by = "join_id"
        ) |> 
        select(!join_id) |> 
        mutate(data.set = "hungary_ecoserv_diatoms",
               EPSG = 23700) |> 
        rename(taxon = PhyBENT_TaxonName)

sites <- unique(data, by = c("original_site_name"))
sites <- st_as_sf(sites, coords = c("x.coord", "y.coord"), crs = sites$EPSG[1])
mapview(sites)

# taxonomic harmonization -----------------------------------------------------------
taxontable <- readRDS("data/diatoms/2022-05-17_taxontable_diatoms.rds")

setDT(data)
data <- data[! taxon %in% c("Centrales")]

TU <- sort(unique(data$taxon))
TU <- setdiff(TU, taxontable$original_name)

## what is the most similar in in taxontable 
strdist_id  <- amatch(TU, taxontable$original_name, maxDist = 1000)
strdist_tbl <- data.table(taxontable_name = taxontable$original_name[strdist_id], TU) |> 
        filter(!is.na(taxontable_name)) 

taxontable <- append_to_tt("Aulacoseira italica var.tenuissima","Aulacoseira italica var.tenuissima")
taxontable <- append_to_tt("Mayamaea atomus var.alcimonica","Navicula atomus var alcimonica")
taxontable <- append_to_tt("Pinnularia microstauron var.brebissonii","Pinnularia microstauron var. brebissonii")

TU <- unique(data$taxon) |> sort()
TU <- setdiff(TU, taxontable$original_name)

## check against fwb table 
for (i in seq_along(TU)){
        i.tu  <- TU[i]
        if(check_fwb(i.tu)){
                x <- get_fwb(i.tu)
                taxontable <- add_entry_tt(x)
        }
        rm(i.tu)
}; rm(i)

TU <- unique(data$taxon) |> sort()
TU <- setdiff(TU, taxontable$original_name)

## open taxontable and dia1 

for (i in seq_along(TU)) {
        i.tu  <- TU[i]
        i.det <- str_detect(dia2$taxon, i.tu)
        if (any(i.det)) {
                i.id <- which(i.det)
                print(paste("name:", i.tu))
                print(paste("suggestions:", dia2$taxon[i.id]))
                i.rl <- readline()
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
                        print(
                                paste("Final name: ", dia2$taxon[i.id])
                        )
                        i.final <- readline()
                        ## check that against fwb
                        if (check_fwb(i.final)) {
                                i.final <- get_fwb(i.final)
                        }
                        taxontable <- add_entry_tt(i.final)
                }
        }
        rm(list = ls()[grepl(pattern = "^i\\.", x = ls())])
}

taxontable[species == "Cyclostephanos ", species := NA]

TU <- unique(data$taxon) |> sort()
TU <- setdiff(TU, taxontable$original_name)

taxontable <- new_entry("Navicula cincta f. minuta", fix = "Navicula cincta-heufleri", spe = "Navicula cincta-heufleri", gen = "Navicula")
taxontable <- new_entry("Nitzschia levidensis var. halophila", fix = "Tryblionella", spe = NA, gen = "Tryblionella")

check_taxon_table(taxontable)
taxontable <- taxontable[!duplicated(taxontable$original_name)]

saveRDS(taxontable, paste0("data/diatoms/",Sys.Date(),"_taxontable_diatoms.rds"))

# join 
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
data2[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_hungary_ecoserv_diatoms")]

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
data7 <- data6[richness > 10]

# - drop sites far removed from ECRINS river network 
data8 <- data7[distance < 300]
rl <- readRDS("data/diatoms/original_data/hungary_ecosurv/2022-01-19_remove_list.rds")
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
saveRDS(updated_type, paste0("data/diatoms/original_data/hungary_ecosurv/", Sys.Date(), "_updated_type.rds"))

#- drop remove sites 
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
data10 <- data9[month(date) %in% 5:9]

saveRDS(data10, paste0("data/diatoms/original_data/hungary_ecosurv/",Sys.Date(),"_final_aggregated.rds"))
#data9 <- readRDS("data/diatoms/original_data/hungary_ecosurv/2022-01-18_final_aggregated.rds")

# statistics -------------------------------------------------------------------------
# time span
summary(data9$year)
# all sites and samples
uniqueN(data5$site_id)
uniqueN(data5$gr_sample_id)
# least impacted sites and samples
uniqueN(data6$site_id)
uniqueN(data6$gr_sample_id)
# no sites with <5 taxa 
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

