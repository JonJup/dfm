# ———————————————————————————————————————————————— #
# ——— Clean Diatom data from Mixed - WISER  ——————— # 
# ———————————————————————————————————————————————— #

# ———————————————————————————————————
# date first written: 22-01-12
# date last modified: 22-01-18
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: In this script I create a harmonized spatial data set of diatom from the 
# raw data from the WISER project.   
# CRS: 4326
# ————————————————


# setup -----------------------------------------------------------------------------
#devtools::install_github("https://github.com/jonjup/jjmisc")
library(data.table)
library(dplyr)
library(fs)
library(jjmisc)
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

# load data -------------------------------------------------------------------------
## directory with all biological data 
bio_wd <- "data/diatoms/original_data/germany_WISER/raw/WISER_Diatom_taxa.xlsx"
sit_wd <- "data/diatoms/original_data/germany_WISER/raw/WISER_Metadata_Abiotics.xls"
bio   = read_excel(bio_wd)
sites = read_excel(sit_wd) 
setDT(bio)
setDT(sites)


# prepare data ----------------------------------------------------------------------
bio <- bio[, list(
        "site" = StationCode,
        "taxon" = TaxonName,
        "sample_id" = Site_SampleCode,
        "abundance" = PhBAbundance_Rarefaction
)] 

sites <- sites[, list(
        StationCode,
        "x.coord" = Longitude,
        "y.coord" = Latitude,
        "site_name" = StationName,
        "waterbody" = RiverName
)]

# join biological data with site data 
data = bio[sites, on = c("site" = "StationCode")]
# - remove rows with missing taxon 
data = data[!(is.na(taxon))]
# - remove rows with missing coordinates 
data <- data[!is.na(x.coord)]
# - assign EPSG code for coordinate reference system
data$EPSG = 4326
# - check site names 
unique(data$site_name)
# - some site names have multiple coordinates
data[, coords := uniqueN(x.coord), by = "site_name"]
multi_coord <- data[coords != 1]



all(data[, uniqueN(x.coord), by = "site_name"][, "V1"] == 1)



data2 <- data2[site_name != "<NEW>"]

data2 <- data2[!is.na(taxon)]
data2 <- data2[!is.na(x.coord)]
data2 <- data2[!is.na(y.coord)]

sites <- unique(data2, by = "original_site_name")
sites %<>% st_as_sf(coords = c("x.coord", "y.coord"), crs = data2$EPSG[1])
mapview(sites)

data2 <- data2[, .(
        original_site_name, 
        date = as.Date(NA),
        year = as.numeric(NA),
        season,
        taxon,
        abundance,
        x.coord,
        y.coord,
        EPSG
)]

# - clean memory 
rm(bio, sites)
gc()
# taxonomic harmonization -----------------------------------------------------------

taxontable <- readRDS("data/diatoms/2022-01-17_taxontable_diatoms.rds")
data <- data2

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

dia1 <- readRDS("data/diatoms/fwb_table.rds")
dia2 <- readRDS("data/diatoms/omn_table.rds")

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

TU <- unique(data$taxon) |> sort()
TU <- setdiff(TU, taxontable$original_name)

taxontable <- new_entry("ACHNANTHES J.B.M. Bory de St. Vincent", fix = "Achnanthes", spe = NA, gen = "Achnanthes")
taxontable <- new_entry("AMPHORA  C.G. Ehrenberg ex F.T. Kützing", fix = "Amphora", spe = NA, gen = "Amphora")
taxontable <- new_entry("AULACOSEIRA  G.H.K. Thwaites", fix = "Aulacoseira", spe = NA, gen = "Aulacoseira")
taxontable <- new_entry("COCCONEIS  C.G. Ehrenberg", fix = "Cocconeis", spe = NA, gen = "Cocconeis")
taxontable <- new_entry("CYCLOTELLA  F.T. Kützing ex A de Brébisson", fix = "Cyclotella", spe = NA, gen = "Cyclotella")
taxontable <- new_entry("DIATOMA  J.B.M. Bory de St. Vincent", fix = "Diatoma", spe = NA, gen = "Diatoma")
taxontable <- new_entry("EPITHEMIA  F.T. Kützing", fix = "Epithemia", spe = NA, gen = "Epithemia")
taxontable <- new_entry("FRAGILARIA  H.C. Lyngbye", fix = "Fragilaria", spe = NA, gen = "Fragilaria")
taxontable <- new_entry("GYROSIGMA  A. Hassall", fix = "Gyrosigma", spe = NA, gen = "Gyrosigma")
taxontable <- new_entry("SKELETONEMA  R.K. Greville", fix = "Skeletonema", spe = NA, gen = "Skeletonema")
taxontable <- new_entry("STAURONEIS  C.G. Ehrenberg", fix = "Stauroneis", spe = NA, gen = "Stauroneis")
taxontable <- new_entry("STEPHANODISCUS  C.G. Ehrenberg", fix = "Stephanodiscus", spe = NA, gen = "Stephanodiscus")
taxontable <- new_entry("SYNEDRA  C.G. Ehrenberg", fix = "Synedra", spe = NA, gen = "Synedra")
taxontable <- new_entry("TABELLARIA  C.G. Ehrenberg", fix = "Tabellaria", spe = NA, gen = "Tabellaria")

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
data2[, site_id := as.numeric(site_id)]
data2[, date_id := .GRP, by = "season"]

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
data2[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_mixed_star_diatoms")]


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
        data.set = "mixed_star_diatoms"
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

## save to file
saveRDS(data5, paste0("data/diatoms/original_data/germany_STAR/",Sys.Date(),"_final_non_aggregated.rds"))

# temporal aggregation --------------------------------------------------------------
agg <- data5 |> unique(by = "gr_sample_id")
unique(table(agg$site_id))

# statistics -------------------------------------------------------------------------

data5 |> 
        filter(least.impacted == TRUE) |> 
        group_by(gr_sample_id) |> 
        count() |> 
        arrange(n) |> 
        pull(n) |> table()


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
