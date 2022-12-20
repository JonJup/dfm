# ————————————————————————————————————————————————————————————— #
# ——— Clean Diatom data from Romania - Mirella cimpean  ——————— # 
# ————————————————————————————————————————————————————————————— #

# ———————————————————————————————————
# date first written: 12-01-22
# date last modified: 12-01-22
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: In this script I create a harmonized spatial data set of diatom from the 
# raw data from Romania - Mirella Cimpean 
# CRS: 4326 
# ————————————————


# 01. Setup -------------------------------------------------------------------
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
setwd("data/diatoms/original_data/romania_cimpean/raw/")

data1 <- read_xlsx("Aries_River_Romania_GetReal.xlsx", 
                   col_types = c("text", "text", "date", "text", "text", "text", "numeric"), 
                   skip = 1) |> 
        setDT()

data2 <- read_xlsx("Capra_River_Romania_GetReal -.xlsx") |> 
        setDT()

data3 <- xlsx::read.xlsx2("Caras_River_Romania.xls", 
                          sheetIndex = 1, 
                          colIndex = 1:8, 
                          colClasses = c("character", "character", "Date", "character", "character", "character", "numeric")
) %>% 
        setDT

data4 <- read_xlsx("Dejani_River_Romania_GetReal.xlsx") %>% 
        setDT

data5 <- read_xlsx("Sebes_River_Romania_GetReal.xlsx") %>% 
                setDT

data6 <- xlsx::read.xlsx2("Tur_River_Romana.xls",
                           sheetIndex = 1, 
                           colClasses = c("character", "character", "Date", "character", "character", "character", "numeric")) %>%
                setDT

# 02. Data Cleaning ----------------------------------------------------------
## -- data set 1 Aries river 
data1 %<>% rename(original_site_name = "Name of sampling site if known",
                  date = "date at which the sample was taken",
                  taxon = "taxon name at highest taxonomic resolution available", 
                  "x.coord" = longitude,
                  "y.coord" = latitude,
                  EPSG = "Preferebly the EPSG code")

x = data1[,.(x.coord, y.coord)]

names(x) <- c("lat", "long")        
chd = substr(x$lat, 5,5)[1]
chm = substr(x$lat, 8,8)[1]
chs = substr(x$lat, 11, 11)[1]

x$lat  %<>% str_remove("N") %>% str_trim    
x$long %<>% str_remove("E") %>% str_trim    

cd.lat  = sp::char2dms(x$lat, chd = chd, chm = chm, chs = chs)
cd.long = sp::char2dms(x$long, chd = chd, chm = chm, chs = chs)

data12 <- copy(data1)

data12$x.coord <- as.numeric(cd.long)
data12$y.coord <- as.numeric(cd.lat)

data12[, c("year", "season") := list(
        year = year(date),
        case_when(
                month(date) %in% c(12,1,2) ~ "winter",
                month(date) %in% c(3,4,5) ~ "spring",
                month(date) %in% c(6,7,8) ~ "summer",
                month(date) %in% c(9,10,11) ~ "autumn")
)]
names(data12)[2] <- "xx"
data12[, xx :=NULL]

## -- data set 2 
data21 = data2[, list(
        original_site_name = Code,
        date = data2$`sampling date` %>% dmy(),
        taxon = data2$`taxon name`,
        x.coord = data2$Longitudine,
        y.coord = data2$Latitudine,
        EPSG = data2$`Coordinate reference system`
)]

x = data21[,c("x.coord", "y.coord")]

names(x) <- c("long", "lat")        
chd = "°"
chm = "'"
chs = "\""

x$lat %<>% str_remove_all("N") %>%
        str_trim
x$long %<>% str_remove_all("E") %>%
        str_trim

cd.lat = sp::char2dms(x$lat, chd = chd, chm = chm, chs = chs)
cd.long = sp::char2dms(x$long, chd = chd, chm = chm, chs = chs)

data21 = data21[, c("x.coord", "y.coord", "year", "season") := list(
        as.numeric(cd.long),
        as.numeric(cd.lat),
        year(date),
        case_when(
                month(date) %in% c(12,1,2) ~ "winter",
                month(date) %in% c(3,4,5) ~ "spring",
                month(date) %in% c(6,7,8) ~ "summer",
                month(date) %in% c(9,10,11) ~ "autumn")
)]

## -- data set 3
data3 <- data3[-1, ]
x = data3[,c("X.coordinates.", "Y.coordinates.")]

names(x) <- c("lat", "long")        

x$lat %<>% str_replace_all("\"", "_")
x$long %<>% str_replace_all("\"", "_")

chd = "°"
chm = "'"
chs = "_"

x$lat %<>% str_remove("N") %>% str_trim    
x$long %<>% str_remove("E") %>% str_trim    

cd.lat  = sp::char2dms(x$lat, chd = "º", chm = "'", chs = "_")
cd.long = sp::char2dms(x$long, chd = "º", chm = "'", chs = "_")

data32 = data3[, list(
        original_site_name = Site_name.,
        date = ymd(`sampling.date.`),
        taxon = `taxon.name.`,
        x.coord = as.numeric(cd.long),
        y.coord = as.numeric(cd.lat),
        EPSG = `Coordinate.reference.system.`
)]

data32[, c("year", "season") := list(
        year = year(date),
        case_when(
                month(date) %in% c(12,1,2) ~ "winter",
                month(date) %in% c(3,4,5) ~ "spring",
                month(date) %in% c(6,7,8) ~ "summer",
                month(date) %in% c(9,10,11) ~ "autumn")
)]

## -- data set 4
data41 = data4[, list(
        original_site_name = Code,
        date = data4$`sampling date` %>% dmy(),
        taxon = data4$`taxon name`,
        x.coord = data4$Longitudine,
        y.coord = data4$Latitudine,
        EPSG = data4$`Coordinate reference system`
)]

x = data41[,c("x.coord", "y.coord")]

names(x) <- c("long", "lat")        

x$lat %<>% str_remove_all("N") %>%
        str_trim
x$long %<>% str_remove_all("N") %>%
        str_trim

cd.lat = sp::char2dms(x$lat, chd = "°", chm = "'", chs = "\"")
cd.long = sp::char2dms(x$long, chd = "°", chm = "'", chs = "\"")

data41 = data41[, c("x.coord", "y.coord", "year", "season") := list(
        as.numeric(cd.long),
        as.numeric(cd.lat),
        year(date),
        case_when(
                month(date) %in% c(12,1,2) ~ "winter",
                month(date) %in% c(3,4,5) ~ "spring",
                month(date) %in% c(6,7,8) ~ "summer",
                month(date) %in% c(9,10,11) ~ "autumn")
)]

## -- data set 5
data51 = data5[, list(
        original_site_name = Code,
        date = data5$`sampling date` %>% dmy(),
        taxon = data5$`taxon name`,
        x.coord = data5$Longitudine,
        y.coord = data5$Latitudine,
        EPSG = data5$`Coordinate reference system`
)]

x = data51[,c("x.coord", "y.coord")]

names(x) <- c("long", "lat")        

x$lat %<>% str_remove_all("N") %>%
        str_trim
x$long %<>% str_remove_all("N") %>%
        str_trim

cd.lat = sp::char2dms(x$lat, chd = "°", chm = "'", chs = "\"")
cd.long = sp::char2dms(x$long, chd = "°", chm = "'", chs = "\"")

data51 = data51[, c("x.coord", "y.coord", "year", "season") := list(
        as.numeric(cd.long),
        as.numeric(cd.lat),
        year(date),
        case_when(
                month(date) %in% c(12,1,2) ~ "winter",
                month(date) %in% c(3,4,5) ~ "spring",
                month(date) %in% c(6,7,8) ~ "summer",
                month(date) %in% c(9,10,11) ~ "autumn")
)]

## -- data set 6

data6 = data6[-1,]
x = data6[,c("X.coordinates.", "Y.coordinates.")]

names(x) <- c("lat", "long")        

x$lat %<>% str_remove("N") %>% str_trim    
x$long %<>% str_remove("E") %>% str_trim    

cd.lat = sp::char2dms(x$lat, chd = "°", chm = "’", chs = "’’")
cd.long = sp::char2dms(x$long, chd = "°", chm = "’", chs = "’’")

data62 = data6[, list(
        original_site_name = Site_name.,
        date = ymd(`sampling.date.`),
        taxon = `taxon.name.`,
        x.coord = as.numeric(cd.long),
        y.coord = as.numeric(cd.lat),
        EPSG = `Coordinate.reference.system.`
)]

data62[, c("year", "season") := list(
        year = year(date),
        case_when(
                month(date) %in% c(12,1,2) ~ "winter",
                month(date) %in% c(3,4,5) ~ "spring",
                month(date) %in% c(6,7,8) ~ "summer",
                month(date) %in% c(9,10,11) ~ "autumn")
)]


## -- combine into one dataset 
data12$date %<>% ymd()
data = rbindlist(list(data62, data51, data41, data32, data21, data12))

#- check on map 
sites <- unique(data, by = "original_site_name")
sites2 <- st_as_sf(sites, coords = c("x.coord", "y.coord"), crs = sites$EPSG[1])
mapview(sites2)

# harmonize taxonomy ----------------------------------------------------------------
#- extract diatom observations 
data = data[
        str_detect(taxon, "Achnanthidium") |
                str_detect(taxon, "Adlafia") |
                str_detect(taxon, "Amphora") |
                str_detect(taxon, "Aneumastus") |
                str_detect(taxon, "Aulacoseira") |
                str_detect(taxon, "Bacillaria") |
                str_detect(taxon, "Brachysira") |
                str_detect(taxon, "Brebissonia") |
                str_detect(taxon, "Caloneis") |
                str_detect(taxon, "Cocconeis") |
                str_detect(taxon, "Craticula") |
                str_detect(taxon, "Cyclotella") |
                str_detect(taxon, "Cymatopleura") |
                str_detect(taxon, "Cymbella") |
                str_detect(taxon, "Cymbopleura") |
                str_detect(taxon, "Diatoma") |
                str_detect(taxon, "Didymosphenia") |
                str_detect(taxon, "Diploneis") |
                str_detect(taxon, "Encyonema") |
                str_detect(taxon, "Eunotia") |
                str_detect(taxon, "Fragilaria") |
                str_detect(taxon, "Frustulia") |
                str_detect(taxon, "Gomphonella") |
                str_detect(taxon, "Gomphonema") |
                str_detect(taxon, "Gyrosigma") |
                str_detect(taxon, "Halamphora") |
                str_detect(taxon, "Hannaea") |
                str_detect(taxon, "Hantzschia") |
                str_detect(taxon, "Hippodonta") |
                str_detect(taxon, "Iconella") |
                str_detect(taxon, "Lemnicola") |
                str_detect(taxon, "Luticola") |
                str_detect(taxon, "Melosira") |
                str_detect(taxon, "Meridion") |
                str_detect(taxon, "Navicula") |
                str_detect(taxon, "Navigeia") |
                str_detect(taxon, "Neidium") |
                str_detect(taxon, "Nitzscha") |
                str_detect(taxon, "Odontidium") |
                str_detect(taxon, "Orthoseira") |
                str_detect(taxon, "Paraplaconeis") |
                str_detect(taxon, "Pinnularia") |
                str_detect(taxon, "Placoneis") |
                str_detect(taxon, "Psammothidium") |
                str_detect(taxon, "Reimeria") |
                str_detect(taxon, "Stauroneis") |
                str_detect(taxon, "Staurosirella") |
                str_detect(taxon, "Surirella") |
                str_detect(taxon, "Tabellaria") |
                str_detect(taxon, "Tetracyclus") |
                str_detect(taxon, "Tryblionella") |
                str_detect(taxon, "Ulnaria")
]

#- load taxon table 
taxontable <- readRDS("../../../2022-01-12_taxontable_diatoms.rds")

TU <- unique(data$taxon)
TU <- setdiff(TU, taxontable$original_name)

taxontable <- append_to_tt("Eunotia elegans", "Eunotia exigua var. bidens")
taxontable <- append_to_tt("Fragilaria bidens", "Fragilaria capucina et var. gracilis, var rumpens, var vaucheriae")
taxontable <- append_to_tt("Navicula exigua var. exigua", "Navicula exigua")
taxontable <- append_to_tt("Brebissonia lanceolata", " Brebissonia lanceolata")

saveRDS(taxontable, paste0("../../../",Sys.Date(),"_taxontable_diatoms.rds"))

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
data2[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_romania_cimpean_diatoms")]


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
        abundance = NA,
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

data3 <- unique(data3, by = c("gr_sample_id", "lowest.taxon"))
data4 <- copy(data3)
typologies <- readRDS("../../../../all_typologies.rds")
data5 <- add_typologies(data4)

## visual checks
sites <- unique(data5, by = "gr_sample_id") |> st_as_sf(coords = c("x.coord", "y.coord"), crs = data5$EPSG[1])

mapview(sites, zcol = "brt12")
mapview(sites, zcol = "ife")
mapview(sites, zcol = "bgr")
mapview(sites, zcol = "least.impacted")

## save to file
saveRDS(data5, paste0("../../romania_cimpean/",Sys.Date(),"_final_non_aggregated.rds"))

# temporal aggregation --------------------------------------------------------------

agg <- data5 |> unique(by = "gr_sample_id")
unique(table(agg$site_id))

source("../../../../../R/functions/newest_sample.R")
data6 <- newest_sample(data5)
saveRDS(data6, paste0("../..//romania_cimpean/",Sys.Date(),"_final_aggregated.rds"))



# statistics -------------------------------------------------------------------------


# time span
summary(data5$year)
# number of sites
uniqueN(data5$site_id)
# number of samples
uniqueN(data5$gr_sample_id)
# most recent
data6[, uniqueN(gr_sample_id)]
# reference condition
data6[least.impacted == TRUE, uniqueN(site_id)]
data6[least.impacted == TRUE, uniqueN(gr_sample_id)]
