# ------------------------------------------------ #
# --- Clean Diatom data from Czech - krkonose  --- #
# ------------------------------------------------ #

# -----------------------------------
# date first written: 27-04-22
# date last modified: 27-04-22
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: In this script I create a harmonized spatial data set of diatom from the 
# raw data Krkonose data from the Czech Republic provided by Petr Paril (30.03.22). 
# CRS: 
# ----------------


# setup -----------------------------------------------------------------------------
options(box.path = "~/R/box_modules")
box::use(readxl[read_excel],
         d = dplyr,
         sf = sf,
         box/dfm)

library(tidyr)
library(data.table)
library(magrittr)
library(sf)
library(stringr)
library(janitor)
# load data -------------------------------------------------------------------------

# load data ----------------------------------------------------------------------
data <- read_excel("data/diatoms/original_data/czech_krkonose/raw/Diatoms_artificial_snow_making_small stream in Krkonose Mts_Giant Mts_Straka.xlsx")
                  

dia1 <- readRDS("data/diatoms/fwb_table.rds")
dia2 <- readRDS("data/diatoms/omn_table.rds")
taxontable <- readRDS("data/diatoms/2022-04-22_taxontable_diatoms.rds")

# prepare data  ---------------------------------------------------------------------

data
var.date <- lubridate::ymd(data$`Sampling date`)
var.name <- data$Site_ID
coords   <- 
        data$Coordinates |> 
        str_split(",") |> 
        lapply(FUN = str_trim) |> 
        purrr::transpose()
var.x <-
        unlist(coords[[2]]) %>%
        sp::char2dms(chd = "°", chm = "'", chs = "\"") %>%
        as.numeric()
var.y <-
        unlist(coords[[1]])  %>% 
        sp::char2dms(chd = "°", chm = "'", chs = "\"") %>%
        as.numeric()      
var.tax <- data$species

data <- data.table(
        original_site_name = var.name, 
        date               = var.date,
        taxon              = var.tax,
        abundance          = 1,
        x.coord            = var.x,
        y.coord            = var.y
)

data[,c("year", "season") := .(year(date), 
                               d$case_when(month(date) %in% c(12,1,2)  ~ "winter",
                                         month(date) %in% c(3,4,5)   ~ "spring",
                                         month(date) %in% c(6,7,8)   ~ "summer",
                                         month(date) %in% c(9,10,11) ~ "autumn"))]


sites <- unique(data, by = "original_site_name")
sites %<>% st_as_sf(coords = c("x.coord", "y.coord"), crs = 4326)
mapview::mapview(sites)
## taxonomic cleaning in fix_tax script. 

## add taxon information 
data <- d$rename(data, original_name = taxon)
setDT(data)
data2 <- taxontable[data, on = "original_name"]  
(missing_taxa <- data2[is.na(kingdom), unique(original_name)])

## check
sort(unique(data2$phylum))
sort(unique(data2$class))

## add site and date ids for this I need to round coordinates because some samples are
## categorized as from different sites even though they are from the same.
# data2[, c("x.coord_round", "y.coord_round") := .(round(x.coord, 5), 
#                                                  round(y.coord, 5))]

data2[, site_id := .GRP, by = "original_site_name"]
data2[, date_id := .GRP, by = "date"]

## add leading zeros
data2[, site_id := d$case_when(
        nchar(trunc(site_id)) == 1 ~ paste0("0000", site_id),
        nchar(trunc(site_id)) == 2 ~ paste0("000", site_id),
        nchar(trunc(site_id)) == 3 ~ paste0("00", site_id),
        nchar(trunc(site_id)) == 4 ~ paste0("0", site_id),
        nchar(trunc(site_id)) == 5 ~ paste0(site_id))]
data2[, date_id := d$case_when(
        nchar(trunc(date_id)) == 1 ~ paste0("0000", date_id),
        nchar(trunc(date_id)) == 2 ~ paste0("000",  date_id),
        nchar(trunc(date_id)) == 3 ~ paste0("00",   date_id),
        nchar(trunc(date_id)) == 4 ~ paste0("0",    date_id),
        nchar(trunc(date_id)) == 5 ~ paste0(        date_id))]

## add gr_sample_id
data2[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_czech_krkonose_diatoms")]


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
        EPSG = 4326,
        data.set = "czech_krkonose_diatoms"
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
data5 <- jjmisc::add_typologies(data4)

# # ## visual checks
sites <- unique(data5, by = "gr_sample_id") |> sf$st_as_sf(coords = c("x.coord", "y.coord"), crs = data5$EPSG[1])
mapview::mapview(sites, zcol = "least.impacted")

# - subset to least impacted catchments 
data6 <- data5[least.impacted == TRUE]

sites <- unique(data6, by = "site_id") |> sf$st_as_sf(coords = c("x.coord", "y.coord"), crs = data5$EPSG[1])

## look for sites with different ID but same coordinates 
distances  <- sf$st_distance(sites)
distances2 <- as.matrix(distances)
diag(distances2) <- 999
(duplicate_sites <- which(distances2 < units::as_units(1, "m")))

dfm$find_point(6, distances2)
sites

## at this point there would only be a single site left 