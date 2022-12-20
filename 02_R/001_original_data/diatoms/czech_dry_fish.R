# —————————————————————————————————————————————————————————————————— #
# ——— Clean Diatom data from Czech - dry fishponds  ———————————————— #
# —————————————————————————————————————————————————————————————————— #

# ———————————————————————————————————
# date first written: 31-03-22
# date last modified: 31-03-22
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: In this script I create a harmonized spatial data set of diatom from the 
# raw data dry fishpond data from the Czech Republic provided by Petr Paril (30.03.22). 
# CRS: 
# ————————————————


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
bio <- read_excel("data/diatoms/original_data/czech_dry_fishpond/raw/fytobentos_fish_dry-fishpond_data_Polasek.xlsx", sheet = 1) 
sit <- read_excel("data/diatoms/original_data/czech_dry_fishpond/raw/fytobentos_fish_dry-fishpond_data_Polasek.xlsx", sheet = 3) 

dia1 <- readRDS("data/diatoms/fwb_table.rds")
dia2 <- readRDS("data/diatoms/omn_table.rds")
taxontable <- readRDS("data/diatoms/2022-03-30_taxontable_diatoms.rds")

# prepare data  ---------------------------------------------------------------------

site_codes <- 
        names(bio)[-1] |> 
        str_remove("\\.\\.\\..*") 
        
site_codes2 <- 
        paste0(site_codes, 1:110)


dates        <- copy(bio)
dates        <- dates[1,-1]     
names(dates) <- site_codes2
dates        %<>% 
        str_replace_all(pattern = ".*\\..*", "") %>%
        as.numeric() %>%
        excel_numeric_to_date()
dates <- data.table(
        site_code = site_codes2,
        join_code = site_codes,
        date = dates
)
dates[which(dates$site_code == "PLO_2109")]$date      <- lubridate::ymd("2020-12-18")
dates[which(dates$site_code == "MVN_N_NAD_242")]$date <- lubridate::ymd("2010-10-31")

# - drop date row
bio <- bio[-1, ]
# - change cell values to numeric to prevent error in pivot. (currently mixed class). 
bio <- cbind(bio$`site code`, data.frame(apply(bio[,-1], 2, as.numeric)))
# - change column name to prevent error in pivor and have unique sample ids
names(bio) <- append("taxon", site_codes2)
bio2 <- 
        bio |> 
        pivot_longer(cols = -taxon, names_to = "site_code", values_to = "abundance") |> 
        d$filter(!is.na(abundance)) |> 
        setDT()
bio3 <- dates[bio2, on = "site_code"]
sit %<>% d$rename(join_code = "site code")
setDT(sit)
data <- sit[bio3, on = "join_code"]

data[, original_site_name := join_code]
data[, join_code := NULL]
data[, site_code := NULL]
data <- data[!is.na(GPS)]
coordinates <- 
        data$GPS |> 
        str_split(",") |> 
        lapply(FUN = function(x) str_remove(string = x, pattern = "N")) |> 
        lapply(FUN = function(x) str_remove(string = x, pattern = "E")) |> 
        lapply(FUN = str_trim      ) |> 
        lapply(FUN = as.numeric    ) |> 
        purrr::transpose()
y.coordinates <- unlist(coordinates[[1]])
x.coordinates <- unlist(coordinates[[2]])
data[, c("x.coord", "y.coord", "GPS") := .(x.coordinates, y.coordinates, NULL)]
rm(coordinates, x.coordinates, y.coordinates)
data[, water_body := `Stream name`]
data[, `Stream name` := NULL]
data[, `Locality name` := NULL]
data[,c("year", "month") := list(lubridate::year(date), lubridate::month(date))]
data[,"season" := ifelse(month %in% c(12,1,2), "winter", ifelse(month %in% c(3,4,5), "spring", ifelse(month %in% c(6,7,8), "summer", "autumn")))]
data <- data[!is.na(x.coord)]
sites <- unique(data, by = "original_site_name")
sites %<>% sf$st_as_sf(coords = c("x.coord", "y.coord"), crs = 4326)
mapview::mapview(sites)

## taxonomic cleaning in fix_tax script. 
taxontable <- readRDS("data/diatoms/2022-03-31_taxontable_diatoms.rds")

## add taxon information 
data <- d$rename(data, original_name = taxon)
setDT(data)
data2 <- taxontable[data, on = "original_name"]  
(missing_taxa <- data2[is.na(kingdom), unique(original_name)])

## check
sort(unique(data2$phylum))
sort(unique(data2$class))

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
data2[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_czech_dry_pond_diatoms")]


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
        data.set = "czech_dry_pond_diatoms",
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

# - drop sites with very few species 
data6[, richness := uniqueN(lowest.taxon), by = "gr_sample_id"]

data7 <- data6[richness > 10]

# - drop sites far removed from ECRINS river network 
data8 <- data7[distance < 300]

# - no site met this condition 