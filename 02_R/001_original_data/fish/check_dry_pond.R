### ----------------------------- ###
### --- Clean FISH CZECH drypong --- ### 
### ----------------------------- ###

# -------------------------------
# date written: 02.05.22
# date last modified: 02.05.22
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: Clean Czech FISH CHMI Data 
# Notes: 
# -------------------------------

# setup -----------------------------------------------

library(pacman)

options("box.path" = "~/R/box_modules/")

p_load(data.table,
       magrittr,
       #jjmisc,
       mapview,
       tidyr,
       mapview,
       dplyr,
       readxl,
       stringr,
       sf,
       stringdist,
       lubridate,
       units)

box::use(box/dfm)

# load data -------------------------------------------
bio   <- read_excel("data/fish/original_data/czech_dry_fishpond/raw/fytobentos_fish_dry-fishpond_data_Polasek.xlsx", sheet = 2)
sites <- read_excel("data/fish/original_data/czech_dry_fishpond/raw/fytobentos_fish_dry-fishpond_data_Polasek.xlsx", sheet = 3)
taxontable <- readRDS("data/fish/2022-04-27_taxontable_fish.rds")
typologies <- readRDS("data/all_typologies.rds")
source("R/functions/add_typologies.R")


# prepare data ----------------------------------------

setDT(sites)
names(sites)[1] <- "site_code"
date <- 
        bio |> 
        t() |> 
        {\(x) x[-1,1]}() |> 
        as.numeric() |> 
        as.Date(origin = "1899-12-31")

site_names <- names(bio)[-1]

date2 <- data.table(site_code = site_names,
                    date = date)

bio <- bio[-1,]
bio %<>%
        pivot_longer(cols = !`site code`)
setDT(bio)
bio <- bio[!is.na(value)]
bio[, site_code := name]
bio <- date2[bio, on = "site_code"]
bio[, site_code := str_remove(site_code, "\\..*")]
data <- sites[bio, on = "site_code"]
data[, date := ymd(date)]
rm(sites, bio, date, date2)


data[, original_site_name := site_code]
data[, waterbody := `Stream name`]
data[, taxon := `site code`]
data[, abundance := value]

data[, c("name", "Locality name", "site_code", "Stream name", "site code", "value") := NULL]
y.coord <- 
        str_split(data$GPS, pattern = ",") |> 
        sapply(function(x) x[1]) |> 
        str_remove("N$") |> 
        as.numeric()
x.coord <- 
        str_split(data$GPS, pattern = ",") |> 
        sapply(function(x) x[2]) |> 
        str_remove("E$") |> 
        as.numeric()

data[, c("x.coord", "y.coord") := .(x.coord, y.coord)]
data[, GPS := NULL]
data <- data[!is.na(x.coord) & !is.na(y.coord)]
rm(x.coord, y.coord)
sites <- unique(data, by = "original_site_name")
sites %<>% st_as_sf(coords = c("x.coord", "y.coord"), crs = "EPSG:4326")
mapview(sites)
rm(sites)

data[, c("EPSG", "data.set") := .(4326, "czech_drypond_fish")]


## add season and year 
data[,c("season", "year") := .(case_when(month(date) %in% c(12,1,2)  ~ "winter",
                                            month(date) %in% c(3,4,5)   ~ "spring",
                                            month(date) %in% c(6,7,8)   ~ "summer",
                                            month(date) %in% c(9,10,11) ~ "autumn"), 
                               year(date))]

TU <- unique(data$taxon)
TU <- setdiff(TU, taxontable$original_name)

data%<>%rename("original_name" = taxon)
data3 <- taxontable[data, on = "original_name"]

data3[, site_id := .GRP, by = "original_site_name"]
#data3[, site_id := as.numeric(site_id)]
data3[, date_id := .GRP, by = "date"]

## add leading zeros
data3[, site_id := case_when(
        nchar(trunc(site_id)) == 1 ~ paste0("0000", site_id),
        nchar(trunc(site_id)) == 2 ~ paste0("000", site_id),
        nchar(trunc(site_id)) == 3 ~ paste0("00", site_id),
        nchar(trunc(site_id)) == 4 ~ paste0("0", site_id),
        nchar(trunc(site_id)) == 5 ~ paste0(site_id))]
data3[, date_id := case_when(
        nchar(trunc(date_id)) == 1 ~ paste0("0000", date_id),
        nchar(trunc(date_id)) == 2 ~ paste0("000",  date_id),
        nchar(trunc(date_id)) == 3 ~ paste0("00",   date_id),
        nchar(trunc(date_id)) == 4 ~ paste0("0",    date_id),
        nchar(trunc(date_id)) == 5 ~ paste0(        date_id))]

## add gr_sample_id
data3[,gr_sample_id := paste0("site_", site_id, "_date_", date_id, "_czech_drypond_fish")]

# - check that gr_sample_id matches sample_id

## reshape data
data4 <- data3[, list(
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
        waterbody
)]

## combine entries of same taxon
data4[, lowest.taxon := ifelse(!is.na(species), species,
                               ifelse(!is.na(genus), genus,
                                      ifelse(!is.na(family), family,
                                             ifelse(!is.na(order), order,
                                                    ifelse(!is.na(class), class,
                                                           ifelse(!is.na(phylum), phylum, kingdom))))))]

data4[, abundance := 1]
data5 <- unique(data4, by = c("gr_sample_id", "lowest.taxon"))
data5 <- add_typologies(data5)

## visual checks
sites <- 
        unique(data5, by = "site_id") |> 
        st_as_sf(coords = c("x.coord", "y.coord"), crs = data5$EPSG[1])
mapview(sites, zcol = "least.impacted")

# - subset to least impacted catchments 
data6 <- data5[least.impacted == TRUE]

## -- no sites remains