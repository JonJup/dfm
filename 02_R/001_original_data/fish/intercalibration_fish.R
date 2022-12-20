### -------------------------------- ###
### --- Intercalibration Fish DB --- ### 
### -------------------------------- ###

# -------------------------------
# date written: 22.04.22
# date last modified: 09.06.22
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: Clean intercalibration fish DB
# Notes: 
# -------------------------------

# setup -----------------------------------------------

options("box.path" = "~/R/box_modules/")

pacman::p_load(data.table,
               magrittr,
              
               mapview,
               tidyr,
               mapview, 
               lubridate, 
               sf,
               dplyr,
               readxl,
               stringr)
source("R/functions/add_typologies.R")

# load data -------------------------------------------
bio <- read_excel("data/fish/original_data/mixed_intercalibration_didier_pont/raw/Catch selection final.xlsx")
sites <- read_excel("data/fish/original_data/mixed_intercalibration_didier_pont/raw/Fishing_Occasion selection final.xlsx")
taxontable <- readRDS("data/fish/2022-04-27_taxontable_fish.rds")
typologies <- readRDS("data/all_typologies.rds")

# prepare data ----------------------------------------

setDT(bio)
setDT(sites)

data <- sites[bio, on = "Site_code"]

data <- data[, c("Site_code", "Date", "Species", "Number", "E_latitude", "E_longitude", "Country_code")]
names(data) <- c("original_site_name", "date", "taxon","abundance", "y.coord", "x.coord", "country")
data$date <- ymd(data$date)
# sites <- unique(data, by = "original_site_name")
# sites %<>% st_as_sf(coords = c("x.coord", "y.coord"), crs = 4326)
# mapview::mapview(sites)

data$EPSG <- 4326
data$data.set <- "interaclibration_fish_db"

## add season and year 
data[,c("season", "year") := .(case_when(month(date) %in% c(12,1,2)  ~ "winter",
                                            month(date) %in% c(3,4,5)   ~ "spring",
                                            month(date) %in% c(6,7,8)   ~ "summer",
                                            month(date) %in% c(9,10,11) ~ "autumn"), 
                               year(date))]

data$taxon <- str_replace_all(data$taxon, "_", "\\ ")



TU <- unique(data$taxon)
TU <- setdiff(TU, taxontable$original_name)

#taxontable <- update_taxonomy2(TU)
#saveRDS(taxontable, "data/fish/2022-04-22_taxontable_fish.rds")

data%<>%rename("original_name" = taxon)
data3 <- taxontable[data, on = "original_name"]

data3[, site_id := .GRP, by = "original_site_name"]
data3[, site_id := as.numeric(site_id)]
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
data3[,gr_sample_id := paste0("site_", site_id, "_date_", date_id, "_intercalibration_fish")]

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
        country
)]

## combine entries of same taxon
data4[, lowest.taxon := ifelse(!is.na(species), species,
                               ifelse(!is.na(genus), genus,
                                      ifelse(!is.na(family), family,
                                             ifelse(!is.na(order), order,
                                                    ifelse(!is.na(class), class,
                                                           ifelse(!is.na(phylum), phylum, kingdom))))))]

data4[, abundance := as.numeric(abundance)]
data4[, abundance := sum(abundance), by = c("gr_sample_id", "lowest.taxon")]
data5 <- unique(data4, by = c("gr_sample_id", "lowest.taxon"))
data5 <- add_typologies(data5)
data5[country == "NO", least.impacted := TRUE]
## visual checks
sites <- unique(data5, by = "site_id") |> st_as_sf(coords = c("x.coord", "y.coord"), crs = data5$EPSG[1])

# mapview(sites, zcol = "brt12")
# mapview(sites, zcol = "ife")
# mapview(sites, zcol = "bgr")
mapview(sites, zcol = "least.impacted")

# - subset to least impacted catchments 
data6 <- data5[least.impacted == TRUE]
data6 <- data6[abundance != 0]
sites <- unique(data6, by = "site_id") |> st_as_sf(coords = c("x.coord", "y.coord"), crs = data5$EPSG[1])

## look for sites with different ID but same coordinates 
distances  <- st_distance(sites)
distances2 <- as.matrix(distances)
diag(distances2) <- 999
(duplicate_sites <- which(distances2 < units::as_units(1, "m")))

data6[, richness := uniqueN(lowest.taxon), by = "gr_sample_id"]
summary(data6$richness)
hist(data6$richness)
data7 <- data6[richness > 2]

# - drop sites far removed from ECRINS river network 
data8 <- data7[distance < 300]
updated_type_old <- readRDS("data/fish/original_data/mixed_intercalibration_didier_pont/2022-04-22_updated_type.rds")

## how many sites in data8: 314      
uniqueN(data8$site_id)
## sites covered in updated_type_old: 250  
sum(updated_type_old$site_id %in% data8$site_id)
## so we are missing: 64

# - visually check the assignment of sites 
rt <-
        data8 |>
        filter(!site_id %in% updated_type_old$site_id) |> 
        unique(by = "site_id") |>
        st_as_sf(coords = c("x.coord", "y.coord"),
                    crs = data5$EPSG[1])

plot_typology <- st_crop(typologies, st_transform(sites, crs = st_crs(typologies)))

updated_type <- data.table(site_id = rt$site_id)

options(warn = -1)

for (i in 1:nrow(rt)){
        #if (i < 327) next()
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

updated_type_combi <- bind_rows(updated_type, updated_type_old)

#- save the remove list. 
saveRDS(updated_type, paste0("data/fish/original_data/mixed_intercalibration_didier_pont/", Sys.Date(), "_updated_type_combi.rds"))
data9 <- left_join(data8, 
                   updated_type_combi, 
                   by = "site_id")

#- drop "drop" rows determined in for-loop      
data9 <- data9[new_type != "drop"]
data9[, brt12 := NULL]
data9 <- rename(data9, brt12 = new_type)

# temporal aggregation --------------------------------------------------------------
agg <- data9 |> unique(by = "gr_sample_id")
unique(table(agg$site_id))
# no 
#source("R/functions/newest_sample.R")
data10 <- copy(data9)

data10$data.set <- "intercalibration_fish"
data10[, country := NULL]
saveRDS(data10, paste0("data/fish/original_data/mixed_intercalibration_didier_pont/",Sys.Date(),"_final_aggregated.rds"))
data10 <- readRDS("data/fish/original_data/mixed_intercalibration_didier_pont/2022-04-25_final_aggregated.rds")

# statistics -------------------------------------------------------------------------
# time span
summary(data$year)
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
# histogram richness
unique(data10, by = "gr_sample_id") |> pull(richness) |> hist()