## -- Portugal Macrophytes

# date written: 17.06.22
# date last modified: 17.06.22
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: Clean macrophyte data for Portugal provided by Maria Teresa Ferreira 16.06.22
# CRS: 4326

# setup -----------------------------------------------

library(pacman)
p_load(
        data.table,
        magrittr,
        mapview,
        ggplot2,
        readxl,
        lubridate,
        stringdist,
        sf,
        dplyr,
        readxl,
        tidyr
)

source("R/functions/add_typologies.R")
source("R/functions/dfm.R")

# load data -------------------------------------------------------------------------

data <- read_excel("data/macrophytes/original_data/portugal_monitoring/raw/Macrophytes matrix.xlsx")
taxontable <- readRDS("data/macrophytes/2022-06-14_taxontable_macrophytes.rds")
typologies <- readRDS("data/all_typologies.rds")

# prepare data -----------------------------------------------------------------------

# - I will need to split the data in sites and biodata first. 
sites <- data[1:6, ]

slice(sites, 1) |> select(-c(1:2)) |> as.character()


sites2 <- data.table(
        waterbody  = slice(sites, 1) |> select(-c(1:2)) |> as.character(), 
        wb_code    = slice(sites, 2) |> select(-c(1:2)) |> as.character(),  
        longitude  = slice(sites, 3) |> select(-c(1:2)) |> as.numeric(),  
        latitude   = slice(sites, 4) |> select(-c(1:2)) |> as.numeric(),
        date       = slice(sites, 5) |> select(-c(1:2)) |> as.numeric(),
        sampling_site = slice(sites, 6) |> select(-c(1:2)) |> as.character()
)

sites2[,date := as.Date(date, origin = "1899-12-30")]
sites2[, original_site_name := paste0(wb_code, "_", sampling_site)]
# Fist 28 rows are algae
bio <- data[-c(1:34), -1]
names(bio)[1] <- "taxon"
names(bio)[2:ncol(bio)] <- sites2$original_site_name
bio <- pivot_longer(bio, cols = !taxon, names_to = "original_site_name", values_to = "abundance")
bio <- filter(bio, !is.na(abundance))
setDT(bio)
data <- bio[sites2, on = "original_site_name"]

# - check sites on map 
sites <- 
        unique(data, by = "original_site_name") |> 
        st_as_sf(coords = c("longitude", "latitude"), crs = "EPSG:4326")
mapview(sites)
data[, c("EPSG", "data.set") := .(4326, "portugal_monitoring_macrophytes")]

TU <- unique(data$taxon)
TU <- setdiff(TU, taxontable$original_name) |> sort()

# - fixtax script

data <- rename(data, original_name = taxon)
data2 <- taxontable[data, on = "original_name"]  
rm(taxontable, data, TU)

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
data2[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_portugal_monitoring_macrophytes")]

## reshape data 
data3 <- data2[, list(
        gr_sample_id,
        original_site_name,
        date,
        year = year(date),
        season = NA,
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
        abundance = NA,
        x.coord = longitude,
        y.coord = latitude,
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

# - visual checks 
sites <- unique(data5, by = "gr_sample_id") |> st_as_sf(coords = c("x.coord", "y.coord"), crs = data5$EPSG[1])

mapview(sites, zcol = "brt12")
# mapview(sites, zcol = "ife")
# mapview(sites, zcol = "bgr")
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
data7 <- data6[richness > 1]

# - drop sites far removed from ECRINS river network 
data8 <- data7[distance < 300]

# - visually check the assignment of sites 
rt <- 
        data8 |> 
        unique(by = "site_id") |> 
        st_as_sf(coords = c("x.coord", "y.coord"), 
                 crs = data5$EPSG[1])

plot_typology <- st_crop(typologies, st_transform(sites, crs = st_crs(typologies)))
options(warn = -1)
# updated_type <- data.table(site_id = rt$site_id)
# 
# for (i in 1:nrow(rt)){
#         i.rt <- rt[i, ]
#         i.plot_typology <- st_crop(plot_typology, st_buffer(st_transform(i.rt, crs = st_crs(typologies)), dist =  2000))
#         x <- mapview::mapview(i.plot_typology, 
#                               zcol = "brt"#,
#                               #map.type = "OpenStreetMap.DE"
#         ) + mapview::mapview(i.rt, color = "red")
#         print(x)
#         i.bool <- "n"
#         i.bool <- readline(paste(i, ":"))
#         if (i.bool == "break"){
#                 break()
#         }
#         if (i.bool == "n"){
#                 # remove_list[length(remove_list) + 1] <- i.rt$site_id 
#                 updated_type[site_id == i.rt$site_id, new_type := "drop"]
#         } else if (i.bool == "c"){
#                 i.towhat <- readline("change to:")
#                 updated_type[site_id == i.rt$site_id, new_type := i.towhat]
#         } else {
#                 updated_type[site_id == i.rt$site_id, new_type := i.rt$brt12]
#         }
#         
#         rm(list = ls()[grepl("i\\.", ls())])
# }
# 
# #- save the remove list. 
# saveRDS(updated_type, paste0("data/macrophytes/original_data/portugal_monitoring/", Sys.Date(), "_updated_type.rds"))

updated_type <- readRDS("data/macrophytes/original_data/portugal_monitoring/2022-06-17_updated_type.rds")

data9 <- left_join(data8, 
                   updated_type, 
                   by = "site_id")

#- drop "drop" rows determined in for-loop      
data9 <- data9[new_type != "drop"]
data9[, brt12 := NULL]
data9 <- rename(data9, brt12 = new_type)

data9 <- data9[species != ""]
data9$brt12 |> unique()

# temporal aggregation --------------------------------------------------------------
agg <- data9 |> unique(by = "gr_sample_id")
unique(table(agg$site_id))
# - no 
data10 <- data9[month(date) %in% 5:9]
saveRDS(data10, paste0("data/macrophytes/original_data/portugal_monitoring/",Sys.Date(),"_final_aggregated.rds"))

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
