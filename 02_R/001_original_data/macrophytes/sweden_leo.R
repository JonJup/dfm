## -- Sweden Macrophytes

# date written: 14.06.22
# date last modified: 14.06.22
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: Clean macrophyte data for Sweden provided by Leonard Sandin 14.06.22
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
        readxl
)

source("R/functions/add_typologies.R")
source("R/functions/dfm.R")

# load data -------------------------------------------------------------------------

data <- fread("data/macrophytes/original_data/sweden_leo/raw/Macrophytes_Sweden_LEO.csv")
sites <- read_excel("data/macrophytes/original_data/sweden_leo/raw/Site_coordinates.xlsx")
taxontable <- readRDS("data/macrophytes/2022-06-14_taxontable_macrophytes.rds")
typologies <- readRDS("data/all_typologies.rds")


# prepare data ----------------------------------------------------------------------

data %<>% 
        rename(waterbody = Watercourse, taxon = Species, Site_code = 'Site_Number of Untitled 3') %>%
        select(Site_code, taxon, waterbody)
setDT(sites)
sites <- sites[, c("Site_code", "Latitude", "Longitude")]
data2 <- sites[data, on = "Site_code"]

data2 <- data2[!taxon %in% c("Unknown", "No macrophytes in the water")]

data2[, date := ymd("2005-12-01")]

TU <- data2$taxon |>  unique()
TU <- setdiff(TU, taxontable$original_name) |> sort()

data2 <- rename(data2, original_name = taxon)
data2 <- taxontable[data2, on = "original_name"]  
rm(taxontable, data, TU)

data2[, site_id := .GRP, by = "Site_code"]
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
data2[,gr_sample_id := paste0("site_", site_id, "_date_", date_id, "_sweden_leo_macrophytes")]

## reshape data
data3 <- data2[, list(
        gr_sample_id,
        original_site_name = Site_code,
        date,
        year = NA,
        season = NA,
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
        x.coord = Longitude,
        y.coord = Latitude,
        EPSG = 4326,
        data.set = "sweden_macrophytes_leo"
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
data5 <- add_typologies(data4)

## visual checks
sites <- unique(data5, by = "site_id") |> st_as_sf(coords = c("x.coord", "y.coord"), crs = data5$EPSG[1])
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
data7 <- data6#[richness > 10]

# - drop sites far removed from ECRINS river network 
data8 <- data7[distance < 300]

# # - visually check the assignment of sites 
# rt <- 
#         data8 |> 
#         unique(by = "site_id") |> 
#         st_as_sf(coords = c("x.coord", "y.coord"), 
#                     crs = data5$EPSG[1])
# 
# plot_typology <- st_crop(typologies, st_transform(sites, crs = st_crs(typologies)))
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
# 
# #- save the remove list. 
# saveRDS(updated_type, paste0("data/macrophytes/original_data/sweden_leo/", Sys.Date(), "_updated_type_combined.rds"))

updated_type <- readRDS("data/macrophytes/original_data/sweden_leo/2022-06-17_updated_type_combined.rds")

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
data10 <- data9

saveRDS(data10, paste0("data/macrophytes/original_data/sweden_leo/",Sys.Date(),"_final_aggregated.rds"))

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
# histogram richness
unique(data10, by = "gr_sample_id") |> pull(richness) |> hist()


