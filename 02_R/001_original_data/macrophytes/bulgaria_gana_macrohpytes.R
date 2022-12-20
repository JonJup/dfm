### --- Bulgaria Macrophytes  --- ### 

# date written: 11.03.22
# date last modified: 12.06.22
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: Clean macrophyte data provided by Gana Gecheva for Bulgaria 


# setup -----------------------------------------------

options("box.path" = "~/R/box_modules/")

library(data.table)
library(magrittr)
library(mapview)
library(ggplot2)
box::use(readxl[read_excel],
         lubridate[dmy, year, month],
         stringdist[amatch],
         sf = sf,
         dp = dplyr,
         box/dfm)
source("R/functions/add_typologies.R")
# load data -------------------------------------------------------------------------

data <- read_excel("data/macrophytes/original_data/bulgaria_gana/raw/template_jupke-BG.xlsx")
taxontable <- readRDS("data/macrophytes/2022-05-17_taxontable_macrophytes.rds")
typologies <- readRDS("data/all_typologies.rds")

# prepare data ----------------------------------------------------------------------
data2 <- data
setDT(data2)
data2[,Sample_ID := NULL]
names(data2)[2] <- "date"

## split into two date types 
data2.1 <- data2[stringr::str_detect(date, "/"), ]
data2.2 <- data2[!site_id %in% data2.1$site_id]

data2.1[,date := dmy(date)]
data2.2[,date := as.Date(as.numeric(date), origin = "1899-12-30")]

data2 <- rbindlist(list(data2.1, data2.2))

data2[,c("season", "year") := .(dp$case_when(month(date) %in% c(12,1,2)  ~ "winter",
                                             month(date) %in% c(3,4,5)   ~ "spring",
                                             month(date) %in% c(6,7,8)   ~ "summer",
                                             month(date) %in% c(9,10,11) ~ "autumn"), 
                                lubridate::year(date))]

data2$abundance <- 1
data2 %<>% dp$rename(y.coord = "Latitude", 
                     x.coord = "Longitude",
                     EPSG     = "Coordinate Refernce System",
                     taxon = "taxon_name")
data2$data.set <- "bulgaria_gana_macrophytes"

TU <- data2$taxon |>  unique()
TU <- dp$setdiff(TU, taxontable$original_name)

# - need to call function from dfm script 
# taxontable <- dfm$update_taxonomy_macrophytes(TU, taxontable_arg = taxontable)
# 
# taxontable[original_name == "Equisetum sp.", taxon_state := "drop"]
# taxontable[original_name == "Lemanea sp.", taxon_state := "red_algae"]
# taxontable[original_name == "Rumex sp.", taxon_state := "drop"]
# taxontable[original_name == "Cyperus glomeratus", taxon_state := "drop"]
# taxontable[original_name == "Juncus maritimus", taxon_state := "helophyte"]
# taxontable[original_name == "Carex sp.", taxon_state := "helophyte"]
# taxontable[original_name == "Lemna polyrhiza", taxon_state := "hydrophytes"]
# taxontable[original_name == "Polygonum sp.", taxon_state := "hydrophytes"]
# taxontable[original_name == "Mentha sp.", taxon_state := "drop"]
# taxontable[original_name == "Typha sp.", taxon_state := "helophytes"]
# taxontable[original_name == "Bidens frondosus", taxon_state := "helophytes"]
# taxontable[original_name == "Entodon concinnus", taxon_state := "moss"]
# saveRDS(taxontable, paste0("data/macrophytes/",Sys.Date(),"_taxontable_macrophytes.rds"))

data <- dp$rename(data2, original_name = taxon)
data2 <- taxontable[data, on = "original_name"]  
rm(taxontable, data, TU)

data2[, date_id := .GRP, by = "date"]

## add leading zeros
data2[, site_id := dp$case_when(
        nchar(trunc(site_id)) == 1 ~ paste0("0000", site_id),
        nchar(trunc(site_id)) == 2 ~ paste0("000", site_id),
        nchar(trunc(site_id)) == 3 ~ paste0("00", site_id),
        nchar(trunc(site_id)) == 4 ~ paste0("0", site_id),
        nchar(trunc(site_id)) == 5 ~ paste0(site_id))]
data2[, date_id := dp$case_when(
        nchar(trunc(date_id)) == 1 ~ paste0("0000", date_id),
        nchar(trunc(date_id)) == 2 ~ paste0("000",  date_id),
        nchar(trunc(date_id)) == 3 ~ paste0("00",   date_id),
        nchar(trunc(date_id)) == 4 ~ paste0("0",    date_id),
        nchar(trunc(date_id)) == 5 ~ paste0(        date_id))]

## add gr_sample_id
data2[,gr_sample_id := paste0("site_", site_id, "_date_", date_id, "_bulgaria_monitoring_macrophytes")]

## reshape data
data3 <- data2[, list(
        gr_sample_id,
        original_site_name = site_id,
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
data5 <- add_typologies(data4)

## visual checks
sites <- unique(data5, by = "site_id") |> sf$st_as_sf(coords = c("x.coord", "y.coord"), crs = data5$EPSG[1])
mapview(sites, zcol = "least.impacted")

# - subset to least impacted catchments 
data6 <- data5[least.impacted == TRUE]

sites <- unique(data6, by = "site_id") |> sf$st_as_sf(coords = c("x.coord", "y.coord"), crs = data5$EPSG[1])

## look for sites with different ID but same coordinates 
distances  <- sf$st_distance(sites)
distances2 <- as.matrix(distances)
diag(distances2) <- 999
(duplicate_sites <- which(distances2 < units::as_units(1, "m")))

dfm$find_point(17, distances2)
sites[c(3,2), ]

# - I drop the third site (duplicates are sites 2 and 3). No rule for selecting between sites obvious. 
data6 <- data6[gr_sample_id != "site_00008_date_00003_bulgaria_monitoring_macrophytes"]

# - drop sites with very few species 
data6[, richness := uniqueN(lowest.taxon), by = "gr_sample_id"]
data7 <- data6#[richness > 10]

# - drop sites far removed from ECRINS river network 
data8 <- data7[distance < 300]
uto <- readRDS("data/macrophytes/original_data/bulgaria_gana/2022-03-17_updated_type.rds")

# - visually check the assignment of sites 
rt <- 
        data8 |> 
        dp$filter(!site_id %in% uto$site_id) |> 
        unique(by = "site_id") |> 
        sf$st_as_sf(coords = c("x.coord", "y.coord"), 
                    crs = data5$EPSG[1])

plot_typology <- sf$st_crop(typologies, sf$st_transform(sites, crs = sf$st_crs(typologies)))
updated_type <- data.table(site_id = rt$site_id)

for (i in 1:nrow(rt)){
        i.rt <- rt[i, ]
        i.plot_typology <- sf$st_crop(plot_typology, sf$st_buffer(sf$st_transform(i.rt, crs = sf$st_crs(typologies)), dist =  2000))
        x <- mapview::mapview(i.plot_typology, zcol = "brt", map.type = "OpenStreetMap.DE") + mapview::mapview(i.rt, color = "red")
        print(x)
        i.bool <- "n"
        i.bool <- readline(paste(i, ":"))
        if (i.bool == "break"){
                break()
        }
        if (i.bool == "n"){
                # remove_list[length(remove_list) + 1] <- i.rt$site_id 
                updated_type[site_id == i.rt$site_id, new_type := "drop"]
        } else if (i.bool == "change"){
                i.towhat <- readline("change to:")
                updated_type[site_id == i.rt$site_id, new_type := i.towhat]
        } else {
                updated_type[site_id == i.rt$site_id, new_type := i.rt$brt12]
        }
        
        rm(list = ls()[grepl("i\\.", ls())])
}

utc <- dp$bind_rows(uto, updated_type)

# - save the remove list. 
saveRDS(utc, paste0("data/macrophytes/original_data/bulgaria_gana/", Sys.Date(), "_updated_type_combined.rds"))

data9 <- dp$left_join(data8, 
                   utc, 
                   by = "site_id")

#- drop "drop" rows determined in for-loop      
data9 <- data9[new_type != "drop"]
data9[, brt12 := NULL]
data9 <- dp$rename(data9, brt12 = new_type)

data9 <- data9[species != ""]
data9$brt12 |> unique()

# temporal aggregation --------------------------------------------------------------
agg <- data9 |> unique(by = "gr_sample_id")
unique(table(agg$site_id))
# - no 
data10 <- data9[month(date) %in% 5:9]

saveRDS(data10, paste0("data/macrophytes/original_data/bulgaria_gana/",Sys.Date(),"_final_aggregated.rds"))

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
unique(data10, by = "gr_sample_id") |> dp$pull(richness) |> mean()
# histogram richness
unique(data10, by = "gr_sample_id") |> dp$pull(richness) |> hist()
