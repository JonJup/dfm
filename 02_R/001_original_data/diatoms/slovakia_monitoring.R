# ---------------------------------------- #
# --- Clean Diatom data from Slovakia  --- #
# ---------------------------------------- #

# -----------------------------------
# date first written: 03-05-22
# date last modified: 11.06.22
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: In this script I create a harmonized spatial data set of diatom from the 
# raw data Slovkian monitoring provided by Lucia Sochuliakova (02.05.22) 
# CRS: 5514
# ----------------


# setup -----------------------------------------------------------------------------
options(box.path = "~/R/box_modules")
box::use(box/dfm)
library(pacman)
p_load(dplyr, 
       data.table,
       jjmisc,
       lubridate,
       magrittr, 
       mapview,
       sf,
       tidyr,
       stringr,
       stringdist,
       units,
       readxl)
source("R/functions/add_typologies.R")
# load data ----------------------------------------------------------------------
data <- read_excel("data/diatoms/original_data/slovakia/raw/Diatoms_data_Slovakia.xlsx") 

# dia1 <- readRDS("data/diatoms/fwb_table.rds")
# dia2 <- readRDS("data/diatoms/omn_table.rds")
taxontable <- readRDS("data/diatoms/2022-05-17_taxontable_diatoms.rds")

# prepare data  ---------------------------------------------------------------------

data %<>% rename(original_site_name = site_id,
                 date = sapling_data,
                 taxon = taxon_name, 
                 y.coord = Latitude,
                 x.coord = Longitude,
                 EPSG    = `Coordinate Reference System`)
setDT(data)
data[, c("Sample_ID", "code_of_taxa", "name_of_locality") := NULL]
data[, c("abundance", "data.set", "date") := .(1, "slovakia_monitoring_diatoms", ymd(date))]

data[,c("year", "month") := list(year(date), month(date))]
data[,"season" := ifelse(month %in% c(12,1,2), "winter", ifelse(month %in% c(3,4,5), "spring", ifelse(month %in% c(6,7,8), "summer", "autumn")))]
data <- data[!is.na(x.coord)]
sites <- unique(data, by = "original_site_name")
sites %<>% st_as_sf(coords = c("x.coord", "y.coord"), crs = data$EPSG[1])
mapview(sites)

## taxonomic cleaning in fix_tax script. 

## add taxon information 
data <- rename(data, original_name = taxon)
setDT(data)
data2 <- taxontable[data, on = "original_name"]  
## any taxa missing?
data2[is.na(kingdom), unique(original_name)]

## check
sort(unique(data2$phylum))
sort(unique(data2$class))

#data2[, x.coord_round := round(x.coord, 5)]
#data2[, y.coord_round := round(y.coord, 5)]

#data2[, site_id := .GRP, by = c("x.coord_round", "y.coord_round")]
data2[, site_id := .GRP, by = "original_site_name"]
data2[, date_id := .GRP, by = "date"]
data2[, site_id := as.numeric(site_id)]
data2[, date_id := as.numeric(date_id)]

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
data2[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_slovakian_monitoring_diatoms")]

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
data3 <- unique(data3, by = c("gr_sample_id", "lowest.taxon"))
data4 <- copy(data3)
typologies <- readRDS("data/all_typologies.rds")
data5 <- add_typologies(data4)

# - visual checks
sites <- unique(data5, by = "gr_sample_id") |> st_as_sf(coords = c("x.coord", "y.coord"), crs = data5$EPSG[1])
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
uto <- readRDS("data/diatoms/original_data/slovakia/2022-05-03_updated_type.rds")

# - 29
uniqueN(data8$site_id)
# - 19 
sum(unique(data8$site_id) %in% uto$site_id)
# - 10 missing 


# - visually check the assignment of sites 
rt <- 
        data8 |> 
        filter(!site_id %in% uto$site_id) |> 
        unique(by = "site_id") |> 
        st_as_sf(coords = c("x.coord", "y.coord"), 
                    crs = data5$EPSG[1])

plot_typology <- st_crop(typologies, st_transform(sites, crs = st_crs(typologies)))
options(warn = -1)

updated_type <- data.table(site_id = rt$site_id)

for (i in 1:nrow(rt)){
        i.rt <- rt[i, ]
        i.plot_typology <- st_crop(plot_typology, st_buffer(st_transform(i.rt, crs = st_crs(typologies)), dist =  2000))
        x <- mapview(i.plot_typology, zcol = "brt") + mapview(i.rt, popup = c("water_body"), color = "red")
        print(x)
        #i.bool <- "n"
        i.bool <- readline(paste(i, "/", nrow(rt), ":"))
        if (i.bool == "break"){
                break()
        } else if (i.bool == "n"){
                updated_type[site_id == i.rt$site_id, new_type := "drop"]
        } else if (i.bool == "c"){
                i.towhat <- readline("change to:")
                updated_type[site_id == i.rt$site_id, new_type := i.towhat]
        } else {
                updated_type[site_id == i.rt$site_id, new_type := i.rt$brt12]
        }
        
        rm(list = ls()[grepl("i\\.", ls())])
}

utc <- bind_rows(uto, updated_type)

# - save the remove list. 
saveRDS(utc, paste0("data/diatoms/original_data/slovakia//", Sys.Date(), "_updated_type_combined.rds"))

# - join updated types to data 8
data9 <- left_join(data8, 
                   utc, 
                   by = "site_id")

# - drop "drop" rows determined in for-loop
data9 <- data9[new_type != "drop"]
data9[, brt12 := NULL]
data9 <- rename(data9, brt12 = new_type)

# temporal aggregation --------------------------------------------------------------
agg <- data9 |> unique(by = "gr_sample_id")
unique(table(agg$site_id))
# yes 
# drop sites from outsite focal months
data10 <- data9[month(date) %in% 5:9]
source("R/functions/newest_sample.R")
data10 <- newest_sample(data10, season_available = FALSE)


saveRDS(data10, paste0("data/diatoms/original_data/slovakia/",Sys.Date(),"_final_aggregated.rds"))
#data10 <- readRDS("data/diatoms/original_data/czech_chmi/2022-03-31_final_aggregated.rds")

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





