# —————————————————————————————————————————————————————————— #
# ——— Clean Diatom data from Germany, Bavaria——— # 
# —————————————————————————————————————————————————————————— #

# ———————————————————————————————————
# date first written: 21-12-13
# date last modified: 22-01-25
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: In this script I create a harmonized spatial data set of diatom from the 
# raw data from Germany, Bavaria.  
# Temporal aggregation:  
# CRS: 
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
library(readxl)
library(tidyr)

source("R/functions/harmonize diatoms.R")

# load data -------------------------------------------------------------------------
## directory with all biological data 
bio_wd <- "data/diatoms/original_data/germany_bavaria/raw/files/"


bio_files <- dir_ls(bio_wd)
## read and format all files. Combine them in one table  
for (i in seq_along(bio_files)){
        
        ## create list to save list output 
        if (i == 1) out = list()
        
        x <- fread(bio_files[i], fill = TRUE)
        
        header  <- x[1:7,]
        ms_nr   <- pull(header[5,2])
        x.coord <- pull(header[7,2])
        y.coord <- pull(header[7,4])
        
        data <- x[10:nrow(x),]
        data %<>% filter(V7 %in% c("Pennales", "Centrales"))
        date <- dmy(pull(data[,1]))
        taxa <- pull(data[,3])
        abundance <- as.numeric(str_replace(data$V5, ",", "."))
        
        xx <- data.table(original_site_name = ms_nr, 
                         date = date, 
                         taxon = taxa,
                         abundance = abundance,
                         x.coord = x.coord, 
                         y.coord = y.coord)
        
        out[[i]] <- xx
        if (i == length(bio_files)) out <- rbindlist(out)
        rm(i, x, header, x.coord, y.coord, data, xx, abundance, taxa, ms_nr)
        
}

dia1 <- readRDS("data/diatoms/fwb_table.rds")
dia2 <- readRDS("data/diatoms/omn_table.rds")
#non_diatom_algae <- readRDS("data/diatoms/2021-12-08_non_diatom_algae.rds")

# prepare data ----------------------------------------------------------------------

data <- out 
rm(out )
## add season 
data[,c("year", "season") := .(year(date), 
                               case_when(month(date) %in% c(12,1,2)  ~ "winter",
                                         month(date) %in% c(3,4,5)   ~ "spring",
                                         month(date) %in% c(6,7,8)   ~ "summer",
                                         month(date) %in% c(9,10,11) ~ "autumn"))]
## remove sites without coordinates
data <- data[!is.na(x.coord)]
data <- data[!is.na(y.coord)]

## add EPSG
data[, EPSG :=25832]

## check sites 
data |> 
        unique(by = "original_site_name") |> 
        pull(x.coord) |> 
        unique()
data |> 
        unique(by = "original_site_name") |> 
        pull(y.coord) |> 
        unique()

sites <- unique(data, by = "original_site_name")
sites2 <- st_as_sf(sites, coords = c("x.coord", "y.coord"), crs = 25832)
mapview(sites2)



taxontable <- readRDS("data/diatoms/2022-01-25_taxontable_diatoms.rds")

data <- data[!taxon %in% c("Centrales")]

TU <- unique(data$taxon) |> sort()
TU <- setdiff(TU, taxontable$original_name)

# join 
data <- rename(data, original_name = taxon)
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
data2[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_germany_bavaria_diatoms")]

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
        data.set = "germany_bavaria_diatoms"
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

# - visually check the assignment of sites 

rt <- 
        data8 |> 
        unique(by = "site_id") |> 
        st_as_sf(coords = c("x.coord", "y.coord"), 
                 crs = data5$EPSG[1])

plot_typology <- st_crop(typologies, st_transform(sites, crs = st_crs(typologies)))

remove_list <- c()

options(warn = -1)

for (i in 1:nrow(rt)){
        i.rt <- rt[i, ]
        i.plot_typology <- st_crop(plot_typology, 
                                   st_buffer(
                                           st_transform(i.rt, 
                                                        crs = st_crs(typologies)
                                           ), 
                                           dist =  2000)
        )
        i.x <- 
                mapview(i.plot_typology, zcol = "brt12") + 
                mapview(i.rt, color = "red")
        print(i.x)
        i.bool <- "n"
        i.bool <- readline(paste(i,"/",nrow(rt), ":"))
        if (i.bool == "n"){
                remove_list[length(remove_list) + 1] <- i.rt$site_id  
        }
        if (i.bool == "c"){
                i.towhat <- readline("change to:")
                data8[site_id == i.rt$site_id, brt12 := i.towhat]
        }
        rm(list = ls()[grepl("i\\.", ls())])
}

#- save the remove list. 
saveRDS(remove_list, paste0("data/diatoms/original_data/germany_bavaria/", Sys.Date(), "_remove_list.rds"))

connection_list <- data10[, c("site_id", "brt12")] |> unique(by = "site_id")
saveRDS(connection_list, paste0("data/diatoms/original_data/french_irstea/", Sys.Date(), "_connection_list.rds"))
#- drop remove sites 
data9 <- data8[!site_id %in% remove_list]


agg <- data9 |> unique(by = "gr_sample_id")
unique(table(agg$site_id))
source("R/functions/newest_sample.R")
data10 <- newest_sample(data9)
data10
saveRDS(data10, paste0("data/diatoms/original_data/germany_bavaria/",Sys.Date(),"_final_aggregated.rds"))
data10 <- readRDS("data/diatoms/original_data/spain_goma/2022-01-25_final_aggregated.rds")

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