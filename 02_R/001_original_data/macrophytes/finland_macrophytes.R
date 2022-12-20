# ——— Clean macrophyte data from Finland ——— # 

# date first written: 14-02-22
# date last modified: 14-02-22
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: In this script I create a harmonized spatial data set from the raw data provided 
# from Finland. 
# CRS: 4055

# setup -----------------------------------------------------------------------------
options("box.path" = "~/R/box_modules/")
library(data.table)
library(ggplot2)
library(plotly)
box::use(readxl[read_excel], 
         l = lubridate,
         t = tidyr,
         s = sf,
         d = dplyr,
         st = stringr)

box::use(box/dfm)
source("R/functions/add_typologies.R")
# load data -------------------------------------------------------------------------
data     <- read_excel("data/macrophytes/original_data/finland_jukka/raw/MacrophytesAndBryophytes_MeanCoverPerRiffleSite_N188_ToJupke.xlsx", sheet = 2) 
setDT(data)

# prepare data ----------------------------------------------------------------------
## change names 
names(data)[1:5] <-c("sample_id", "original_site_name", "y.coord", "x.coord", "date")

## change date format 
data[, date := l$ymd(date)]

## pivot to long format 
data2 <- t$pivot_longer(data, cols = !(sample_id:date), names_to = "taxon", values_to = "coverage")
setDT(data2)
## drop zero coverage observations 
data2 <- data2[coverage > 0]

## find CRS 
data2[,uniqueN(y.coord), by = "original_site_name"]$V1 |>  unique()
data2[,uniqueN(x.coord), by = "original_site_name"]$V1 |>  unique()
data2[,uniqueN(original_site_name), by = "x.coord"]$V1 |>  unique()
data2[,uniqueN(original_site_name), by = "y.coord"]$V1 |>  unique()

# sites <- unique(data2, by = "original_site_name")
# sites <- s$st_as_sf(sites, coords = c("x.coord", "y.coord"), crs = 4055)
mapview::mapview(sites)

rm(sites)

data2[, EPSG := 4055]
data2[, data.set := "finnland_monitoring_macrophytes"]

## add season 
data2[,c("year", "season") := .(l$year(date), 
                               d$case_when(l$month(date) %in% c(12,1,2)  ~ "winter",
                                         l$month(date) %in% c(3,4,5)   ~ "spring",
                                         l$month(date) %in% c(6,7,8)   ~ "summer",
                                         l$month(date) %in% c(9,10,11) ~ "autumn"))]


taxontable <- readRDS("data/macrophytes/2022-06-12_taxontable_macrophytes.rds")
typologies <- readRDS("data/all_typologies.rds")


data <- data2
rm(data2)


data[, taxon := st$str_replace(taxon, "\\.", "\\ ")]
data[, taxon := st$str_remove(taxon, "\\.L\\.$")]
data <- data[!taxon %in% c("Bryophytes_TotalCoverInMacrophyteSurvey", "Dichelymoides", "Mycopus europeus")]
# clean taxonomy --------------------------------------------------------------------
(TU <- unique(data$taxon) |> sort())

(TU <- setdiff(TU,taxontable$original_name))

taxontable <- dfm$update_taxonomy_macrophytes(TU, taxontable)

taxontable[clean == FALSE, original_name]

dfm$new_entry(ori = "",  )

taxontable[taxon_state == "", original_name]

# taxontable <- new_entry_nd(ori = "Agrostis sp.", spe = NA, gen = "Agrostis")                               
# taxontable <- new_entry_nd(ori = "Gardamine pratensis", spe = "Cardamine pratensis")                       
# taxontable <- new_entry_nd(ori = "Glyseria sp.", sp = NA, gen = "Glyceria")                               
# taxontable <- new_entry_nd(ori = "Hydrocharis morsus.ranae", sp = "Hydrocharis morsus-ranae")                  
# taxontable <- new_entry_nd(ori = "Marsupella sp ", spe = NA, gen = "Marsupella")                             
# taxontable <- new_entry_nd(ori = "Nymphaea ssp.candida.C.Presl.J.Persl.Korsh", spe = "Nymphaea candida") 
# taxontable <- new_entry_nd(ori = "Petasites sp.ruttojuuri", spe = NA, gen = "Petasites")                   
# taxontable <- new_entry_nd(ori = "Ranunculus ssp.peltatus", spe = "Ranunculus peltatus")    

taxontable[original_name %in% c("Heinä sp.", "Heinä sp.2"), c("species", "genus") := NA]
taxontable[original_name %in% c("Heinä sp.", "Heinä sp.2"), c("family", "order", "class", "phylum", "kingdom") := .("Poaceae", "Poales", "Liliopsida", "Tracheophyta", "Plantae")]

taxontable <- taxontable[!duplicated(taxontable$original_name)]

saveRDS(taxontable, paste0("data/macrophytes/",Sys.Date(),"_taxontable_macrophytes.rds"))


# combine data ----------------------------------------------------------------------

data <- d$rename(data, original_name = taxon)
data2 <- taxontable[data, on = "original_name"]  

rm(taxontable, data, TU)

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
data2[,gr_sample_id := paste0("site_", site_id, "_date_", date_id, "_finland_monitoring_macrophytes")]

# - check that gr_sample_id matches sample_id

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
        data.set = "finland_monitoring_macrophytes"
)]

## combine entries of same taxon
data3[, lowest.taxon := ifelse(!is.na(species), species,
                               ifelse(!is.na(genus), genus,
                                      ifelse(!is.na(family), family,
                                             ifelse(!is.na(order), order,
                                                    ifelse(!is.na(class), class,
                                                           ifelse(!is.na(phylum), phylum, kingdom))))))]

#data3[, abundance := as.numeric(abundance)]
#data3[, abundance := sum(abundance), by = c("gr_sample_id", "lowest.taxon")]
data3 <- unique(data3, by = c("gr_sample_id", "lowest.taxon"))
data4 <- copy(data3)
#typologies <- readRDS("data/all_typologies.rds")
data5 <- add_typologies(data4)

## visual checks
sites <- unique(data5, by = "site_id") |> s$st_as_sf(coords = c("x.coord", "y.coord"), crs = data5$EPSG[1])

library(mapview)

mapview(sites, zcol = "brt12")
mapview(sites, zcol = "ife")
mapview(sites, zcol = "bgr")
mapview(sites, zcol = "least.impacted")

# - subset to least impacted catchments 
data6 <- data5[least.impacted == TRUE]

sites <- unique(data6, by = "site_id") |> s$st_as_sf(coords = c("x.coord", "y.coord"), crs = data5$EPSG[1])

## look for sites with different ID but same coordinates 
distances  <- s$st_distance(sites)
distances2 <- as.matrix(distances)
diag(distances2) <- 999
(duplicate_sites <- which(distances2 < units::as_units(1, "m")))

# - drop sites with very few species 
data6[, richness := uniqueN(lowest.taxon), by = "gr_sample_id"]
data7 <- data6#[richness > 10]

# - drop sites far removed from ECRINS river network 
data8 <- data7[distance < 300]
uto <- readRDS("data/macrophytes/original_data/finland_jukka/2022-02-22_updated_type.rds")

# - visually check the assignment of sites 
rt <- 
        data8 |>
        d$filter(!site_id %in% uto$site_id) |> 
        unique(by = "site_id") |> 
        s$st_as_sf(coords = c("x.coord", "y.coord"), 
                    crs = data5$EPSG[1])

plot_typology <- s$st_crop(typologies, s$st_transform(sites, crs = s$st_crs(typologies)))

updated_type <- data.table(site_id = rt$site_id)

for (i in 1:nrow(rt)){
        print(paste(i, "/", nrow(rt)))
        i.rt <- rt[i, ]
        i.plot_typology <- s$st_crop(plot_typology, s$st_buffer(s$st_transform(i.rt, crs = s$st_crs(typologies)), dist =  2000))
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

utc <- d$bind_rows(uto, updated_type)

#- save the remove list. 
saveRDS(utc, paste0("data/macrophytes/original_data/finland_jukka//", Sys.Date(), "_updated_type_combined.rds"))

data9 <- d$left_join(data8, 
                      utc, 
                      by = "site_id")


#- drop "drop" rows determined in for-loop      
data9 <- data9[new_type != "drop"]
data9[, brt12 := NULL]
data9 <- d$rename(data9, brt12 = new_type)

data9 <- data9[species != ""]
data9$brt12 |> unique()

# temporal aggregation --------------------------------------------------------------
agg <- data9 |> unique(by = "gr_sample_id")
unique(table(agg$site_id))
# - no 
data10 <- data9[month(date) %in% 5:9]

saveRDS(data10, paste0("data/macrophytes/original_data/finland_jukka/",Sys.Date(),"_final_aggregated.rds"))

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
unique(data10, by = "gr_sample_id") |> d$pull(richness) |> mean()
# histogram richness
unique(data10, by = "gr_sample_id") |> d$pull(richness) |> hist()

