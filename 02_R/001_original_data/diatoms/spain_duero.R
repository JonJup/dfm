### ------------------------------------------- ###
### --- Diatom data from Duerto Basin Spain --- ### 
### ------------------------------------------- ###

# date written: 22-02-02
# date last modified: 11.06.22
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: Clean and harmonize data provided by Duero River authority
# CRS: 

# setup -----------------------------------------------
options("box.path" = "~/R/box_modules/")

library(data.table)
library(magrittr)
library(mapview)
box::use(readxl[read_excel],
         lubridate[ymd_hms, year, month],
         stringdist[amatch],
         sf = sf,
         dp = dplyr,
         box/dfm)
source("R/functions/add_typologies.R")
# load data -------------------------------------------

bio      <- setDT(read_excel("data/diatoms/original_data/spain_duero/raw_data/Datos_MACROF_DIATO_2016_20.xlsx"))
bio_id   <- setDT(read_excel("data/diatoms/original_data/spain_duero/raw_data/TAXONES_ID_TAXON.xlsx"))
sites    <- setDT(read_excel("data/diatoms/original_data/spain_duero/raw_data/INFO_PUNTOS_MUESTREO.xlsx"))
sampling <- setDT(read_excel("data/diatoms/original_data/spain_duero/raw_data/METODO_MUESTREO.xlsx"))

# dia1 <- readRDS("data/diatoms/fwb_table.rds")
# dia2 <- readRDS("data/diatoms/omn_table.rds")

taxontable <- readRDS("data/diatoms/2022-05-17_taxontable_diatoms.rds")

# prepare data ----------------------------------------

## join bio_id to bio 
bio2 <- bio_id[bio, on = "ID_TAXON"]
rm(bio_id);gc()

## subset to diatoms 
bio2 <- bio2[FILO_DIVISION == "Bacillariophyta"]

## add samplig information 
all(bio2$`PUNTO DE MUESTREO` %in% sites$`PUNTO DE MUESTREO`)
bio3 <- sites[bio2, on = "PUNTO DE MUESTREO"]

## add sampling method 
names(sampling)[which(names(sampling) == "COD_METMUES")] <- "CODIGO_METODO MUESTREO"
bio4 <- sampling[bio3, on = "CODIGO_METODO MUESTREO"]
bio4

## what parameters where measured? 
table(bio4$`CÓDIGO PARÁMETRO`)

bio5 <- bio4[`CÓDIGO PARÁMETRO` == "NUMVAL"]

## check parameters that should be all the same now 
unique(bio5$UNIDAD)
unique(bio5$`NOMBRE PARÁMETRO`)

## remove lakes 
bio5 <- bio5[LAGO == FALSE]
bio5 <- bio5[SISTQE == "DIATOMEAS"]
unique(bio5$DESCRIPCION)

## reshape 
data <- data.table(
        original_site_name = bio5$'PUNTO DE MUESTREO', 
        date               = ymd_hms(bio5$`FECHA MUESTREO`),
        taxon              = bio5$NOMBRE, 
        abundance          = bio5$`VALOR NUMÉRICO`,
        x.coord            = bio5$X30_PM_Corr_ETRS89,
        y.coord            = bio5$Y30_PM_Corr_ETRS89,
        EPSG               = 25830,
        data.set              = "spain_duero_diatom"
)

## add season and year 
data[,c("year", "season") := .(year(date), 
                               dp$case_when(month(date) %in% c(12,1,2)  ~ "winter",
                                         month(date) %in% c(3,4,5)   ~ "spring",
                                         month(date) %in% c(6,7,8)   ~ "summer",
                                         month(date) %in% c(9,10,11) ~ "autumn"))]

## check CRS 
# sites <- unique(data, by = "original_site_name")
# sites2 <- sf$st_as_sf(sites, coords = c("x.coord", "y.coord"), crs = 25830)

## drop taxa 
data <- data[!taxon %in% c("Sin asignar", "Bacillariophyta")]

rm (sites, sites2, sampling)
rm(list = ls()[grepl(pattern = "^bio", x = ls())])
# harmonize taxonomy ----------------------------------------------------------------

## in external script (fix_tax)

# join ------------------------------------------------------------------------------

data <- dp$rename(data, original_name = taxon)
data2 <- taxontable[data, on = "original_name"]  

rm(taxontable, dia1, dia2)

data2[, site_id := .GRP, by = "original_site_name"]
data2[, date_id := .GRP, by = "date"]

all (dp$pull( data2[,uniqueN(x.coord), by = "site_id"], V1) == 1)

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
data2[,gr_sample_id := paste0("site_", site_id, "_date_", date_id, "_spain_duero_diatoms")]

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
        abundance,
        x.coord,
        y.coord,
        EPSG,
        data.set = "spain_duero_diatom"
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

# - visual checks
# sites <- unique(data5, by = "site_id") |> sf$st_as_sf(coords = c("x.coord", "y.coord"), crs = data5$EPSG[1])
# mapview(sites, zcol = "brt12")
# mapview(sites, zcol = "ife")
# mapview(sites, zcol = "bgr")
# mapview(sites, zcol = "least.impacted")

# - subset to least impacted catchments 
data6 <- data5[least.impacted == TRUE]
sites <- unique(data6, by = "site_id") |> sf$st_as_sf(coords = c("x.coord", "y.coord"), crs = data5$EPSG[1])

# - look for sites with different ID but same coordinates 
distances  <- sf$st_distance(sites)
distances2 <- as.matrix(distances)
diag(distances2) <- 999
(duplicate_sites <- which(distances2 < units::as_units(1, "m")))

# - drop sites with very few species 
data6[, richness := uniqueN(lowest.taxon), by = "gr_sample_id"]
data7 <- data6[richness > 10]

# - drop sites far removed from ECRINS river network 
data8 <- data7[distance < 300]
rl <- readRDS("data/diatoms/original_data/spain_duero/2022-02-02_remove_list.rds")
data8 <- data8[!site_id %in% rl]

# - visually check the assignment of sites 
rt <- 
        data8 |> 
        unique(by = "site_id") |> 
        sf$st_as_sf(coords = c("x.coord", "y.coord"), 
                 crs = data5$EPSG[1])

plot_typology <- sf$st_crop(typologies, sf$st_transform(sites, crs = sf$st_crs(typologies)))
options(warn = -1)
updated_type <- data.table(site_id = rt$site_id)
for (i in 1:nrow(rt)) {
        i.rt <- rt[i,]
        i.plot_typology <-
                sf$st_crop(plot_typology,
                        sf$st_buffer(sf$st_transform(i.rt, crs = sf$st_crs(typologies)), dist =  2000))
        x <-
                mapview(i.plot_typology,
                        zcol = "brt",
                        map.type = "OpenStreetMap.DE") + mapview(i.rt,
                                                                 popup = c("water_body"),
                                                                 color = "red")
        print(x)
        #i.bool <- "n"
        i.bool <- readline(paste(i, "/", nrow(rt), ":"))
        if (i.bool == "break") {
                break()
        } else if (i.bool == "n") {
                updated_type[site_id == i.rt$site_id, new_type := "drop"]
        } else if (i.bool == "c") {
                i.towhat <- readline("change to:")
                updated_type[site_id == i.rt$site_id, new_type := i.towhat]
        } else {
                updated_type[site_id == i.rt$site_id, new_type := i.rt$brt12]
        }
        
        rm(list = ls()[grepl("i\\.", ls())])
}

#- save the remove list. 
saveRDS(updated_type, paste0("data/diatoms/original_data/spain_duero//", Sys.Date(), "_updated_type.rds"))

# - drop remove sites 
data9 <- dp$left_join(data8, 
                      updated_type, 
                   by = "site_id")

#- drop "drop" rows determined in for-loop
data9 <- data9[new_type != "drop"]
data9[, brt12 := NULL]
data9 <- dp$rename(data9, brt12 = new_type)

# temporal aggregation --------------------------------------------------------------
agg <- data9 |> unique(by = "gr_sample_id")
unique(table(agg$site_id))
# - yes 
data10 <- data9[lubridate::month(date) %in% 5:9]
source("R/functions/newest_sample.R")
data10 <- newest_sample(data10, season_available = FALSE)
saveRDS(data10, paste0("data/diatoms/original_data/spain_duero/",Sys.Date(),"_final_aggregated.rds"))


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

