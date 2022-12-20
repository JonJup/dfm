### --- Macrophytes data from Duerto Basin Spain --- ### 

# -------------------------------
# date written: 02.02.22
# date last modified: 14.06.21
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: Clean and harmonize data provided by Duero River authority
# CRS: 

# setup -----------------------------------------------

options("box.path" = "~/R/box_modules/")

library(data.table)
library(magrittr)
library(ggplot2)
library(plotly)
library(mapview)

box::use(readxl[read_excel],
         lubridate[ymd_hms, year, month],
         stringdist[amatch],
         sf = sf,
         dp = dplyr,
         box/dfm)

source("R/functions/add_typologies.R")
source("R/functions/dfm.R")

# load data -------------------------------------------

bio      <- setDT(read_excel("data/macrophytes/original_data/spain_duero/raw_data/Datos_MACROF_DIATO_2016_20.xlsx"))
bio_id   <- setDT(read_excel("data/macrophytes/original_data/spain_duero/raw_data/TAXONES_ID_TAXON.xlsx"))
sites    <- setDT(read_excel("data/macrophytes/original_data/spain_duero/raw_data/INFO_PUNTOS_MUESTREO.xlsx"))
sampling <- setDT(read_excel("data/macrophytes/original_data/spain_duero/raw_data/METODO_MUESTREO.xlsx"))

taxontable <- readRDS("data/macrophytes/2022-06-14_taxontable_macrophytes.rds")

# prepare data ----------------------------------------

## join bio_id to bio 
bio2 <- bio_id[bio, on = "ID_TAXON"]
rm(bio_id);gc()

## subset to diatoms 
unique(bio2$FILO_DIVISION)
bio2 <- bio2[FILO_DIVISION %in% c("Chlorophyta", "Bryophyta", "Spermatophyta")]

## add samplig information 
all(bio2$`PUNTO DE MUESTREO` %in% sites$`PUNTO DE MUESTREO`)
bio3 <- sites[bio2, on = "PUNTO DE MUESTREO"]

## add sampling method 
names(sampling)[which(names(sampling) == "COD_METMUES")] <- "CODIGO_METODO MUESTREO"
bio4 <- sampling[bio3, on = "CODIGO_METODO MUESTREO"]
bio4

## remove lakes 
bio4 <- bio4[LAGO == FALSE]
bio4 <- bio4[SISTQE == "MACROFITOS"]

## what parameters where measured? 
table(bio4$DESCRIPCION)
table(bio4$`CÓDIGO PARÁMETRO`)

# data5 <- bio4[DESCRIPCION == "Protocolo de muestreo y laboratorio de Macrófitos en Ríos"]
# data6 <- bio4[DESCRIPCION == "Nuevo protocolo de macrófitos en río en fase de prueba"]

summary(data5$`FECHA MUESTREO`)
summary(data6$`FECHA MUESTREO`)

## check parameters that should be all the same now 
unique(bio5$UNIDAD)
unique(bio5$`NOMBRE PARÁMETRO`)
unique(bio5$DESCRIPCION)

## reshape 
bio5 <- bio4
data <- data.table(
        original_site_name = bio5$'PUNTO DE MUESTREO', 
        date               = ymd_hms(bio5$`FECHA MUESTREO`),
        taxon              = bio5$NOMBRE, 
        abundance          = NA,
        x.coord            = bio5$X30_PM_Corr_ETRS89,
        y.coord            = bio5$Y30_PM_Corr_ETRS89,
        EPSG               = 25830,
        data.set              = "spain_duero_macrophyte", 
        sampling.method    = bio4$DESCRIPCION
)

## add season and year 
data[,c("year", "season") := .(year(date), 
                               dp$case_when(month(date) %in% c(12,1,2)  ~ "winter",
                                            month(date) %in% c(3,4,5)   ~ "spring",
                                            month(date) %in% c(6,7,8)   ~ "summer",
                                            month(date) %in% c(9,10,11) ~ "autumn"))]

## check CRS 
# sites <- unique(data, by = "original_site_name")
# sites2 <- st_as_sf(sites, coords = c("x.coord", "y.coord"), crs = 25830)
# mapview::mapview(sites2)

## drop taxa 
TU <- sort(unique(data$taxon))

#data <- data[!taxon %in% c("Sin asignar", "Bacillariophyta")]

rm (sites, sites2, sampling)
rm(list = ls()[grepl(pattern = "^bio", x = ls())])
# harmonize taxonomy ----------------------------------------------------------------

TU <- TU[which(!TU %in% taxontable$original_name)]

taxontable[, clean := TRUE]
(TU <- TU[which(!TU %in% taxontable$original_name)])
# join ------------------------------------------------------------------------------

data <- dp$rename(data, original_name = taxon)
data2 <- taxontable[data, on = "original_name"]  

rm(taxontable, data, TU)

data2[, site_id := .GRP, by = "original_site_name"]
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
data2[,gr_sample_id := paste0("site_", site_id, "_date_", date_id, "_spain_duero_macrophytes")]

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
        data.set = "spain_duero_macrophytes",
        sampling.method
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
data7 <- data6#[richness > 10]

# - drop sites far removed from ECRINS river network 
data8 <- data7[distance < 300]
rl <- readRDS("data/macrophytes/original_data/spain_duero/2022-02-03_remove_list.rds")
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

for (i in 1:nrow(rt)){
        i.rt <- rt[i, ]
        i.plot_typology <- sf$st_crop(plot_typology, sf$st_buffer(sf$st_transform(i.rt, crs = sf$st_crs(typologies)), dist =  2000))
        x <- mapview::mapview(i.plot_typology, 
                              zcol = "brt"#,
                              #map.type = "OpenStreetMap.DE"
        ) + mapview::mapview(i.rt, color = "red")
        print(x)
        i.bool <- "n"
        i.bool <- readline(paste(i, ":"))
        if (i.bool == "break"){
                break()
        }
        if (i.bool == "n"){
                # remove_list[length(remove_list) + 1] <- i.rt$site_id 
                updated_type[site_id == i.rt$site_id, new_type := "drop"]
        } else if (i.bool == "c"){
                i.towhat <- readline("change to:")
                updated_type[site_id == i.rt$site_id, new_type := i.towhat]
        } else {
                updated_type[site_id == i.rt$site_id, new_type := i.rt$brt12]
        }
        
        rm(list = ls()[grepl("i\\.", ls())])
}

#- save the remove list. 
saveRDS(updated_type, paste0("data/macrophytes/original_data/spain_duero//", Sys.Date(), "_updated_type.rds"))

#- drop remove sites 
data9 <- dp$left_join(data8, 
                      updated_type, 
                      by = "site_id")

#- drop "drop" rows determined in for-loop      
data9 <- data9[new_type != "drop"]
data9[, brt12 := NULL]
data9 <- dp$rename(data9, brt12 = new_type)

data9sf <- unique(data9, by = "site_id")
data9sf <- sf$st_as_sf(data9sf, coords = c("x.coord", "y.coord"), crs = data9$EPSG[1])
mapview(data9sf, zcol = "sampling.method")
# decide on sampling method 

table(data9$sampling.method)
unique(data9, by = "gr_sample_id") |> dp$pull(richness) |> mean()
unique(data9, by = "gr_sample_id") |> dp$group_by(sampling.method) |> dp$summarize(richness = mean(richness))

## 7 zu 5 

data9$phylum |> table()
data9.1 <- data9[phylum %in% c("Bryophyta", "Tracheophyta")]

data9.1[, richness := uniqueN(lowest.taxon), by = "gr_sample_id"]

(fig <- plot_ly(x = ~data9.1$richness, type = "histogram", color = data9.1$sampling.method))

## are species poor sites spatially clustered ? 
data9.1[richness < 4, poor := TRUE]
data9.1[is.na(poor), poor := FALSE]
data9sf <- copy(data9.1)
data9sf %<>% unique(by = "gr_sample_id") %>%
        sf$st_as_sf(coords = c("x.coord", "y.coord"), crs = data9$EPSG[1])
mapview(data9sf, zcol = "poor")
## No spatial clustering of species poor sites. 


## drop richness below 4
data9.2 <- data9.1[richness > 3]
data9.2.sites <- copy(data9.2)
data9.2.sites %<>% unique(by = "gr_sample_id") %>%
        sf$st_as_sf(coords = c("x.coord", "y.coord"), crs = data9$EPSG[1])

data9.2 <- 
        data9.2 |> 
        dp$mutate(abundance = 1) |> 
        tidyr::pivot_wider(id_cols = c(gr_sample_id, sampling.method), names_from = lowest.taxon, values_from = abundance, values_fill = 0)

data9.method <- dp$pull(data9.2, sampling.method)
dm <- vegan::vegdist(data9.2[,-c(1,2)], method = "jaccard")
(anosim.result <- vegan::anosim(dm, grouping = data9.method))
nmds.result <- vegan::metaMDS(dm)
nmds.result2 <- data.frame(nmds.result$points) |> dp$mutate(method = data9.method)

ggplotly(ggplot(nmds.result2, aes(x = MDS1, y = MDS2, fill = method)) + 
        geom_point(shape = 21, size = 4))


## compare dissimilarity with mantel test of compositional and spatial dissimilarity/ distance
## and regression 

## look for sites with different ID but same coordinates 
distances  <- sf$st_distance(data9.2.sites)
distances2 <- as.matrix(distances)


## weak (0.18) but statistically significant (0.001) correlation
vegan::mantel(xdis = distances2, ydis = dm )
vegan::mantel(xdis = dm, ydis = distances2 )

distance_regression <- data.table(space = units::drop_units(c(distances2)) , 
                                ecology = c(dm))

ggplot(distance_regression, aes(x = space, y = ecology)) + 
        geom_point(alpha = 0.1) + 
        geom_smooth()

## compare to ebro data 
data.ebro <- readRDS("data/macrophytes/original_data/spain_ebro/2022-01-27_final_aggregated.rds")
## what phlya are sampled in ebro? 
unique(data.ebro$phylum)
data.ebro2 <- data.ebro[phylum %in% c("Bryophyta", "Tracheophyta")]
data.ebro2[, sampling.method := "ebro"]

data.ebro2[, richness := uniqueN(lowest.taxon), by = "gr_sample_id"]
data.ebro2 <- data.ebro2[gr_sample_id != "site_00262_date_00431_spain_ebro_macrophytes"]
fig <- plot_ly(x =  ~ data.ebro2$richness, type = "histogram")
data.ebro2 <- data.ebro2[richness > 3]
data9.2 <- data9.1[richness > 3]
data9.2[, date := lubridate::round_date(date, unit = "day")]
data9.2[, date := lubridate::ymd(date)]

data.combined <- rbindlist(list(data9.2, data.ebro2), fill = TRUE)

data.combined2 <- 
        data.combined |> 
        dp$mutate(abundance = 1) |> 
        tidyr::pivot_wider(id_cols = c(gr_sample_id, sampling.method), names_from = lowest.taxon, values_from = abundance, values_fill = 0)

dm <- vegdist(data.combined2[,-c(1,2)], method = "jaccard")
(anosim.result <- anosim(dm, grouping = data.combined2$sampling.method))
nmds.result <- metaMDS(dm)
nmds.result2 <- data.frame(nmds.result$points) |> dp$mutate(method = data.combined2$sampling.method, id = data.combined2$gr_sample_id)
ggplotly(ggplotly(ggplot(nmds.result2, aes(x = MDS1, y = MDS2, fill = method)) + 
        geom_point(shape = 21, size = 4)))

## repeat at genus level 

data.combined2 <- 
        data.combined |> 
        unique(by = c("genus", "gr_sample_id")) |> 
        dp$mutate(abundance = 1) |> 
        tidyr::pivot_wider(id_cols = c(gr_sample_id, sampling.method), names_from = genus, values_from = abundance, values_fill = 0)

dm <- vegdist(data.combined2[,-c(1,2)], method = "jaccard")
(anosim.result <- anosim(dm, grouping = data.combined2$sampling.method))
nmds.result <- metaMDS(dm)
nmds.result2 <- data.frame(nmds.result$points) |> dp$mutate(method = data.combined2$sampling.method, id = data.combined2$gr_sample_id)
ggplotly(ggplotly(ggplot(nmds.result2, aes(x = MDS1, y = MDS2, fill = method)) + 
                          geom_point(shape = 21, size = 4)))

## For now I will use both. Each one gets a separate data.set Id 

data9.1 <- data9[sampling.method == "Protocolo de muestreo y laboratorio de Macrófitos en Ríos"]
data9.2 <- data9[sampling.method == "Nuevo protocolo de macrófitos en río en fase de prueba"]

data9.1[,date.set := paste0(data.set,"_old_sampling")]
data9.2[,date.set := paste0(data.set,"_new_sampling")]

# temporal aggregation --------------------------------------------------------------
agg1 <- data9.1 |> unique(by = "gr_sample_id")
agg2 <- data9.2 |> unique(by = "gr_sample_id")
unique(table(agg1$site_id))
unique(table(agg2$site_id))
# - yes 
source("R/functions/newest_sample.R")
data10.1 <- data9.1[month(date) %in% 5:9]
data10.2 <- data9.2[month(date) %in% 5:9]
data10.1 <- newest_sample(data10.1)
data10.2 <- newest_sample(data10.2)

data10 <- rbindlist(list(data10.1, data10.2))

saveRDS(data10, paste0("data/macrophytes/original_data/spain_duero/",Sys.Date(),"_final_aggregated.rds"))


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

