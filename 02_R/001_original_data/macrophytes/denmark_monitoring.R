### ------------------------------ ###
### --- Denmark   Macrophytes  --- ### 
### ------------------------------ ###

# -------------------------------
# date written: 25.04.22
# date last modified: 12.06.22
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: Clean macrophyte data for Denmark
# CRS: 25832
# -------------------------------

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

data1 <- fread("data/macrophytes/original_data/denmark_monitoring/raw/bigdata_Transect_DK.csv")
data2 <- fread("data/macrophytes/original_data/denmark_monitoring/raw/bigdata_Extra_plants_DK.csv")
taxontable <- readRDS("data/macrophytes/2022-05-17_taxontable_macrophytes.rds")
typologies <- readRDS("data/all_typologies.rds")


# prepare data ----------------------------------------------------------------------

data <- rbindlist(list(data1, data2), fill = TRUE)

data[, date := lubridate::ymd(Sampling_date)]
data[, original_site_name := LocationName]
data[, taxon := taxon_name]
data[, c("x.coord", "y.coord", "EPSG") := .(Xutm_Euref89_Zone32, Yutm_Euref89_Zone32, 25832)]
data[, c("Site_ID_ObservationPlaceNr", "Taxa_Code", "Transect_nr", 
         "Coverage", "V11", "V12", "V13", "Sampling_date", "LocationName", 
         "Xutm_Euref89_Zone32", "Yutm_Euref89_Zone32", "taxon_name") := NULL]

# sites <- unique(data, by = "original_site_name")
# sites <- sf$st_as_sf(sites, coords = c("x.coord", "y.coord"), crs = 25832)
# mapview::mapview(sites)
# rm(sites)

data[, c("data.set") := "denmark_monitoring_macrophytes"]
data[,c("season", "year") := .(dp$case_when(month(date) %in% c(12,1,2)  ~ "winter",
                                            month(date) %in% c(3,4,5)   ~ "spring",
                                            month(date) %in% c(6,7,8)   ~ "summer",
                                            month(date) %in% c(9,10,11) ~ "autumn"), 
                               lubridate::year(date))]
data <- data[!taxon %in% c("Trådalger", "Ingen planter", "Tr\xe5dalger")]

TU <- data$taxon |>  unique()
TU <- dp$setdiff(TU, taxontable$original_name)
TU %<>% sort()

taxontable <- dfm$update_taxonomy_macrophytes(TU, taxontable_arg = taxontable)

saveRDS(taxontable, "data/macrophytes/2022-04-25_danish-temporary-taxontable.rds")
setorderv(taxontable, "original_name")
taxontable[taxon_state == ""]

TU2 <- data$taxon |>  unique()
TU2 <- dp$setdiff(TU2, taxontable$original_name)
TU2 %<>% sort()

taxontable[stringr::str_detect(original_name, "Charophyta")]
taxontable[phylum == "Chlorophyta"]

## fix errors
taxontable[original_name == "Myriophyllum", c("family", "order", "class", "phylum", "kingdom", "taxon_state") := .("Haloragaceae", "Saxifragales", "Magnoliopsida", "Tracheophyta", "Plantae", "hydrophytes")]
taxontable <- taxontable[original_name != "Ingen planter"]
taxontable[original_name == "Calystegia sepium subsp. sepium", c("species", "genus", "family", "order", "subclass", "class", "phylum", "kingdom", "clean", "taxon_state") := 
                   .("Calystegia sepium", "Calystegia", "Convolvulaceae", "Solanales", NA, "Magnoliopsida", "Tracheophyta", "Plantae", TRUE, "drop")]
taxontable[original_name == "Salix", c("family", "order", "class", "phylum", "kingdom", "clean", "taxon_state") := 
                   .("Salicaceae", "Malpighiales", "Magnoliopsida",  "Tracheophyta", "Plantae", TRUE, "drop")]
taxontable[original_name == "Iris", c("family", "order", "class", "phylum", "kingdom", "clean", "taxon_state") := 
                   .("Iridaceae", "Asparagales", "Liliopsida",  "Tracheophyta", "Plantae", TRUE, "drop")]
taxontable[original_name == "Galeopsis", c("family", "order", "class", "phylum", "kingdom", "clean", "taxon_state") := 
                   .("Lamiaceae", "Lamiales", "Magnoliopsida", "Tracheophyta", "Plantae", TRUE, "drop")]
taxontable[original_name == "Pohlia", c("family", "order", "class", "phylum", "kingdom", "clean", "taxon_state") := 
                   .("Bryaceae", "Bryales", "Bryopsida", "Bryophyta", "Plantae", TRUE, "moss")]
taxontable[original_name == "Sagittaria f. submersa", c("family", "order", "class", "phylum", "kingdom", "clean", "taxon_state") := 
                   .("Alismataceae", "Alismatales", "Liliopsida", "Bryophyta", "Plantae", TRUE, "hydrophytes")]


taxontable[class == "Bryopsida", taxon_state := "moss"]
taxontable[phylum == "Marchantiophyta", taxon_state := "moss"]
taxontable[phylum == "Charophyta", taxon_state := "green_algae"]
taxontable[phylum == "Chlorophyta", taxon_state := "green_algae"]
taxontable[phylum == "Ochrophyta", taxon_state := "yellow_green_algae"]
taxontable[phylum == "Rhodophyta", taxon_state := "red_algae"]

taxontable[clean == FALSE, original_name]
taxontable[genus == "Chrysosplenium"]
taxontable[original_name == "Elytrigia repens subsp. repens"]

taxontable[genus == "Agrostis"                                             , taxon_state := "drop"]
taxontable[original_name == "Alisma plantago-aquatica f. submersum"        , taxon_state := "helophytes"]
taxontable[original_name == "Angelica archangelica subsp. litoralis"       , taxon_state := "drop"]
taxontable[original_name == "Argentina anserina"                           , taxon_state := "drop"]
taxontable[original_name == "Atriplex latifolia"                           , taxon_state := "drop"]
taxontable[original_name == "Barbarea"                                     , taxon_state := "drop"]
taxontable[original_name == "Batrachium"                                   , taxon_state := "hydrophytes"]
taxontable[original_name == "Berula"                                       , taxon_state := "drop"]
taxontable[original_name == "Bidens"                                       , taxon_state := "helophytes"]
taxontable[original_name == "Callitrichaceae"                              , taxon_state := "hydrophytes"]
taxontable[original_name == "Callitriche"                                  , taxon_state := "hydrophytes"]
taxontable[original_name == "Caltha palustris var. palustris"              , taxon_state := "helophytes"]
taxontable[original_name == "Cardamine flexuosa x pratensis"               , taxon_state := "drop"]
taxontable[original_name == "Cardamine pratensis subsp. pratensis"         , taxon_state := "drop"]
taxontable[original_name == "Carex"                                        , taxon_state := "helophytes"]
taxontable[original_name == "Carex limosa"                                 , taxon_state := "helophytes"]
taxontable[original_name == "Carex nigra var. nigra"                       , taxon_state := "helophytes"]
taxontable[original_name == "Chenopodium album coll."                      , taxon_state := "drop"]
taxontable[original_name == "Chenopodium album subsp. album"               , taxon_state := "drop"]
taxontable[original_name == "Chrysosplenium"                               , taxon_state := "drop"]
taxontable[original_name == "Cornus alba subsp. stolonifera"               , taxon_state := "drop"]
taxontable[original_name == "Crataegus"                                    , taxon_state := "drop"]
taxontable[original_name == "Eleocharis palustris subsp. waltersii"        , taxon_state := "helophytes"]
taxontable[original_name == "Elodea"                                       , taxon_state := "hydrophytes"]
taxontable[original_name == "Elytrigia repens subsp. repens"               , taxon_state := "drop"]
taxontable[original_name == "Epilobium"                                    , taxon_state := "drop"]
taxontable[original_name == "Epilobium lamyi"                              , taxon_state := "drop"]
taxontable[original_name == "Equisetaceae"                                 , taxon_state := "drop"]
taxontable[original_name == "Equisetum"                                    , taxon_state := "drop"]
taxontable[original_name == "Eupatorium"                                   , taxon_state := "drop"]
taxontable[original_name == "Ficaria verna subsp. verna"                   , taxon_state := "drop"]
taxontable[original_name == "Filipendula"                                  , taxon_state := "drop"]
taxontable[original_name == "Galium"                                       , taxon_state := "drop"]
taxontable[original_name == "Galium palustre subsp. palustre"              , taxon_state := "helophytes"]
taxontable[original_name == "Glyceria"                                     , taxon_state := "drop"]
taxontable[original_name == "Grossulariaceae"                              , taxon_state := "drop"]
taxontable[original_name == "Juncus"                                       , taxon_state := "helophytes"]
taxontable[original_name == "Juncus bulbosus subsp. bulbosus"              , taxon_state := "helophytes"]
taxontable[original_name == "Lactuca muralis"                              , taxon_state := "drop"]
taxontable[original_name == "Lepidium"                                     , taxon_state := "drop"]
taxontable[original_name == "Lepidium latifolium"                          , taxon_state := "drop"]
taxontable[original_name == "Lotus corniculatus var. corniculatus"         , taxon_state := "drop"]
taxontable[original_name == "Lotus pedunculatus var. pedunculatus"         , taxon_state := "drop"]
taxontable[original_name == "Mentha"                                       , taxon_state := "drop"]
taxontable[original_name == "Montia fontana subsp. fontana"                , taxon_state := "hydrophytes"]
taxontable[original_name == "Myosotis"                                     , taxon_state := "drop"]
taxontable[original_name == "Myosotis caespitosa"                          , taxon_state := "drop"]
taxontable[original_name == "Myosotis laxa subsp. caespitosa"              , taxon_state := "drop"]
taxontable[original_name == "Myrsinaceae"                                  , taxon_state := "drop"]
taxontable[original_name == "Nasturtium"                                   , taxon_state := "helophytes"]
taxontable[original_name == "Nuphar"                                       , taxon_state := "hydrophytes"]
taxontable[original_name == "Nymphaeaceae f. submersa"                     , taxon_state := "hydrophytes"]
taxontable[original_name == "Persicaria"                                   , taxon_state := "drop"]
taxontable[original_name == "Persicaria lapathifolia subsp. lapathifolia"  , taxon_state := "drop"]
taxontable[original_name == "Persicaria lapathifolia subsp. pallida"       , taxon_state := "drop"]
taxontable[original_name == "Persicaria maculosa subsp. maculosa"          , taxon_state := "drop"]
taxontable[original_name == "Phalaris"                                     , taxon_state := "drop"]
taxontable[original_name == "Poa"                                          , taxon_state := "drop"]
taxontable[original_name == "Poa pratensis subsp. pratensis"               , taxon_state := "drop"]
taxontable[original_name == "Poaceae"                                      , taxon_state := "drop"]
taxontable[original_name == "Polygonaceae"                                 , taxon_state := "drop"]
taxontable[original_name == "Polygonum"                                    , taxon_state := "drop"]
taxontable[original_name == "Populus x berolinensis"                       , taxon_state := "drop"]
taxontable[original_name == "Potamogeton"                                  , taxon_state := "hydrophytes"]
taxontable[original_name == "Potamogeton coloratus f. submersus"           , taxon_state := "hydrophytes"]
taxontable[original_name == "Potamogeton gramineus x natans"               , taxon_state := "hydrophytes"]
taxontable[original_name == "Potamogeton lucens x natans"                  , taxon_state := "hydrophytes"]
taxontable[original_name == "Potamogeton natans f. submersus"              , taxon_state := "hydrophytes"]
taxontable[original_name == "Primulaceae"                                  , taxon_state := "drop"]
taxontable[original_name == "Ranunculus"                                   , taxon_state := "drop"]
taxontable[original_name == "Ranunculus aquatilis var. aquatilis"          , taxon_state := "hydrophytes"]
taxontable[original_name == "Ranunculus peltatus subsp. baudotii"          , taxon_state := "hydrophytes"]
taxontable[original_name == "Ranunculus peltatus subsp. peltatus"          , taxon_state := "hydrophytes"]
taxontable[original_name == "Reynoutria sp."                               , taxon_state := "drop"]
taxontable[original_name == "Ribes spicatum"                               , taxon_state := "drop"]
taxontable[original_name == "Rubus allegheniensis"                         , taxon_state := "drop"]
taxontable[original_name == "Rubus sect. Rubus"                            , taxon_state := "drop"]
taxontable[original_name == "Rumex"                                        , taxon_state := "drop"]
taxontable[original_name == "Rumex longifolius"                            , taxon_state := "drop"]
taxontable[original_name == "Rumex obtusifolius subsp. obtusifolius"       , taxon_state := "drop"]
taxontable[original_name == "Salix alba var. alba"                         , taxon_state := "drop"]
taxontable[original_name == "Salix alba x fragilis"                        , taxon_state := "drop"]
taxontable[original_name == "Salix fragilis x pentandra"                   , taxon_state := "drop"]
taxontable[original_name == "Salix repens subsp. repens var. repens"       , taxon_state := "drop"]
taxontable[original_name == "Schedonorus pratensis"                        , taxon_state := "drop"]
taxontable[original_name == "Schoenoplectus lacustris f. submersus"        , taxon_state := "drop"]
taxontable[original_name == "Schoenoplectus maritimus"                     , taxon_state := "drop"]
taxontable[original_name == "Scrophularia umbrosa var. umbrosa"            , taxon_state := "drop"]
taxontable[original_name == "Solanum dulcamara var. dulcamara"             , taxon_state := "drop"]
taxontable[original_name == "Sparganium"                                   , taxon_state := "helophytes"]
taxontable[original_name == "Sparganium emersum f. submersum"              , taxon_state := "helophytes"]
taxontable[original_name == "Sparganium emersum x erectum"                 , taxon_state := "helophytes"]
taxontable[original_name == "Sparganium erectum s.l."                      , taxon_state := "helophytes"]
taxontable[original_name == "Sparganium erectum subsp. erectum"            , taxon_state := "helophytes"]
taxontable[original_name == "Sparganium erectum subsp. microcarpum"        , taxon_state := "helophytes"]
taxontable[original_name == "Sparganium erectum subsp. neglectum"          , taxon_state := "helophytes"]
taxontable[original_name == "Spiraea"                                      , taxon_state := "drop"]
taxontable[original_name == "Stellaria"                                    , taxon_state := "drop"]
taxontable[original_name == "Symphytum x uplandicum"                       , taxon_state := "drop"]
taxontable[original_name == "Taraxacum"                                    , taxon_state := "drop"]
taxontable[original_name == "Tripleurospermum inodorum"                    , taxon_state := "drop"]
taxontable[original_name == "Urtica dioica subsp. dioica var. dioica"      , taxon_state := "drop"]
taxontable[original_name == "Valeriana"                                    , taxon_state := "drop"]
taxontable[original_name == "Valeriana sambucifolia subsp. procurrens"     , taxon_state := "drop"]
taxontable[original_name == "Valeriana sambucifolia subsp. sambucifolia"   , taxon_state := "drop"]
taxontable[original_name == "Valerianaceae"                                , taxon_state := "drop"]
taxontable[original_name == "Veronica"                                     , taxon_state := "drop"]
taxontable[original_name == "Vicia sativa subsp. nigra"                    , taxon_state := "drop"]
taxontable[original_name == "Zannichellia"                                 , taxon_state := "hydrophytes"]
taxontable[original_name == "Zannichellia palustris var. palustris"        , taxon_state := "hydrophytes"]
taxontable[original_name == "Zannichellia palustris var. pedecillata"      , taxon_state := "hydrophytes"]

taxontable[genus == "Juncus", taxon_state := "helophytes"]
taxontable[taxon_state == "helophyte", taxon_state := "helophytes"]

## check if taxontable is complete now
TU <- data$taxon |>  unique()
TU <- dp$setdiff(TU, taxontable$original_name)

# saveRDS(taxontable, paste0("data/macrophytes/",Sys.Date(),"_taxontable_macrophytes.rds"))

data <- dp$rename(data, original_name = taxon)
data2 <- taxontable[data, on = "original_name"]  
rm(taxontable, data, TU)

data2[, site_id := .GRP, by = "original_site_name"]
data2[, date_id := .GRP, by = "date"]

## add site and date ids for this I need to round coordinates because some samples are
## categorized as from different sites even though they are from the same.
# data2[, c("x.coord_round", "y.coord_round") := .(round(x.coord, 5), 
#                                                  round(y.coord, 5))]

# data2[, site_id := .GRP, by = c("x.coord_round", "y.coord_round")]
# data2[, site_id := as.numeric(site_id)]

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
data2[,gr_sample_id := paste0("site_", site_id, "_date_", date_id, "_denmark_monitoring_macrophytes")]

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

# - drop sites with very few species 
data6[, richness := uniqueN(lowest.taxon), by = "gr_sample_id"]
data7 <- data6#[richness > 10]

# - drop sites far removed from ECRINS river network 
data8 <- data7[distance < 300]
uto <- readRDS("data/macrophytes/original_data/denmark_monitoring/2022-04-26_updated_type.rds")

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

utc <- dp$bind_rows(uto, updated_type)

#- save the remove list. 
saveRDS(utc, paste0("data/macrophytes/original_data/denmark_monitoring/", Sys.Date(), "_updated_type_combined.rds"))

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
source("R/functions/newest_sample.R")
data10 <- data9[month(date) %in% 5:9]
data10 <- newest_sample(data10, season_available = FALSE)

saveRDS(data10, paste0("data/macrophytes/original_data/denmark_monitoring/",Sys.Date(),"_final_aggregated.rds"))

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


