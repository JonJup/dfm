### ------------------------------ ###
### --- Lithuania Macrophytes  --- ### 
### ------------------------------ ###

# -------------------------------
# date written: 28.03.22
# date last modified: 13.06.22
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: Clean macrophyte data for Lithuania
# -------------------------------

# setup -----------------------------------------------

options("box.path" = "~/R/box_modules/")

library(data.table)
library(magrittr)
library(mapview)
library(ggplot2)
library(stringr)
box::use(readxl[read_excel],
         lubridate[dmy, year, month],
         stringdist[amatch],
         sf = sf,
         dp = dplyr,
         box/dfm)

source("R/functions/add_typologies.R")
source("R/functions/dfm.R")

# load data -------------------------------------------------------------------------

bio <- readRDS("data/macrophytes/original_data/lithuania_macrophytes/lith_data.rds")
taxontable <- readRDS("data/macrophytes/2022-06-13_taxontable_macrophytes.rds")
typologies <- readRDS("data/all_typologies.rds")

# prepare data ----------------------------------------------------------------------

bio

bio2 <- bio |> 
        dp$mutate(
                  EPSG = 2583, ## Guestimated with projfinder yo 
                  data.set = "lithuania_monitoring_macrophytes") 
setDT(bio2)
bio2[site_id == "LTR1299", y.coord := 6000716]
bio2[,c("season", "year") := .(dp$case_when(month(date) %in% c(12,1,2)  ~ "winter",
                                            month(date) %in% c(3,4,5)   ~ "spring",
                                            month(date) %in% c(6,7,8)   ~ "summer",
                                            month(date) %in% c(9,10,11) ~ "autumn"), 
                               lubridate::year(date))]

sites <- unique(bio2, by = "site_id")
sites[site_id == "LTR1299", y.coord := 6000716]
sites <- sf$st_as_sf(sites, coords = c("x.coord", "y.coord"), crs = "EPSG:2583")
mapview(sites)


data <- bio2 
#rm(bio, bio2); gc()

TU <- data$taxon |>  unique()
TU <- dp$setdiff(TU, taxontable$original_name)
TU %<>% sort()
drop <-
        c(
                "Dovinė",
                "Kranto tipas/modifikacija",
                "Makrofitų išsidėstymas",
                "Spalva",
                "Substratas (1-3 balų skalė)",
                "Upės atkarpa",
                "Upės ID",
                "Užpavėsinimas (Wörlein, 1992)",
                "Vandens drumstumas",
                "Vandens kvapas",
                "Vandens lygis",
                "Vandens tėkmė (BLFW, 1995)",
                "Vidutinis gylis",
                "Vidutinis plotis",
                "Žemėnauda netoli kranto",
                "Buveinės ant kranto"
        )


data[str_detect(taxon, "Cladophora"), taxon := "Cladophora sp."]
data[str_detect(taxon, "Cladaphora"), taxon := "Cladophora sp."]
data[str_detect(taxon, "B. trichophyllum"), taxon := "Blepharostoma trichophyllum"]
data[str_detect(taxon, "Batrachium cf. fluitans"), taxon := "Batrachium fluitans"]

data[taxon == "Callitriche sp. (C. hermaphroditica)", taxon := "Callitriche hermaphroditica "]     
data[taxon == "Callitriche sp. (C.cophocarpa)"      , taxon := "Callitriche cophocarpa"]     
data[taxon == "Callitriche sp. (C.hermaphroditica)" , taxon := "Callitriche hermaphroditica"]     
data[taxon == "P. acutifolius" , taxon := "Potamogeton acutifolius"]  

data <- data[taxon != ""]
data <- data[!taxon %in% drop]

TU2 <- data$taxon |>  unique()
TU2 <- dp$setdiff(TU2, taxontable$original_name)
(TU2 %<>% sort())


## need to call function from dfm script 
# taxontable <- dfm$update_taxonomy_macrophytes(TU2, taxontable_arg = taxontable)
# 
# taxontable[original_name == "Acrus calmus"                 , taxon_state := "helophytes"]
# taxontable[original_name == "Batrachium circinatum"        , taxon_state := "hydrophytes"]
# taxontable[original_name == "Batrachium fluitans"          , taxon_state := "hydrophytes"]
# taxontable[original_name == "Blepharostoma trichophyllum"  , taxon_state := "moss"]
# taxontable[original_name == "Bryophyta"                    , taxon_state := "moss"]
# taxontable[original_name == "Callitriche hermaphroditica " , taxon_state := "hydrophytes"]
# taxontable[original_name == "Cichorium intybus"            , taxon_state := "drop"]
# taxontable[original_name == "Glyceria nemoralis"           , taxon_state := "drop"]
# taxontable[original_name == "Hydrocharis morsus ranae"     , taxon_state := "hydrophytes"]
# taxontable[original_name == "Phalaroides arundinacea"      , taxon_state := "drop"]
# taxontable[original_name == "Phalaroides arundinaceaacea"  , taxon_state := "drop"]
# taxontable[original_name == "Plantago sp."                 , taxon_state := "drop"]
# taxontable[original_name == "Potamogeton × angustifolius"  , taxon_state := "hydrophytes"]
# taxontable[original_name == "Potamogeton × fennicus"       , taxon_state := "hydrophytes"]
# taxontable[original_name == "Potamogeton × nitens"         , taxon_state := "hydrophytes"]
# taxontable[original_name == "Potamogeton × salicifolius"   , taxon_state := "hydrophytes"]
# taxontable[original_name == "Potamogeton x fennicus"       , taxon_state := "hydrophytes"]
# taxontable[original_name == "Roribba amphibium"            , taxon_state := "hydrophytes"]
# taxontable[original_name == "Rorippa amphibium"            , taxon_state := "hydrophytes"]
# taxontable[original_name == "Rumex acuatica"               , taxon_state := "drop"]
# taxontable[original_name == "Rumex aquatica"               , taxon_state := "drop"]
# taxontable[original_name == "Sagittaria sagitifolia"       , taxon_state := "hydrophytes"]
# taxontable[original_name == "Scirpus sylvatica"            , taxon_state := "drop"]

#saveRDS(taxontable, paste0("data/macrophytes/",Sys.Date(),"_taxontable_macrophytes.rds"))

data <- dp$rename(data, original_name = taxon)
data2 <- taxontable[data, on = "original_name"]  
rm(taxontable, data, TU)

data2[, date_id := .GRP, by = "date"]

## add site and date ids for this I need to round coordinates because some samples are
## categorized as from different sites even though they are from the same.
data2[, c("x.coord_round", "y.coord_round") := .(round(x.coord, 5), 
                                                 round(y.coord, 5))]

data2[, site_id := .GRP, by = c("x.coord_round", "y.coord_round")]
data2[, site_id := as.numeric(site_id)]

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
data2[,gr_sample_id := paste0("site_", site_id, "_date_", date_id, "_lithuania_monitoring_macrophytes")]

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
        abundance = NA,
        x.coord,
        y.coord,
        EPSG,
        data.set = "lithuania_monitroing_macrophytes"
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
uto <- readRDS("data/macrophytes/original_data/lithuania_macrophytes/2022-03-28_updated_type.rds")

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
saveRDS(utc, paste0("data/macrophytes/original_data/lithuania_macrophytes/", Sys.Date(), "_utc.rds"))

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
data10 <- data9[month(date) %in% 5:9]

saveRDS(data10, paste0("data/macrophytes/original_data/lithuania_macrophytes/",Sys.Date(),"_final_aggregated.rds"))

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
