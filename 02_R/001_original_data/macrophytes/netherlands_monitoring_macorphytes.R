### --- Netherlands Macrophytes  --- ### 

# date written: 24.02.22
# date last modified: 13.06.22
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: Clean fish data provided by for the Netherlands 

# setup -----------------------------------------------

options("box.path" = "~/R/box_modules/")

library(data.table)
library(magrittr)
library(mapview)
library(ggplot2)
box::use(readxl[read_excel],
         lubridate[ymd, year, month],
         stringdist[amatch],
         sf = sf,
         dp = dplyr,
         box/dfm)

source("R/functions/add_typologies.R")
source("R/functions/dfm.R")

# load data -------------------------------------------------------------------------

load("data/macrophytes/original_data/netherlands_waterkwaliteitsportaal/raw/AqPlants11_19_XY.RData")
taxontable <- readRDS("data/macrophytes/2022-06-13_taxontable_macrophytes.rds")
typologies <- readRDS("data/all_typologies.rds")

# prepare data ----------------------------------------------------------------------

data <- data6
setDT(data)
rm(data6)
## Variable descriptions froms README file ## 

# Meetobject.namespace: code for a regional water manager
# Meetobject.lokaalID: location
# both combined provides a unique location
# Resultaatdatum: date of sampling
# Biotaxon.naam: Latin name
# Hoedanigheid.omschrijving: description of method followed for inventory
# AnalyseCompartiment.omschrijving: which part of the water system is investigated
# Numeriekewaarde: value (depends on Hoedanigheid.omschrijving)
# GeometriePunt.X_RD & .Y_RD: coordinates
# KRWwatertype.code: indication for watertype

## unique sampling methods 
table(data$Hoedanigheid.omschrijving)

# Drijvende fractie (174) - swimming fraction
# Emerse fractie (75) - emergent fraction
# Niet van toepassing (250630) - Not applicable
# Submerse fractie (145) - Submergt fraction 
# uitgedrukt volgens Braun-Blanquet-Schaal (32516) - expressed according to Braun-Blanquet-scale
# uitgedrukt volgens Tansley-schaal (197063) - expressed according to Tansley-schaal

## unique Units 
table(data$Grootheid.omschrijving)

## all the same Bedekking 

## Waterbody type 
table(data$KRWwatertype.code)

# Code values can be retrieved from 
# https://www.aquo.nl/index.php/Id-a02053a8-9132-40e4-bb71-7b271f1dabc9

translation.table <- data.table(
        KRWwatertype.code = unique(data$KRWwatertype.code),
        KRWwatertype.name = character(0),
        KRWwatertype.name.eng = character(0)
)

translation.table[KRWwatertype.code == "M11", c("KRWwatertype.name", "KRWwatertype.name.eng") := .("Kleine ondiepe gebufferde plassen", "Small shallow buffered puddles")]
translation.table[KRWwatertype.code == "M3" , c("KRWwatertype.name", "KRWwatertype.name.eng") := .("Gebufferde (regionale) kanalen", "Buffered (regional) channels")]
translation.table[KRWwatertype.code == "M14", c("KRWwatertype.name", "KRWwatertype.name.eng") := .("Grote ondiepe gebufferde plassen", "Large shallow buffered puddles")]
translation.table[KRWwatertype.code == "M1a", c("KRWwatertype.name", "KRWwatertype.name.eng") := .("Zoete gebufferde sloten", "Sweet Buffered Locks")]
translation.table[KRWwatertype.code == "M25", c("KRWwatertype.name", "KRWwatertype.name.eng") := .("Ondiepe laagveenplassen", "Shallow peat lakes ")]
translation.table[KRWwatertype.code == "M8" , c("KRWwatertype.name", "KRWwatertype.name.eng") := .("Gebufferde laagveensloten", "Buffered peat ditches ")]
translation.table[KRWwatertype.code == "M10", c("KRWwatertype.name", "KRWwatertype.name.eng") := .("Laagveen vaarten en kanalen", "Low moor waterways and canals ")]
translation.table[KRWwatertype.code == "M27", c("KRWwatertype.name", "KRWwatertype.name.eng") := .("Matig grote ondiepe laagveenplassen", "Moderately large shallow peat lakes ")]
translation.table[KRWwatertype.code == "M22", c("KRWwatertype.name", "KRWwatertype.name.eng") := .("Kleine ondiepe kalkrijke plassen", "Small shallow calcareous lakes ")]
translation.table[KRWwatertype.code == "M30", c("KRWwatertype.name", "KRWwatertype.name.eng") := .("Zwak brakke wateren", "Weak brackish waters")]  
translation.table[KRWwatertype.code == "R5" , c("KRWwatertype.name", "KRWwatertype.name.eng") := .("Langzaam stromende middenloop/benedenloop op zand", "Slow flowing mid/down run on sand ")]  
translation.table[KRWwatertype.code == "M1b", c("KRWwatertype.name", "KRWwatertype.name.eng") := .("Niet-zoete gebufferde sloten", "Non Sweet Buffered Locks ")]
translation.table[KRWwatertype.code == "M6b", c("KRWwatertype.name", "KRWwatertype.name.eng") := .("Grote ondiepe kanalen met scheepvaart", "Large shallow canals with shipping ")]  
translation.table[KRWwatertype.code == "M7b", c("KRWwatertype.name", "KRWwatertype.name.eng") := .("Grote diepe kanalen met scheepvaart", "Large deep canals with shipping ")]  
translation.table[KRWwatertype.code == "R4" , c("KRWwatertype.name", "KRWwatertype.name.eng") := .("Permanente langzaam stromende bovenloop op zand", "Permanent slow flowing headwaters on sand ")]  
translation.table[KRWwatertype.code == "M16", c("KRWwatertype.name", "KRWwatertype.name.eng") := .("Diepe gebufferde meren", "Deep buffered lakes")] 
translation.table[KRWwatertype.code == "R6" , c("KRWwatertype.name", "KRWwatertype.name.eng") := .("Langzaam stromend riviertje op zand/klei", "Slowly flowing river on sand/clay")]  
translation.table[KRWwatertype.code == "M6a", c("KRWwatertype.name", "KRWwatertype.name.eng") := .("Grote ondiepe kanalen zonder scheepvaart", "Large shallow channels without shipping")]  
translation.table[KRWwatertype.code == ""   , c("KRWwatertype.name", "KRWwatertype.name.eng") := .("", "")]  
translation.table[KRWwatertype.code == "R7" , c("KRWwatertype.name", "KRWwatertype.name.eng") := .("Langzaam stromende rivier/nevengeul op zand/klei", "Slow flowing river/side channel on sand/clay ")]  
translation.table[KRWwatertype.code == "R8" , c("KRWwatertype.name", "KRWwatertype.name.eng") := .("Zoet getijdenwater (uitlopers rivier) op zand/klei", "Fresh tidal water (river foothills) on sand/clay")]  
translation.table[KRWwatertype.code == "M19", c("KRWwatertype.name", "KRWwatertype.name.eng") := .("Diepe meren in open verbinding met rivier", "Deep lakes in open connection with river")]  
translation.table[KRWwatertype.code == "R12", c("KRWwatertype.name", "KRWwatertype.name.eng") := .("Langzaam stromende middenloop/benedenloop op veenbodem", "Slow flowing middle course/lower course on peat soil")]  
translation.table[KRWwatertype.code == "M12", c("KRWwatertype.name", "KRWwatertype.name.eng") := .("Kleine ondiepe zwak gebufferde plassen (vennen)", "Small shallow weakly buffered pools (fens) ")]  
translation.table[KRWwatertype.code == "M20", c("KRWwatertype.name", "KRWwatertype.name.eng") := .("Moerasbeek", "swamp brook ")]  
translation.table[KRWwatertype.code == "R3" , c("KRWwatertype.name", "KRWwatertype.name.eng") := .("Droogvallende langzaam stromende bovenloop op zand", "Drying slowly flowing headwaters on sand")]  
translation.table[KRWwatertype.code == "M31", c("KRWwatertype.name", "KRWwatertype.name.eng") := .("Kleine brakke tot zoute wateren", "Small brackish to saline waters ")]  
translation.table[KRWwatertype.code == "M21", c("KRWwatertype.name", "KRWwatertype.name.eng") := .("Grote diepe gebufferde meren", "Large deep buffered lakes")]  
translation.table[KRWwatertype.code == "M7a", c("KRWwatertype.name", "KRWwatertype.name.eng") := .("Grote diepe kanalen zonder scheepvaart", "Large deep canals without shipping ")]  
translation.table[KRWwatertype.code == "M23", c("KRWwatertype.name", "KRWwatertype.name.eng") := .("Grote ondiepe kalkrijke plassen", "Large shallow calcareous lakes ")]  
translation.table[KRWwatertype.code == "R4a", c("KRWwatertype.name", "KRWwatertype.name.eng") := .("Permanente langzaam stromende laagland bovenloop op zand", "Permanent slow-flowing lowland headwaters on sand ")]  
translation.table[KRWwatertype.code == "R19", c("KRWwatertype.name", "KRWwatertype.name.eng") := .("Doorstroommoeras", "flow-through swamp ")] 
translation.table[KRWwatertype.code == "M13", c("KRWwatertype.name", "KRWwatertype.name.eng") := .("Kleine ondiepe zure plassen (vennen)", "Small shallow acidic pools (fens)")]  
translation.table[KRWwatertype.code == "M26", c("KRWwatertype.name", "KRWwatertype.name.eng") := .("Ondiepe zwak gebufferde hoogveenplassen/vennen", "Shallow weakly buffered raised bog lakes/fens")]  
translation.table[KRWwatertype.code == "R20", c("KRWwatertype.name", "KRWwatertype.name.eng") := .("Moerasbeek", "swamp brook ")]  
translation.table[KRWwatertype.code == "R13", c("KRWwatertype.name", "KRWwatertype.name.eng") := .("Snelstromende bovenloop op zand", "Fast-flowing headwaters on sand ")]  
translation.table[KRWwatertype.code == "R1" , c("KRWwatertype.name", "KRWwatertype.name.eng") := .("Droogvallende bron", "Drying source")]  
translation.table[KRWwatertype.code == "R2" , c("KRWwatertype.name", "KRWwatertype.name.eng") := .("Permanente bron", "Permanent source")]  
translation.table[KRWwatertype.code == "R17", c("KRWwatertype.name", "KRWwatertype.name.eng") := .("Diepe zwakgebufferde meren", "Deep weakly buffered lakes")]  
translation.table[KRWwatertype.code == "R18", c("KRWwatertype.name", "KRWwatertype.name.eng") := .("Diepe zure meren", "Deep acid lakes")]  
translation.table[KRWwatertype.code == "R15", c("KRWwatertype.name", "KRWwatertype.name.eng") := .("Snelstromend riviertje op kiezelhoudende bodem", "Fast-flowing river on siliceous soil")]  
translation.table[KRWwatertype.code == "R4b", c("KRWwatertype.name", "KRWwatertype.name.eng") := .("Permanente langzaam stromende heuvelland bovenloop op zand", "Permanent slow-flowing hill country headwaters on sand")]  
translation.table[KRWwatertype.code == "R14", c("KRWwatertype.name", "KRWwatertype.name.eng") := .("Snelstromende middenloop/benedenloop op zand", "Fast-flowing mid-range/down-run on sand")]  
translation.table[KRWwatertype.code == NA   , c("KRWwatertype.name", "KRWwatertype.name.eng") := .(NA, NA)] 

data <- translation.table[data, on = "KRWwatertype.code"]

data <- data[KRWwatertype.code %in% c(
        "M3",
        "R5",
        "M8",
        "R14",
        "R4b",
        "R15",
        "R2",
        "R13",
        "R4a",
        "M7a",
        "R3",
        "M20",
        "R7",
        "M6a",
        "M6",
        "R4"
)]

unique(data$KRWwatertype.name.eng)

table(data$AnalyseCompartiment.omschrijving)

data <- data[AnalyseCompartiment.omschrijving %in% c(
        "Emerse zone", 
        "Oppervlaktewater", 
        "Submerse zone"
)]


data2 <- 
        data.table(
                original_site_name = data$original_site_name, 
                date               = lubridate::ymd(data$Resultaatdatum),
                taxon              = data$Biotaxon.naam, 
                abundance          = data$Numeriekewaarde, 
                x.coord            = data$GeometriePunt.X_RD,
                y.coord            = data$GeometriePunt.Y_RD, 
                EPSG               = 28992, 
                data.set           = "netherlands_monitoring_macrophytes"
        )
data2[,c("season", "year") := .(dp$case_when(month(date) %in% c(12,1,2)  ~ "winter",
                                             month(date) %in% c(3,4,5)   ~ "spring",
                                             month(date) %in% c(6,7,8)   ~ "summer",
                                             month(date) %in% c(9,10,11) ~ "autumn"), 
                                lubridate::year(date))]

data2[, site_id := .GRP, by = c("x.coord", "y.coord")]

sites <- unique(data2, by = "site_id")
sites <- sf$st_as_sf(sites, coords = c("x.coord", "y.coord"), crs = 28992)
#mapview(sites)

NE <- geodata::gadm(country = "NL", level = 0, path = "data/macrophytes/original_data/netherlands_waterkwaliteitsportaal")
NE %<>% sf$st_as_sf()
NE %<>% sf$st_transform(crs = sf$st_crs(sites))

sites <- sf$st_filter(sites, 
                      NE)
data2 <- data2[site_id %in% sites$site_id]

TU <- data2$taxon |>  unique()
TU <- dp$setdiff(TU, taxontable$original_name)
 
# taxontable <- update_taxonomy_macrophytes(TU, taxontable)
# taxontable[clean == FALSE]
# taxontable[taxon_state == "", "original_name"]       
# 
# taxontable[genus == "Cornus"]
# taxontable[original_name == "Phytolacca"]
# 
# taxontable[original_name == "Populus", taxon_state := "drop"]
# taxontable[original_name == "Juncus x kern-reichgeltii", taxon_state := "helophytes"]
# taxontable[original_name == "Wolffia", taxon_state := "hydrophytes" ]
# taxontable[original_name == "Bolboschoenus", taxon_state := "helophytes" ]
# taxontable[original_name == "Schoenoplectus maritimus", taxon_state := "helophytes" ]
# taxontable[original_name == "Rhododendron", taxon_state := "drop" ]
# taxontable[original_name == "Betula", taxon_state := "drop"]
# taxontable[original_name == "Quercus", taxon_state := "drop"]
# taxontable[original_name == "Platanus", taxon_state := "drop"]
# taxontable[original_name == "Nymphaeaceae", taxon_state := "drop"]
# taxontable[original_name == "Brassica", taxon_state := "drop"]
# taxontable[original_name == "Sonchus", taxon_state := "drop"]
# taxontable[original_name == "Sambucus", taxon_state := "drop"]
# taxontable[original_name == "Hedera", taxon_state := "drop"]
# taxontable[original_name == "Tilia", taxon_state := "drop"]
# taxontable[original_name == "Cerastium", taxon_state := "drop"]
# taxontable[original_name == "Potamogetonaceae", taxon_state := "drop"]
# taxontable[original_name == "Angelica", taxon_state := "drop"]
# taxontable[original_name == "Dryopteridaceae", taxon_state := "drop"]
# taxontable[original_name == "Prunus", taxon_state := "drop"]
# taxontable[original_name == "Ranunculaceae", taxon_state := "drop"]
# taxontable[original_name == "Oenanthe [PLANTAE]", taxon_state := "drop"]
# taxontable[original_name == "Gnaphalium", taxon_state := "drop"]
# taxontable[original_name == "Convolvulus", taxon_state := "drop"]
# taxontable[original_name == "Lolium", taxon_state := "drop"]
# taxontable[original_name == "Carduus", taxon_state := "drop"]
# taxontable[original_name == "Scrophulariaceae", taxon_state := "drop"]
# taxontable[original_name == "Robinia", taxon_state := "drop"]
# taxontable[original_name == "Vicia", taxon_state := "drop"]
# taxontable[original_name == "Betulaceae", taxon_state := "drop"]
# taxontable[original_name == "Heracleum", taxon_state := "drop"]
# taxontable[original_name == "Lamium", taxon_state := "drop"]
# taxontable[original_name == "Thelypteridaceae", taxon_state := "drop"]
# taxontable[original_name == "Oenothera", taxon_state := "drop"]
# taxontable[original_name == "Ruppia", taxon_state := "hydrophytes"]
# taxontable[original_name == "Rosa", taxon_state := "drop"]
# taxontable[original_name == "Aronia", taxon_state := "drop"]
# taxontable[original_name == "Dactylorhiza", taxon_state := "helophytes"]
# taxontable[original_name == "Spirodela", taxon_state := "hydrophytes"]
# taxontable[original_name == "Fallopia", taxon_state := "drop"]
# taxontable[original_name == "Galinsoga", taxon_state := "drop"]
# taxontable[original_name == "Phytolacca", taxon_state := "drop" ]
# taxontable[original_name == "Pachysandra", taxon_state := "drop"]
# taxontable[original_name == "Bromopsis", taxon_state := "drop"]
# taxontable[original_name == "Galanthus", taxon_state := "drop"]
# taxontable[original_name == "Cornus", taxon_state := "drop"]
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
data2[,gr_sample_id := paste0("site_", site_id, "_date_", date_id, "_neherlands_monitoring_macrophytes")]

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

# - drop sites with very few species 
data6[, richness := uniqueN(lowest.taxon), by = "gr_sample_id"]
data7 <- data6#[richness > 10]

# - drop sites far removed from ECRINS river network 
data8 <- data7[distance < 300]
uto <- readRDS("data/macrophytes/original_data/netherlands_waterkwaliteitsportaal/2022-02-25_updated_type.rds")


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
        i.bool <- readline(paste(i,"/",nrow(rt), ":"))
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

#- save the remove list. 
saveRDS(utc, paste0("data/macrophytes/original_data/netherlands_waterkwaliteitsportaal/", Sys.Date(), "_updated_type.rds"))

data9 <- dp$left_join(data8, 
                   updated_type, 
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
# - yes 
source("R/functions/newest_sample.R")
data10 <- data9[month(date) %in% 5:9]
data10 <- newest_sample(data10, season_available = FALSE)
saveRDS(data10, paste0("data/macrophytes/original_data/netherlands_waterkwaliteitsportaal/",Sys.Date(),"_final_aggregated.rds"))

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
