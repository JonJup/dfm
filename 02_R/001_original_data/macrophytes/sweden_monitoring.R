### ------------------------------ ###
### --- Denmark   Macrophytes  --- ### 
### ------------------------------ ###

# -------------------------------
# date written: 16.05.22
# date last modified: 17.05.22
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: Clean macrophyte data for Sweden from https://miljodata.slu.se/MVM 
# EPSG: 3006
# -------------------------------

# setup -----------------------------------------------

options("box.path" = "~/R/box_modules/")

#devtools::install_git(url = "https://github.com/JonJup/jjmisc")

library(pacman)
p_load(
        data.table,
        magrittr,
        jjmisc,
        mapview,
        ggplot2,
        mapview,
        readxl,
        lubridate,
        stringdist,
        sf,
        dplyr,
        stringr,
        units
)

box::use(box/dfm)

# load data -------------------------------------------------------------------------

data <- read_excel("data/macrophytes/original_data/sweden_monitoring/raw/slu_mvm_220516_163417489_data.xlsx", sheet = 2)
taxontable <- readRDS("data/macrophytes/2022-05-03_taxontable_macrophytes.rds")
typologies <- readRDS("data/all_typologies.rds")

# prepare data ----------------------------------------------------------------------

#- remove old protocols 
setDT(data)
data<-data[!Undersökningstyp %in% c("Annan", "Makrofyter i sjöar v1")]


data2 <- data.table(
        original_site_name = data$`Nationellt övervakningsstations-ID`,
        date               = ymd(data$`Provtagningens startdatum`),
        taxon              = data$Taxonnamn,
        abundance          = data$Frekvens,
        y.coord            = data$`Stationskoordinat N/X`,
        x.coord            = data$`Stationskoordinat E/Y`,
        EPSG               = 3006,
        data.set           = "sweden_monitoring_macrophytes"
)

sites <- unique(data2, by = "original_site_name")
sites <- st_as_sf(sites, coords = c("x.coord", "y.coord"), crs = data2$EPSG[1])
mapview(sites)
rm(sites, data)

data2[,c("season", "year") := .(case_when(month(date) %in% c(12,1,2)  ~ "winter",
                                          month(date) %in% c(3,4,5)   ~ "spring",
                                          month(date) %in% c(6,7,8)   ~ "summer",
                                          month(date) %in% c(9,10,11) ~ "autumn"), 
                                year(date))]

data2 <- data2[!taxon %in% c("Algae", "Chlorophyta")]

TU <- data2$taxon |>  unique()
TU <- setdiff(TU, taxontable$original_name)
TU %<>% sort()

taxontable <- update_taxonomy_macrophytes(TU, taxontable_arg = taxontable)

taxontable[taxon_state == ""]
taxontable[taxon_state == "" & kingdom == "Chromista", ]


taxontable[phylum == "Bryophyta", taxon_state := "moss"]
taxontable[phylum == "Marchantiophyta", taxon_state := "moss"]
taxontable[phylum == "Rhodophyta", taxon_state := "red_algae"]
taxontable[phylum == "Porifera", taxon_state := "sponge"]
taxontable[phylum == "Cyanobacteria", taxon_state := "cyanobacteria"]
taxontable[phylum == "Chlorophyta", taxon_state := "green_algae"]
taxontable[phylum == "Charophyta", taxon_state := "green_algae"]
taxontable[class == "Xanthophyceae", taxon_state := "yellow_green_algae"]
taxontable[class == "Polypodiopsida", taxon_state := "fern"]
taxontable[class == "Lycopodiopsida", taxon_state := "lycopod"]

taxontable[original_name == "Argentina anserina", c("species", "genus", "taxon_state") := .("Potentilla anserina", "Potentilla", "drop")]
taxontable[genus == "Sagittaria", c("family", "order", "class", "phylum", "kingdom", "taxon_state") := .("Alismataceae", "Alismatales", "Liliopsida", "Tracheophyta", "Plantae", "hydrophytes")]
taxontable[original_name == "trådstarr", c("species", "genus", "family", "order", "class", "phylum", "kingdom", "taxon_state") := .("Carex lasiocarpa", "Carex", "Cyperaceae", "Poales", "Liliopsida","Tracheophyta", "Plantae", "helophytes")]


taxontable[taxon_state == ""][1]
taxontable[genus == "Zannichellia"]
taxontable[species == "Vaccinium oxycoccos"]


taxontable[genus == "Agrostis", taxon_state := "drop"]
taxontable[genus == "Alopecurus", taxon_state := "drop"]
taxontable[genus == "Andromeda", taxon_state := "drop"]
taxontable[genus == "Betula", taxon_state := "drop"]
taxontable[genus == "Bidens", taxon_state := "helophytes"]
taxontable[genus == "Calamagrostis", taxon_state := "drop"]
taxontable[genus == "Callitriche", taxon_state := "hydrophytes"]
taxontable[genus == "Campanula", taxon_state := "drop"]
taxontable[genus == "Cardamine", taxon_state := "drop"]
taxontable[genus == "Carex", taxon_state := "helophytes"]
taxontable[genus == "Cladium", taxon_state := "drop"]
taxontable[genus == "Deschampsia", taxon_state := "drop"]
taxontable[genus == "Drosera", taxon_state := "helophytes"]
taxontable[genus == "Elatine", taxon_state := "hydrophytes"]
taxontable[genus == "Eleocharis", taxon_state := "helophytes"]
taxontable[genus == "Epilobium", taxon_state := "drop"]
taxontable[genus == "Eriophorum", taxon_state := "drop"]
taxontable[species == "Galium palustre", taxon_state := "helophytes"]
taxontable[species == "Galium trifidum", taxon_state := "drop"]
taxontable[genus == "Iris", taxon_state := "drop"]
taxontable[genus == "Isoetes", taxon_state := "hydrophytes"]
taxontable[genus == "Juncus", taxon_state := "helophytes"]
taxontable[species == "Lemna trisulca", taxon_state := "hydrophytes"]
taxontable[species == "Limosella aquatica", taxon_state := "hydrophytes"]
taxontable[genus == "Lysimachia", taxon_state := "drop"]
taxontable[genus == "Myriophyllum", family := "Haloragaceae"]
taxontable[genus == "Myriophyllum", order  := "Saxifragales"]
taxontable[genus == "Myriophyllum", taxon_state := "hydrophytes"]
taxontable[genus == "Myosotis", taxon_state := "drop"]
taxontable[genus == "Nuphar", taxon_state := "hydrophytes"]
taxontable[genus == "Nymphaea", taxon_state := "hydrophytes"]
taxontable[species == "Orontium aquaticum", taxon_state := "hydrophytes"]
taxontable[species == "Pedicularis palustris", taxon_state := "drop"]
taxontable[species == "Plantago unibracteata", taxon_state := "drop"]
taxontable[original_name == "Poaceae", taxon_state := "drop"]
taxontable[genus == "Potamogeton", taxon_state := "hydrophytes"]
taxontable[original_name == "Ranunculus", taxon_state := "drop"]
taxontable[species == "Ranunculus aquatilis", taxon_state := "hydrophytes"]
taxontable[original_name == "Ranunculus Batrachium agg.", taxon_state := "drop"]
taxontable[species == "Ranunculus trichophyllus", taxon_state := "hydrophytes"]
taxontable[species == "Ranunculus peltatus", taxon_state := "hydrophytes"]
taxontable[species == "Rhododendron tomentosum", taxon_state := "drop"]
taxontable[genus == "Rumex", taxon_state := "drop"]
taxontable[genus == "Salix", taxon_state := "drop"]
taxontable[species == "Schoenus nigricans", taxon_state := "drop"]
taxontable[genus == "Scutellaria", taxon_state := "drop"]
taxontable[species == "Sium latifolium", taxon_state := "hydrophytes"]
taxontable[species == "Solanum dulcamara", taxon_state := "drop"]
taxontable[genus == "Sparganium", taxon_state := "helophytes"]
taxontable[species == "Spirodela polyrhiza", taxon_state := "hydrophytes"]
taxontable[genus == "Stuckenia", taxon_state := "hydrophytes"]
taxontable[genus == "Tofieldia", taxon_state := "drop"]
taxontable[genus == "Utricularia", taxon_state := "helophytes"]
taxontable[genus == "Vaccinium", taxon_state := "drop"]
taxontable[genus == "Viola", taxon_state := "drop"]
taxontable[genus == "Zannichellia", taxon_state := "hydrophytes"]

taxontable[original_name == "knoppslinga", c("species", "genus", "family", "order", "subclass", "class", "phylum", "kingdom", "clean", "taxon_state") := 
                   .("Myriophyllum sibiricum", "Myriophyllum", "Haloragaceae", "Saxifragales", NA, "Magnoliopsida", "Tracheophyta", "Plantae", TRUE, "hydrophytes")]
taxontable[original_name == "Myriophyllum", c("species", "genus", "family", "order", "subclass", "class", "phylum", "kingdom", "clean", "taxon_state") := 
                   .(NA, "Myriophyllum", "Haloragaceae", "Saxifragales", NA, "Magnoliopsida", "Tracheophyta", "Plantae", TRUE, "hydrophytes")]
taxontable[original_name %in% c("Sphagnum subg. Cuspidata"), c("species", "genus", "family", "order", "subclass", "class", "phylum", "kingdom", "clean", "taxon_state") := 
                   .(NA, "Sphagnum", "Sphagnaceae", "Sphagnales", NA, "Sphagnopsida", "Bryophyta", "Plantae", TRUE, "moss")]

## check if taxontable is complete now
TU <- data2$taxon |>  unique()
TU <- setdiff(TU, taxontable$original_name)

saveRDS(taxontable, paste0("data/macrophytes/",Sys.Date(),"_taxontable_macrophytes.rds"))


data2 <- rename(data2, original_name = taxon)
data2 <- taxontable[data2, on = "original_name"]  
rm(taxontable, data, TU)

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
data2[,gr_sample_id := paste0("site_", site_id, "_date_", date_id, "_sweden_monitoring_macrophytes")]

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

# - visually check the assignment of sites 
rt <- 
        data8 |> 
        unique(by = "site_id") |> 
        st_as_sf(coords = c("x.coord", "y.coord"), 
                 crs = data5$EPSG[1])

plot_typology <- st_crop(typologies, st_transform(sites, crs = st_crs(typologies)))

updated_type <- data.table(site_id = rt$site_id)

for (i in 1:nrow(rt)){
        i.rt <- rt[i, ]
        i.plot_typology <- st_crop(plot_typology, st_buffer(st_transform(i.rt, crs = st_crs(typologies)), dist =  2000))
        x <- mapview(i.plot_typology, 
                     zcol = "brt12"#,
                     #map.type = "OpenStreetMap.DE"
        ) + mapview(i.rt, color = "red")
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
saveRDS(updated_type, paste0("data/macrophytes/original_data/czech_chmi/", Sys.Date(), "_updated_type"))

#data9 <- data8[!site_id %in% remove_list]
data9 <- updated_type[data8, on = "site_id"]

#- drop "drop" rows determined in for-loop      
data9 <- data9[new_type != "drop"]
data9[, brt12 := NULL]
data9 <- rename(data9, brt12 = new_type)

data9 <- data9[species != ""]
data9$brt12 |> unique()
data9[brt12 == "RT7", brt12 := "RT07"]
data9[brt12 == "RT8", brt12 := "RT08"]


# temporal aggregation --------------------------------------------------------------
agg <- data9 |> unique(by = "gr_sample_id")
unique(table(agg$site_id))
source("R/functions/newest_sample.R")
data10 <- newest_sample(data9, season_available = FALSE)

saveRDS(data10, paste0("data/macrophytes/original_data/czech_chmi/",Sys.Date(),"_final_aggregated.rds"))

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


