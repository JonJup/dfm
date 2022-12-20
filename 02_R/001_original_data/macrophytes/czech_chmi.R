### --- Czech   Macrophytes  --- ### 

# date written: 02.05.22
# date last modified: 12.06.22
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: Clean macrophyte data for Czech CHMI 
# CRS: EPSG: 5514; 

# setup -----------------------------------------------

options("box.path" = "~/R/box_modules/")

library(pacman)
p_load(
        data.table,
        magrittr,
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
source("R/functions/add_typologies.R")


# load data -------------------------------------------------------------------------

data <- read_excel("data/macrophytes/original_data/czech_chmi/raw/Exportu macrophytes_CHMI_Baresova.xlsx")
taxontable <- readRDS("data/macrophytes/2022-05-17_taxontable_macrophytes.rds")
typologies <- readRDS("data/all_typologies.rds")


# prepare data ----------------------------------------------------------------------

data <- setDT(data)

data2 <- data.table(
        original_site_name = data$LOCALITY_NAME,
        date               = ymd(data$DATUM),
        taxon              = data$TAXON,
        abundance          = 1,
        x.coord            = data$COORDINATE_X,
        y.coord            = data$COORDINATE_Y,
        EPSG               = 5514,
        data.set           = "chezch_chmi_macrophytes",
        waterbody          = data$RIVER_NAME
)

# - some of the coordinates are entered with "," instead of ".". 
data2[, x.coord := str_replace(x.coord, ",", ".")]
data2[, y.coord := str_replace(y.coord, ",", ".")]
data2[, x.coord := as.numeric(x.coord)]
data2[, y.coord := as.numeric(y.coord)]

data2 <- data2[!(is.na(x.coord) | is.na(y.coord))]

sites <- unique(data2, by = "original_site_name")
sites <- st_as_sf(sites, coords = c("x.coord", "y.coord"), crs = data2$EPSG[1])
mapview(sites)
rm(sites, data)

data2[,c("season", "year") := .(case_when(month(date) %in% c(12,1,2)  ~ "winter",
                                            month(date) %in% c(3,4,5)   ~ "spring",
                                            month(date) %in% c(6,7,8)   ~ "summer",
                                            month(date) %in% c(9,10,11) ~ "autumn"), 
                               year(date))]

data2 <- data2[!taxon %in% c("Phalacrocera replicata", "Viridiplantae")]

TU <- data2$taxon |>  unique()
TU <- setdiff(TU, taxontable$original_name)
TU %<>% sort()

# taxontable <- dfm$update_taxonomy_macrophytes(TU, taxontable_arg = taxontable)
# 
# taxontable[taxon_state == ""]
# taxontable[taxon_state == "" & genus == "Ranunculus", ]
# 
# taxontable[class == "Bryopsida", taxon_state := "moss"]
# taxontable[phylum == "Marchantiophyta", taxon_state := "moss"]
# taxontable[phylum == "Rhodophyta", taxon_state := "red_algae"]
# 
# taxontable[genus == "Myosotis", taxon_state := "drop"]
# taxontable[genus == "Reynoutria", taxon_state := "drop"]
# taxontable[genus == "Alisma", taxon_state := "helophytes"]
# taxontable[genus == "Barbarea", taxon_state := "drop"]
# taxontable[genus == "Ranunculus" & is.na(species) , taxon_state := "drop"]
# taxontable[species == "Ranunculus aquatilis", taxon_state := "hydrophytes"]
# taxontable[species == "Ranunculus fluitans", taxon_state := "hydrophytes"]
# taxontable[species == "Ranunculus peltatus", taxon_state := "hydrophytes"]
# taxontable[species == "Ranunculus penicillatus", taxon_state := "hydrophytes"]
# taxontable[species == "Ranunculus trichophyllus", taxon_state := "hydrophytes"]
# taxontable[genus == "Bolboschoenus", taxon_state := "helophytes"]
# taxontable[genus == "Calamagrostis", taxon_state := "drop"]
# taxontable[genus == "Cardamine", taxon_state := "drop"]
# taxontable[species == "Carex bohemica", taxon_state := "drop"]
# taxontable[species == "Carex vratislaviensis", taxon_state := "helophytes"]
# taxontable[genus == "Chrysosplenium", taxon_state := "drop"]
# taxontable[original_name == "Cyperus sp.", taxon_state := "drop"]
# taxontable[species == "Eleocharis palustris", taxon_state := "helophytes"]
# taxontable[genus == "Eleocharis", taxon_state := "helophytes"]
# taxontable[genus == "Glyceria", taxon_state := "drop"]
# taxontable[genus == "Hydrocharis", taxon_state := "hydrophytes"]
# taxontable[genus == "Impatiens", taxon_state := "drop"]
# taxontable[genus == "Lemna", taxon_state := "hydrophytes"]
# taxontable[genus == "Lotus", taxon_state := "drop"]
# taxontable[genus == "Lysimachia", taxon_state := "drop"]
# taxontable[genus == "Nasturtium", taxon_state := "helophytes"]
# taxontable[genus == "Nymphaea", taxon_state := "hydrophytes"]
# taxontable[original_name == "Persicaria sp.", taxon_state := "drop"]
# taxontable[genus == "Petasites", taxon_state := "drop"]
# taxontable[genus == "Phalacrocera", taxon_state := "drop"]
# taxontable[genus == "Phalaris", taxon_state := "drop"]
# taxontable[genus == "Potamogeton", taxon_state := "hydrophytes"]
# taxontable[genus == "Potentilla", taxon_state := "drop"]
# taxontable[genus == "Scirpus", taxon_state := "drop"]
# taxontable[genus == "Solanum", taxon_state := "drop"]
# taxontable[genus == "Sparganium", taxon_state := "helophytes"]
# taxontable[genus == "Spirodela", taxon_state := "hydrophytes"]
# taxontable[genus == "Valeriana", taxon_state := "drop"]
# taxontable[genus == "Veronica", taxon_state := "drop"]
# 
# taxontable[original_name == "Asteraceae Gen. sp.", taxon_state := "drop"]
# taxontable[original_name == "Callitrichaceae Gen. sp.", taxon_state := "drop"]
# taxontable[original_name == "Lamiaceae Gen. sp.", taxon_state := "drop"]
# taxontable[original_name == "Lythraceae Gen. sp.", taxon_state := "drop"]
# taxontable[original_name == "Plantae", taxon_state := "drop"]
# taxontable[original_name == "Scrophulariaceae Gen. sp.", taxon_state := "drop"]
# 
# taxontable[original_name == "Myriophyllum sp.", c("family", "order", "class", "phylum", "kingdom", "taxon_state") := 
#                    .("Haloragaceae", "Saxifragales", "Magnoliopsida", "Tracheophyta", "Plantae", "hydrophytes")]
# taxontable[original_name == "Rorippa sylvestris x amphibia", c("species", "genus", "family", "order", "subclass", "class", "phylum", "kingdom", "clean", "taxon_state") := 
#                    .("Rorippa sylvestris", "Rorippa", "Brassicaceae", "Brassicales", NA, "Magnoliopsida", "Tracheophyta", "Plantae", TRUE, "hydrophytes")]
# 
# ## check if taxontable is complete now
# TU <- data$taxon |>  unique()
# TU <- setdiff(TU, taxontable$original_name)

#saveRDS(taxontable, paste0("data/macrophytes/",Sys.Date(),"_taxontable_macrophytes.rds"))


data2 <- rename(data2, original_name = taxon)
data2 <- taxontable[data2, on = "original_name"]  
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
data2[,gr_sample_id := paste0("site_", site_id, "_date_", date_id, "_czech_chmi_macrophytes")]

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
        abundance = 1,
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
uto <- readRDS("data/macrophytes/original_data/czech_chmi/2022-05-03_updated_type.rds")
# - visually check the assignment of sites 
rt <- 
        data8 |> 
        filter(!site_id %in% uto$site_id) |> 
        unique(by = "site_id") |> 
        st_as_sf(coords = c("x.coord", "y.coord"), 
                    crs = data5$EPSG[1])

plot_typology <- st_crop(typologies, st_transform(sites, crs = st_crs(typologies)))
updated_type <- data.table(site_id = rt$site_id)

for (i in 1:nrow(rt)){
        i.rt <- rt[i, ]
        i.plot_typology <- st_crop(plot_typology, st_buffer(st_transform(i.rt, crs = st_crs(typologies)), dist =  2000))
        x <- mapview(i.plot_typology, 
                              zcol = "brt"#,
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

utc <- bind_rows(uto, updated_type)

#- save the remove list. 
saveRDS(utc, paste0("data/macrophytes/original_data/czech_chmi/", Sys.Date(), "_updated_type_combined.rds"))

data9 <- left_join(data8, 
                   updated_type, 
                   by = "site_id")

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
data10 <- data9[month(date) %in% 5:9]
data10 <- newest_sample(data10, season_available = FALSE)
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


