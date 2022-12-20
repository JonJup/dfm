# Clean Diatom data from Finland - Monitoring #

#  created: 22-03-07
# modified: 10-06-24
#  Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
#  Purpose: In this script I create a harmonized spatial data set of diatom from the
#           raw data from Finland provided by Satu Maaria Karjalainen
#      CRS: EPSG: 3067; ETRS89 / TM35FIN(E,N) -- Finland
#    Notes: The littoral data are from lakes.
#         + Some codes are not in the original OMNIDIA file. Satu Maaria provided these in
#         + the file unknown_codes_satu.csv

# setup -----------------------------------------------------------------------------
options(box.path = "~/R/box_modules")
box::use(box / dfm)
library(pacman)
p_load(dplyr, 
       fs,
       sf, 
       tidyr, 
       data.table, 
       lubridate,
       magrittr,
       mapview,
       readxl,
       stringr,
       units)
source("R/functions/add_typologies.R")

# load data ----------------------------------------------------------------------
bio <-
        fread(
                "data/diatoms/original_data/finland_satu_maria/raw/Phytobenthos_species_river_FI_N3106.csv"
        )
sit <-
        fread(
                "data/diatoms/original_data/finland_satu_maria/raw/Phytobenthos_river_sites_FI_N3106.csv"
        )


dia1 <- readRDS("data/diatoms/fwb_table.rds")
dia2 <- readRDS("data/diatoms/omn_table.rds")
dia3 <- fread("data/diatoms/original_data/finland_satu_maria/unknown_codes_satu.csv")

## taxoncodes for genera
# dia3 <-
#         read_excel("data/diatoms/OMNIDIAFILE.xls", sheet = 2)  |>
#         select(code = CODE,
#                  taxon = DENOM,
#                 new = SYNO)
dia2 <- rbindlist(list(dia2, dia3))
taxontable <-
        readRDS("data/diatoms/2022-05-17_taxontable_diatoms.rds")
typologies <- readRDS("data/all_typologies.rds")

# prepare data  ---------------------------------------------------------------------
bio %<>%
        pivot_longer(cols = !DIANR,
                     names_to = "taxon_code",
                     values_to = "abundance") %>%
        setDT()

data <- sit[bio, on = "DIANR"]
data <- data[abundance != 0]
data <- data[, date := dmy(DATE)]
data[, c("DATE", "PVM", "DIANR") := NULL]
data %<>% rename(x.coord = "X_EUREF_FIN",
                   y.coord = "Y_EUREF_FIN")

data[, EPSG := 3067]

data[, site_id := .GRP, by = c("x.coord", "y.coord")]
data <- data[!is.na(x.coord)]

sites <- unique(data, by = c("site_id"))
sites %<>% st_as_sf(coords = c("x.coord", "y.coord"),
                       crs = sites$EPSG[1])
mapview(sites)

# - add taxon names from OMNIDIA
not.found <- c()

# - first round 
unicode <- unique(data$taxon_code)
# - data
unicode <- data[is.na(taxon), unique(taxon_code)]
#unicode <- setdiff(unicode, not.found)

for (i in seq_along(unicode)) {
        print(paste(i, "-", length(unicode)))
        # - select code for this iteration
        i.code <- unicode[i]
        # - check if code is in dia2
        if (i.code %in% dia2$code) {
                # - is it a synonym?
                i.new <- dia2$new[which(dia2$code == i.code)]
                if (is.na(i.new) |
                    stringr::str_detect(i.new, "^\\(")) {
                        i.new <- FALSE
                        i.name <-
                                dia2$taxon[which(dia2$code == i.code)]
                } else {
                        print(i.new)
                        i.rl <- readline()
                        if (i.rl == "break")
                                break()
                        i.name <-
                                dia2$taxon[which(dia2$code == i.rl)]
                        print(i.name)
                        i.rl2 <- readline()
                        if (i.rl2 == "manual") {
                                i.rl3 <- readline()
                                i.name = i.rl3
                        }
                        
                }
                
        } else if (i.code %in% dia3$code) {
                # - is it a synonym?
                i.new <- dia3$new[which(dia3$code == i.code)]
                if (is.na(i.new) |
                    stringr::str_detect(i.new, "^\\(")) {
                        i.new <- FALSE
                        i.name <-
                                dia3$taxon[which(dia3$code == i.code)]
                } else {
                        print(i.new)
                        i.rl <- readline()
                        if (i.rl == "break")
                                break()
                        i.name <-
                                dia3$taxon[which(dia3$code == i.rl)]
                        print(i.name)
                        i.rl2 <- readline()
                        if (i.rl2 != "manual") {
                                i.rl3 <- readline()
                                i.name = i.rl3
                        }
                }
        }
        
        if (!"i.name" %in% ls()) {
                not.found <- append(not.found, i.code)
        } else {
                data[taxon_code == i.code, taxon := i.name]
        }

        rm(list = ls()[grepl(pattern = "^i\\.", x = ls())])
        rm(i)
}
gc()

# - try to find missing entries in genus data 
dia3 <-
        read_excel("data/diatoms/OMNIDIAFILE.xls", sheet = 2)  |>
        select(code = CODE,
               taxon = DENOM,
               new = SYNO)
setDT(dia3)
#136 of the 239 missing taxa are in genus level entries sum(not.found %in% dia3$code)
for (i in seq_along(not.found)){
        # - feedback
        print(i)
        # - select taxon for loop
        i.tax <- not.found[i]
        # - if not in genus level data - skip 
        if (!i.tax %in% dia3$code){
                next()
        }
        data[taxon_code == i.tax, taxon := dia3[code == i.tax, taxon]]
        # - clean up 
        rm(list = ls()[grepl(pattern = "^i\\.", x = ls())])
        rm(i)
}

sv <- data[, c("taxon_code", "original_name")]
saveRDS(sv, "data/diatoms/original_data/finland_satu_maria/names_to_codes_smk.rds")


# - Some taxa are still missing (approx 1%)  — I remove them. 
data[is.na(taxon)]
data[is.na(taxon), .N] / data[, .N]
data2 <- data[!is.na(taxon)]
data2[, c("year", "month") := list(year(date), month(date))]
data2[, date_id := .GRP, by = date]
data2 <- rename(data2, "original_site_name" = "DIANIMI_EKOLAS")
data2[, site_id := .GRP, by = original_site_name]
data2[, gr_sample_id := paste0("finland_", site_id, "_", date_id)]

data2[, "season" := ifelse(month %in% c(12, 1, 2),
                          "winter",
                          ifelse(
                                  month %in% c(3, 4, 5),
                                  "spring",
                                  ifelse(month %in% c(6, 7, 8), "summer", "autumn")
                          ))]

data2[, data.set := "finland_satu_maria_diatoms"]
saveRDS(data, "data/diatoms/original_data/finland_satu_maria/quick_save.rds")

data <- copy(data2)

data <- data[!taxon %in% c("GENRE NON IDENTIFIE", "Centric Diatoms Diatomées centriques indifférenciées", "DIATOMEE NON IDENTIFIEE (indeterminée)")]

## taxonomic cleaning in fix_tax script.

TU <- data[, unique(taxon)]
any(!TU %in% taxontable$original_name)
TU[which(!TU %in% taxontable$original_name)]

# fix_tax2 

## add taxon information
data <- rename(data, original_name = taxon)
setDT(data)
data2 <- taxontable[data, on = "original_name"]
(missing_taxa <- data2[is.na(kingdom), unique(original_name)])

## check
sort(unique(data2$phylum))
sort(unique(data2$class))

# ## add site and date ids for this I need to round coordinates because some samples are
# ## categorized as from different sites even though they are from the same.
# data2[, c("x.coord_round", "y.coord_round") := .(round(x.coord, 5),
#                                                  round(y.coord, 5))]
## add leading zeros
data2[, site_id := case_when(
        nchar(trunc(site_id)) == 1 ~ paste0("0000", site_id),
        nchar(trunc(site_id)) == 2 ~ paste0("000", site_id),
        nchar(trunc(site_id)) == 3 ~ paste0("00", site_id),
        nchar(trunc(site_id)) == 4 ~ paste0("0", site_id),
        nchar(trunc(site_id)) == 5 ~ paste0(site_id)
)]
data2[, date_id := case_when(
        nchar(trunc(date_id)) == 1 ~ paste0("0000", date_id),
        nchar(trunc(date_id)) == 2 ~ paste0("000",  date_id),
        nchar(trunc(date_id)) == 3 ~ paste0("00",   date_id),
        nchar(trunc(date_id)) == 4 ~ paste0("0",    date_id),
        nchar(trunc(date_id)) == 5 ~ paste0(date_id)
)]

## add gr_sample_id
data2[, gr_sample_id := paste0("site_",
                               site_id,
                               "_date_",
                               date_id,
                               "_findland_monitroting_diatoms")]


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
        data.set = "findland_monitoring_diatom"
)]

## combine entries of same taxon
data3[, lowest.taxon := ifelse(!is.na(species), species,
                               ifelse(!is.na(genus), genus,
                                      ifelse(
                                              !is.na(family), family,
                                              ifelse(!is.na(order), order,
                                                     ifelse(
                                                             !is.na(class), class,
                                                             ifelse(!is.na(phylum), phylum, kingdom)
                                                     ))
                                      )))]

data3[, abundance := as.numeric(abundance)]
data3[, abundance := sum(abundance), by = c("gr_sample_id", "lowest.taxon")]
data3 <- unique(data3, by = c("gr_sample_id", "lowest.taxon"))
data4 <- copy(data3)
data5 <- add_typologies(data4)

# saveRDS(data5, "data/diatoms/original_data/finland_satu_maria/data5_save.rds")
# data5 <- readRDS("data/diatoms/original_data/finland_satu_maria/data5_save.rds")
# # ## visual checks
# sites <- unique(data5, by = "gr_sample_id") |> st_as_sf(coords = c("x.coord", "y.coord"), crs = data5$EPSG[1])
#
# mapview(sites, zcol = "brt12")
# # mapview(sites, zcol = "ife")
# # mapview(sites, zcol = "bgr")
# mapview(sites, zcol = "least.impacted")

# - subset to least impacted catchments
data6 <- data5[least.impacted == TRUE]

sites <-
        unique(data6, by = "site_id") |> st_as_sf(coords = c("x.coord", "y.coord"),
                                                     crs = data5$EPSG[1])

# - Look for sites with different ID but same coordinates
distances  <- st_distance(sites)
distances2 <- as.matrix(distances)
diag(distances2) <- 999
(duplicate_sites <- which(distances2 < units::as_units(1, "m")))

# - many duplicates assign new site_id 
data5[, c("x.coord_round", "y.coord_round") := .(round(x.coord, 5),
                                                 round(y.coord, 5))]

data5[, site_id := .GRP, by = c("x.coord_round", "y.coord_round")]
data5[, site_id := as.numeric(site_id)]

# - add leading zeros
data5[, site_id := case_when(
        nchar(trunc(site_id)) == 1 ~ paste0("0000", site_id),
        nchar(trunc(site_id)) == 2 ~ paste0("000", site_id),
        nchar(trunc(site_id)) == 3 ~ paste0("00", site_id),
        nchar(trunc(site_id)) == 4 ~ paste0("0", site_id),
        nchar(trunc(site_id)) == 5 ~ paste0(site_id)
)]

# - check for duplicates again 
data6 <- data5[least.impacted == TRUE]
sites <-
        unique(data6, by = "site_id") |> 
        st_as_sf(coords = c("x.coord", "y.coord"),
                    crs = data5$EPSG[1])
distances  <- st_distance(sites)
distances2 <- as.matrix(distances)
diag(distances2) <- 999
(duplicate_sites <- which(distances2 < units::as_units(1, "m")))

# - drop sites with very few species
data6[, richness := uniqueN(lowest.taxon), by = "gr_sample_id"]
hist(data6$richness)
data7 <- data6[richness > 10]

# - drop sites far removed from ECRINS river network
data8 <- data7[distance < 300]

uto <- readRDS("data/diatoms/original_data/finland_satu_maria/2022-03-28_updated_type.rds")

# 460
uniqueN(data8$site_id)
# 449 
sum(unique(data8$site_id) %in% uto$site_id)

# 11 missing 

# - visually check the assignment of sites

rt <-
        data8 |>
        filter(!site_id %in% uto$site_id) |> 
        unique(by = "site_id") |>
        st_as_sf(coords = c("x.coord", "y.coord"),
                    crs = data5$EPSG[1])

plot_typology <-
        st_crop(typologies, st_transform(sites, crs = st_crs(typologies)))
options(warn = -1)

updated_type <- data.table(site_id = rt$site_id)

for (i in 1:nrow(rt)) {
        i.rt <- rt[i,]
        i.plot_typology <-
                st_crop(plot_typology,
                           st_buffer(st_transform(i.rt, crs = st_crs(typologies)), dist =  2000))
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

utc <- bind_rows(updated_type, uto)

#- save the remove list.
saveRDS(
        utc,
        paste0(
                "data/diatoms/original_data/finland_satu_maria/",
                Sys.Date(),
                "_updated_type_combined.rds"
        )
)

#- join updated types to data 8
data9 <- left_join(data8, 
                   utc, 
                   by = "site_id")

#- drop "drop" rows determined in for-loop
data9 <- data9[new_type != "drop"]
data9[, brt12 := NULL]
data9 <- rename(data9, brt12 = new_type)

# temporal aggregation --------------------------------------------------------------
agg <- data9 |> unique(by = "gr_sample_id")
unique(table(agg$site_id))
# - yes
data9 <- data10[year > 2005 & year < 2023] 
data9 <- data9[month(date) %in% 5:9]
source("R/functions/newest_sample_no_season.R")
data10 <- newest_sample(data9)

data10[, c("sampling.events", "richness", "x.coord_round", "y.coord_round") := NULL]
data10[, data.set := "finland_monitoring_diatom"]

saveRDS(
        data10,
        paste0(
                "data/diatoms/original_data/finland_satu_maria/",
                Sys.Date(),
                "_final_aggregated.rds"
        )
)

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
#data10[, richness := uniqueN(lowest.taxon), by = "gr_sample_id"]
unique(data10, by = "gr_sample_id") |> pull(richness) |> mean()
