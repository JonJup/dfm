# Clean diatom data from Finland —— Janne Soininen #

#  created: 21-11-29
# modified: 22-01-20
#  Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
#  Purpose: In this script I create a harmonized spatial data set of diatoms from the 
#           raw data provided by the Janne Soininen.
#      CRS: KKJ / Finland Uniform Coordinate System; EPSG: 2393
# Comment: All sites where sampled once in August 2001 or August 2004. 


# setup -----------------------------------------------------------------------------
options(box.path = "~/R/box_modules")
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
       units)
box::use(box/dfm)
source("R/functions/add_typologies.R")
source("R/functions/harmonize diatoms.R")

# load data -------------------------------------------------------------------------
## directory with all biological data 
bio_wd <- "data/diatoms/original_data/finland_soininen/raw/Species_diatoms_Finland.xlsx"
sit_wd <- "data/diatoms/original_data/finland_soininen/raw/Streams_environment_Finland.xlsx"

bio <- read_excel(bio_wd, sheet = 1) |> setDT()
sit <- read_excel(sit_wd, sheet = 1) |> setDT()
typologies <- readRDS("data/all_typologies.rds")
taxontable <- readRDS("data/diatoms/2022-05-17_taxontable_diatoms.rds")

dia1 <- readRDS("data/diatoms/harmonization_tables/fwb_table.rds")
dia2 <- readRDS("data/diatoms/harmonization_tables/omn_table.rds")

# prepare data ----------------------------------------------------------------------
## sampling method 
bio |> 
        select(-c("Species", "genus", "abbr. name")) |> 
        pivot_longer(cols = !`full name`, names_to = "original_site_name", "abundance") -> 
        bio2 
names(bio2)[1] <- "taxon"
bio2 %<>% filter(value!=0)


sit %<>% select(Site, longitude, latitude)


data <- left_join(bio2, sit, by = c("original_site_name" = "Site"))

# all sampling was conducted in August 2001 or 2004. See Soininen (2008): The
# Ecological Characteristics of Idiosyncratic and Nested Diatoms

data |> 
        mutate(EPSG = 2393,
               date = NA,
               year = NA,
               season = "autumn",
               data.set = "finalnd_janne_soininen_diatoms") |> 
        rename(x.coord = longitude, 
               y.coord = latitude) -> 
        data
setDT(data)
## remove sites without coordinates
data <- data[!is.na(x.coord)]
data <- data[!is.na(y.coord)]
data <- data[!is.na(taxon)]

# sites <- unique(data, by = "original_site_name")
# sites2 <- st_as_sf(sites, coords = c("x.coord", "y.coord"), crs = data$EPSG[1])
# mapview(sites2)

(TU <- unique(data$taxon) |> sort())
(TU <- setdiff(TU,taxontable$original_name))
length(TU) == 0

# JOIN DATA -------------------------------------------------------------------------
names(data)[which(names(data) == "taxon")] <- "original_name"
data2 <- taxontable[data, on = "original_name"]

sort(unique(data2$kingdom))
sort(unique(data2$phylum))
sort(unique(data2$class))
sort(unique(data2$subclass))
sort(unique(data2$order))

## add site and date ids
#data2[, site_id := .GRP, by = "original_site_name"]
data2[, site_id := .GRP, by = c("x.coord", "y.coord")]
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
data2[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_finland_janne_soininen_diatoms")]

# - how many samples? 
uniqueN(data2$gr_sample_id)

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
        abundance = value,
        x.coord,
        y.coord,
        EPSG,
        data.set = "finland_janne_soininen_diatoms"
)]

## combine entries of same taxon
data3[, lowest.taxon := ifelse(!is.na(species), species,
                               ifelse(!is.na(genus), genus,
                                      ifelse(!is.na(family), family,
                                             ifelse(!is.na(order), order,
                                                           ifelse(!is.na(class), class,
                                                                  ifelse(!is.na(phylum), phylum, kingdom))))))]

data3[, abundance := sum(abundance), by = c("gr_sample_id", "lowest.taxon")]
data3 <- unique(data3, by = c("gr_sample_id", "lowest.taxon"))
data4 <- copy(data3)
data5 <- add_typologies(data4)

# ## visual checks
# sites <- unique(data5, by = "gr_sample_id") |> st_as_sf(coords = c("x.coord", "y.coord"), crs = data5$EPSG[1])
# 
# mapview(sites, zcol = "brt12")
# mapview(sites, zcol = "ife")
# mapview(sites, zcol = "bgr")
# mapview(sites, zcol = "least.impacted")

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

rl <- readRDS("data/diatoms/original_data/finland_soininen/2022-01-20_remove_list.rds")
data8 <- data8[!site_id %in% rl]

# - visually check the assignment of sites 
rt <- 
        data8 |> 
        unique(by = "site_id") |> 
        st_as_sf(coords = c("x.coord", "y.coord"), 
                 crs = data5$EPSG[1])

# plot_typology <- st_crop(typologies, st_transform(sites, crs = st_crs(typologies)))
# updated_type <- data.table(site_id = rt$site_id)
# options(warn = -1)
# 
# for (i in 1:nrow(rt)){
#         i.rt <- rt[i, ]
#         i.plot_typology <- st_crop(plot_typology, st_buffer(st_transform(i.rt, crs = st_crs(typologies)), dist =  2000))
#         x <- mapview(i.plot_typology, zcol = "brt") + mapview(i.rt, popup = "original_site_name", color = "red")
#         print(x)
#         #i.bool <- "n"
#         i.bool <- readline(paste(i, "/", nrow(rt), ":"))
#         if (i.bool == "break"){
#                 break()
#         } else if (i.bool == "n"){
#                 updated_type[site_id == i.rt$site_id, new_type := "drop"]
#         } else if (i.bool == "c"){
#                 i.towhat <- readline("change to:")
#                 updated_type[site_id == i.rt$site_id, new_type := i.towhat]
#         } else {
#                 updated_type[site_id == i.rt$site_id, new_type := i.rt$brt12]
#         }
#         
#         rm(list = ls()[grepl("i\\.", ls())])
# }



#- save the remove list. 
saveRDS(updated_type, paste0("data/diatoms/original_data/finland_soininen/", Sys.Date(), "_updated_type.rds"))
updated_type <- readRDS("data/diatoms/original_data/finland_soininen/2022-06-10_updated_type.rds")

# - join data to updated types
data9 <- left_join(data8, 
                   updated_type, 
                   by = "site_id")

# - drop "drop" rows and fix changed types 
data9 <- data9[new_type != "drop"]
data9[, brt12 := NULL]
data9 <- rename(data9, brt12 = new_type)

# temporal aggregation --------------------------------------------------------------
agg <- data9 |> unique(by = "gr_sample_id")
unique(table(agg$site_id))
# no 
data10 <- data9
data10[,date:=NULL]
data10$date <- lubridate::dmy("1-8-2004")

saveRDS(data10, paste0("data/diatoms/original_data/finland_soininen/",Sys.Date(),"_final_aggregated.rds"))
#data10 <- readRDS("data/diatoms/original_data/finland_soininen/2022-01-19_final_aggregated.rds")

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
