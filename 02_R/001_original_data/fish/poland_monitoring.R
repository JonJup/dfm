### ----------------------------------------- ###
### --- Fish data from Polish Monitoring  --- ### 
### ----------------------------------------- ###

# -------------------------------
# date written: 22-02-23
# date last modified: 09.06.22
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: Clean and harmonize fish data provided by Piotr Panek.  
# CRS: 4326
# -------------------------------

options("box.path" = "~/R/box_modules/")

library(data.table)
library(magrittr)
library(mapview)
#library(mapview)
box::use(readxl[read_excel],
         lubridate[ymd, year, month],
         stringdist[amatch],
         sf = sf,
         dp = dplyr,
         box/dfm)


# read data -------------------------------------------------------------------------

data <- read_excel("data/fish/original_data/poland_monitoring/raw/Fish PL RW 2011_2021.xlsx")
taxontable <- readRDS("data/fish/2022-04-27_taxontable_fish.rds")
typologies <- readRDS("data/all_typologies.rds")
source("R/functions/add_typologies.R")

# prepare data  ---------------------------------------------------------------------

## translation of variable names 

# Nazwa_stanowiska                  -> name_station 
# Rzeka                             -> Fluss
# Rzedowosc                         -> Governance/ Ordentlichkeit (WFD state?)
# Województwo                       -> Woiwodschaft (polnischer Verwaltungsbezirk)
# Typ_abiotyczny                    -> typ abiotisch 
# Typ_geologiczny                   -> typ geologisch 
# Recypient                         -> Empfänger (Wo fließt der Fluss rein Ostsee, )
# Dorzecze                          -> Becken 
# rok                               -> Jahr 
# Odleg_ od_zródel_km               -> distance to source 
# Wys. n.p.m_m                      -> Höhe üNN
# "Szer_rzeki_m"                    -> Flussbreite 
# "Zbiorniki_zaporowe_pow."         -> Querbauwerke? 
# "Jeziora_pow."                    -> Lakes
# "Typ_geomorfologiczny"            -> Type geomorphologisch 
# "Dawna_dolina_zalewowa"           -> Ehemaliges Lagunental 
# "Źródło_zasilania_w_wodę"         -> Wasserkraftwerk 
# "Reżim_przepływu"                 -> Flussregime 
# "Zakłócenia_przepływu"            -> Flussstörung 
# "Dorzecze_km2"                    -> Catchment area 
# "Śred_rocz_temp_powietrza_ºC"     -> Avg. Air Temperature
# "Śred_temp_powietrza_styczeń"     -> Avg. Air Temperature January 
# "Śred_temp_powietrza_lipiec"      -> Avg. Air Temperature July 
# "Temp_wody"                       -> Water Temperature  
# "Dług_stan_m"                     -> ??? 
# "Strefa_połowu"                   -> Fangzone 
# "Szer_strefy_połowu_m"            -> Fish zones 
# "Pow_połowu_m2"                   -> Area fishing m^2  
# "Śred_głęb_cm"                    -> Diameter [cm]
# "Spadek_‰"                        -> Slope [‰]  
# "Umiejscowienie_stanowiska"       -> ???
# "Umocnienia_brzegu"               -> Shore Fortifications 
# "Drzewa"                          -> Trees
# "Krzewy"                          -> Shrubs 
# "Dno_urozmaicenie_1-3-5"          -> Substrate variety ? 
# "Struktura_dna"                   -> Bottom structure  
# "V_przepływu_m/s"                 -> Flow velocity [m/s]
# "Przewodność_µS/cm"               -> Conductivity [µS/cm]
# "Bliskie_otoczenie"               -> Close surroundings (e.g. Field, Forest, City)
# "Zespół_badawczy"                 -> Research Team 
# "Typ_agregatu"                    -> Aggregate type (stationary or backpack)
# "Moc_kW"                          -> Power [kw]
# "Amper"                           -> Amper 
# "Volt"                            -> Volt 
# "Sposób_łowienia"                 -> way of fishing/ samplig method (wading, boad, mixed)
# "Gatunek"                         -> Taxon
# "Osobn"                           -> Abundance
# "Masa_g"                          -> Mass [g]
# "Osobn_<150_mm"                   -> Abundance of Fish <150mm
# "Osobn_>150_mm"                   -> Abundance of Fish >150mm
# "Ryby_z_anomaliami_hybrydy_Osobn" -> Abundance hybrid Fish

data2 <- data.table(
        original_site_name = data$Nazwa_stanowiska,
        x.coord = data$`Dług_geogr_ E`,
        y.coord = data$`Szer_ geogr_N`,
        waterbody = data$Rzeka,
        lake = data$Jeziora_pow., 
        taxon = data$Gatunek,
        abundance = data$Osobn,
        month = data$`M-c`,
        day = data$Dz.,
        year = data$Rok,
        EPSG = 4326,
        data.set = "poland_monitoring_fish"
)

# create date variable 
data2[, date := ymd(paste0(year,"-",month,"-", day))]

# drop brak ryb (no fish) and brak wody (lack of water)
data2 <- data2[!taxon %in% c("brak wody", "brak ryb")]

# add season 
## add season and year 
data2[,c("season") := .(dp$case_when(month(date) %in% c(12,1,2)  ~ "winter",
                                            month(date) %in% c(3,4,5)   ~ "spring",
                                            month(date) %in% c(6,7,8)   ~ "summer",
                                            month(date) %in% c(9,10,11) ~ "autumn"))]

## explore sites 
# sites <- unique(data2, by = "original_site_name")
# sites <- sf$st_as_sf(sites, coords = c("x.coord", "y.coord"), crs = 4326)
# mapview::mapview(sites)

# TU <- setdiff(unique(data2$taxon), taxontable$original_name)
# taxontable <- update_taxonomy2(TU)
# saveRDS(taxontable, paste0("data/fish/",Sys.Date(),"_taxontable_fish.rds"))      

data2 <- dp$rename(data2, original_name = taxon)
data3 <- taxontable[data2, on = "original_name"]
data3[, c("x.coord_round", "y.coord_round") := .(round(x.coord, 5), 
                                                 round(y.coord, 5))]

data3[, site_id := .GRP, by = c("x.coord_round", "y.coord_round")]
data3[, date_id := .GRP, by = "date"]

## add leading zeros
data3[, site_id := dp$case_when(
        nchar(trunc(site_id)) == 1 ~ paste0("0000", site_id),
        nchar(trunc(site_id)) == 2 ~ paste0("000", site_id),
        nchar(trunc(site_id)) == 3 ~ paste0("00", site_id),
        nchar(trunc(site_id)) == 4 ~ paste0("0", site_id),
        nchar(trunc(site_id)) == 5 ~ paste0(site_id))]
data3[, date_id := dp$case_when(
        nchar(trunc(date_id)) == 1 ~ paste0("0000", date_id),
        nchar(trunc(date_id)) == 2 ~ paste0("000",  date_id),
        nchar(trunc(date_id)) == 3 ~ paste0("00",   date_id),
        nchar(trunc(date_id)) == 4 ~ paste0("0",    date_id),
        nchar(trunc(date_id)) == 5 ~ paste0(        date_id))]

## add gr_sample_id
data3[,gr_sample_id := paste0("site_", site_id, "_date_", date_id, "_poland_monitoring_fish")]

# - check that gr_sample_id matches sample_id

## reshape data
data4 <- data3[, list(
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
        data.set,
        waterbody
)]

## combine entries of same taxon
data4[, lowest.taxon := ifelse(!is.na(species), species,
                               ifelse(!is.na(genus), genus,
                                      ifelse(!is.na(family), family,
                                             ifelse(!is.na(order), order,
                                                    ifelse(!is.na(class), class,
                                                           ifelse(!is.na(phylum), phylum, kingdom))))))]

data4[, abundance := as.numeric(abundance)]
data4[, abundance := sum(abundance), by = c("gr_sample_id", "lowest.taxon")]
data5 <- unique(data4, by = c("gr_sample_id", "lowest.taxon"))
data5 <- add_typologies(data5)

## visual checks
# sites <- unique(data5, by = "site_id") |> sf$st_as_sf(coords = c("x.coord", "y.coord"), crs = data5$EPSG[1])
# 
# library(mapview)
# 
# mapview(sites, zcol = "brt12")
# mapview(sites, zcol = "ife")
# mapview(sites, zcol = "bgr")
# mapview(sites, zcol = "least.impacted")

# - subset to least impacted catchments 
data6 <- data5[least.impacted == TRUE]

sites <- unique(data6, by = "site_id") |> sf$st_as_sf(coords = c("x.coord", "y.coord"), crs = data5$EPSG[1])

## look for sites with different ID but same coordinates 
distances  <- sf$st_distance(sites)
distances2 <- as.matrix(distances)
diag(distances2) <- 999
(duplicate_sites <- which(distances2 < units::as_units(1, "m")))

for (i in seq_along(duplicate_sites)){
        if (i == 1){
                duplicate_site_locations <- data.table(id = 1:length(duplicate_sites))
        }
        i.location <- dfm$find_point(duplicate_sites[i], 
                       distane_matrix = distances2)
        i.data <- sites[i.location, ]
        data6[site_id == i.data$site_id[1], site_id := i.data$site_id[2]]
        rm(list = ls()[grepl("i\\.", ls())])
}

data6[, richness := uniqueN(lowest.taxon), by = "gr_sample_id"]
summary(data6$richness)
hist(data6$richness)
data7 <- data6[richness > 2]

# - drop sites far removed from ECRINS river network 
data8 <- data7[distance < 300]

# - empty 
# updated_type_old <- readRDS("data/fish/original_data/poland_monitoring/2022-02-23_updated_type.rds")

# - visually check the assignment of sites 
rt <- 
        data8 |> 
        unique(by = "site_id") |> 
        sf$st_as_sf(coords = c("x.coord", "y.coord"), 
                    crs = data5$EPSG[1])

plot_typology <- sf$st_crop(typologies, sf$st_transform(sites, crs = sf$st_crs(typologies)))

updated_type <- data.table(site_id = rt$site_id)

options(warn = -1)

for (i in 1:nrow(rt)){
        i.percent <- i/nrow(rt) * 100
        i.rt <- rt[i, ]
        i.plot_typology <- sf$st_crop(plot_typology, sf$st_buffer(sf$st_transform(i.rt, crs = sf$st_crs(typologies)), dist =  2000))
        x <- mapview(i.plot_typology, zcol = "brt") + mapview(i.rt, popup = "waterbody", color = "red")
        print(x)
        i.bool <- "n"
        i.bool <- readline(paste0(i,"/", nrow(rt)))
        if (i.bool == "break")
                break()
        if (i.bool == "n"){
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
saveRDS(updated_type, paste0("data/fish/original_data/poland_monitoring//", Sys.Date(), "_updated_type.rds"))

data9 <- dp$left_join(data8, 
                   updated_type, 
                   by = "site_id")
any(is.na(data9$new_type))

#- drop "drop" rows determined in for-loop      
data9 <- data9[new_type != "drop"]
data9[, brt12 := NULL]
data9 <- dp$rename(data9, brt12 = new_type)

# temporal aggregation --------------------------------------------------------------
agg <- data9 |> unique(by = "gr_sample_id")
unique(table(agg$site_id))
data9[, month := lubridate::month(date)]
data9 <- data9[month %in% 5:9]
# yes 
source("R/functions/newest_sample.R")
data10 <- newest_sample(data9, season_available = FALSE)

saveRDS(data10, paste0("data/fish/original_data/poland_monitoring/",Sys.Date(),"_final_aggregated.rds"))


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


