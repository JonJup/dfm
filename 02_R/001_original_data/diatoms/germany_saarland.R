# ——————————————————————————————————————————————— #
# ——— Clean Diatom data from Germany Saarland ——— # 
# ——————————————————————————————————————————————— #

# ———————————————————————————————————
# date first written: 21-12-09
# date last modified: 22-01-19
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: In this script I create a harmonized spatial data set of diatom from the 
# raw data from Germany Saarland.  
# Temporal aggregation:  yes
# CRS: 31466
# Comment: Not a single sampling site fulfills the criteria.
# ————————————————

# setup -----------------------------------------------------------------------------
#devtools::install_github("https://github.com/jonjup/jjmisc")
library(data.table)
library(dplyr)
library(fs)
library(jjmisc)
library(lubridate)
library(mapview)
library(magrittr)
library(sf)
library(stringr)
library(stringdist)
library(readxl)
library(tidyr)

source("R/functions/harmonize diatoms.R")

# load data -------------------------------------------------------------------------
## directory with all biological data 
bio_wd <- "data/macrophytes/original_data/germany_saarland/raw/Daten_SAL_2013-2018.xlsx"

bio <- read_excel(bio_wd, sheet = 2) |> setDT()
sit <- read_excel(bio_wd, sheet = 4) |> setDT()

dia1 <- readRDS("data/diatoms/fwb_table.rds")
dia2 <- readRDS("data/diatoms/omn_table.rds")
non_diatom_algae <- readRDS("data/diatoms/2021-12-08_non_diatom_algae.rds")

# prepare data ----------------------------------------------------------------------

## reshape and rename 
bio2 <- 
        data.table(
                original_site_name = bio$`Messst-Nr.`,
                date = dmy(bio$`Datum Probenahme`), 
                taxon = bio$Taxon,
                abundance = bio$`Objektzahl Taxon absolut`
        )

sit2 <- data.table(
        original_site_name = sit$`Messst-Nr.`,
        x.coord = sit$`R-Wert`,
        y.coord = sit$`H-Wert`,
        EPSG = 31466
)

data <- sit2[bio2, on = "original_site_name"]

data[,c("year", "season") := .(year(date), 
                               case_when(month(date) %in% c(12,1,2)  ~ "winter",
                                         month(date) %in% c(3,4,5)   ~ "spring",
                                         month(date) %in% c(6,7,8)   ~ "summer",
                                         month(date) %in% c(9,10,11) ~ "autumn"))]

## remove sites without coordinates and taxa 
data <- data[!is.na(x.coord)]
data <- data[!is.na(y.coord)]
data <- data[!is.na(taxon)]

sites <- unique(data, by = "original_site_name")
sites2 <- st_as_sf(sites, coords = c("x.coord", "y.coord"), crs = data$EPSG[1])
mapview(sites2)

# hamornize taxon names  ------------------------------------------------------------
taxontable <- readRDS("data/diatoms/2022-01-19_taxontable_diatoms.rds")

TU <- unique(data$taxon) |> sort()
TU <- setdiff(TU, taxontable$original_name)

## what is the most similar in in taxontable 
strdist_id  <- amatch(TU, taxontable$original_name, maxDist = 1000)
strdist_tbl <- data.table(taxontable_name = taxontable$original_name[strdist_id], TU) |> 
        filter(!is.na(taxontable_name)) 

## manual additions to taxon table 
taxontable <- append_to_tt("Chamaepinnularia soehrensis var.muscicola","Chamaepinnularia soehrensis var. muscicola")
taxontable <- append_to_tt("Eunotia nymanniana","Eunotia nymanniana Lectotypus")
taxontable <- append_to_tt("Fragilaria ulna acus-Sippen","Fragilaria ulna var. acus")
taxontable <- append_to_tt("Gomphonema parvulum var.lagenula","Gomphonema parvulum var. lagenula")
taxontable <- append_to_tt("Mayamaea fossalis var.obsidialis","Mayamaea fossalis var. obsidialis")
taxontable <- append_to_tt("Nitzschia acicularis -Formenkreis","Nitzschia acicularis - Formenkreis")
taxontable <- append_to_tt("Pinnularia rhombarea var.halophila","Pinnularia rhombarea var. halophila")
taxontable <- append_to_tt("Pinnularia subgibba var.undulata","Pinnularia subgibba var. undulata")

TU <- setdiff(TU, taxontable$original_name)

## check against fwb table 
for (i in seq_along(TU)){
        i.tu  <- TU[i]
        if(check_fwb(i.tu)){
                x <- get_fwb(i.tu)
                taxontable <- add_entry_tt(x)
        }
        rm(i.tu)
}; rm(i)

TU <- setdiff(TU, taxontable$original_name)

for (i in seq_along(TU)) {
        i.tu  <- TU[i]
        i.det <- str_detect(dia2$taxon, i.tu)
        if (any(i.det)) {
                i.id <- which(i.det)
                print(paste("name:", i.tu))
                print(paste("suggestions:", dia2$taxon[i.id]))
                i.rl <- readline()
                if (i.rl != "break") {
                        i.id <- i.id[as.numeric(i.rl)]
                        ## is it a synonym?
                        if (!is.na(dia2$new[i.id])) {
                                print(paste("new code:",
                                            dia2$new[i.id]))
                                ## enter new code
                                i.rl2 <- readline()
                                i.id <- which(dia2$code == i.rl2)
                        }
                        print(
                                paste("Final name: ", dia2$taxon[i.id])
                        )
                        i.final <- readline()
                        ## check that against fwb
                        if (check_fwb(i.final)) {
                                i.final <- get_fwb(i.final)
                        }
                        taxontable <- add_entry_tt(i.final)
                }
        }
        rm(list = ls()[grepl(pattern = "^i\\.", x = ls())])
}

TU <- setdiff(TU, taxontable$original_name)

taxontable[,clean := TRUE]

taxontable <- new_genus(ori = "Caloneis schumanniana var. schumanniana",
                        fix = "Caloneis",
                        spe = NA,
                        gen = "Caloneis",
                        fam = "Naviculaceae",
                        ord = "Naviculales",
                        cla = "Bacillariophyceae",
                        phy = "Bacillariophyta",
                        kin = "Chromista"
)
taxontable <- new_genus(ori = "Cymbella hustedtii var. hustedtii",
                        fix = "Cymbella",
                        spe = NA,
                        gen = "Cymbella",
                        fam = "Cymbellaceae",
                        ord = "Cymbellales", 
                        cla = "Bacillariophyceae",
                        phy = "Bacillariophyta",
                        kin = "Chromista")
taxontable <- new_genus(ori = "Cymbopleura hybrida var. hybrida",
                        fix = "Cymbopleura",
                        spe = NA,
                        gen = "Cymbopleura",
                        fam = "Cymbellaceae",
                        ord = "Cymbellales",
                        cla = "Bacillariophyceae",
                        phy = "Bacillariophyta",
                        kin = "Chromista")
taxontable <- new_genus(ori = "Eunotia arcus sensu stricto",
                        fix = "Eunotia arcus/mucophila/bilunaris Complex",
                        spe = "Eunotia arcus/mucophila/bilunaris Complex",
                        gen = "Eunotia",
                        fam = "Eunotiaceae",
                        ord = "Eunotiales",
                        cla = "Bacillariophyceae",
                        phy = "Bacillariophyta",
                        kin = "Chromista")
taxontable <- new_genus(ori = "Eunotia bilunaris var. bilunaris",
                        fix = "Eunotia arcus/mucophila/bilunaris Complex",
                        spe = "Eunotia arcus/mucophila/bilunaris Complex",
                        gen = "Eunotia",
                        fam = "Eunotiaceae",
                        ord = "Eunotiales",
                        cla = "Bacillariophyceae",
                        phy = "Bacillariophyta",
                        kin = "Chromista")
taxontable <- new_genus(ori = "Eunotia paludosa var. trinacria",
                        fix = "Eunotia",
                        spe = NA,
                        gen = "Eunotia",
                        fam = "Eunotiaceae",
                        ord = "Eunotiales",
                        cla = "Bacillariophyceae",
                        phy = "Bacillariophyta",
                        kin = "Chromista")
taxontable <- new_genus(ori = "Eunotia praerupta var. praerupta" ,
                        fix = "Eunotia praerupta Complex",
                        spe = "Eunotia praerupta Complex",
                        gen = "Eunotia",
                        fam = "Eunotiaceae",
                        ord = "Eunotiales",
                        cla = "Bacillariophyceae",
                        phy = "Bacillariophyta",
                        kin = "Chromista")
taxontable <- new_genus(ori = "Fragilaria brevistriata var. brevistriata" ,
                        fix = "Staurosira brevistriata",
                        spe = "Staurosira brevistriata",
                        gen = "Staurosira",
                        fam = "Staurosiraceae",
                        ord = "Fragilariales",
                        cla = "Bacillariophyceae",
                        phy = "Bacillariophyta",
                        kin = "Chromista")
taxontable <- new_genus(ori = "Fragilaria pinnata var. pinnata"  ,
                        fix = "Staurosira mutabilis",
                        spe = "Staurosira mutabilis",
                        gen = "Staurosira",
                        fam = "Staurosiraceae",
                        ord = "Fragilariales",
                        cla = "Bacillariophyceae",
                        phy = "Bacillariophyta",
                        kin = "Chromista")
taxontable <- new_genus(ori = "Fragilaria virescens var. virescens"  ,
                        fix = "Fragilaria virescens complex",
                        spe = "Fragilaria virescens complex",
                        gen = "Fragilaria",
                        fam = "Fragilariaceae",
                        ord = "Fragilariales",
                        cla = "Bacillariophyceae",
                        phy = "Bacillariophyta",
                        kin = "Chromista")
taxontable <- new_genus(ori = "Germainiella enigmatica",
                        fix = "Germainiella enigmatica",
                        spe = "Germainiella enigmatica",
                        gen = "Germainiella",
                        fam = "Naviculaceae", 
                        ord = "Naviculales",
                        cla = "Bacillariophyceae",
                        phy = "Bacillariophyta",
                        kin = "Chromista")
taxontable <- new_genus(ori = "Gyrosigma acuminatum var. acuminatum"  ,
                        fix = "Gyrosigma",
                        spe = NA,
                        gen = "Gyrosigma",
                        fam = "Naviculaceae",
                        ord = "Naviculales",
                        cla = "Bacillariophyceae",
                        phy = "Bacillariophyta",
                        kin = "Chromista")
taxontable <- new_genus(ori = "Hantzschia amphioxys sensu stricto",
                        fix = "Hantzschia amphioxys",
                        spe = "Hantzschia amphioxys",
                        gen = "Hantzschia",
                        fam = "Bacillariaceae",
                        ord = "Bacillariales",
                        cla = "Bacillariophyceae",
                        phy = "Bacillariophyta",
                        kin = "Chromista")
taxontable <- new_genus(ori = "Luticola mutica var. mutica"  ,
                        fix = "Luticola",
                        spe = NA,
                        gen = "Luticola",
                        fam = "Diadesmidaceae",
                        ord = "Naviculales",
                        cla = "Bacillariophyceae",
                        phy = "Bacillariophyta",
                        kin = "Chromista")
taxontable <- new_genus(ori = "Navicula arvensis var. major",
                        fix = "Navicula arvensis-difficillima++",
                        spe = "Navicula arvensis-difficillima++",
                        gen = "Navicula",
                        fam = "Naviculaceae",
                        ord = "Naviculales",
                        cla = "Bacillariophyceae",
                        phy = "Bacillariophyta",
                        kin = "Chromista")
taxontable <- new_genus(ori = "Navicula cryptocephala var. cryptocephala",
                        fix = "Navicula cryptocephala",
                        spe = "Navicula cryptocephala",
                        gen = "Navicula",
                        fam = "Naviculaceae",
                        ord = "Naviculales",
                        cla = "Bacillariophyceae",
                        phy = "Bacillariophyta",
                        kin = "Chromista")
taxontable <- new_genus(ori = "Navicula enigmatica",
                        fix = "Germainiella enigmatica",
                        spe = "Germainiella enigmatica",
                        gen = "Germainiella",
                        fam = "Naviculaceae",
                        ord = "Naviculales",
                        cla = "Bacillariophyceae",
                        phy = "Bacillariophyta",
                        kin = "Chromista")
taxontable <- new_genus(ori = "Navicula kotschyi var. kotschyi",
                        fix = "Navicula kotschyi",
                        spe = "Navicula kotschyi",
                        gen = "Navicula",
                        fam = "Naviculaceae",
                        ord = "Naviculales",
                        cla = "Bacillariophyceae",
                        phy = "Bacillariophyta",
                        kin = "Chromista")
taxontable <- new_genus(ori = "Navicula radiosa var. radiosa" ,
                        fix = "Navicula radiosa",
                        spe = "Navicula radiosa",
                        gen = "Navicula",
                        fam = "Naviculaceae",
                        ord = "Naviculales",
                        cla = "Bacillariophyceae",
                        phy = "Bacillariophyta",
                        kin = "Chromista")
taxontable <- new_genus(ori = "Navicula reichardtiana var. reichardtiana" ,
                        fix = "Navicula reichardtiana-caterva",
                        spe = "Navicula reichardtiana-caterva",
                        gen = "Navicula",
                        fam = "Naviculaceae",
                        ord = "Naviculales",
                        cla = "Bacillariophyceae",
                        phy = "Bacillariophyta",
                        kin = "Chromista")
taxontable <- new_genus(ori = "Nitzschia calida var. calida",
                        fix = "Tryblionella calida",
                        spe = "Tryblionella calida",
                        gen = "Tryblionella",
                        fam = "Bacillariaceae",
                        ord = "Bacillariales",
                        cla = "Bacillariophyceae",
                        phy = "Bacillariophyta",
                        kin = "Chromista")
taxontable <- new_genus(ori = "Nitzschia capitellata var. capitellata",
                        fix = "Nitzschia palea complex",
                        spe = "Nitzschia palea complex",
                        gen = "Nitzschia",
                        fam = "Bacillariaceae",
                        ord = "Bacillariales",
                        cla = "Bacillariophyceae",
                        phy = "Bacillariophyta",
                        kin = "Chromista")
taxontable <- new_genus(ori = "Nitzschia filiformis var. filiformis",
                        fix = "Nitzschia filiformis",
                        spe = "Nitzschia filiformis",
                        gen = "Nitzschia",
                        fam = "Bacillariaceae",
                        ord = "Bacillariales",
                        cla = "Bacillariophyceae",
                        phy = "Bacillariophyta",
                        kin = "Chromista")
taxontable <- new_genus(ori = "Nitzschia levidensis var. levidensis",
                        fix = "Tryblionella",
                        spe = NA,
                        gen = "Tryblionella",
                        fam = "Bacillariaceae",
                        ord = "Bacillariales",
                        cla = "Bacillariophyceae",
                        phy = "Bacillariophyta",
                        kin = "Chromista")
taxontable <- new_genus(ori = "Nitzschia recta var. recta" ,
                        fix = "Nitzschia dissipata-recta Complex",
                        spe = "Nitzschia dissipata-recta Complex",
                        gen = "Nitzschia",
                        fam = "Bacillariaceae",
                        ord = "Bacillariales",
                        cla = "Bacillariophyceae",
                        phy = "Bacillariophyta",
                        kin = "Chromista")
taxontable <- new_genus(ori = "Pinnularia rupestris var. rupestris",
                        fix = "Pinnularia rupestris-sudetica",
                        spe = "Pinnularia rupestris-sudetica",
                        gen = "Pinnularia",
                        fam = "Pinnulariaceae",
                        ord = "Naviculales",
                        cla = "Bacillariophyceae",
                        phy = "Bacillariophyta",
                        kin = "Chromista")
taxontable <- new_genus(ori = "Pinnularia subrupestris var. subrupestris",
                        fix = "Pinnularia",
                        spe = NA,
                        gen = "Pinnularia",
                        fam = "Pinnulariaceae",
                        ord = "Naviculales",
                        cla = "Bacillariophyceae",
                        phy = "Bacillariophyta",
                        kin = "Chromista")
taxontable <- new_genus(ori = "Reimeria sinuata var. sinuata",
                        fix = "Reimeria sinuata",
                        spe = "Reimeria sinuata",
                        gen = "Reimeria",
                        fam = "Gomphonemataceae",
                        ord = "Cymbellales",
                        cla = "Bacillariophyceae",
                        phy = "Bacillariophyta",
                        kin = "Chromista")
taxontable <- new_genus(ori = "Tabellaria flocculosa var. flocculosa",
                        fix = "Tabellaria flocculosa Complex",
                        spe = "Tabellaria flocculosa Complex",
                        gen = "Tabellaria",
                        fam = "Tabellariaceae",
                        ord = "Rhabdonematales",
                        cla = "Bacillariophyceae",
                        phy = "Bacillariophyta",
                        kin = "Chromista")

View(taxontable)
taxontable <- new_entry("Achnanthes koenigii", "Achnanthes koenigii")
taxontable <- new_entry("Navicula enigmatica", "Germainiella enigmatica")
taxontable <- new_entry("Nitzschia lacunarum", "Nitzschia lacunarum")

saveRDS(taxontable, paste0("data/diatoms/",Sys.Date(),"_taxontable_diatoms.rds"))

## check that TU is empty now
TU <- setdiff(TU, taxontable$original_name)
  
# join 
data <- rename(data, original_name = taxon)
data2 <- taxontable[data, on = "original_name"]  

## check
sort(unique(data2$phylum))
sort(unique(data2$class))

## add site and date ids
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
data2[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_germany_saarland_diatoms")]

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
        data.set = "germany_saarland_diatoms"
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

## visual checks
sites <- unique(data5, by = "gr_sample_id") |> st_as_sf(coords = c("x.coord", "y.coord"), crs = data5$EPSG[1])

mapview(sites, zcol = "brt12")
mapview(sites, zcol = "ife")
mapview(sites, zcol = "bgr")
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
data7 <- data6[richness > 10]

# - drop sites far removed from ECRINS river network 
data8 <- data7[distance < 300]

# - visually check the assignment of sites 

rt <- 
  data8 |> 
  unique(by = "site_id") |> 
  st_as_sf(coords = c("x.coord", "y.coord"), 
           crs = data5$EPSG[1])

plot_typology <- st_crop(typologies, st_transform(sites, crs = st_crs(typologies)))

remove_list <- c()

options(warn = -1)

for (i in 1:nrow(rt)){
  i.rt <- rt[i, ]
  i.plot_typology <- st_crop(plot_typology, st_buffer(st_transform(i.rt, crs = st_crs(typologies)), dist =  2000))
  x <- mapview(i.plot_typology, zcol = "brt12") + mapview(i.rt, popup = c("water_body"), color = "red")
  print(x)
  i.bool <- "n"
  i.bool <- readline("ok?")
  if (i.bool == "n"){
    remove_list[length(remove_list) + 1] <- i.rt$site_id  
  }
  if (i.bool == "change"){
    i.towhat <- readline("change to:")
    data8[site_id == i.rt$site_id, brt12 := i.towhat]
  }
  rm(list = ls()[grepl("i\\.", ls())])
}

#- save the remove list. 
saveRDS(remove_list, paste0("data/diatoms/original_data/hungary_ecosurv/", Sys.Date(), "_remove_list.rds"))

#- drop remove sites 
data9 <- data8[!site_id %in% remove_list]


# temporal aggregation --------------------------------------------------------------
agg <- data9 |> unique(by = "gr_sample_id")
unique(table(agg$site_id))
saveRDS(data9, paste0("data/diatoms/original_data/hungary_ecosurv/",Sys.Date(),"_final_aggregated.rds"))
data9 <- readRDS("data/diatoms/original_data/hungary_ecosurv/2022-01-18_final_aggregated.rds")

# statistics -------------------------------------------------------------------------
# time span
summary(data9$year)
# all sites and samples
uniqueN(data5$site_id)
uniqueN(data5$gr_sample_id)
# least impacted sites and samples
uniqueN(data6$site_id)
uniqueN(data6$gr_sample_id)
# no sites with <5 taxa 
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
data9[, uniqueN(lowest.taxon), by = "gr_sample_id"] |> 
  pull(V1) |> 
  mean()



# temporal aggregation --------------------------------------------------------------
agg <- data5 |> unique(by = "gr_sample_id")
unique(table(agg$site_id))

# statistics -------------------------------------------------------------------------
# time span
summary(data5$year)
# number of sites
uniqueN(data5$site_id)
# number of samples
uniqueN(data5$gr_sample_id)
# most recent
data5[, uniqueN(gr_sample_id)]
# reference condition
data5[least.impacted == TRUE, uniqueN(site_id)]
data5[least.impacted == TRUE, uniqueN(gr_sample_id)]




## save to file
saveRDS(data5, paste0("data/diatoms/original_data/germany_saarland/",Sys.Date(),"_final_non_aggregated.rds"))

# temporal aggregation --------------------------------------------------------------
agg <- data5 |> unique(by = "gr_sample_id")
unique(table(agg$site_id))

source("R/functions/newest_sample.R")
data6 <- newest_sample(data5)
saveRDS(data6, paste0("data/diatoms/original_data/germany_saarland/",Sys.Date(),"_final_aggregated.rds"))

# statistics -------------------------------------------------------------------------
# time span
summary(data5$year)
# number of sites
uniqueN(data5$site_id)
# number of samples
uniqueN(data5$gr_sample_id)
# most recent
data6[, uniqueN(gr_sample_id)]
# reference condition
data6[least.impacted == TRUE, uniqueN(site_id)]
data6[least.impacted == TRUE, uniqueN(gr_sample_id)]
