# ———————————————————————————————————————————————— #
# ——— Clean diatom data from Austria —— WISA   ——— # 
# ———————————————————————————————————————————————— #

# ———————————————————————————————————
# date first written: 21-12-07
# date last modified: 22-01-19
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: In this script I create a harmonized spatial data set of diatoms from the 
# raw data provided by WISA, Austria.
# Temporal aggregation:  
# CRS:  
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


# functions -------------------------------------------------------------------------
source("R/functions/harmonize diatoms.R")

# load data -------------------------------------------------------------------------
## directory with all biological data 
bio_wd <- "data/diatoms/original_data/austria/raw/WISA_diatoms.xlsx"

bio <- read_excel(bio_wd, sheet = 3, skip = 2) |> setDT()
sit <- read_excel(bio_wd, sheet = 2, skip = 2) |> setDT()

dia1 <- readRDS("data/diatoms/fwb_table.rds")
dia2 <- readRDS("data/diatoms/omn_table.rds")

# prepare data ----------------------------------------------------------------------

## rename key variables as original names are not practical in R. 
names(bio)[c(1, 3, 6,11)] <- c("sample_id", "original_site_name", "date", "parameter")
## what parameters were measured? 
bio$parameter |> table()

## subset to phytobenthos samples. 
bio <- bio[parameter == "PHB Aufnahme Artenliste"]

## reshape bio data 
bio2 <- data.table(
        "original_site_name" = bio$original_site_name,
        sample_id            = bio$sample_id,
        "date"                 = ymd(bio$date),
        taxon                  = bio$'Taxaliste - Art (wiss. Name)'
)

## rename variables in sites
names(sit)[c(3,4,23,24)] <- c("original_site_name", "name","y.coord", "x.coord")
## subset sites to key variables 
sit2 <- select(sit, original_site_name, x.coord, y.coord,name) |> setDT()
## some sites have multiple entries in sit2. Remove duplicate as they break the join. 
sit2 <- unique(sit2, by = "original_site_name")

swapx <-  sit2[original_site_name == "FW31101227", x.coord]
swapy <-  sit2[original_site_name == "FW31101227", y.coord]
sit2[original_site_name == "FW31101227", c("x.coord", "y.coord") := .(swapy,swapx)]

sites1 <- sit2 |> filter(x.coord > 600000)
sites2 <- sit2 |> filter(x.coord < 600000 & x.coord > 350000)
sites3 <- sit2 |> filter(x.coord < 350000)

sites1 %<>% st_as_sf(coords = c("x.coord", "y.coord"), crs = 31259)
sites2 %<>% st_as_sf(coords = c("x.coord", "y.coord"), crs = 31258)
sites3 %<>% st_as_sf(coords = c("x.coord", "y.coord"), crs = 31257)

sites2 %<>% st_transform(crs = st_crs(sites1))
sites3 %<>% st_transform(crs = st_crs(sites1))

sit3 <- bind_rows(sites1, sites2, sites3)
coords <- st_coordinates(sit3)
sit3 <- st_drop_geometry(sit3) |> setDT()
sit3[, x.coord := coords[,1]]
sit3[, y.coord := coords[,2]]

## join biotic data with site data 
data <- sit3[bio2, on = "original_site_name"]

## remove sites without coordinates
data <- data[!is.na(x.coord)]
data <- data[!is.na(y.coord)]
data <- data[y.coord != 0.0]
data <- data[x.coord != 0.0]
data <- data[!is.na(taxon)]
data <- data[y.coord != sit3[original_site_name == "FW61404607", y.coord]]



## add variables 
data$EPSG <- 31259
data[,c("year", "season") := .(year(date), 
                               case_when(month(date) %in% c(12,1,2)  ~ "winter",
                                         month(date) %in% c(3,4,5)   ~ "spring",
                                         month(date) %in% c(6,7,8)   ~ "summer",
                                         month(date) %in% c(9,10,11) ~ "autumn"))]

all(pull(data[, uniqueN(x.coord), by = "original_site_name"], V1) == 1)
all(pull(data[, uniqueN(x.coord), by = "sample_id"], V1) == 1)

sites <- unique(data, by = "original_site_name")
sites2 <- st_as_sf(sites, coords = c("x.coord", "y.coord"), crs = sites$EPSG[1])
mapview(sites2)




## —— Inspect taxa ———————————————————————————————————————————————————————————————————————

taxontable <- readRDS("data/diatoms/2022-01-19_taxontable_diatoms.rds")

(TU <- unique(data$taxon) |> sort())
(TU <- setdiff(TU,taxontable$original_name))

## what is the most similar in in taxontable 
strdist_id  <- amatch(TU, taxontable$original_name, maxDist = 100)
strdist_tbl <- data.table(taxontable_name = taxontable$original_name[strdist_id], TU) |> 
        filter(!is.na(taxontable_name)) 

# manual additions to taxontable  
taxontable <- append_to_tt("Achnanthes biasolettiana var.thienemannii", "Achnanthes biasolettiana var. thienemannii")
taxontable <- append_to_tt("Achnanthes lanceolata", "Achnanthes lanceolata-Sippen")
taxontable <- append_to_tt("Achnanthes lanceolata var.frequentissima", "Achnanthes lanceolata ssp. frequentissima")
taxontable <- append_to_tt("Achnanthes lanceolata var.rostrata", "Achnanthes lanceolata ssp. rostrata")
taxontable <- append_to_tt("Achnanthidium minutissima var.affinis", "Achnanthes minutissima var. affinis")
taxontable <- append_to_tt("Achnanthes sp1", "Achnanthes sp.")
taxontable <- append_to_tt("Achnanthidium linearioide", "Achnanthidium linearioides")
taxontable <- append_to_tt("Achnanthidium minutissimum", "Achnanthidium minutissimum Gruppe")
taxontable <- append_to_tt("Achnanthidium", "Achnanthidium sp.")
taxontable <- append_to_tt("Adlafia minuscula var.muralis","Adlafia minuscula var. muralis")
taxontable <- append_to_tt("Adlafia","Adlafia sp.")
taxontable <- append_to_tt("Amphora copulata", "Amphora copulata Gruppe")
taxontable <- append_to_tt("Amphora normanii", "Amphora normannii")
taxontable <- append_to_tt("Amphora sp", "Amphora sp.")
taxontable <- append_to_tt("Aulacoseira sp1", "Aulacoseira sp.")
taxontable <- append_to_tt("Brachysira", "Brachysira sp.")
taxontable <- append_to_tt("Caloneis sp", "Caloneis sp.")
taxontable <- append_to_tt("Cymatopleura solea var.apiculata", "Cymatopleura solea var. apiculata")
taxontable <- append_to_tt("Cymatopleura", "Cymatopleura sp.")
taxontable <- append_to_tt("Cymbella sp1", "Cymbella sp.")
taxontable <- append_to_tt("Cymbopleura", "Cymbopleura sp.")
taxontable <- append_to_tt("Denticula", "Denticula sp.")
taxontable <- append_to_tt("Diadesmis", "Diadesmis sp.")
taxontable <- append_to_tt("Diatoma moniliformis var.ovalis",
                           "Diatoma moniliformis var. ovalis")
taxontable <- append_to_tt("Diatoma",
                           "Diatoma sp.")
taxontable <- append_to_tt("Encyonema",
                           "Encyonema sp.")
taxontable <- append_to_tt("Encyonema vulgare",
                           "Encyonema vulgare var. vulgare")
taxontable <- append_to_tt("Epithemia",
                           "Epithemia sp.")
taxontable <- append_to_tt("Eunotia sp1",
                           "Eunotia sp.")
taxontable <- append_to_tt("Fallacia pygmaea",
                           "Fallacia pygmaea ssp. pygmaea")
taxontable <- append_to_tt("Fragilaria arcus",
                           "Fragilaria acus")
taxontable <- append_to_tt("Fragilaria capucina var.capitellata",
                           "Fragilaria capucina capitellata-Sippen")
taxontable <- append_to_tt("Fragilaria capucina 70",
                           "Fragilaria capucina Gruppe")
taxontable <- append_to_tt("Fragilaria capucina var.perminuta",
                           "Fragilaria capucina perminuta-Sippen")
taxontable <- append_to_tt("Fragilaria capucina var.gracilis",
                           "Fragilaria capucina var. gracilis")
taxontable <- append_to_tt("Fragilaria capucina var.mesolepta",
                           "Fragilaria capucina var. mesolepta")
taxontable <- append_to_tt("Fragilaria capucina var.perminuta",
                           "Fragilaria capucina var. perminuta")
taxontable <- append_to_tt("Fragilaria capucina var rumpens",
                           "Fragilaria capucina var. rumpens")
taxontable <- append_to_tt("Fragilaria capucina var.vaucheriae",
                           "Fragilaria capucina var. vaucheriae")
taxontable <- append_to_tt("Fragilaria construens",
                           "Fragilaria construens Gruppe")
taxontable <- append_to_tt("Fragilaria leptostauron",
                           "Fragilaria leptostauron var. leptostauron")
taxontable <- append_to_tt("Fragilaria parasitica var.subconstricta",
                           "Fragilaria parasitica var. subconstricta")
taxontable <- append_to_tt("Fragilaria sp1",
                           "Fragilaria sp.")
taxontable <- append_to_tt("Fragilaria ulna",
                           "Fragilaria ulna Gruppe")
taxontable <- append_to_tt("Frustulia rhomboides var.amphipleuroides",
                           "Frustulia rhomboides var. amphipleuroides")
taxontable <- append_to_tt("Frustulia rhomboides var viridula",
                           "Frustulia rhomboides var. viridula")
taxontable <- append_to_tt("Frustulia",
                           "Frustulia sp.")
taxontable <- append_to_tt("Gomphonema parvulum",
                           "Gomphonema parvulum Gruppe")
taxontable <- append_to_tt("Gomphonema parvulum var.exilissimum",
                           "Gomphonema parvulum var. exilissimum")
taxontable <- append_to_tt("Gomphonema pumilum",
                           "Gomphonema pumilum Gruppe")
taxontable <- append_to_tt("Gomphonema sp1",
                           "Gomphonema sp.")
taxontable <- append_to_tt("Gomphosphenia",
                           "Gomphosphenia sp.")
taxontable <- append_to_tt("Gyrosigma sciotense",
                           "Gyrosigma sciotoense")
taxontable <- append_to_tt("Gyrosigma",
                           "Gyrosigma sp.")
taxontable <- append_to_tt("Hantzschia",
                           "Hantzschia sp.")
taxontable <- append_to_tt("Hippodonta",
                           "Hippodonta sp.")
taxontable <- append_to_tt("Hippodonta",
                           "Hippodonta sp.")
taxontable <- append_to_tt("Mayamaea atomus var.permitis",
                           "Mayamaea atomus var. permitis")
taxontable <- append_to_tt("Mayamaea",
                           "Mayamaea sp.")
taxontable <- append_to_tt("Meridion circulare var.constrictum",
                           "Meridion circulare var. constrictum")
taxontable <- append_to_tt("Meridion",
                           "Meridion sp.")
taxontable <- append_to_tt("Navicula capitata var.hungarica",
                           "Navicula capitata var. hungarica")
taxontable <- append_to_tt("Navicula gallica var.perpusilla",
                           "Navicula gallica var. perpusilla")
taxontable <- append_to_tt("Navicula ignota var.acceptata",
                           "Navicula ignota var. acceptata")
taxontable <- append_to_tt("Navicula longicephala var.vilaplan",
                           "Navicula longicephala var. villaplanii")
taxontable <- append_to_tt("Navicula menisculus var.grunowii",
                           "Navicula menisculus var. grunowii")
taxontable <- append_to_tt("Navicula mutica var.ventricosa",
                           "Navicula mutica var. ventricosa")
taxontable <- append_to_tt("Navicula pupula var.pseudopupula",
                           "Navicula pupula var. pseudopupula")
taxontable <- append_to_tt("Navicula salinarum var.minima",
                           "Navicula salinarum var. minima")
taxontable <- append_to_tt("Navicula sp1",
                           "Navicula sp.")
taxontable <- append_to_tt("Naviculadicta",
                           "Naviculadicta sp.")
taxontable <- append_to_tt("Neidium affine var.longiceps",
                           "Neidium affine var. longiceps")
taxontable <- append_to_tt("Neidium sp",
                           "Neidium sp.")
taxontable <- append_to_tt("Nitzschia dissipata var.media",
                           "Nitzschia dissipata var. media")
taxontable <- append_to_tt("Nitzschia frustulum var.inconspicua",
                           "Nitzschia frustulum var. inconspicua")
taxontable <- append_to_tt("Nitzschia linearis var.subtilis",
                           "Nitzschia linearis var. subtilis")
taxontable <- append_to_tt("Nitzschia palea var.debilis",
                           "Nitzschia palea var. debilis")
taxontable <- append_to_tt("Nitzschia sinuata var.delognei",
                           "Nitzschia sinuata var. delognei")
taxontable <- append_to_tt("Nitzschia sp1",
                           "Nitzschia sp.")
taxontable <- append_to_tt("Pinnularia sp1",
                           "Pinnularia sp.")
taxontable <- append_to_tt("Pinnularia subcapitata var.elongata",
                           "Pinnularia subcapitata var. elongata")
taxontable <- append_to_tt("Placoneis pseudanglica",
                           "Placoneis pseudanglica var. pseudanglica")
taxontable <- append_to_tt("Planothidium lanceolatum",
                           "Planothidium lanceolatum Sippen")
taxontable <- append_to_tt("Planothidium peragallii",
                           "Planothidium peragalli")
taxontable <- append_to_tt("Planothidium",
                           "Planothidium sp.")
taxontable <- append_to_tt("Platessa",
                           "Platessa sp.")
taxontable <- append_to_tt("Psammothidium",
                           "Psammothidium sp.")
taxontable <- append_to_tt("Rhopalodia",
                           "Rhopalodia sp.")
taxontable <- append_to_tt("Sellaphora pupula",
                           "Sellaphora pupula Gruppe")
taxontable <- append_to_tt("Sellaphora",
                           "Sellaphora sp.")
taxontable <- append_to_tt("Stauroneis kriegerii",
                           "Stauroneis kriegeri")
taxontable <- append_to_tt("Stauroneis",
                           "Stauroneis sp.")
taxontable <- append_to_tt("Surirella linearis var.helvetica",
                           "Surirella linearis var. helvetica")
taxontable <- append_to_tt("Surirella",
                           "Surirella sp.")
taxontable <- append_to_tt("Tabellaria",
                           "Tabellaria sp.")
taxontable <- append_to_tt("Tabularia",
                           "Tabularia sp.")


TU <- setdiff(sort(unique(data$taxon)),taxontable$original_name)

## check against fwb table 

for (i in seq_along(TU)){
        i.tu  <- TU[i]
        if(check_fwb(i.tu)){
                x <- get_fwb(i.tu)
                taxontable <- add_entry_tt(x)
        }
        rm(i.tu)
}; rm(i)

setorderv(taxontable, "original_name")

taxontable <- new_genus(ori = "Hannaea arcus",
          fix = "Synedra cyclopum/Hannaea arcus",
          spe = "Synedra cyclopum/Hannaea arcus",
          gen = "Synedra",
          fam = "Fragilariaceae",
          ord = "Fragilariales",
          cla = "Bacillariophyceae",
          phy = "Bacillariophyta",
          kin = "Chromista"
)

TU <- setdiff(sort(unique(data$taxon)),taxontable$original_name)

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
setorderv(taxontable, "original_name")

taxontable[species == "Luticola species", species := NA]

taxontable <- new_genus(ori = "Oestrupia bicontracta",
                        fix = "Oestrupia bicontracta",
                        spe = "Oestrupia bicontracta",
                        gen = "Oestrupia",
                        fam = "Pinnulariaceae",
                        ord = "Naviculales",
                        cla = "Bacillariophyceae",
                        phy = "Bacillariophyta",
                        kin = "Chromista"
)


TU <- setdiff(sort(unique(data$taxon)),taxontable$original_name)

taxontable <- new_genus(ori = "Achnanthes biasolettiana var. subatomus",
                        fix = "Achnanthidium subatomus",
                        spe = "Achnanthidium subatomus",
                        gen = "Achnanthidium",
                        fam = "Achnanthidiaceae",
                        ord = "Cocconeidales",
                        cla = "Bacillariophyceae",
                        phy = "Bacillariophyta",
                        kin = "Chromista"
)
taxontable <- new_genus(ori = "Achnanthes bioretii" ,
                        fix = "Psammothidium helveticum/chlidanos/daonense",
                        spe = "Psammothidium helveticum/chlidanos/daonense",
                        gen = "Psammothidium",
                        fam = "Achnanthidiaceae",
                        ord = "Cocconeidales",
                        cla = "Bacillariophyceae",
                        phy = "Bacillariophyta",
                        kin = "Chromista"
)
taxontable <- new_genus(ori = "Achnanthes lanceolata ssp. dubia" ,
                        fix = "Planothidium lanceolatum",
                        spe = "Planothidium lanceolatum",
                        gen = "Planothidium",
                        fam = "Achnanthidiaceae",
                        ord = "Cocconeidales",
                        cla = "Bacillariophyceae",
                        phy = "Bacillariophyta",
                        kin = "Chromista"
)
taxontable <- new_genus(ori = "Achnanthes lanceolata ssp. lanceolata" ,
                        fix = "Planothidium lanceolatum",
                        spe = "Planothidium lanceolatum",
                        gen = "Planothidium",
                        fam = "Achnanthidiaceae",
                        ord = "Cocconeidales",
                        cla = "Bacillariophyceae",
                        phy = "Bacillariophyta",
                        kin = "Chromista"
)
taxontable <- new_genus(ori = "Achnanthes minutissima var. gracillima",
                        fix = "Achnanthidium minutissimum",
                        spe = "Achnanthidium minutissimum",
                        gen = "Achnanthidium",
                        fam = "Achnanthidiaceae",
                        ord = "Cocconeidales",
                        cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista"
)
taxontable <- new_genus(ori = "Achnanthes minutissima var. scotica",
                        fix = "Achnanthidium caledonicum",
                        spe = "Achnanthidium caledonicum",
                        gen = "Achnanthidium",
                        fam = "Achnanthidiaceae",
                        ord = "Cocconeidales",
                        cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista"
)
taxontable <- new_genus(ori = "Adlafia minuscula var. minuscula",
                        fix = "Navicula krasskei-egregia-minuscula",
                        spe = "Navicula krasskei-egregia-minuscula",
                        gen = "Navicula",
                        fam = "Naviculaceae",
                        ord = "Naviculales",
                        cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista"
)
taxontable <- new_genus(ori = "Navicula minuscula var. muralis",
                        fix = "Navicula krasskei-egregia-minuscula",
                        spe = "Navicula krasskei-egregia-minuscula",
                        gen = "Navicula",
                        fam = "Naviculaceae",
                        ord = "Naviculales",
                        cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista"
)
taxontable <- new_genus(ori = "Caloneis amphisbaena f. amphisbaena",
                        fix = "Caloneis amphisbaena",
                        spe = "Caloneis amphisbaena",
                        gen = "Caloneis",
                        fam = "Naviculaceae",
                        ord = "Naviculales",
                        cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista"
)
taxontable <- new_genus(ori = "Cocconeis placentula Gruppe",
                        fix = "Cocconeis placentula",
                        spe = "Cocconeis placentula",
                        gen = "Cocconeis",
                        fam = "Cocconeidaceae",
                        ord = "Cocconeidales",
                        cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista"
)
taxontable <- new_genus(ori = "Cocconeis placentula var. pseudolineata",
                        fix = "Cocconeis placentula",
                        spe = "Cocconeis placentula",
                        gen = "Cocconeis",
                        fam = "Cocconeidaceae",
                        ord = "Cocconeidales",
                        cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista"
)
taxontable <- new_genus(ori = "Cymatopleura elliptica var. elliptica",
                        fix = "Cymatopleura elliptica",
                        spe = "Cymatopleura elliptica",
                        gen = "Cymatopleura",
                        fam = "Surirellaceae",
                        ord = "Surirellales",
                        cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista"
)
taxontable <- new_genus(ori = "Cymatopleura solea var. solea",
                        fix = "Cymatopleura solea",
                        spe = "Cymatopleura solea",
                        gen = "Cymatopleura",
                        fam = "Surirellaceae",
                        ord = "Surirellales",
                        cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista"
)
taxontable <- new_genus(ori = "Cymbella helvetica var. compacta",
                        fix = "Cymbella",
                        spe = NA,
                        gen = "Cymbella",
                        fam = "Cymbellaceae",
                        ord = "Cymbellales",
                        cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista"
)
taxontable <- new_genus(ori = "Cymbella lanceolata var. lanceolata",
                        fix = "Cymbella cistula group",
                        spe = "Cymbella cistula group",
                        gen = "Cymbella",
                        fam = "Cymbellaceae",
                        ord = "Cymbellales",
                        cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista"
)
taxontable <- new_genus(ori = "Diatoma moniliformis ssp. moniliformis",
                        fix = "Diatoma moniliformis/tenuis",
                        spe = "Diatoma moniliformis/tenuis",
                        gen = "Diatoma",
                        fam = "Tabellariaceae",
                        ord = "Rhabdonematales",
                        cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista"
)
taxontable <- new_genus(ori = "Fallacia pygmaea ssp. subpygmaea",
                        fix = "Fallacia pygmaea-forcipata",
                        spe = "Fallacia pygmaea-forcipata",
                        gen = "Fallacia",
                        fam = "Sellaphoraceae",
                        ord = "Naviculales",
                        cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista"
)
taxontable <- new_genus(ori = "Fragilaria capucina radians-Sippen",
                        fix = "Fragilaria capucina complex",
                        spe = "Fragilaria capucina complex",
                        gen = "Fragilaria",
                        fam = "Fragilariaceae",
                        ord = "Fragilariales",
                        cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista"
)
taxontable <- new_genus(ori = "Fragilaria capucina var. austriaca",
                        fix = "Fragilaria capucina complex",
                        spe = "Fragilaria capucina complex",
                        gen = "Fragilaria",
                        fam = "Fragilariaceae",
                        ord = "Fragilariales",
                        cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista"
)
taxontable <- new_genus(ori = "Fragilaria construens f. construens",
                        fix = "Fragilaria construens-pseudoconstruens",
                        spe = "Fragilaria construens-pseudoconstruens",
                        gen = "Fragilaria",
                        fam = "Fragilariaceae",
                        ord = "Fragilariales",
                        cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista"
)
taxontable <- new_genus(ori = "Fragilaria construens f. exigua",
                        fix = "Fragilaria construens-pseudoconstruens",
                        spe = "Fragilaria construens-pseudoconstruens",
                        gen = "Fragilaria",
                        fam = "Fragilariaceae",
                        ord = "Fragilariales",
                        cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista"
)
taxontable <- new_genus(ori = "Fragilaria construens f. subsalina",
                        fix = "Fragilaria construens-pseudoconstruens",
                        spe = "Fragilaria construens-pseudoconstruens",
                        gen = "Fragilaria",
                        fam = "Fragilariaceae",
                        ord = "Fragilariales",
                        cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista"
)
taxontable <- new_genus(ori = "Fragilaria construens f. venter",
                        fix = "Fragilaria construens-pseudoconstruens",
                        spe = "Fragilaria construens-pseudoconstruens",
                        gen = "Fragilaria",
                        fam = "Fragilariaceae",
                        ord = "Fragilariales",
                        cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista"
)
taxontable <- new_genus(ori = "Fragilaria leptostauron var. dubia",
                        fix = "Staurosira dubia",
                        spe = "Staurosira dubia",
                        gen = "Staurosira",
                        fam = "Staurosiraceae",
                        ord = "Fragilariales",
                        cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista"
)
taxontable <- new_genus(ori = "Fragilaria parasitica var. parasitica",
                        fix = "Pseudostaurosira parasitica complex",
                        spe = "Pseudostaurosira parasitica complex",
                        gen = "Pseudostaurosira",
                        fam = "Staurosiraceae",
                        ord = "Fragilariales",
                        cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista"
)
taxontable <- new_genus(ori = "Fragilaria ulna acus-Sippen",
                        fix = "Ulnaria ulna var. acus",
                        spe = "Ulnaria ulna var. acus",
                        gen = "Ulnaria",
                        fam = "Ulnariaceae",
                        ord = "Licmophorales",
                        cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista"
)
taxontable <- new_genus(ori = "Fragilaria ulna var. ulna",
                        fix = "Ulnaria ulna complex",
                        spe = "Ulnaria ulna complex",
                        gen = "Ulnaria",
                        fam = "Ulnariaceae",
                        ord = "Licmophorales",
                        cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista"
)
taxontable <- new_genus(ori = "Frustulia rhomboides var. crassinervia",
                        fix = "Frustulia rhomboides Complex",
                        spe = "Frustulia rhomboides Complex",
                        gen = "Frustulia",
                        fam = "Amphipleuraceae",
                        ord = "Naviculales",
                        cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista"
)
taxontable <- new_genus(ori = "Gomphoneis sp.",
                        fix = "Gomphoneis",
                        spe = NA,
                        gen = "Gomphoneis",
                        fam = "Gomphonemataceae",
                        ord = "Cymbellales",
                        cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista"
)
taxontable <- new_genus(ori = "Gomphoneis transsilvanica",
                        fix = "Gomphonella transylvanica",
                        spe = "Gomphonella transylvanica",
                        gen = "Gomphonella",
                        fam = "Cymbellales incertae sedis",
                        ord = "Cymbellales",
                        cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista"
)
taxontable <- new_genus(ori = "Gomphonema acuminatum var. acuminatum",
                        fix = "Gomphonema acuminatum Complex",
                        spe = "Gomphonema acuminatum Complex",
                        gen = "Gomphonema",
                        fam = "Gomphonemataceae",
                        ord = "Cymbellales",
                        cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista"
)
taxontable <- new_genus(ori = "Gomphonema olivaceum var. olivaceoides",
                        fix = "Gomphonema olivaceum/olivaceoides",
                        spe = "Gomphonema olivaceum/olivaceoides",
                        gen = "Gomphonema",
                        fam = "Gomphonemataceae",
                        ord = "Cymbellales",
                        cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista"
)
taxontable <- new_genus(ori = "Gomphonema olivaceum var. olivaceolacuum",
                        fix = "Gomphonema olivaceum/olivaceoides",
                        spe = "Gomphonema olivaceum/olivaceoides",
                        gen = "Gomphonema",
                        fam = "Gomphonemataceae",
                        ord = "Cymbellales",
                        cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista"
)
taxontable <- new_genus(ori = "Gomphonema olivaceum var. olivaceum",
                        fix = "Gomphonema olivaceum/olivaceoides",
                        spe = "Gomphonema olivaceum/olivaceoides",
                        gen = "Gomphonema",
                        fam = "Gomphonemataceae",
                        ord = "Cymbellales",
                        cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista"
)
taxontable <- new_genus(ori = "Gomphonema parvulum var. parvulius",
                        fix = "Gomphonema parvulum Complex",
                        spe = "Gomphonema parvulum Complex",
                        gen = "Gomphonema",
                        fam = "Gomphonemataceae",
                        ord = "Cymbellales",
                        cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista"
)
taxontable <- new_genus(ori = "Gomphonema parvulum var. parvulum f. parvulum",
                        fix = "Gomphonema parvulum Complex",
                        spe = "Gomphonema parvulum Complex",
                        gen = "Gomphonema",
                        fam = "Gomphonemataceae",
                        ord = "Cymbellales",
                        cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista"
)
taxontable <- new_genus(ori = "Gomphonema parvulum var. parvulum f. saprophilum",
                        fix = "Gomphonema parvulum Complex",
                        spe = "Gomphonema parvulum Complex",
                        gen = "Gomphonema",
                        fam = "Gomphonemataceae",
                        ord = "Cymbellales",
                        cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista"
)
taxontable <- new_genus(ori = "Gomphonema pumilum var. pumilum",
                        fix = "Gomphonema pumilum complex",
                        spe = "Gomphonema pumilum complex",
                        gen = "Gomphonema",
                        fam = "Gomphonemataceae",
                        ord = "Cymbellales",
                        cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista"
)
taxontable <- new_genus(ori = "Karayevia clevei var. clevei",
                        fix = "Karayevia clevei",
                        spe = "Karayevia clevei",
                        gen = "Karayevia",
                        fam = "Achnanthidiaceae",
                        ord = "Cocconeidales",
                        cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista"
)
taxontable <- new_genus(ori = "Karayevia sp.",
                        fix = "Karayevia",
                        spe = NA,
                        gen = "Karayevia",
                        fam = "Achnanthidiaceae",
                        ord = "Cocconeidales",
                        cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista"
)
taxontable <- new_genus(ori = "Mayamaea fossalis var. fossalis",
                        fix = "Mayamaea fossalis",
                        spe = "Mayamaea fossalis",
                        gen = "Mayamaea",
                        fam = "Naviculales incertae sedis",
                        ord = "Naviculales",
                        cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista"
)
taxontable <- new_genus(ori = "Navicula atomus var. permitis",
                        fix = "Mayamaea",
                        spe = NA,
                        gen = "Mayamaea",
                        fam = "Naviculales incertae sedis",
                        ord = "Naviculales",
                        cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista"
)
taxontable <- new_genus(ori = "Navicula capitata var. lueneburgensis",
                        fix = "Hippodonta capitata",
                        spe = "Hippodonta capitata",
                        gen = "Hippodonta",
                        fam = "Naviculaceae",
                        ord = "Naviculales",
                        cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista"
)
taxontable <- new_genus(ori = "Navicula confervaceae",
                        fix = "Diadesmis confervacea",
                        spe = "Diadesmis confervacea",
                        gen = "Diadesmis",
                        fam = "Diadesmidaceae",
                        ord = "Naviculales",
                        cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista"
)
taxontable <- new_genus(ori = "Navicula menisculus var. upsaliensis",
                        fix = "Navicula menisculus/antonii",
                        spe = "Navicula menisculus/antonii",
                        gen = "Navicula",
                        fam = "Naviculaceae",
                        ord = "Naviculales",
                        cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista"
)
taxontable <- new_genus(ori = "Navicula pupula var. mutata",
                        fix = "Sellaphora mutatoides",
                        spe = "Sellaphora mutatoides",
                        gen = "Sellaphora",
                        fam = "Sellaphoraceae",
                        ord = "Naviculales",
                        cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista"
)
taxontable <- new_genus(ori = "Navicula salinarum var. salinarum",
                        fix = "Navicula trivialis-salinarum",
                        spe = "Navicula trivialis-salinarum",
                        gen = "Navicula",
                        fam = "Naviculaceae",
                        ord = "Naviculales",
                        cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista"
)
taxontable <- new_genus(ori = "Navicula viridula var. germainii",
                        fix = "Navicula viridula complex",
                        spe = "Navicula viridula complex",
                        gen = "Navicula",
                        fam = "Naviculaceae",
                        ord = "Naviculales",
                        cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista"
)
taxontable <- new_genus(ori = "Navicula viridula var. rostellata",
                        fix = "Navicula viridula complex",
                        spe = "Navicula viridula complex",
                        gen = "Navicula",
                        fam = "Naviculaceae",
                        ord = "Naviculales",
                        cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista"
)
taxontable <- new_genus(ori = "Nitzschia acicularis var. acicularis",
                        fix = "Nitzschia acicularis Complex",
                        spe = "Nitzschia acicularis Complex",
                        gen = "Nitzschia",
                        fam = "Bacillariaceae",
                        ord = "Bacillariales",
                        cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista"
)
taxontable <- new_genus(ori = "Nitzschia dissipata ssp. dissipata",
                        fix = "Nitzschia dissipata-recta Complex",
                        spe = "Nitzschia dissipata-recta Complex",
                        gen = "Nitzschia",
                        fam = "Bacillariaceae",
                        ord = "Bacillariales",
                        cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista"
)
taxontable <- new_genus(ori = "Nitzschia palea-Sippen",
                        fix = "Nitzschia palea complex",
                        spe = "Nitzschia palea complex",
                        gen = "Nitzschia",
                        fam = "Bacillariaceae",
                        ord = "Bacillariales",
                        cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista"
)
taxontable <- new_genus(ori = "Nitzschia palea - Sippen",
                        fix = "Nitzschia palea complex",
                        spe = "Nitzschia palea complex",
                        gen = "Nitzschia",
                        fam = "Bacillariaceae",
                        ord = "Bacillariales",
                        cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista"
)
taxontable <- new_genus(ori = "Nitzschia palea tenuirostris-Sippen",
                        fix = "Nitzschia palea complex",
                        spe = "Nitzschia palea complex",
                        gen = "Nitzschia",
                        fam = "Bacillariaceae",
                        ord = "Bacillariales",
                        cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista"
)
taxontable <- new_genus(ori = "Nitzschia palea var. tenuirostris sensu",
                        fix = "Nitzschia palea complex",
                        spe = "Nitzschia palea complex",
                        gen = "Nitzschia",
                        fam = "Bacillariaceae",
                        ord = "Bacillariales",
                        cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista"
)
taxontable <- new_genus(ori = "Pinnularia subcapitata var. hilseana",
                        fix = "Pinnularia subcapitata Complex",
                        spe = "Pinnularia subcapitata Complex",
                        gen = "Pinnularia",
                        fam = "Pinnulariaceae",
                        ord = "Naviculales",
                        cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista"
)
taxontable <- new_genus(ori = "Placoneis pseudanglica var. signata",
                        fix = "Placoneis",
                        spe = NA,
                        gen = "Placoneis",
                        fam = "Gomphonemataceae",
                        ord = "Cymbellales",
                        cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista"
)
taxontable <- new_genus(ori = "Planothidium frequentissimum var. frequentissimum",
                        fix = "Planothidium lanceolatum",
                        spe = "Planothidium lanceolatum",
                        gen = "Planothidium",
                        fam = "Achnanthidiaceae",
                        ord = "Cocconeidales",
                        cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista"
)
taxontable <- new_genus(ori = "Sellaphora pupula var. pupula",
                        fix = "Sellaphora pupula Complex",
                        spe = "Sellaphora pupula Complex",
                        gen = "Sellaphora",
                        fam = "Sellaphoraceae",
                        ord = "Naviculales",
                        cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista"
)

TU <- setdiff(sort(unique(data$taxon)),taxontable$original_name)
non_diatom_algae <- TU

taxontable[original_name == "Gomphonema olivaceolacuum", c("family", "order") := .("Gomphonemataceae","Cymbellales")]

saveRDS(taxontable, paste0("data/diatoms/",Sys.Date(),"_taxontable_diatoms.rds"))
saveRDS(non_diatom_algae, paste0("data/diatoms/",Sys.Date(),"_non_diatom_algae.rds"))



data <- data[!taxon %in% TU]

names(data)[which(names(data) == "taxon")] <- "original_name"
data2 <- taxontable[data, on = "original_name"]

sort(unique(data2$kingdom))
sort(unique(data2$phylum))
sort(unique(data2$class))
sort(unique(data2$order))

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
data2[,gr_sample_id := paste0(sample_id,"_austria_wisa_diatoms")]

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
        data.set = "austria_wisa_diatoms"
)]

## combine entries of same taxon
data3[, lowest.taxon := ifelse(!is.na(species), species,
                               ifelse(!is.na(genus), genus,
                                      ifelse(!is.na(family), family,
                                             ifelse(!is.na(order), order,
                                                    ifelse(!is.na(class), class,
                                                           ifelse(!is.na(phylum), phylum, kingdom))))))]

#data3[, abundance := sum(abundance), by = c("gr_sample_id", "lowest.taxon")]
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
        i.bool <- readline(paste(i, ":"))
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
saveRDS(remove_list, paste0("data/diatoms/original_data/austria/", Sys.Date(), "_remove_list.rds"))
remove_list <- readRDS("data/diatoms/original_data/austria/2022-01-19_final_aggregated.rds")

#- drop remove sites 
data9 <- data8[!site_id %in% remove_list]


# temporal aggregation --------------------------------------------------------------
agg <- data9 |> unique(by = "gr_sample_id")
unique(table(agg$site_id))
# yes 
source("R/functions/newest_sample.R")
data10 <- newest_sample(data9)

saveRDS(data10, paste0("data/diatoms/original_data/austria/",Sys.Date(),"_final_aggregated.rds"))
data10 <- readRDS("data/diatoms/original_data/austria/2022-01-18_final_aggregated.rds")

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
data10[, richness := uniqueN(lowest.taxon), by = "gr_sample_id"]
unique(data10, by = "gr_sample_id") |> pull(richness) |> mean()

library(ggplot2)

ggplot(data10, aes(x = brt12, y = richness)) + geom_point() + stat_summary(fun = mean, geom = "point", size = 4)

samples <- data10 |> unique(by = "gr_sample_id") 


data9[site_id == "00122"] |> View()

