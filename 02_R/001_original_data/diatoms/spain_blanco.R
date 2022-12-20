# ——————————————————————————————————————————————— #
# ——— Clean Diatom data from Spain Saul Blanco——— # 
# ——————————————————————————————————————————————— #

# ———————————————————————————————————
# date first written: 21-12-17
# date last modified: 21-12-17
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: In this script I create a harmonized spatial data set of diatom from the 
# raw data from Spain from Saul Blanco  
# CRS: Popular Visualisation CRS (DEPRECATED)  —— EPSG: 4055
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
bio_wd <- "data/diatoms/original_data/spain_blanco/raw/"
file.names <- dir_ls(bio_wd)  %>% .[str_detect(.,pattern = ".csv$")]
data <- lst()

for (i in 1:length(file.names)) data[[i]] <- fread(file.names[i])

dia1 <- readRDS("data/diatoms/fwb_table.rds")
dia2 <- readRDS("data/diatoms/omn_table.rds")
non_diatom_algae <- readRDS("data/diatoms/2021-12-08_non_diatom_algae.rds")

# prepare data ----------------------------------------------------------------------
Spe_vec <- c(15,15,21,21,18)
data2 <- data

for (i in seq_along(data2)){
        data2[[i]] %<>% pivot_longer(cols = (Spe_vec[i]+1):ncol(data2[[i]]), names_to = "taxon", values_to = "abundance", values_transform = list(abundance = as.integer))        
        setDT(data2[[i]])
        data2[[i]][,DATE := dmy(str_remove(DATE, "^00"))]
       
}

data3 <- rbindlist(data2, fill = TRUE)

names(data[[3]])
names(data[[1]])

for (i in 1:5) {
        assign(
                x     = paste0("dia_saul_clean_prebind_", i),
                value = get(paste0("dia_saul_clean_", i))[c("CODE", "Rel_Abundance", "LAT", "LON", "DATE", "SPI")] %>%
                        
                        filter(Rel_Abundance != 0)
        )
}

data4 <- data.table(
        original_site_name = data3$CODE,
        date               = data3$DATE,
        code               = data3$taxon,
        abundance          = data3$abundance,
        x.coord            = data3$LON,
        y.coord            = data3$LAT,
        EPSG               = 4055,
        data.set           = "spain_saul_blanco_diatoms"
)

data4 <- data4[abundance != 0]

setDT(dia2)

data5 <- dia2[data4, on = "code"]
data5[, c("code", "new") := NULL]

data5[, c("year", "season") := 
                     list(
                             year(date),
                             ifelse(month(date) %in% c(12,1,2), "winter", 
                                    ifelse(
                                            month(date) %in% c(3,4,5), "spring",
                                            ifelse(
                                                    month(date) %in% c(6,7,8), "summer", "autumn"
                                            )
                                    )
                             ) 
                     )]

data5 <- data5[!is.na(x.coord)]
sites <- unique(data5, by = "original_site_name")
sites <- st_as_sf(sites, coords = c("x.coord", "y.coord"), crs = sites$EPSG[1])
mapview(sites)

data <- data5

# taxonomic harmonization -----------------------------------------------------------
taxontable <- readRDS("data/diatoms/2022-01-17_taxontable_diatoms.rds")

TU <- unique(data$taxon) |> sort()
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

TU <- unique(data$taxon) |> sort()
(TU <- setdiff(TU, taxontable$original_name))

strdist_id  <- amatch(TU, taxontable$original_name, maxDist = 1000)
strdist_tbl <- data.table(taxontable_name = taxontable$original_name[strdist_id], TU) |> 
        filter(!is.na(taxontable_name))

for (i in 1:nrow(strdist_tbl)){
        print(strdist_tbl[i,1])
        print(strdist_tbl[i,2])
        bool <- readline()
        if (bool == "y"){
                taxontable <- append_to_tt(strdist_tbl[i,1], strdist_tbl[i,2])
        }
}


TU <- unique(data$taxon) |> sort()
(TU <- setdiff(TU, taxontable$original_name))

taxontable[species == "Achnanthidium affine" , c("fixed_name", "species") := .("Achnanthes minutissima", "Achnanthes minutissima")]
taxontable[species == "Achnanthidium exiguum", c("fixed_name", "species") := .("Achnanthidium exigua/ziegleri/subexigua", "Achnanthidium exigua/ziegleri/subexigua")]
taxontable[species == "Achnanthidium gracillimum", c("fixed_name", "species") := .("Achnanthidium minutissimum", "Achnanthidium minutissimum")]
taxontable[species == "Achnanthidium subatomoides", c("fixed_name", "species") := .("Psammothidium subatomoides", "Psammothidium subatomoides")]
taxontable[species == "Adlafia minuscula", c("fixed_name", "species") := .("Navicula krasskei-egregia-minuscula", "Navicula krasskei-egregia-minuscula")]
taxontable[species %in% c("Aulacoseira ambigua var.japonica", "Aulacoseira ambigua var.curvata"), c("fixed_name", "species") := .("Aulacoseira ambigua", "Aulacoseira ambigua")]
taxontable[species == "Craticula accomoda", c("fixed_name", "species") := .("Craticula", NA)]
taxontable[genus == "Sellaphora", family := "Sellaphoraceae"]
taxontable[species == "Encyonema silesiacum", c("fixed_name", "species") := .("Encyonema silesicacum/minutum/lange-bertalotii", "Encyonema silesicacum/minutum/lange-bertalotii")]
taxontable[species == "Fallacia helensis", c("fixed_name", "species") := .("Fallacia subhamulata/helensis", "Fallacia subhamulata/helensis")]
taxontable[species %in% c("Geissleria acceptata", "Geissleria ignota"), c("fixed_name", "species") := .("Geissleria ignota complex", "Geissleria ignota complex")]
taxontable[species == "Naviculadicta absoluta", c("fixed_name", "species") := .("Navicula ventralis-medioconvexa++")]
taxontable[species %in% c("Placoneis", "Placoneis elginensis"), c("fixed_name", "species") := .("Placoneis", NA)]
taxontable[fixed_name == "Planothidium rostratum", c("fixed_name", "species"):=.("Planothidium lanceolatum", "Planothidium lanceolatum")]
taxontable[fixed_name == "Platessa conspicua", c("fixed_name", "species", "genus", "family") := .("Planothidium", NA, "Planothidium", "Achnanthidiaceae")]
taxontable[fixed_name == "Psammothidium rechtensis", c("fixed_name", "species") := .("Psammothidium rossii/altaica", "Psammothidium rossii/altaica")]
taxontable[fixed_name == "Sellaphora joubaudii", c("fixed_name", "species", "genus", "family") := .("Eolimna minima-seminulum-atomoides", "Eolimna minima-seminulum-atomoides", "Eolimna", "Naviculaceae")]
taxontable[fixed_name == "Stauroforma exiguiformis", c("fixed_name", "species", "genus") := .("Fragilaria virescens complex", "Fragilaria virescens complex", "Fragilaria")]
taxontable[fixed_name == "Staurosira martyi", c("fixed_name", "species", "genus", "family") := .("Staurosirella leptostauron complex", "Staurosirella leptostauron complex", "Staurosirella", "Fragilariaceae")]
taxontable[fixed_name %in% c("Stephanodiscus hantzschii var.parva", "Stephanodiscus hantzschii var.tenuis"), c("fixed_name", "species") := .("Stephanodiscus hantzschii", "Stephanodiscus hantzschii")]

taxontable <- new_entry(ori = "Achnanthes coarctata (Brebisson) Grunow in Cl. & Grun.", fix = "Achnanthes coarctata" , spe = "Achnanthes coarctata", gen = "Achnanthes")
taxontable <- new_entry(ori = "Achnanthes inflata (Kützing) Grunow", fix = "Achnanthes inflata", spe = "Achnanthes inflata", gen = "Achnanthes")
taxontable <- new_entry(ori = "Achnanthes obliqua (Gregory) Hustedt", fix = "Achnanthes obliqua", spe = "Achnanthes obliqua", gen = "Achnanthes")
taxontable <- new_entry(ori = "Achnanthidium affine (Grun) Czarnecki", fix = "Achnanthidium minutissimum", spe = "Achnanthidium minutissimum", gen = "Achnanthidium")
taxontable <- new_entry(ori = "Achnanthidium atomus (Hustedt) Monnier, Lange-Bertalot & Ector", fix = "Achnanthidium atomus", spe = "Achnanthidium atomus", gen = "Achnanthidium")
taxontable <- new_entry(ori = "Achnanthidium caledonicum(Lange-Bertalot)Lange-Bertalot", fix = "Achnanthidium caledonicum", spe = "Achnanthidium caledonicum", gen = "Achnanthidium")
taxontable <- new_entry(ori = "Achnanthidium catenatum (Bily & Marvan) Lange-Bertalot", fix = "Achnanthidium minutissimum", spe = "Achnanthidium minutissimum", gen = "Achnanthidium")
taxontable <- new_entry(ori = "Achnanthidium daonense (Lange-Bertalot) Lange-Bertalot Monnier & Ector", fix = "Psammothidium helveticum/chlidanos/daonense", spe = "Psammothidium helveticum/chlidanos/daonense", gen = "Psammothidium")
taxontable <- new_entry(ori = "Achnanthidium eutrophilum (Lange-Bertalot)Lange-Bertalot", fix = "Achnanthidium eutrophilum", spe = "Achnanthidium eutrophilum", gen = "Achnanthidium")
taxontable <- new_entry(ori = "Achnanthidium exiguum (Grunow) Czarnecki", fix = "Achnanthidium exigua/ziegleri/subexigua", spe = "Achnanthidium exigua/ziegleri/subexigua", gen = "Achnanthidium")
taxontable <- new_entry(ori = "Achnanthidium exilis (Kütz.)Round & Bukhtiyarova", fix = "Achnanthidium exilis", spe = "Achnanthidium exilis", gen = "Achnanthidium")
taxontable <- new_entry(ori = "Achnanthidium gracillimum (Meister)Lange-Bertalot", fix = "Achnanthidium minutissimum", spe = "Achnanthidium minutissimum", gen = "Achnanthidium")
taxontable <- new_entry(ori = "Achnanthidium helveticum (Hustedt) Monnier Lange-Bertalot & Ector", fix = "Psammothidium helveticum/chlidanos/daonense", spe = "Psammothidium helveticum/chlidanos/daonense", gen = "Psammothidium")
taxontable <- new_entry(ori = "Achnanthidium kranzii (Lange-Bertalot) Round & Bukhtiyarova", fix = "Achnanthidium kranzii", spe = "Achnanthidium kranzii", gen = "Achnanthidium")
taxontable <- new_entry(ori = "Achnanthidium lauenburgianum (Hustedt) Monnier Lange-Bertalot & Ector", fix = "Achnanthidium lauenburgianum", spe = "Achnanthidium lauenburgianum", gen = "Achnanthidium")
taxontable <- new_entry(ori = "Achnanthidium macrocephalum(Hust.)Round & Bukhtiyarova", fix = "Achnanthidium macrocephalum", spe = "Achnanthidium macrocephalum", gen = "Achnanthidium")
taxontable <- new_entry(ori = "Achnanthidium minutissimum (Kützing) Czarnecki var. inconspicua Oestrup", fix = "Achnanthidium minutissimum", spe = "Achnanthidium minutissimum", gen = "Achnanthidium")
taxontable <- new_entry(ori = "Achnanthidium pyrenaicum (Hustedt) Kobayasi", fix = "Achnanthidium minutissimum", spe = "Achnanthidium minutissimum", gen = "Achnanthidium")
taxontable <- new_entry(ori = "Achnanthidium pyrenaicum (Hustedt) Kobayasi abnormal form", fix = "Achnanthidium minutissimum", spe = "Achnanthidium minutissimum", gen = "Achnanthidium")
taxontable <- new_entry(ori = "Achnanthidium saprophilum (Kobayasi et Mayama) Round & Bukhtiyarova", fix = "Achnanthidium saprophilum", spe = "Achnanthidium saprophilum", gen = "Achnanthidium")
taxontable <- new_entry(ori = "Achnanthidium straubianum (Lange-Bertalot)Lange-Bertalot", fix = "Achnanthidium straubianum", spe = "Achnanthidium straubianum", gen = "Achnanthidium")
taxontable <- new_entry(ori = "Achnanthidium subatomoides (Hustedt) Monnier, Lange-Bertalot et Ector", fix = "Psammothidium subatomoides", spe = "Psammothidium subatomoides", gen = "Psammothidium")
taxontable <- new_entry(ori = "Achnanthidium subatomus (Hustedt) Lange-Bertalot", fix = "Achnanthidium subatomus", spe = "Achnanthidium subatomus", gen = "Achnanthidium")
taxontable <- new_entry(ori = "Adlafia aquaeductae (Krasske) Moser Lange-Bertalot & Metzeltin", fix = "Sellaphora pupula Complex", spe = "Sellaphora pupula Complex", gen = "Sellaphora")
taxontable <- new_entry(ori = "Adlafia minuscula (Grunow) Lange-Bertalot", fix = "Navicula krasskei-egregia-minuscula", spe = "Navicula krasskei-egregia-minuscula", gen = "Navicula")
taxontable <- new_entry(ori = "Adlafia minuscula var. muralis (Grunow) Lange-Bertalot", fix = "Navicula krasskei-egregia-minuscula", spe = "Navicula krasskei-egregia-minuscula", gen = "Navicula")
taxontable <- new_entry(ori = "Adlafia muscora (Kociolek & Reviers) Moser Lange-Bertalot & Metzeltin", fix = "Adlafia muscora", spe = "Adlafia muscora", gen = "Adlafia")
taxontable <- new_entry(ori = "Amphora ovalis (Kützing) Kützing abnormal form", fix = "Amphora ovalis", spe = "Amphora ovalis", gen = "Amphora")
taxontable <- new_entry(ori = "Amphora ovalis (Kützing) Kützing var.ovalis", fix = "Amphora ovalis", spe = "Amphora ovalis", gen = "Amphora")
taxontable <- new_entry(ori = "Aulacoseira ambigua (Grunow) Simonsen", fix = "Aulacoseira ambigua", spe = "Aulacoseira ambigua", gen = "Aulacoseira")
taxontable <- new_entry(ori = "Aulacoseira distans (Ehr.)Simonsen", fix = "Aulacoseira distans complex", spe = "Aulacoseira distans complex", gen = "Aulacoseira")
taxontable <- new_entry(ori = "Aulacoseira granulata (Ehr.) Simonsen", fix = "Aulacoseira granulata", spe = "Aulacoseira granulata", gen = "Aulacoseira")
taxontable <- new_entry(ori = "Aulacoseira muzzanensis (Meister) Krammer", fix = "Aulacoseira muzzanensis", spe = "Aulacoseira muzzanensis", gen = "Aulacoseira")
taxontable <- new_entry(ori = "Aulacoseira nivalis(Wm.Sm.)English & Potapova", fix = "Aulacoseira nivalis", spe = "Aulacoseira nivalis", gen = "Aulacoseira")
taxontable <- new_entry(ori = "Aulacoseira pusilla (Meister) Tuji et Houki", fix = "Aulacoseira subarctica complex", spe = "Aulacoseira subarctica complex", gen = "Aulacoseira")
taxontable <- new_entry(ori = "Aulacoseira subarctica (O.Muller) Haworth", fix = "Aulacoseira subarctica complex", spe = "Aulacoseira subarctica complex", gen = "Aulacoseira")
taxontable <- new_entry(ori = "Brachysira microcephala (Grunow) Compère", fix = "Brachysira microcephala", spe = "Brachysira microcephala", gen = "Brachysira")
taxontable <- new_entry(ori = "Caloneis lepidula (Grunow) Cleve", fix = "Caloneis lepidula", spe = "Caloneis lepidula", gen = "Caloneis")
taxontable <- new_entry(ori = "Caloneis molaris (Grunow) Krammer", fix = "Caloneis", spe = NA, gen = "Caloneis")
taxontable <- new_entry(ori = "Cavinula cocconeiformis (Gregory ex Greville) Mann & Stickle", fix = "Cavinula scutelloides/pseudoscutiformis", spe = "Cavinula scutelloides/pseudoscutiformis", gen = "Cavinula")
taxontable <- new_entry(ori = "Cavinula intractata (Hust.) Lange-Bertalot", fix = "Cavinula", spe = NA, gen = "Cavinula")
taxontable <- new_entry(ori = "Cavinula mollicula (Hust.) Lange-Bertalot", fix = "Cavinula", spe = NA, gen = "Cavinula")
taxontable <- new_entry(ori = "Cavinula pseudoscutiformis (Hustedt) Mann & Stickle" , fix = "Cavinula scutelloides/pseudoscutiformis", spe = "Cavinula scutelloides/pseudoscutiformis", gen = "Cavinula")
taxontable <- new_entry(ori = "Chamaepinnularia soehrensis (Krass.)Lange-Bertalot & Krammer", fix = "Chamaepinnularia soehrensis Complex", spe = "Chamaepinnularia soehrensis Complex", gen = "Chamaepinnularia")
taxontable <- new_entry(ori = "Cocconeis disculus (Schumann) Cleve in Cleve & Jentzsch", fix = "Cocconeis", spe = NA, gen = "Cocconeis")
taxontable <- new_entry(ori = "Cocconeis euglyptoides (Geitler) Lange-Bertalot", fix = "Cocconeis euglyptoides", spe = "Cocconeis euglyptoides", gen = "Cocconeis")
taxontable <- new_entry(ori = "Cocconeis pseudolineata (Geitler) Lange-Bertalot", fix = "Cocconeis pseudolineata", spe = "Cocconeis pseudolineata", gen = "Cocconeis")
taxontable <- new_entry(ori = "Craticula accomoda (Hustedt) Mann", fix = "Craticula", spe = NA, gen = "Craticula")
taxontable <- new_entry(ori = "Craticula ambigua (Ehrenberg) Mann", fix = "Craticula ", spe = NA, gen = "Craticula")
taxontable <- new_entry(ori = "Craticula buderi (Hustedt) Lange-Bertalot", fix = "Craticula", spe = NA, gen = "Craticula")
taxontable <- new_entry(ori = "Craticula citrus (Krasske) Reichardt" , fix = "Craticula citrus", spe = "Craticula citrus", gen = "Craticula")
taxontable <- new_entry("Craticula cuspidata (Kützing) Mann", fix = "Craticula", spe = NA, gen ="Craticula" )
taxontable <- new_entry("Craticula halophilioides (Hustedt) Lange-Bertalot", fix = "Craticula molestiformis-halophiloides++", spe = "Craticula molestiformis-halophiloides++", gen = "Craticula" )
taxontable <- new_entry("Craticula minusculoides (Hustedt) Lange-Bertalot", fix = "Craticula molestiformis-halophiloides++", spe = "Craticula molestiformis-halophiloides++", gen = "Craticula" )
taxontable <- new_entry("Ctenophora pulchella (Ralfs ex Kütz.) Williams et Round", fix = "Ctenophora pulchella", spe = "Ctenophora pulchella", gen = "Ctenophora")
taxontable <- new_entry("Cyclostephanos invisitatus(Hohn & Hellerman)Theriot Stoermer & Hakansson", fix = "Cyclostephanos", spe = NA, gen = "Cyclostephanos" )
taxontable <- new_entry("Cyclotella striata(Kützing)Grunow 1880 in Cleve & Grunow", fix = "Cyclotella striata", spe = "Cyclotella striata", gen = "Cyclotella")
taxontable <- new_entry("Cymatopleura solea (Brebisson in Breb. & Godey) W.Smith abnormal form", fix = "Cymatopleura solea", spe = "Cymatopleura solea", gen = "Cymatopleura")
taxontable <- new_entry("Cymbella aspera(Ehrenberg) H.Peragallo", fix = "Cymbella cistula group", spe = "Cymbella cistula group", gen = "Cymbella")
taxontable <- new_entry("Cymbella leptoceros (Ehrenberg) Kützing", fix = "Cymbella leptoceros", spe = "Cymbella leptoceros", gen = "Cymbella" )
taxontable <- new_entry("Cymbella parva (W.Sm.) Kirchner in Cohn", fix = "Cymbella", spe = NA, gen = "Cymbella")
taxontable <- new_entry("Cymbella tumida (Brebisson)Van Heurck", fix = "Cymbella cistula group", spe = "Cymbella cistula group", gen = "Cymbella")
taxontable <- new_entry("Cymbopleura cuspidata (Kützing) Krammer", fix = "Cymbella cistula group", spe = "Cymbella cistula group", gen = "Cymbella")
taxontable <- new_entry("Delicata delicatula (Kützing) Krammer var. delicatula", fix = "Delicata", spe = NA, gen = "Delicata")
taxontable <- new_entry("Diadesmis confervacea Kützing f.rostrata (Krasske) Metzeltin & Lange-Bertalot", fix = "Diadesmis confervacea", spe = "Diadesmis confervacea", gen = "Diadesmis")
taxontable <- new_entry("Diatoma mesodon (Ehrenberg) Kützing", fix = "Diatoma mesodon", spe = "Diatoma mesodon", gen = "Diatoma")
taxontable <- new_entry("Didymosphenia geminata(Lyng.)Schmidt morphotyp geminata Metz&Lange-bertalo", fix = "Didymosphenia geminata", spe = "Didymosphenia geminata", gen = "Didymosphenia")
taxontable <- new_entry("Diploneis oculata (Brebisson) Cleve", fix = "Diploneis", spe = NA, gen = "Diploneis")
taxontable <- new_entry("Discostella pseudostelligera (Hustedt) Houk et Klee", fix = "Discostella", spe = NA, gen = "Discostella")
taxontable <- new_entry("Discostella stelligera (Cleve et Grun.) Houk & Klee", fix = "Discostella", spe = NA, gen = "Discostella")
taxontable <- new_entry("Encyonema krasskei (Krammer) Krammer", fix = "Encyonema krasskei", spe = "Encyonema krasskei", gen = "Encyonema")
taxontable <- new_entry("Encyonema mesianum (Cholnoky) D.G. Mann", fix = "Encyonema silesicacum/minutum/lange-bertalotii", spe = "Encyonema silesicacum/minutum/lange-bertalotii", gen = "Encyonema")
taxontable <- new_entry("Encyonema minutum (Hilse in Rabh.) D.G. Mann", fix = "Encyonema silesicacum/minutum/lange-bertalotii", spe = "Encyonema silesicacum/minutum/lange-bertalotii", gen = "Encyonema")
taxontable <- new_entry("Encyonema silesiacum (Bleisch in Rabh.) D.G. Mann var.altensis Krammer", fix = "Encyonema silesicacum/minutum/lange-bertalotii", spe = "Encyonema silesicacum/minutum/lange-bertalotii", gen = "Encyonema")
taxontable <- new_entry("Encyonema silesiacum (Bleisch in Rabh.) D.G. Mann var.lata Krammer"   , fix = "Encyonema silesicacum/minutum/lange-bertalotii", spe = "Encyonema silesicacum/minutum/lange-bertalotii", gen = "Encyonema")
taxontable <- new_entry("Encyonema silesiacum (Bleisch in Rabh.) DG. Mann var. latarea Krammer", fix = "Encyonema silesicacum/minutum/lange-bertalotii", spe = "Encyonema silesicacum/minutum/lange-bertalotii", gen = "Encyonema")
taxontable <- new_entry("Encyonema triangulum (Ehrenberg) Kützing", fix = "Encyonema triangulum", spe = "Encyonema triangulum", gen = "Encyonema")
taxontable <- new_entry("Encyonema ventricosum (Agardh) Grunow", fix = "Encyonema silesicacum/minutum/lange-bertalotii", spe = "Encyonema silesicacum/minutum/lange-bertalotii", gen = "Encyonema")
taxontable <- new_entry("Encyonopsis microcephala (Grun.) Kram. var. robusta (Hustedt) Krammer", fix = "Encyonopsis descripta/falaisensis/microcephala", spe = "Encyonopsis descripta/falaisensis/microcephala", gen = "Encyonopsis")
taxontable <- new_entry("Encyonopsis ruttnerii (Hustedt) Krammer", fix = "Encyonopsis ruttnerii", spe = "Encyonopsis ruttnerii", gen = "Encyonopsis")
taxontable <- new_entry("Eolimna subadnata(Hustedt) Moser Lange-Bertalot & Metzeltin", fix = "Eolimna subadnata", spe = "Eolimna subadnata", gen = "Eolimna")
taxontable <- new_entry("Eolimna tantula (Hustedt) Lange-Bertalot", fix = "Eolimna tantula", spe = "Eolimna tantula", gen = "Eolimna")
taxontable <- new_entry("Epithemia argus (Ehrenberg) Kützing var.argus", fix = "Epithemia argus", spe = "Epithemia argus", gen = "Epithemia")
taxontable <- new_entry("Eucocconeis ninckei(Guermeur & Manguin) Lange-Bertalot", fix = "Eucocconeis ninckei", spe = "Eucocconeis ninckei", gen = "Eucocconeis")
taxontable <- new_entry("Eunotia exigua (Brebisson ex Kützing) Rabenhorst", fix = "Eunotia  exigua/elegans Complex", spe = "Eunotia  exigua/elegans Complex", gen = "Eunotia")
taxontable <- new_entry("Eunotia intermedia (Krasske ex Hustedt) Nörpel & Lange-Bertalot", fix = "Eunotia incisa Complex", spe = "Eunotia incisa Complex", gen = "Eunotia")
taxontable <- new_entry("Eunotia minor (Kützing) Grunow in Van Heurck", fix = "Eunotia pectinalis Complex", spe = "Eunotia pectinalis Complex", gen = "Eunotia")
taxontable <- new_entry("Eunotia mucophila (Lange-Bert.&Norpel Schempp) Lange-Bertalot", fix = "Eunotia arcus/mucophila/bilunaris Complex", spe = "Eunotia arcus/mucophila/bilunaris Complex", gen = "Eunotia")
taxontable <- new_entry("Eunotia soleirolii (Kützing) Rabenhorst", fix = "Eunotia septentrionalis", spe = "Eunotia septentrionalis", gen = "Eunotia")
taxontable <- new_entry("Eunotia tenella (Grunow) Hustedt", fix = "Eunotia  exigua/elegans Complex", spe = "Eunotia  exigua/elegans Complex", gen = "Eunotia")
taxontable <- new_entry("Fallacia fracta (Hustedt ex Simonsen) D.G. Mann", fix = "Fallacia fracta", spe = "Fallacia fracta", gen = "Fallacia")
taxontable <- new_entry("Fallacia helensis (Schulz.) D.G. Mann", fix = "Fallacia subhamulata/helensis", spe = "Fallacia subhamulata/helensis", gen = "Fallacia")
taxontable <- new_entry("Fallacia mitis (Hustedt) D.G.Mann", fix = "Fallacia", spe = NA, gen = "Fallacia")
taxontable <- new_entry("Fistulifera pelliculosa (Brebisson) Lange-Bertalot", fix = "Fistulifera pelliculosa", spe = "Fistulifera pelliculosa", gen = "Fistulifera")
taxontable <- new_entry("Fistulifera saprophila (Lange-Bertalot & Bonik) Lange-Bertalot", fix = "Fistulifera", spe = NA, gen = "Fistulifera")
taxontable <- new_entry("Fragilaria arcus (Ehrenberg) Cleve var. arcus", fix = "Synedra cyclopum/Hannaea arcus", spe = "Synedra cyclopum/Hannaea arcus", gen = "Synedra")
taxontable <- new_entry("Fragilaria austriaca (Grunow) Lange-Bertalot", fix = "Fragilaria austriaca", spe = "Fragilaria austriaca", gen = "Fragilaria")
taxontable <- new_entry("Fragilaria mazamaensis (Sovereign) Lange-Bertalot", fix = "Fragilaria mazamaensis", spe = "Fragilaria mazamaensis", gen = "Fragilaria")
taxontable <- new_entry("Fragilaria perminuta (Grunow) Lange-Bertalot", fix = "Fragilaria capucina complex", spe = "Fragilaria capucina complex", gen = "Fragilaria")
taxontable <- new_entry("Fragilaria tenera (W.Smith) Lange-Bertalot", fix = "Fragilaria tenera complex", spe = "Fragilaria tenera complex", gen = "Fragilaria")
taxontable <- new_entry("Frustulia crassinervia (Breb.) Lange-Bertalot et Krammer", fix = "Frustulia rhomboides Complex", spe = "Frustulia rhomboides Complex", gen = "Frustulia")
taxontable <- new_entry("Geissleria acceptata (Hust.) Lange-Bertalot & Metzeltin", fix = "Geissleria ignota complex", spe = "Geissleria ignota complex", gen = "Geissleria")
taxontable <- new_entry("Geissleria cummerowi (L.Kalbe) Lange-Bertalot", fix = "Geissleria", spe = NA, gen = "Geissleria")
taxontable <- new_entry("Geissleria decussis(Ostrup) Lange-Bertalot & Metzeltin", fix = "Geissleria decussis", spe = "Geissleria decussis", gen = "Geissleria")
taxontable <- new_entry("Geissleria ignota (Krasske)Lange-Bertalot & Metzeltin", fix = "Geissleria ignota complex", spe = "Geissleria ignota complex" , gen = "Geissleria")
taxontable <- new_entry("Geissleria schoenfeldii (Hustedt) Lange-Bertalot & Metzeltin", fix = "Geissleria schoenfeldii", spe = "Geissleria schoenfeldii", gen = "Geissleria")
taxontable <- new_entry("Gomphoneis minuta (Stone) Kociolek & Stoermer var.minuta", fix = "Gomphonema kociolekii", spe = "Gomphonema kociolekii", gen = "Gomphonema")
taxontable <- new_entry("Gomphonema exilissimum(Grun.) Lange-Bertalot & Reichardt", fix = "Gomphonema parvulum Complex", spe = "Gomphonema parvulum Complex", gen = "Gomphonema" )
taxontable <- new_entry("Gomphonema parvulius Lange-Bertalot & Reichardt", fix = "Gomphonema parvulum Complex", spe = "Gomphonema parvulum Complex", gen = "Gomphonema")
taxontable <- new_entry("Gomphonema productum (Grunow) Lange-Bertalot & Reichardt", fix = "Gomphonema angustatum Complex", spe = "Gomphonema angustatum Complex", gen = "Gomphonema")
taxontable <- new_entry("Gomphonema pumilum (Grunow) Reichardt & Lange-Bertalot", fix = "Gomphonema pumilum complex", spe = "Gomphonema pumilum complex", gen = "Gomphonema")
taxontable <- new_entry("Gomphosphenia lingulatiformis (Lange-Bertalot & Reichardt) Lange-Bertalot", fix = "Gomphosphenia lingulatiformis", spe = "Gomphosphenia lingulatiformis", gen = "Gomphosphenia")
taxontable <- new_entry("Gyrosigma acuminatum (Kützing)Rabenhorst", fix = "Gyrosigma", spe = NA, gen = "Gyrosigma")
taxontable <- new_entry("Gyrosigma attenuatum (Kützing) Rabenhorst", fix = "Gyrosigma", spe = NA, gen = "Gyrosigma")
taxontable <- new_entry("Halamphora montana (Krasske) Levkov", fix = "Halamphora montana", spe = "Halamphora montana", gen = "Halamphora")
taxontable <- new_entry("Halamphora oligotraphenta (Lange-Bertalot) Levkov", fix = "Halamphora oligotraphenta", spe = "Halamphora oligotraphenta", gen = "Halamphora")
taxontable <- new_entry("Halamphora paraveneta (Lange-Bertalot, Cavacini, Tagliaventi & Alfinito) Levkov", fix = "Halamphora paraveneta", spe = "Halamphora paraveneta", gen = "Halamphora")
taxontable <- new_entry("Halamphora veneta (Kützing) Levkov", fix = "Halamphora veneta", spe = "Halamphora veneta", gen = "Halamphora")
taxontable <- new_entry("Hippodonta capitata (Ehr.)Lange-Bert.Metzeltin & Witkowski", fix = "Hippodonta capitata", spe = "Hippodonta capitata", gen = "Hippodonta")
taxontable <- new_entry("Hippodonta costulata (Grunow)Lange-Bertalot Metzeltin & Witkowski", fix = "Hippondonta", spe = NA, gen = "Hippondonta" )
taxontable <- new_entry("Hippodonta hungarica(Grunow) Lange-Bertalot Metzeltin & Witkowski", fix = "Hippodonta hungarica", spe = "Hippodonta hungarica", gen = "Hippodonta")
taxontable <- new_entry("Hippodonta lueneburgensis(Grunow) Lange-Bertalot Metzeltin & Witkowski", fix = "Hippodonta lueneburgensis", spe = "Hippodonta lueneburgensis", gen = "Hippodonta")
taxontable <- new_entry("Hippodonta pseudacceptata(Kobayasi) Lange-Bertalot Metzeltin & Witkowski", fix = "Hippodonta pseudacceptata", spe = "Hippodonta pseudacceptata", gen = "Hippodonta")
taxontable <- new_entry("Karayevia bottnica (P.T. Cleve) Lange-Bertalot", fix = "Karayevia clevei", spe = "Karayevia clevei", gen = "Karayevia" )
taxontable <- new_entry("Karayevia clevei(Grunow) Bukhtiyarova", fix = "Karayevia clevei", spe = "Karayevia clevei", gen = "Karayevia")
taxontable <- new_entry("Karayevia kolbei (Hustedt) Bukhtiyarova", fix = "Karayevia kolbei", spe = "Karayevia kolbei", gen = "Karayevia")
taxontable <- new_entry("Karayevia ploenensis (Hustedt) Bukhtiyarova", fix = "Karayevia ploenensis", spe = "Karayevia ploenensis", gen = "Karayevia")
taxontable <- new_entry("Lemnicola hungarica (Grunow) Round & Basson", fix = "Lemnicola hungarica", spe = "Lemnicola hungarica", gen = "Lemnicola")
taxontable <- new_entry("Luticola charlatii (M. Peragallo) Metzeltin & Lange-Bertalot", fix = "Luticola", spe = NA, gen = "Luticola")
taxontable <- new_entry("Luticola cohnii (Hilse) D.G. Mann", fix = "Luticola", spe = NA, gen = "Luticola")
taxontable <- new_entry("Luticola nivalis (Ehrenberg) D.G. Mann", fix = "Luticola", spe = NA, gen = "Luticola")
taxontable <- new_entry("Mayamaea agrestis(Hustedt) Lange-Bertalot", fix = "Mayamaea", spe = NA, gen = "Mayamaea")
taxontable <- new_entry("Mayamaea fossalis (Krasske) Lange-Bertalot", fix = "Mayamaea fossalis", spe = "Mayamaea fossalis", gen = "Mayamaea")
taxontable <- new_entry("Mayamaea fossaloides (Hustedt) Lange-Bertalot", fix = "Mayamaea fossaloides", spe = "Mayamaea fossaloides", gen = "Mayamaea")
taxontable <- new_entry("Mayamaea lacunolaciniata (Lange-Bertalot & Bonik) Lange-Bertalot", fix = "Mayamaea lacunolaciniata", spe = "Mayamaea lacunolaciniata", gen = "Mayamaea")
taxontable <- new_entry("Mayamaea permitis (Hustedt) Bruder & Medlin", fix = "Mayamaea permitis", spe = "Mayamaea permitis", gen = "Mayamaea")
taxontable <- new_entry("Mayamaea recondita (Hustedt) Lange-Bertalot", fix = "Mayamaea recondita", spe = "Mayamaea recondita", gen = "Mayamaea")
taxontable <- new_entry("Meridion circulare (Greville) Agardh var.constrictum (Ralfs) Van Heurck", fix = "Meridion circulare Complex", spe = "Meridion circulare Complex", gen = "Meridion")
taxontable <- new_entry("Microcostatus kuelbsii (Lange-Bertalot)Lange-Bertalot", fix = "Navicula vitiosa-indifferens-kuelbsii++", spe = "Navicula vitiosa-indifferens-kuelbsii++", gen = "Navicula")
taxontable <- new_entry("Navicula densilineolata (Lange-Bertalot) Lange-Bertalot", fix = "Navicula lanceolata complex", spe = "Navicula lanceolata complex", gen = "Navicula")
taxontable <- new_entry("Navicula lanceolata (Agardh) Ehrenberg", fix = "Navicula lanceolata complex", spe = "Navicula lanceolata complex", gen = "Navicula")
taxontable <- new_entry("Navicula recens (Lange-Bertalot) Lange-Bertalot", fix = "Navicula recens", spe = "Navicula recens", gen = "Navicula")
taxontable <- new_entry("Navicula reinhardtii (Grunow) Grunow in Cl. & Möller", fix = "Navicula aurora-peregrina-reinhardtii++", spe = "Navicula aurora-peregrina-reinhardtii++", gen = "Navicula")
taxontable <- new_entry("Navicula upsaliensis (Grunow) Peragallo", fix = "Navicula cryptotenella/cryptotenelloides", spe = "Navicula cryptotenella/cryptotenelloides", gen = "Navicula")
taxontable <- new_entry("Navicula viridula (Kützing) Ehrenberg", fix = "Navicula viridula complex", spe = "Navicula viridula complex", gen = "Navicula")
taxontable <- new_entry("Naviculadicta absoluta (Hustedt) Lange-Bertalot", fix = "Navicula ventralis-medioconvexa++", spe = "Navicula ventralis-medioconvexa++", gen = "Navicula")
taxontable <- new_entry("Neidium dubium(Ehrenberg)Cleve", fix = "Neidium dubium Complex", spe = "Neidium dubium Complex", gen = "Neidium")
taxontable <- new_entry("Nitzschia bulnheimiana (Rabenhorst) H.L.Smith", fix = "Nitzschia bulnheimiana", spe = "Nitzschia bulnheimiana", gen = "Nitzschia")
taxontable <- new_entry("Nitzschia fasciculata (Grunow)Grunow in V.Heurck", fix = "Nitzschia fasciculata", spe = "Nitzschia fasciculata", gen = "Nitzschia")
taxontable <- new_entry("Nitzschia linearis(Agardh) W.M.Smith var.tenuis (W.Smith) Grunow", fix = "Nitzschia pura-linearis Complex", spe = "Nitzschia pura-linearis Complex", gen = "Nitzschia")
taxontable <- new_entry("Nitzschia palea (Kützing) W.Smith var.debilis(Kützing)Grunow in Cl. & Grun", fix = "Nitzschia palea-paleacea", spe = "Nitzschia palea-paleacea", gen = "Nitzschia")
taxontable <- new_entry("Nitzschia paleacea (Grunow) Grunow in van Heurck", fix = "Nitzschia palea-paleacea", spe = "Nitzschia palea-paleacea", gen = "Nitzschia")
taxontable <- new_entry("Nitzschia pusilla(Kützing)Grunow", fix = "Nitzschia pusilla Complex", spe = "Nitzschia pusilla Complex", gen = "Nitzschia")
taxontable <- new_entry("Nitzschia scalpelliformis (Grunow) Grunow in Cleve & Grunow", fix = "Nitzschia scalpelliformis", spe = "Nitzschia scalpelliformis", gen = "Nitzschia")
taxontable <- new_entry("Nitzschia sigmoidea (Nitzsch)W. Smith", fix = "Nitzschia sigma Complex", spe = "Nitzschia sigma Complex", gen = "Nitzschia")
taxontable <- new_entry("Nitzschia tabellaria (Grun.) Grun. in Cl. & Grun.", fix = "Nitzschia tabellaria", spe = "Nitzschia tabellaria", gen = "Nitzschia")
taxontable <- new_entry("Nitzschia umbonata(Ehrenberg)Lange-Bertalot", fix = "Nitzschia umbonata", spe = "Nitzschia umbonata", gen = "Nitzschia")
taxontable <- new_entry("Nitzschia vermicularis(Kützing)Hantzsch", fix = "Nitzschia vermicularis", spe = "Nitzschia vermicularis", gen = "Nitzschia")
taxontable <- new_entry("Nupela impexiformis (Lange-Bertalot) Lange-Bertalot", fix = "Nupela", spe = NA, gen = "Nupela")
taxontable <- new_entry("Nupela lapidosa (Lange-Bertalot) Lange-Bertalot var.lapidosa", fix = "Nupela lapidosa", spe = "Nupela lapidosa", gen = "Nupela")
taxontable <- new_entry("Nupela tenuicephala (Hustedt) Lange-Bertalot", fix = "Nupela", spe = NA, gen = "Nupela")
taxontable <- new_entry("Parlibellus protracta (Grunow) Witkowski Lange-Bertalot & Metzeltin", fix = "Nupela", spe = NA, gen = "Nupela")
taxontable <- new_entry("Peronia fibula (Breb.ex Kütz.)Ross", fix = "Peronia", spe = NA, gen = "Peronia")
taxontable <- new_entry("Pinnularia lata (Brébisson) Rabenhorst", fix = "Pinnularia lata", spe = "Pinnularia lata", gen = "Pinnularia")
taxontable <- new_entry("Pinnularia microstauron (Ehr.) Cleve var. microstauron", fix = "Pinnularia microstauron Complex", spe = "Pinnularia microstauron Complex", gen = "Pinnularia")
taxontable <- new_entry("Pinnularia transversa (A. Schmidt) Mayer", fix = "Pinnularia transversa", spe = "Pinnularia transversa", gen = "Pinnularia")
taxontable <- new_entry("Placoneis anglica (Ralfs) E.J. Cox", fix = "Placoneis anglica", spe = "Placoneis anglica", gen = "Placoneis")
taxontable <- new_entry("Placoneis clementis (Grun.) Cox", fix = "Placoneis", spe = NA, gen = "Placoneis")
taxontable <- new_entry("Placoneis elginensis (Greg) Cox", fix = "Placoneis", spe = NA, gen = "Placoneis")

taxontable <- new_entry("Placoneis gastrum (Ehr.) Mereschkowsky", fix = "Placoneis", spe = NA, gen = "Placoneis")
taxontable <- new_entry("Planothidium biporomum (Hohn & Hellerman) Lange-Bertalot", fix = "Planothidium lanceolatum", spe = "Planothidium lanceolatum", gen = "Planothidium")
taxontable <- new_entry("Planothidium daui (Foged) Lange-Bertalot", fix = "Planothidium daui-granum", spe = "Planothidium daui-granum", gen = "Planothidium")
taxontable <- new_entry("Planothidium dubium(Grun.)Round & Bukhtiyarova", fix = "Planothidium lanceolatum", spe = "Planothidium lanceolatum", gen = "Planothidium")
taxontable <- new_entry("Planothidium frequentissimum var.minus (Schulz) Lange-Bertalot", fix = "Planothidium lanceolatum", spe = "Planothidium lanceolatum", gen = "Planothidium")
taxontable <- new_entry("Planothidium granum (Hohn & Hellerman) Lange-Bertalot", fix = "Planothidium daui-granum", spe = "Planothidium daui-granum", gen = "Planothidium")
taxontable <- new_entry("Planothidium haynaldii (Schaarschmidt) Lange-Bertalot", fix = "Planothidium", spe = NA, gen = "Planothidium")
taxontable <- new_entry("Planothidium peragallii (Brun & Heribaud)Round & Bukhtiyarova", fix = "Planothidium peragallii", spe = "Planothidium peragallii", gen = "Planothidium")
taxontable <- new_entry("Planothidium rostratum (Oestrup) Lange-Bertalot", fix = "Planothidium lanceolatum", spe = "Planothidium lanceolatum", gen = "Planothidium")
taxontable <- new_entry("Platessa brevicostata (Hustedt) Lange-Bertalot", fix = "Platessa brevicostata", spe = "Platessa brevicostata", gen = "Platessa")
taxontable <- new_entry("Platessa conspicua (A.Mayer) Lange-Bertalot", fix = "Planothidium", spe = NA, gen = "Planothidium")
taxontable <- new_entry("Platessa hustedtii (Krasske) Lange-Bertalot", fix = "Platessa hustedtii", spe = "Platessa hustedtii", gen = "Platessa")
taxontable <- new_entry("Pleurosira laevis (Ehrenberg) Compere f.laevis Ehrenberg", fix = "Pleurosira laevis", spe = "Pleurosira laevis", gen = "Pleurosira")
taxontable <- new_entry("Psammodictyon constrictum (Gregory) D.G. Mann in Round & al.", fix = "Psammodictyon constrictum", spe = "Psammodictyon constrictum", gen = "Psammodictyon" )
taxontable <- new_entry("Psammothidium grischunum (Wuthrich) Bukhtiyarova et Round", fix = "Psammothidium marginulatum/scoticum/lacus-vulcani/levenderi", spe = "Psammothidium marginulatum/scoticum/lacus-vulcani/levenderi", gen = "Psammothidium")
taxontable <- new_entry("Psammothidium kuelbsii (Lange-Bertalot in L.-B. & K.) Bukht. et Round", fix = "Psammothidium marginulatum/scoticum/lacus-vulcani/levenderi", spe = "Psammothidium marginulatum/scoticum/lacus-vulcani/levenderi", gen = "Psammothidium")
taxontable <- new_entry("Psammothidium rechtensis (Leclercq) Lange-Bertalot", fix = "Psammothidium rossii/altaica", spe = "Psammothidium rossii/altaica", gen = "Psammothidium")
taxontable <- new_entry("Pseudostaurosira elliptica (Schumann) Edlund, Morales & Spaulding", fix = "Staurosira construens complex", spe = "Staurosira construens complex", gen = "Staurosira")
taxontable <- new_entry("Pseudostaurosira parasitica (W.Smith) Morales", fix = "Pseudostaurosira parasitica complex", spe = "Pseudostaurosira parasitica complex", gen = "Pseudostaurosira")
taxontable <- new_entry("Sellaphora alastos (Hohn & Hellerman) Lange-Bertalot & Metzeltin", fix = "Sellaphora alastos", spe = "Sellaphora alastos", gen = "Sellaphora")
taxontable <- new_entry("Sellaphora bacillum (Ehrenberg) D.G.Mann", fix = "Sellaphora laevissima Complex", spe = "Sellaphora laevissima Complex", gen = "Sellaphora")
taxontable <- new_entry("Sellaphora disjuncta (Hustedt) D.G. Mann", fix = "Sellaphora pupula Complex", spe = "Sellaphora pupula Complex", gen = "Sellaphora")
taxontable <- new_entry("Sellaphora mutata(Krasske) Lange-Bertalot", fix = "Sellaphora pupula Complex", spe = "Sellaphora pupula Complex", gen = "Sellaphora")
taxontable <- new_entry("Sellaphora nana (Hustedt) Lange-Bertalot, Cavacini, Tagliaventi & Alfinito", fix = "Sellaphora", spe = NA, gen = "Sellaphora")
taxontable <- new_entry("Sellaphora pupula (Kützing) Mereschkowksy", fix = "Sellaphora pupula Complex", spe = "Sellaphora pupula Complex", gen = "Sellaphora")
taxontable <- new_entry("Sellaphora radiosa (Hustedt) Kobayasi in Mayama & al.", fix = "Eolimna minima-seminulum-atomoides", spe = "Eolimna minima-seminulum-atomoides", gen = "Eolimna")
taxontable <- new_entry("Stauroforma exiguiformis (Lange-Bertalot) Flower Jones et Round", fix = "Fragilaria virescens complex" , spe = "Fragilaria virescens complex", gen = "Fragilaria" )
taxontable <- new_entry("Staurosira lapponica (Grunow) Lange-Bertalot", fix = "Staurosira lapponica", spe = "Staurosira lapponica", gen = "Staurosira")
taxontable <- new_entry("Staurosira martyi (Heribaud) Lange-Bertalot", fix = "Staurosirella leptostauron complex", spe = "Staurosirella leptostauron complex", gen = "Staurosirella")
taxontable <- new_entry("Staurosira oldenburgiana (Hustedt)Lange-Bertalot", fix = "Staurosira oldenburgiana", spe = "Staurosira oldenburgiana", gen = "Staurosira")
taxontable <- new_entry("Staurosira venter (Ehr.) Cleve & Moeller", fix = "Staurosira venter", spe = "Staurosira venter", gen = "Staurosira")
taxontable <- new_entry("Stephanodiscus atmosphaerica (Ehrenberg) Hakansson & Locker", fix = "Stephanodiscus atmosphaerica", spe = "Stephanodiscus atmosphaerica", gen = "Stephanodiscus" )
taxontable <- new_entry("Stephanodiscus hantzschii fo.tenuis(Hustedt)Hakansson et Stoermer", fix = "Stephanodiscus hantzschii", spe = "Stephanodiscus hantzschii", gen = "Stephanodiscus")
taxontable <- new_entry("Tabellaria fenestrata(Lyngbye)Kützing", fix = "Tabellaria fenestrata Complex", spe = "Tabellaria fenestrata Complex", gen = "Tabellaria")
taxontable <- new_entry("Tabellaria flocculosa(Roth)Kützing", fix = "Tabellaria flocculosa Complex", spe = "Tabellaria flocculosa Complex", gen = "Tabellaria")
taxontable <- new_entry("Tabularia affinis (Kützing) Snoeijs", fix = "Tabularia affinis", spe = "Tabularia affinis", gen = "Tabularia")
taxontable <- new_entry(ori = "Thalassiosira lacustris (Grunow) Hasle in Hasle & Fryxell", fix = "Thalassiosira lacustris", spe = "Thalassiosira lacustris", gen = "Thalassiosira")
taxontable <- new_entry(ori = "Tryblionella hungarica (Grunow) D.G. Mann", fix = "Tryblionella hungarica", spe = "Tryblionella hungarica", gen = "Tryblionella")
taxontable <- new_entry(ori = "Ulnaria biceps (Kützing) Compère", fix = "Ulnaria ulna complex", spe = "Ulnaria ulna complex", gen = "Ulnaria")
taxontable <- new_entry(ori = "Ulnaria ulna (Nitzsch.) Compère", fix = "Ulnaria ulna complex", spe = "Ulnaria ulna complex", gen = "Ulnaria")
taxontable <- new_entry(ori = "Ulnaria ulna (Nitzsch.) Compère var. acus (Kütz.) Lange-Bertalot", fix = "Ulnaria ulna var.acus", spe = "Ulnaria ulna var.acus", gen = "Ulnaria")



TU <- unique(data$taxon) |> sort()
TU <- setdiff(TU, taxontable$original_name)
taxontable <- taxontable[!duplicated(taxontable$original_name)]
check_taxon_table(taxontable)

taxontable[species == "Psammothidium subatomoides", genus := "Psammothidium"]
taxontable[species == "Navicula krasskei-egregia-minuscula", genus := "Navicula"]
taxontable[species == "Navicula ventralis-medioconvexa++", genus := "Navicula"  ]


saveRDS(taxontable, paste0("data/diatoms/",Sys.Date(),"_taxontable_diatoms.rds"))

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
data2[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_spain_saul_blanco_diatoms")]


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
        data.set = "spain_saul_blanco_diatoms"
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

## save to file
saveRDS(data5, paste0("data/diatoms/original_data/spain_blanco/",Sys.Date(),"_final_non_aggregated.rds"))

# - subset to least impacted catchments 
data6 <- data5[least.impacted == TRUE]

sites <- unique(data6, by = "site_id") |> st_as_sf(coords = c("x.coord", "y.coord"), crs = data5$EPSG[1])

## look for sites with different ID but same coordinates 
distances  <- st_distance(sites)
distances2 <- as.matrix(distances)
diag(distances2) <- 999
(duplicate_sites <- which(distances2 < units::as_units(1, "m")))

x <- find_point(duplicate_sites[1])
(xx <- sites[x,])
mapview(xx)

data6 <- merge_sites(data6, "00146", "00017")

# - adjust gr sample id 
data6[,gr_sample_id := paste0("site_", site_id, "_date_", date_id, "_spain_saul_blanco_diatoms")]

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
saveRDS(remove_list, paste0("data/diatoms/original_data/spain_blanco/", Sys.Date(), "_remove_list.rds"))

#- drop remove sites 
data9 <- data8[!site_id %in% remove_list]


# temporal aggregation --------------------------------------------------------------
agg <- data5 |> unique(by = "gr_sample_id")
unique(table(agg$site_id))

source("R/functions/newest_sample.R")
data10 <- newest_sample(data9)
saveRDS(data10, paste0("data/diatoms/original_data/spain_blanco/",Sys.Date(),"_final_aggregated.rds"))
data10 <- readRDS("data/diatoms/original_data/spain_blanco/2022-01-18_final_aggregated.rds")

# statistics -------------------------------------------------------------------------
# time span
summary(data10$year)
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
data10[, uniqueN(lowest.taxon), by = "gr_sample_id"] |> 
        pull(V1) |> 
        mean()


