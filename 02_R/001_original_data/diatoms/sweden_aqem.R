# ——————————————————————————————————————————— #
# ——— Clean Diatom data from SWEDEN, AQEM ——— # 
# ——————————————————————————————————————————— #

# ———————————————————————————————————
# date first written: 16.12.21
# date last modified: 11.06.22
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: In this script I create a harmonized spatial data set of diatom from the 
# raw data from Sweden.  
# CRS: RT90 2.5 gon W (deprecated); EPSG: 2400
# ————————————————

# setup -----------------------------------------------------------------------------
#devtools::install_github("https://github.com/jonjup/jjmisc")
library(data.table)
library(dplyr)
library(fs)
library(lubridate)
library(mapview)
library(magrittr)
library(sf)
library(stringr)
library(stringdist)
library(readxl)
library(tidyr)

source("R/functions/harmonize diatoms.R")
source("R/functions/add_typologies.R")
# load data -------------------------------------------------------------------------
## directory with all biological data 
bio_wd <- "data/diatoms/original_data/sweden_aqem/raw/AQEMBilaga%205 Excel.xls"
sit_wd <- "data/diatoms/original_data/sweden_aqem/raw/AQEM_diatoms_coordinates.xlsx"

bio <- read_excel(bio_wd) |> setDT()
sit <- read_excel(sit_wd) |> setDT()


# dia1 <- readRDS("data/diatoms/fwb_table.rds")
# dia2 <- readRDS("data/diatoms/omn_table.rds")
#non_diatom_algae <- readRDS("data/diatoms/2021-12-08_non_diatom_algae.rds")

# prepare data ----------------------------------------------------------------------
names(bio)[1:3] <- c("old", "code", "taxon")
bio[is.na(taxon), taxon := old]
bio <- bio[!is.na(taxon)]

bio[, c("old", "code") := NULL]

bio2 <- 
        pivot_longer(bio, cols = !taxon, names_to = "original_site_name", values_to = "abundance") |> 
        filter(!is.na(abundance)) |> 
        filter(abundance != 0) |> 
        filter(!taxon %in% c("SUMMA RÄKNADE SKAL", "SUMMA EUNOTIA", "SUMMA EUNOTIA %", "SUMMA EUNOTIA–E. FORMICA %"))
data <- 
        sit |> 
        select(original_site_name = Site, y.coord = 'X_RAK (Xnew)', x.coord = 'Y_RAK (Xnew)', date = '1st sampling date') |> 
        right_join(bio2, by = "original_site_name") |> 
        mutate(EPSG = 2400, 
               data.set = "sweden_aqem_diatoms",
               date = dmy(date))

data <- data[!is.na(x.coord)]
## add season 
data[,c("year", "season") := .(year(date), 
                               case_when(month(date) %in% c(12,1,2)  ~ "winter",
                                         month(date) %in% c(3,4,5)   ~ "spring",
                                         month(date) %in% c(6,7,8)   ~ "summer",
                                         month(date) %in% c(9,10,11) ~ "autumn"))]

sites <- unique(data, by = "original_site_name")
sites <- st_as_sf(sites, coords = c("x.coord", "y.coord"), crs = sites$EPSG[1])
mapview(sites)


# taxonomic harmonization -----------------------------------------------------------
taxontable <- readRDS("data/diatoms/2022-05-17_taxontable_diatoms.rds")

TU <- unique(data$taxon) |> sort()
TU <- setdiff(TU, taxontable$original_name)

## what is the most similar in in taxontable 
strdist_id  <- amatch(TU, taxontable$original_name, maxDist = 1000)
strdist_tbl <- data.table(taxontable_name = taxontable$original_name[strdist_id], TU) |> 
        filter(!is.na(taxontable_name))

taxontable <- append_to_tt("Achnanthes minutissima var. saprophila", "Achnanthes cf. minutissima var. saprophila Kobayasi & Mayama")
taxontable <- append_to_tt("Achnanthes pseudoswazi", "Achnanthes cf. pseudoswazi Carter")
taxontable <- append_to_tt("Achnanthes minutissima", "Achnanthes minutissima-grupp")
taxontable <- append_to_tt("Achnanthes minutissima", "Achnanthes minutissima (långsmal)")
taxontable <- append_to_tt("Achnanthes minutissima var. saprophila", "Achnanthes minutissima inkl. var. saprophila Kobayasi & Mayama")
taxontable <- append_to_tt("Achnanthes sp.", "Achnanthes spp.")
taxontable <- append_to_tt("Aulacoseira crassipunctata", "Aulacoseira crassipunctata Krammer")
taxontable <- append_to_tt("Aulacoseira sp.", "Aulacoseira species")
taxontable <- append_to_tt("Cocconeis placentula var. placentula", "Cocconeis placentula var. placentula Ehrenberg")
taxontable <- append_to_tt("Cocconeis placentula var. pseudolineata", "Cocconeis placentula var. pseudolineata Geitler")
taxontable <- append_to_tt("Cyclotella distinguenda var.unipunctata", "Cyclotella distinguenda unipunctata")
taxontable <- append_to_tt("Cyclotella distinguenda var.mesoleia", "Cyclotella distinguenda var. mesoleia (Grunow) Håkansson")
taxontable <- append_to_tt("Cyclotella sp", "Cyclotella spp.")
taxontable <- append_to_tt("Cymbella affiniformis", "Cymbella affinis Kützing")
taxontable <- append_to_tt("Cymbella caespitosa", "Cymbella caespitosa (Kützing) Brun")
taxontable <- append_to_tt("Cymbella cymbiformis", "Cymbella cymbiformis Agardh")
taxontable <- append_to_tt("Cymbella delicatula", "Cymbella delicatula Kützing")
taxontable <- append_to_tt("Cymbella gaeumannii", "Cymbella gaeumannii Meister")
taxontable <- append_to_tt("Cymbella helvetica", "Cymbella helvetica Kützing")
taxontable <- append_to_tt("Cymbella incerta", "Cymbella incerta (Grunow) Cleve")
taxontable <- append_to_tt("Cymbella minuta", "Cymbella minuta Hilse")
taxontable <- append_to_tt("Cymbella naviculiformis", "Cymbella naviculiformis (Auerswald) Cleve")
taxontable <- append_to_tt("Cymbella silesiaca", "Cymbella silesiaca Bleisch")
taxontable <- append_to_tt("Cymbella sinuata", "Cymbella sinuata Gregory")
taxontable <- append_to_tt("Cymbella sp.", "Cymbella spp.")
taxontable <- append_to_tt("Diploneis petersenii", "Diploneis peterseni")
taxontable <- append_to_tt("Encyonopsis", "Encyonopsis sp.")
taxontable <- append_to_tt("Eunotia bilunaris var.linearis", "Eunotia bilunaris var. linearis")
taxontable <- append_to_tt("Eunotia meisteri", "Eunotia cf. meisteri Hustedt")
taxontable <- append_to_tt("Eunotia naegelii", "Eunotia naegeli")
taxontable <- append_to_tt("Eunotia pectinalis var.ventralis", "Eunotia pectinalis var. ventralis")
taxontable <- append_to_tt("Eunotia zazuminensis", "Eunotia zasuminensis")
taxontable <- append_to_tt("Fragilaria capucina Gruppe", "Fragilaria capucina group")
taxontable <- append_to_tt("Fragilaria capucina var. gracilis", "Fragilaria capucina var. gracilis (missbildade)")
taxontable <- append_to_tt("Gomphonema clavatum", "Gomphonema clavatum s.l.")
taxontable <- append_to_tt("Gomphonema parvulum var. exilissimum", "Gomphonema parvulum var. exilissimum Grunow")
taxontable <- append_to_tt("Gomphonema pumilum Gruppe", "Gomphonema pumilum group")
taxontable <- append_to_tt("Gomphonema sp.", "Gomphonema spp.")
taxontable <- append_to_tt("Navicula accomoda", "Navicula accomoda Hustedt")
taxontable <- append_to_tt("Navicula bryophila", "Navicula bryophila Petersen")
taxontable <- append_to_tt("Navicula capitata", "Navicula capitata Ehrenberg")
taxontable <- append_to_tt("Navicula costulata", "Navicula costulata Grunow")
taxontable <- append_to_tt("Navicula gallica var. perpusilla", "Navicula gallica var. perpusilla (Grunow) Lange-Bertalot")
taxontable <- append_to_tt("Navicula mutica var. mutica", "Navicula mutica var. mutica Kützing")
taxontable <- append_to_tt("Navicula mutica var. ventricosa", "Navicula mutica var. ventricosa (Kützing) Cleve & Grunow")
taxontable <- append_to_tt("Navicula pseudoscutiformis", "Navicula pseudoscutiformis Hustedt")
taxontable <- append_to_tt("Navicula soehrensis var. muscicola", "Navicula soehrensis var. muscicola (Petersen) Krasske")
taxontable <- append_to_tt("Navicula soehrensis var. soehrensis", "Navicula soehrensis var. soehrensis Krasske")
taxontable <- append_to_tt("Navicula sp.", "Navicula spp.")
taxontable <- append_to_tt("Nitzschia dissipata ssp. dissipata", "Nitzschia dissipata var. dissipata")
taxontable <- append_to_tt("Pinnularia sp.", "Pinnularia spp.")
taxontable <- append_to_tt("Pinnularia viridiformis var. viridiformis", "Pinnularia viridiformis var. viridiformis morphotype 1")
taxontable <- append_to_tt("Stauroneis anceps", "Stauroneis anceps s.l.")
taxontable <- append_to_tt("Stauroneis nobilis", "Stauroneis nobilis Schumann")

TU <- unique(data$taxon) |> sort()
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

TU <- unique(data$taxon) |> sort()
(TU <- setdiff(TU, taxontable$original_name))


taxontable = new_entry("Achnanthes alpestris (Brun) Lange-Bertalot & Metzeltin", fix = "Eucocconeis laevis/alpestris", spe = "Eucocconeis laevis/alpestris", gen = "Eucocconeis")
taxontable = new_entry("Achnanthes depressa (Cleve) Hustedt", fix = "Eucocconeis", spe = NA, gen = "Eucocconeis")
taxontable = new_entry("Achnanthes lanceolata var. abbreviata", fix = "Planothidium pseudotanense", spe = "Planothidium pseudotanense", gen = "Planothidium")
taxontable = new_entry("Achnanthes stolida  Krasske  alt. Navicula schmassmannii Hustedt (jfr 2/4, T24)", fix = "Achnanthes acares/ricula/carissima", spe = "Achnanthes acares/ricula/carissima", gen = "Achnanthes")
taxontable = new_entry("Brachysira species", fix = "Brachysira", spe = NA, gen = "Brachysira")
taxontable = new_entry("Cocconeis placentula var. euglypta (Ehrenberg) Grunow + var. lineata", fix = "Cocconeis placentula", spe = "Cocconeis placentula", gen = "Cocconeis")
taxontable = new_entry("Cymbella cesatii (Rabenhorst) Grunow", fix = "Encyonopsis cesatii", spe = "Encyonopsis cesatii", gen = "Encyonopsis")
taxontable = new_entry("Cymbella descripta (Hustedt) Krammer & Lange-Bertalot", fix = "Encyonopsis descripta/falaisensis/microcephala", spe = "Encyonopsis descripta/falaisensis/microcephala", gen = "Encyonopsis")
taxontable = new_entry("Cymbella falaisensis (Grunow) Krammer & Lange-Bertalot", fix = "Encyonopsis descripta/falaisensis/microcephala", spe = "Encyonopsis descripta/falaisensis/microcephala", gen = "Encyonopsis")
taxontable = new_entry("Cymbella gracilis (Ehrenberg) Kützing", fix = "Encyonema neogracile", spe = "Encyonema neogracile", gen = "Encyonema")
taxontable = new_entry("Eunotia species", fix = "Eunotia", spe = NA, gen = "Eunotia")
taxontable = new_entry("Fragilaria dilatata (Brébisson) Lange-Bertalot", fix = "Ulnaria ulna complex", spe = "Ulnaria ulna complex", gen = "Ulnaria")
taxontable = new_entry("Fragilaria ulna f. angustissima", fix = "Ulnaria ulna complex", spe = "Ulnaria ulna complex", gen = "Ulnaria")
taxontable = new_entry("Navicula soehrensis var. hassiaca (Krasske) Lange-Bertalot", fix = "Chamaepinnularia hassiaca", spe = "Chamaepinnularia hassiaca", gen = "Chamaepinnularia")
taxontable = new_entry("Nitzschia cf. palea (Kützing) W. Smith", fix = "Nitzschia palea-paleacea", spe = "Nitzschia palea-paleacea", gen = "Nitzschia")
taxontable = new_entry("Nitzschia cf. perminuta (Grunow) M. Peragallo", fix = "Nitzschia perminuta Complex", spe = "Nitzschia perminuta Complex", gen = "Nitzschia")
taxontable = new_entry("Pinnularia mesolepta (Ehrenberg) W. Smith Morphotyp 5", fix = "Pinnularia mesolepta Complex", spe = "Pinnularia mesolepta Complex", gen = "Pinnularia")
taxontable = new_entry("Stauroneis anceps var. hyalina Peragallo & Brun", fix = "Stauroneis anceps Complex", spe = "Stauroneis anceps Complex", gen = "Stauroneis")
taxontable = new_entry("Stauroneis anceps var. siberica Grunow", fix = "Stauroneis anceps Complex", spe = "Stauroneis anceps Complex", gen = "Stauroneis")


taxontable[original_name == "Achnanthes stolida", c("fixed_name", "species") :=
                   .("Achnanthes acares/ricula/carissima", "Achnanthes acares/ricula/carissima")]
taxontable[original_name == "Cymbella cesatii var.paradoxa", c("fixed_name", "species", "genus") :=
                   .("Encyonopsis cesatii", "Encyonopsis cesatii", "Encyonopsis")]
taxontable[original_name == "Cymbella falaisensis var.lanceola", c("fixed_name", "species", "genus") :=
                   .("Encyonopsis descripta/falaisensis/microcephala", "Encyonopsis descripta/falaisensis/microcephala", "Encyonopsis")]
taxontable[original_name == "Pinnularia mesolepta", c("fixed_name", "species") :=
                   .("Pinnularia mesolepta Complex", "Pinnularia mesolepta Complex")]

check_taxon_table(taxontable)
taxontable <- taxontable[!duplicated(taxontable$original_name)]

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
data2[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_sweden_aqem_diatoms")]


data2[,c("year", "season") := .(year(date), 
                                case_when(month(date) %in% c(12,1,2)  ~ "winter",
                                          month(date) %in% c(3,4,5)   ~ "spring",
                                          month(date) %in% c(6,7,8)   ~ "summer",
                                          month(date) %in% c(9,10,11) ~ "autumn"))]

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
data3[, abundance := sum(abundance), by = c("gr_sample_id", "lowest.taxon")]
data3 <- unique(data3, by = c("gr_sample_id", "lowest.taxon"))
data4 <- copy(data3)
typologies <- readRDS("data/all_typologies.rds")
data5 <- add_typologies(data4)

# - visual checks
# sites <- unique(data5, by = "gr_sample_id") |> st_as_sf(coords = c("x.coord", "y.coord"), crs = data5$EPSG[1])
# mapview(sites, zcol = "brt12")
# mapview(sites, zcol = "ife")
# mapview(sites, zcol = "bgr")
# mapview(sites, zcol = "least.impacted")

## save to file
#saveRDS(data5, paste0("data/diatoms/original_data/sweden_aqem/",Sys.Date(),"_final_non_aggregated.rds"))

# - subset to least impacted catchments 
data6 <- data5[least.impacted == TRUE]

sites <- unique(data6, by = "site_id") |> st_as_sf(coords = c("x.coord", "y.coord"), crs = data5$EPSG[1])

## look for sites with different ID but same coordinates 
distances  <- st_distance(sites)
distances2 <- as.matrix(distances)
diag(distances2) <- 999
(duplicate_sites <- which(distances2 < units::as_units(1, "m")))

# - no duplicate sites 

# - drop sites with very few species 
data6[, richness := uniqueN(lowest.taxon), by = "gr_sample_id"]
data7 <- data6[richness > 10]
# - drop sites far removed from ECRINS river network 
data8 <- data7[distance < 300]
rl <- readRDS("data/diatoms/original_data/sweden_aqem/2022-01-18_remove_list.rds")
data8 <- data8[!site_id %in% rl]
# - visually check the assignment of sites 

rt <- 
        data8 |> 
        unique(by = "site_id") |> 
        st_as_sf(coords = c("x.coord", "y.coord"), 
                 crs = data5$EPSG[1])

plot_typology <- st_crop(typologies, st_transform(sites, crs = st_crs(typologies)))
options(warn = -1)
updated_type <- data.table(site_id = rt$site_id)

for (i in 1:nrow(rt)){
        i.rt <- rt[i, ]
        i.plot_typology <- st_crop(plot_typology, st_buffer(st_transform(i.rt, crs = st_crs(typologies)), dist =  2000))
        x <- mapview(i.plot_typology, zcol = "brt") + mapview(i.rt, popup = c("water_body"), color = "red")
        print(x)
        #i.bool <- "n"
        i.bool <- readline(paste(i, "/", nrow(rt), ":"))
        if (i.bool == "break"){
                break()
        } else if (i.bool == "n"){
                updated_type[site_id == i.rt$site_id, new_type := "drop"]
        } else if (i.bool == "c"){
                i.towhat <- readline("change to:")
                updated_type[site_id == i.rt$site_id, new_type := i.towhat]
        } else {
                updated_type[site_id == i.rt$site_id, new_type := i.rt$brt12]
        }
        
        rm(list = ls()[grepl("i\\.", ls())])
}


# - save the remove list. 
saveRDS(updated_type, paste0("data/diatoms/original_data/sweden_aqem/", Sys.Date(), "_updated_type.rds"))
# - drop remove sites 
data9 <- left_join(data8, 
                   updated_type, 
                   by = "site_id")
# - drop "drop" rows determined in for-loop
data9[, brt12 := NULL]
data9 <- rename(data9, brt12 = new_type)
data9 <- data9[brt12 != "drop"]


# temporal aggregation --------------------------------------------------------------
agg <- data9 |> unique(by = "gr_sample_id")
unique(table(agg$site_id))
# - no
data10 <- data9[month(date) %in% 5:9]
saveRDS(data10, paste0("data/diatoms/original_data/sweden_aqem/",Sys.Date(),"_final_aggregated.rds"))

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
# only close sites 
uniqueN(data8$site_id)
# no sites that can not definitively be assigned to a river segment
uniqueN(data9$site_id)

# mean richness: 
data9[, uniqueN(lowest.taxon), by = "gr_sample_id"] |> 
        pull(V1) |> 
        mean()


