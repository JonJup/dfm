# ——— Clean Diatom data from Mixed - STAR  ——————— # 

# date first written: 21.12.21
# date last modified: 11.06.22
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: In this script I create a harmonized spatial data set of diatom from the 
# raw data from the STAR project.   
# CRS: 4326

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
library(readr)
library(readxl)
library(tidyr)

source("R/functions/harmonize diatoms.R")
source("R/functions/add_typologies.R")
# load data -------------------------------------------------------------------------
## directory with all biological data 
bio_wd <- "data/diatoms/original_data/germany_STAR/raw/STAR_diatoms.xls"
sit_wd <- "data/diatoms/original_data/germany_STAR/raw/STAR_Metadata_all_Sites.xls"


dia1 <- read_excel(bio_wd, sheet = 1) 
dia2 <- read_excel(bio_wd, sheet = 2)

# the link from abbreviation to full name can be made using this file 
link_names = read_excel("data/diatoms/OMNIDIAFILE.xls", col_names = T, skip = 1, sheet = 1)
link_names2 = read_excel("data/diatoms/OMNIDIAFILE.xls", col_names = T, skip = 0, sheet = 2)

link_names_bind = bind_rows(link_names, link_names2)
rm(link_names, link_names2)

# file for dates 
dates = read_excel(sit_wd) |> setDT()

# Initial data cleaning ---------------------------------------------------

link_names.sub = select(link_names_bind,CODE,
                        DENOM)
rm(link_names_bind)
taxa.names = dia1[,1] %>% .[-c(1:4),]
taxa.names2 = dia2[,1] %>% .[-c(1:4),]
names(taxa.names) = "Taxa_Code"
names(taxa.names2) = "Taxa_Code"

# join Codes with species names 
taxa.names_join = left_join(x = taxa.names,
                            y = link_names.sub,
                            by = c("Taxa_Code" = "CODE"))

taxa.names2_join = left_join(x = taxa.names2,
                             y = link_names.sub,
                             by = c("Taxa_Code" = "CODE"))
rm(taxa.names, taxa.names2)

# fill new table with full names and long format ...
# ... for mountainous sites 
for (i in 2:ncol(dia1)) {
        
        sd = dia1[,c(1,i)]
        sd2 = sd[-c(1:4), ]
        sd2[,2] = as.numeric(pull(sd2, 2))
        sd3 = sd2[which(sd2[,2] > 0), ]
        sd3.j = left_join(sd3,
                          taxa.names_join,
                          by = c("...1" = "Taxa_Code" ))
        
        sd3.j.sub = sd3.j[,2:3]  
        
        
        if (nrow(sd3.j.sub) > 0) {
                
                assign(
                        paste0("dia1",i,"_tb"),
                        tibble(taxon = sd3.j.sub$DENOM,
                               abundance = pull(sd3.j.sub[,1]),
                               stream = sd[1,2] %>% rep(nrow(sd3.j.sub)) %>% unlist, 
                               site_name = sd[2,2] %>% rep(nrow(sd3.j.sub)) %>% unlist, 
                               site_id = names(sd2)[2] %>%  rep(nrow(sd3.j.sub)),
                               sampling_id =  paste0(sd[4,2], "-", str_remove(sd[3,2], " -")) %>% rep(nrow(sd3.j.sub))
                               
                        ))
        }
}

star.dia.mount = rbindlist(mget(ls()[grepl(x = ls(), pattern = "_tb$")]))
rm(list = ls()[grepl(x = ls(), pattern = "_tb$")])
rm(dia1, sd, sd2, sd3, sd3.j, sd3.j.sub)
## find dates / seasons 
dia_spring = pull(dates[,11])
dia_summer = pull(dates[,12])
dia_autumn = pull(dates[,13])
for (dataset in c("dia_spring", "dia_summer", "dia_autumn")) {
        
        data = get(dataset)
        length.index = length(data)
        for (k in 1:length.index) {
                if (is.na(data[k])) next()
                if (str_detect(data[k], ";")) {
                        new_sampd_id = str_split(data[k], "; ")
                        data = data[-k]
                        data = append(data,
                                      unlist(new_sampd_id),
                                      after = k - 1)
                        length.index = append(length.index, 
                                              max(length.index) + 1)      
                }
        }
        assign(dataset, data)
}
rm(data, dataset, i,k,length.index, new_sampd_id)

for (i in seq_len(length(unique(star.dia.mount$sampling_id)))) {
        
        samp_ID =  unique(star.dia.mount$sampling_id)[i]
        
        ids = which(star.dia.mount$sampling_id == samp_ID)
        
        spring = samp_ID %in% dia_spring
        summer = samp_ID %in% dia_summer
        autumn = samp_ID %in% dia_autumn
        
        # no date -> skip
        if (!(any(spring, summer, autumn))) next()
        if (sum(spring, summer, autumn) > 1) break("more than one")
        
        star.dia.mount[ids, "season" := ifelse(spring, "spring", ifelse(
                summer, "summer", "autumn"))
        ]
}

## have a look at those that are still na 
no_season_sites = str_remove(star.dia.mount[is.na(season), unique(site_id)],
                             "S")
for (q in seq_len(length(no_season_sites))) {
        
        rowid = which(dates[,1] == no_season_sites[q])
        dates_subset = dates[rowid, .SD, .SDcols = 11:13]
        na_sum = sum(is.na(dates_subset))
        if (na_sum == 2) {
                ex_season = c("spring","summer","autumn")[!(is.na(dates_subset))]
                star.dia.mount[site_id == paste0("S", no_season_sites[q]),
                               season := ex_season]
                
        }        
}
# ... for lowland sites 

for (i in 2:ncol(dia2)) {
        
        sd = dia2[,c(1,i)]
        sd2 = sd[-c(1:4), ]
        sd2[,2] = as.numeric(pull(sd2, 2))
        sd3 = sd2[which(sd2[,2] > 0), ]
        sd3.j = left_join(sd3,
                          taxa.names2_join,
                          by = c("...1" = "Taxa_Code" ))
        
        sd3.j.sub = sd3.j[,2:3]
        
        
        if (nrow(sd3.j.sub) > 0) {
                
                assign(
                        paste0("dia1",i,"_tb"),
                        tibble(taxon = sd3.j.sub$DENOM, 
                               abundance = pull(sd3.j.sub[,1]), 
                               stream = sd[1,2] %>% rep(nrow(sd3.j.sub)) %>% unlist, 
                               site_name = sd[2,2] %>% rep(nrow(sd3.j.sub)) %>% unlist, 
                               site_id = names(sd2)[2] %>%  rep(nrow(sd3.j.sub)),
                               sampling_id =  paste0(sd[4,2], "-", str_remove(sd[3,2], " -")) %>% rep(nrow(sd3.j.sub)))
                )
        }
}

star.dia.low = rbindlist(mget(ls()[grepl(x = ls(), pattern = "_tb$")]))
rm(list = ls()[grepl(x = ls(), pattern = "_tb$")])
rm(dia2, sd, sd2, sd3, sd3.j, sd3.j.sub)

for (i in seq_len(length(unique(star.dia.low$sampling_id)))) {
        
        samp_ID =  unique(star.dia.low$sampling_id)[i]
        
        ids = which(star.dia.mount$low == samp_ID)
        
        spring = samp_ID %in% dia_spring
        summer = samp_ID %in% dia_summer
        autumn = samp_ID %in% dia_autumn
        
        # no date -> skip
        if (!(any(spring, summer, autumn))) next()
        if (sum(spring, summer, autumn) > 1) break("more than one")
        
        star.dia.low[ids, "season" := ifelse(spring, "spring", ifelse(
                summer, "summer", "autumn"))
        ]
}

## have a look at those that are still na 
no_season_sites = str_remove(star.dia.low[is.na(season), unique(site_id)],
                             "S")
for (q in seq_len(length(no_season_sites))) {
        
        rowid = which(dates[,1] == no_season_sites[q])
        dates_subset = dates[rowid, .SD, .SDcols = 11:13]
        na_sum = sum(is.na(dates_subset))
        if (na_sum == 2) {
                ex_season = c("spring","summer","autumn")[!(is.na(dates_subset))]
                star.dia.low[site_id == paste0("S", no_season_sites[q]),
                             season := ex_season]
                
        }        
}
rm(spring, autumn, summer, dates, dates_subset, dia_autumn, dia_spring, dia_summer, 
   ex_season, i, ids, link_names.sub, na_sum, no_season_sites, q, rowid, samp_ID, taxa.names_join, 
   taxa.names2_join)


# combine both datasets 
data = bind_rows(star.dia.mount, star.dia.low) %>% setDT
rm(star.dia.low, star.dia.mount)
###  now we can add coordinates 
# create a vector with all sites 
all.sites = data$site_id %>% unique
# one sites is missing the S
all.sites[!(str_detect(all.sites, "^S"))]  <- str_glue("S",
                                                       all.sites[!(str_detect(all.sites, "^S"))])
# new data table with site id column 
stars.coords = data.table("ID" = all.sites)
rm(all.sites)
wiserstar = read_excel("data/diatoms/original_data/germany_STAR/raw/WISER-STAR_Code_Assignment.xlsx", sheet = 1) %>% 
        setDT

# add S to ids 
wiserstar$Site_Number = paste0("S",wiserstar$Site_Number)

wiser = read_excel("data/diatoms/original_data/germany_WISER/raw/WISER_Metadata_Abiotics.xls") %>% 
        setDT
wiser.sub = wiser[,1:3]
stars.coo.wi = stars.coords[wiserstar, on = c("ID" = "Site_Number")] 
stars.coo.wi2 = stars.coo.wi[wiser.sub, on = c("Station_Code_WISER" = "StationCode")] 
stars.coo.wi3 = stars.coo.wi2[,-c(5:7)]
data2 = left_join(data, stars.coo.wi3, by = c("site_id" = "ID")) %>% setDT
rm(data, wiserstar, wiser, wiser.sub, stars.coo.wi, stars.coo.wi2, stars.coo.wi3, stars.coords)

data2$EPSG = 4326

data2[, uniqueN(Longitude), by = "site_name"][, "V1"]

data2 <- data2[site_name != "<NEW>"]
data2 %<>% rename(x.coord = Longitude, y.coord = Latitude, original_site_name = site_name)

data2 <- data2[!is.na(taxon)]
data2 <- data2[!is.na(x.coord)]
data2 <- data2[!is.na(y.coord)]
data2 <- data2[!is.na(season)]

sites <- unique(data2, by = "original_site_name")
sites %<>% st_as_sf(coords = c("x.coord", "y.coord"), crs = data2$EPSG[1])
mapview(sites)

data2 <- data2[, .(
        original_site_name, 
        date = as.Date(NA),
        year = as.numeric(NA),
        season,
        taxon,
        abundance,
        x.coord,
        y.coord,
        EPSG
)]

rm(sit_wd, bio_wd);gc()

# taxonomic harmonization -----------------------------------------------------------

taxontable <- readRDS("data/diatoms/2022-05-17_taxontable_diatoms.rds")
data <- data2

TU <- sort(unique(data$taxon))
TU <- setdiff(TU, taxontable$original_name)

## what is the most similar in in taxontable 
strdist_id  <- amatch(TU, taxontable$original_name, maxDist = 100000)
strdist_tbl <- data.table(taxontable_name = taxontable$original_name[strdist_id], TU) |> 
        filter(!is.na(taxontable_name)) 

for (i in 1:nrow(strdist_tbl)){
        
        i.1 <- pull(strdist_tbl[i,1])
        i.2 <- pull(strdist_tbl[i,2])
        
        print(i.1)
        print(i.2)
        
        i.bool <- readline("match?:")
        
        if (i.bool == "y"){
                taxontable <- append_to_tt(i.1,i.2)
        } else if (i.bool == "n") {
                next()
        }
        rm(list = ls()[grepl("^i", ls())])
}

TU <- unique(data$taxon) |> sort()
TU <- setdiff(TU, taxontable$original_name)

dia1 <- readRDS("data/diatoms/fwb_table.rds")
dia2 <- readRDS("data/diatoms/omn_table.rds")

for (i in seq_along(TU)) {
        i.tu  <- TU[i]
        i.det <- str_detect(dia1$taxon_old, i.tu)
        if (any(i.det)) {
                i.id <- which(i.det)
                print(paste("name:", i.tu))
                print(paste("suggestions:", dia2$taxon[i.id]))
                i.rl <- readline()
        } else {
                i.id <- amatch(i.tu, dia1$taxon_old, maxDist = 100000) 
                if (is.null(i.id))
                        next()
                print(paste("name:", i.tu))
                print(paste("suggestions:", dia1$taxon_old[i.id]))
                i.rl <- readline()
        }        
        if (i.rl != "break") {
                i.id <- i.id[as.numeric(i.rl)]
                ## is it a synonym?
                print(paste("Final name: ", dia1$taxon_new[i.id]))
                i.final <- readline()
                ## check that against fwb
                taxontable <- add_entry_tt(i.final)
        }
        rm(list = ls()[grepl(pattern = "^i\\.", x = ls())])
}


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
        } else {
                i.id <- amatch(i.tu, dia2$taxon, maxDist = 100000) 
                if (is.null(i.id))
                        next()
                print(paste("name:", i.tu))
                print(paste("suggestions:", dia2$taxon[i.id]))
                i.rl <- readline()
        }        
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
                print(paste("Final name: ", dia2$taxon[i.id]))
                i.final <- readline()
                ## check that against fwb
                if (check_fwb(i.final)) {
                        i.final <- get_fwb(i.final)
                }
                taxontable <- add_entry_tt(i.final)
        }
        rm(list = ls()[grepl(pattern = "^i\\.", x = ls())])
}

TU <- unique(data$taxon) |> sort()
TU <- setdiff(TU, taxontable$original_name)

taxontable <- new_entry("ACHNANTHES J.B.M. Bory de St. Vincent", fix = "Achnanthes", spe = NA, gen = "Achnanthes")
taxontable <- new_entry("AMPHORA  C.G. Ehrenberg ex F.T. Kützing", fix = "Amphora", spe = NA, gen = "Amphora")
taxontable <- new_entry("AULACOSEIRA  G.H.K. Thwaites", fix = "Aulacoseira", spe = NA, gen = "Aulacoseira")
taxontable <- new_entry("COCCONEIS  C.G. Ehrenberg", fix = "Cocconeis", spe = NA, gen = "Cocconeis")
taxontable <- new_entry("CYCLOTELLA  F.T. Kützing ex A de Brébisson", fix = "Cyclotella", spe = NA, gen = "Cyclotella")
taxontable <- new_entry("DIATOMA  J.B.M. Bory de St. Vincent", fix = "Diatoma", spe = NA, gen = "Diatoma")
taxontable <- new_entry("EPITHEMIA  F.T. Kützing", fix = "Epithemia", spe = NA, gen = "Epithemia")
taxontable <- new_entry("FRAGILARIA  H.C. Lyngbye", fix = "Fragilaria", spe = NA, gen = "Fragilaria")
taxontable <- new_entry("GYROSIGMA  A. Hassall", fix = "Gyrosigma", spe = NA, gen = "Gyrosigma")
taxontable <- new_entry("SKELETONEMA  R.K. Greville", fix = "Skeletonema", spe = NA, gen = "Skeletonema")
taxontable <- new_entry("STAURONEIS  C.G. Ehrenberg", fix = "Stauroneis", spe = NA, gen = "Stauroneis")
taxontable <- new_entry("STEPHANODISCUS  C.G. Ehrenberg", fix = "Stephanodiscus", spe = NA, gen = "Stephanodiscus")
taxontable <- new_entry("SYNEDRA  C.G. Ehrenberg", fix = "Synedra", spe = NA, gen = "Synedra")
taxontable <- new_entry("TABELLARIA  C.G. Ehrenberg", fix = "Tabellaria", spe = NA, gen = "Tabellaria")

saveRDS(taxontable, paste0("data/diatoms/",Sys.Date(),"_taxontable_diatoms.rds"))

# join 
data <- rename(data, original_name = taxon)
setDT(data)
data2 <- taxontable[data, on = "original_name"]  

## check
sort(unique(data2$phylum))
sort(unique(data2$class))

## add site and date ids
data2[, site_id := .GRP, by = "original_site_name"]
data2[, site_id := as.numeric(site_id)]
data2[, date_id := .GRP, by = "season"]

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
data2[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_mixed_star_diatoms")]


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
        data.set = "mixed_star_diatoms"
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

# ## save to file
# saveRDS(data5, paste0("data/diatoms/original_data/germany_STAR/",Sys.Date(),"_final_non_aggregated.rds"))

# 141 samples 
uniqueN(data5$gr_sample_id)
# 141 sites
uniqueN(data5$site_id)

# - subset to least impacted catchments 
data6 <- data5[least.impacted == TRUE]

sites <- unique(data6, by = "site_id") |> st_as_sf(coords = c("x.coord", "y.coord"), crs = data5$EPSG[1])

## look for sites with different ID but same coordinates 
distances  <- st_distance(sites)
distances2 <- as.matrix(distances)
diag(distances2) <- 999
duplicate_sites <- which(distances2 < units::as_units(1, "m"))

# - no duplicate sites 

# - drop sites with very few species 
data6[, richness := uniqueN(lowest.taxon), by = "gr_sample_id"]
data7 <- data6[richness > 10]

# - drop sites far removed from ECRINS river network 
data8 <- data7[distance < 300]
rl <- readRDS("data/diatoms/original_data/germany_STAR/2022-01-18_remove_list.rds")
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
saveRDS(updated_type, paste0("data/diatoms/original_data/germany_STAR/", Sys.Date(), "_updated_type.rds"))
updated_type <- readRDS("data/diatoms/original_data/germany_STAR/2022-06-11_updated_type.rds")
# - join updated types to data
data9 <- left_join(data8, 
                   updated_type, 
                   by = "site_id")

# - drop "drop" rows determined in for-loop
data9 <- data9[new_type != "drop"]
data9[, brt12 := NULL]
data9 <- rename(data9, brt12 = new_type)

# temporal aggregation --------------------------------------------------------------
agg <- data9 |> unique(by = "gr_sample_id")
unique(table(agg$site_id))
data10 <- data9

# save to file ----------------------------------------------------------------------
saveRDS(data10, paste0("data/diatoms/original_data/germany_STAR/",Sys.Date(),"_final_aggregated.rds"))

# statistics -------------------------------------------------------------------------

# time span
summary(data9$year)
# all sites and samples
uniqueN(data5$site_id)
uniqueN(data5$gr_sample_id)
# least impacted sites and samples
uniqueN(data6$site_id)
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

