# -- Combine data sets 
# --- diatoms 


# created: 08-12-21
# modified: 18-05-22
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: Combine diatom data


# SETUP -----------------------------------------------------------------------------
pacman::p_load(data.table, dplyr, fs, magrittr, sf, stringr, tmap)
options("box.path" = "~/R/box_modules/")
box::use(box/box1)
# LOAD DATA -------------------------------------------------------------------------

## list of all data sets 
data.sets <- dir_ls("data/diatoms/original_data", type = "directory", regexp = "pre_", invert = TRUE)
## At this point several data sets are omitted from the analysis. 
data.sets <- data.sets[- which(data.sets == "data/diatoms/original_data/germany_saxony_anhalt")]
data.sets <- data.sets[- which(data.sets == "data/diatoms/original_data/germany_WISER")]
data.sets <- data.sets[- which(data.sets == "data/diatoms/original_data/austria")]
data.sets <- data.sets[- which(data.sets == "data/diatoms/original_data/germany_schleswig_holstein")]
data.sets <- data.sets[- which(data.sets == "data/diatoms/original_data/germany_bavaria")]
data.sets <- data.sets[- which(data.sets == "data/diatoms/original_data/germany_nrw")]
data.sets <- data.sets[- which(data.sets == "data/diatoms/original_data/germany_mecklenburg")]
data.sets <- data.sets[- which(data.sets == "data/diatoms/original_data/germany_saarland")]
data.sets <- data.sets[- which(data.sets == "data/diatoms/original_data/romania_cimpean")]
data.sets <- data.sets[- which(data.sets == "data/diatoms/original_data/spain_blanco")]
#data.sets <- data.sets[- which(data.sets == "data/diatoms/original_data/czech_chmi")]
data.sets <- data.sets[- which(data.sets == "data/diatoms/original_data/czech_dry_fishpond")]
data.sets <- data.sets[- which(data.sets == "data/diatoms/original_data/czech_krkonose")]

## loop over all data sets to load them as elements of the list 
data <- list()
for (i in seq_along(data.sets)){
        i.ds <- data.sets[i]
        print(paste("LOADING", str_remove(i.ds, "data/diatoms/original_data/")))
        i.files <- dir_ls(i.ds, regexp = "final_aggregated")
        if(length(i.files) == 0) 
                i.files <- dir_ls(i.ds, regexp = "final_non_aggregated")
        if(length(i.files) == 0)
                next()
        i.x     <- readRDS(i.files)
        setDT(i.x)
        # - drop waterbody column if present 
        if ("waterbody" %in% names(i.x)){
                i.x[, waterbody := NULL]
        }
        data[[i]] <- i.x 
        rm(list = ls()[grepl(x = ls(), pattern = "^i\\.")])
}

# PREPARE DATA ----------------------------------------------------------------------
#- In these next steps, I apply several functions to all elements of the list "data", 
#- i.e. data sets to ensure that they are harmonized. 

#- Make sure all date variables are formatted as such:
data2 <- lapply(data, function(x) x[, date := as.Date(date)])
#- Make data spatial (sf) and transform to common coordinate reference system (LAEA Europe). 
data.st <- lapply(data2, function(x) st_as_sf(x, coords = c("x.coord", "y.coord"), crs = x$EPSG[1]))
data.st <- lapply(data.st, function(x) st_transform(x, crs = 3035))


#- Turn back into data.table to bind rows of list elements 
data2   <- lapply(data.st, setDT)
data2   <- rbindlist(data2, fill = TRUE, use.names = TRUE)
sample_counter <- data2[,uniqueN(gr_sample_id), by = "data.set"]
sample_counter[, after.first.join := V1]
sample_counter[, V1 := NULL]

data2[, c("water_body", "sampling.events", "richness", "brtXbgr", "brtXife") := NULL ]
data2[ brt12 == "\"RT11\"" , brt12 := "RT11"]
data2[ brt12 ==  "\"RT02\"", brt12 := "RT02"]
data2[ brt12 == "\"RT04\"" , brt12 := "RT04"]
data2[ brt12 == "\"RT05\"" , brt12 := "RT05"]
data2[ brt12 == "\"RT07\"" , brt12 := "RT07"]
data2[ brt12 == "RT1"      , brt12 := "RT01"]
data2[ brt12 == "RT8"      , brt12 := "RT08"]
data2[brt12 == "RT2",        brt12 := "RT02"]
data2[brt12 == "RT3",        brt12 := "RT03"]
data2[brt12 == "RT4",        brt12 := "RT04"]
data2[brt12 == "RT5",        brt12 := "RT05"]
data2[brt12 == "RT6",        brt12 := "RT06"]
data2[brt12 == "RT7",        brt12 := "RT07"]
data2[brt12 == "RT9",        brt12 := "RT09"]
sort(data2[, unique(brt12)])
#- Remove data from catchments that are missing in the data from Lemm et al. 2021
data3 <- data2[!is.na(least.impacted)]
drop_id <- c(
        
        "site_00367_date_00200_france_naiades_diatoms", "site_02656_date_00506_france_naiades_diatoms",
        "site_02659_date_00345_france_naiades_diatoms", "site_07256_date_01614_france_naiades_diatoms",
        "site_07340_date_01668_france_naiades_diatoms", "site_07347_date_01663_france_naiades_diatoms",
        "site_08261_date_01025_france_naiades_diatoms", "site_00187_date_00050_france_naiades_diatoms",
        "site_00189_date_00057_france_naiades_diatoms", "site_00191_date_00057_france_naiades_diatoms",
        "site_00205_date_00057_france_naiades_diatoms", "site_00227_date_00120_france_naiades_diatoms",
        "site_00232_date_00120_france_naiades_diatoms", "site_00233_date_00120_france_naiades_diatoms",
        "site_00298_date_00120_france_naiades_diatoms", "site_00300_date_00120_france_naiades_diatoms",
        "site_00507_date_00260_france_naiades_diatoms", "site_01472_date_00322_france_naiades_diatoms",
        "site_02047_date_00686_france_naiades_diatoms", "site_02048_date_00883_france_naiades_diatoms",
        "site_02167_date_00451_france_naiades_diatoms", "site_02593_date_00503_france_naiades_diatoms",
        "site_02649_date_00022_france_naiades_diatoms", "site_02659_date_00182_france_naiades_diatoms",
        "site_02671_date_00028_france_naiades_diatoms", "site_02688_date_00253_france_naiades_diatoms",
        "site_02807_date_00710_france_naiades_diatoms", "site_02810_date_00107_france_naiades_diatoms",
        "site_02813_date_00040_france_naiades_diatoms", "site_02847_date_00010_france_naiades_diatoms",
        "site_02940_date_00917_france_naiades_diatoms", "site_02942_date_00149_france_naiades_diatoms",
        "site_02953_date_00317_france_naiades_diatoms", "site_02955_date_00917_france_naiades_diatoms",
        "site_02971_date_00267_france_naiades_diatoms", "site_03010_date_00018_france_naiades_diatoms",
        "site_03018_date_00030_france_naiades_diatoms", "site_03038_date_00342_france_naiades_diatoms",
        "site_03255_date_00287_france_naiades_diatoms", "site_04245_date_00329_france_naiades_diatoms",
        "site_04430_date_00073_france_naiades_diatoms", "site_04583_date_00022_france_naiades_diatoms",
        "site_04832_date_00012_france_naiades_diatoms", "site_04874_date_00012_france_naiades_diatoms",
        "site_04888_date_00612_france_naiades_diatoms", "site_06197_date_00339_france_naiades_diatoms",
        "site_06228_date_00321_france_naiades_diatoms", "site_06270_date_00012_france_naiades_diatoms",
        "site_06650_date_00458_france_naiades_diatoms", "site_06887_date_00213_france_naiades_diatoms",
        "site_07358_date_00038_france_naiades_diatoms", "site_07834_date_00034_france_naiades_diatoms",
        "site_07859_date_00043_france_naiades_diatoms", "site_07905_date_00528_france_naiades_diatoms",
        "site_00227_date_00121_france_naiades_diatoms", "site_00299_date_00069_france_naiades_diatoms",
        "site_01971_date_00839_france_naiades_diatoms", "site_02714_date_00655_france_naiades_diatoms",
        "site_02764_date_01022_france_naiades_diatoms", "site_02918_date_01015_france_naiades_diatoms",
        "site_04942_date_00922_france_naiades_diatoms", "site_05302_date_01241_france_naiades_diatoms",
        "site_05638_date_00695_france_naiades_diatoms", "site_05870_date_00417_france_naiades_diatoms",
        "site_06032_date_00922_france_naiades_diatoms", "site_06072_date_00751_france_naiades_diatoms",
        "site_06163_date_00864_france_naiades_diatoms", "site_06191_date_01104_france_naiades_diatoms",
        "site_06762_date_01003_france_naiades_diatoms", "site_07076_date_00238_france_naiades_diatoms",
        "site_07111_date_00383_france_naiades_diatoms", "site_08401_date_00925_france_naiades_diatoms",
        
        "site_00010_date_00001_france_irstea_diatoms",
        "site_00012_date_00003_france_irstea_diatoms",
        "site_00287_date_00006_france_irstea_diatoms",
        "site_00292_date_00027_france_irstea_diatoms",
        "site_00296_date_00027_france_irstea_diatoms",
        "site_00919_date_00007_france_irstea_diatoms",
        "site_00969_date_00083_france_irstea_diatoms",
        "site_01715_date_00013_france_irstea_diatoms",
        
        "site_00608_date_00028_germany_brandenburg_diatoms",
        "site_00098_date_00045_germany_brandenburg_diatoms",
        "site_00553_date_00047_germany_brandenburg_diatoms", 
        
        "site_00266_date_00071_germany_baden_wurttemberg_diatoms", 
        
        "site_00012_date_00033_sweden_miljö_diatoms",
        "site_00082_date_00104_sweden_miljö_diatoms",
        "site_00084_date_00099_sweden_miljö_diatoms",
        "site_00085_date_00099_sweden_miljö_diatoms",
        "site_00214_date_00146_sweden_miljö_diatoms",
        "site_00345_date_00164_sweden_miljö_diatoms",
        "site_00374_date_00013_sweden_miljö_diatoms",
        "site_00397_date_00006_sweden_miljö_diatoms",
        "site_00434_date_00001_sweden_miljö_diatoms",
        "site_00457_date_00046_sweden_miljö_diatoms",
        "site_00507_date_00079_sweden_miljö_diatoms",
        "site_00526_date_00171_sweden_miljö_diatoms",
        "site_00567_date_00057_sweden_miljö_diatoms",
        "site_00574_date_00080_sweden_miljö_diatoms",
        "site_00580_date_00166_sweden_miljö_diatoms",
        "site_00583_date_00109_sweden_miljö_diatoms",
        "site_00619_date_00173_sweden_miljö_diatoms",
        "site_00620_date_00207_sweden_miljö_diatoms",
        "site_00621_date_00168_sweden_miljö_diatoms",
        "site_00634_date_00104_sweden_miljö_diatoms",
        "site_00663_date_00002_sweden_miljö_diatoms",
        "site_00697_date_00191_sweden_miljö_diatoms",
        "site_00725_date_00083_sweden_miljö_diatoms",
        "site_00733_date_00104_sweden_miljö_diatoms",
        "site_00966_date_00079_sweden_miljö_diatoms"
                
)

data3 <- data3[!gr_sample_id %in% drop_id]

sample_counter2 <- data3[,uniqueN(gr_sample_id), by = "data.set"]
sample_counter2[, after.removing.outliers := V1]
sample_counter2[, V1 := NULL]
sample_counter <- sample_counter[sample_counter2, on = "data.set"]

data3[,uniqueN(gr_sample_id), by = "data.set"]

#- Neither IFE nor brt12 having missing values ... 
data3[is.na(ife)]
data3[is.na(brt12)]

# unique(data3$brt12)
# data3$data.set |> unique()

# ——— Harmonize Taxonomy ——— # 

#- Here I want to make sure that the taxonomy is harmonized. The taxontable is constantly 
#- evolving so potentially errors can occur if data sets are combined with different
#- versions of the taxontable. To avoid this, I join the data with the most recent version
#- of the taxontable here again. 

#- Load taxontable and drop "clean" variable 
taxontable <- readRDS("data/diatoms/2022-05-17_taxontable_diatoms.rds")
#taxontable[, clean := NULL]

#- Drop taxon variables except "original_name"
data3 %<>% select( - (species:kingdom))
#- Join data and taxontable
data4 <- taxontable[data3, on = "original_name"]

## combine entries of same taxon 
data4[, lowest.taxon := ifelse(!is.na(species), species,
                               ifelse(!is.na(genus), genus,
                                      ifelse(!is.na(family), family,
                                             ifelse(!is.na(order), order,
                                                           ifelse(!is.na(class), class,
                                                                  ifelse(!is.na(phylum), phylum, kingdom))))))]

data4 <- unique(data4, by = c("lowest.taxon", "gr_sample_id"))

# find duplicates -------------------------------------------------------------------
sites <- unique(data4, by = c("data.set", "site_id"))
sites %<>% st_as_sf()
distances <- st_distance(sites)
distances2 <- as.matrix(distances)
diag(distances2) <- 666
identical_points <- which(distances2< units::as_units(1, "m"))

drop_id <- c()
# what data sets are affected? 
for (i in seq_along(identical_points)){
        x <- box1$find_point(identical_points[i], distances2)
        xx <- sites[x, ] |> pull(original_site_name)
        xxx <- data4[original_site_name %in% xx]
        ## are there more than these two samples? 
        if (uniqueN(xxx$gr_sample_id) > 2){
                ## check seasons 
                season.table <- xxx |>  unique(by = "gr_sample_id") %$% table(season)
                
                ## more than one sample in one season
                if (any(season.table > 1)){
                        seas.table.id <- which(season.table > 1)
                        seas.table.id <- names(season.table)[seas.table.id]
                        for (seas in seas.table.id){
                                ## do both samples share one date? 
                                if (xxx[season == seas, uniqueN(date)] == 1){
                                        ## drop the first one 
                                        drop_id <- append(drop_id, xxx[season == seas, unique(gr_sample_id)][1])
                                } else{
                                        older_sample <- xxx[season == seas, min(date)]
                                        drop_id <- append(drop_id, xxx[date == older_sample, unique(gr_sample_id)])
                                }
                        }
                }
                ## homogenize sites 
                site_id <- xxx[, unique(site_id)][1]
                data.set <- xxx[, unique(data.set)][1]
                
                data4[gr_sample_id %in% unique(xxx$gr_sample_id), c("site_id", "data.set") := .(site_id, data.set)]
        ### There are only these two samples for the site         
        } else {
                ### same season? 
                if (uniqueN(xxx$season) == 1){
                        ### same date? 
                        if (uniqueN(xxx$date) == 1){
                                
                                drop_id <- append(drop_id, xxx[, unique(gr_sample_id)][1])   
                                
                        ### different dates        
                        } else {   
                                older_sample <- xxx[, min(date)]
                                drop_id <- append(drop_id, xxx[date == older_sample, unique(gr_sample_id)])
                        }
                ### different seasons         
                } else {  
                        ## homogenize sites 
                        site_id_var  <- xxx[, unique(site_id)][1]
                        data.set_var <- xxx[, unique(data.set)][1]
                        data4[gr_sample_id %in% unique(xxx$gr_sample_id), c("site_id", "data.set") := .(site_id_var, data.set_var)]
                }
        }
}
data4 <- data4[!gr_sample_id %in% drop_id]

sample_counter2 <- data4[,uniqueN(gr_sample_id), by = "data.set"]
sample_counter2[, after.removing.duplicates := V1]
sample_counter2[, V1 := NULL]
sample_counter <- sample_counter[sample_counter2, on = "data.set"]


# ——— Seasons ——— # 
data4[, month := lubridate::month(date)]
data4 <- data4[month %in% 5:9]
data4[, c("month", "least.impacted","distance","site_id", "date_id", "abundance", "original_site_name", "season", "date", "kingdom", "phylum" ,"original_name", "class", "lowest.taxon") := NULL]

data4[, uniqueN(gr_sample_id), by = "data.set"]



# SAVE TO FILE ----------------------------------------------------------------------
saveRDS(data4, paste0("data/diatoms/combined_data/01_",Sys.Date(),"_combined_data_aggregated.rds"))

