# -- Combine data sets 
# -- Macrophytes 

# date created: 09.12.21
# date last modified: 14.06.22
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: Combine macrophyte data

# SETUP -----------------------------------------------------------------------------
pacman::p_load(data.table, dplyr, fs, magrittr, sf, stringr, tmap)

# LOAD DATA -------------------------------------------------------------------------

taxontable <- readRDS("data/macrophytes/2022-06-14_taxontable_macrophytes.rds")

## list of all data sets 
data.sets <- dir_ls("data/macrophytes/original_data", type = "directory", regexp = "pre_", invert = TRUE)
## At this point several data sets are omitted from the analysis. 
data.sets <- data.sets[-which(data.sets == "data/macrophytes/original_data/germany_saarland")]
#data.sets <- data.sets[-which(data.sets == "data/macrophytes/original_data/hungary_ecosurv")]
data.sets <- data.sets[-which(data.sets == "data/macrophytes/original_data/germany_hesse")]
data.sets <- data.sets[-which(data.sets == "data/macrophytes/original_data/germany_nrw")]
#data.sets <- data.sets[-which(data.sets == "data/macrophytes/original_data/czech_chmi")]
#data.sets <- data.sets[-which(data.sets == "data/macrophytes/original_data/denmark_monitoring")]

data      <- list()
## loop over all (currently 5) data sets to load them as elements of the list 
## "data"
for (i in seq_along(data.sets)){
        i.ds <- data.sets[i]
        print(paste("LOADING", str_remove(i.ds, "data/macrophytes/original_data/")))
        i.files <- dir_ls(i.ds, regexp = "final_aggregated")
        if(length(i.files) == 0) 
                i.files <- dir_ls(i.ds, regexp = "final_non_aggregated")
        if(length(i.files) == 0)
                next()
        i.x     <- readRDS(i.files)
        data[[i]] <- i.x 
        rm(list = ls()[grepl(x = ls(), pattern = "^i\\.")])
}


# PREPARE DATA ----------------------------------------------------------------------
#- In these next steps, I apply several functions to all elements of the list "data", 
#- i.e. data sets to ensure that they are harmonized. 

# unlist(lapply(data, class)) |> 
#         matrix(ncol = 2)
# for (i in 1:20) print(paste0(i,class(data[[i]])))
# 
# data[[3]] <- setDT(data[[3]])
# 
# data[[12]]
# data.sets[[12]]

#- Make sure all date variables are formatted as such:
data2    <- lapply(data, function(x) x[, date := as.Date(date)])
lapply(data2, function(x) unique(x$EPSG))
#- Make data spatial (sf) and transform to common coordinate reference system (LAEA Europe). 
data.st <- lapply(data2, function(x) st_as_sf(x, coords = c("x.coord", "y.coord"), crs = x$EPSG[1]))
data.st <- lapply(data.st, function(x) st_transform(x, crs = 3035))
#- Turn back into data.table to bind rows of list elements 
data2   <- lapply(data.st, setDT)
data2   <- rbindlist(data2, fill = TRUE, use.names = TRUE)

#- Remove data from catchments that are missing in the data from Lemm et al. 2021
data3 <- data2[!is.na(least.impacted)]

sample_counter <- data2[,uniqueN(gr_sample_id), by = "data.set"]
sample_counter[, after.first.join := V1]
sample_counter[, V1 := NULL]

# Remove sites that were identified as outliers in PCCs 
drop_id <- 
        c(
                "site_00131_date_00033_denmark_monitoring_macrophytes",
                
                "site_00232_date_00263_germany_bavaria_macrophytes",
                "site_00376_date_00355_germany_bavaria_macrophytes",
                "site_00528_date_00419_germany_bavaria_macrophytes",
                "site_00753_date_00508_germany_bavaria_macrophytes",
                "site_00157_date_00058_germany_bavaria_macrophytes",
                "site_00241_date_00207_germany_bavaria_macrophytes",
                "site_00247_date_00122_germany_bavaria_macrophytes",
                "site_00511_date_00058_germany_bavaria_macrophytes",
                "site_00512_date_00008_germany_bavaria_macrophytes",
                "site_00549_date_00006_germany_bavaria_macrophytes",
                "site_00661_date_00193_germany_bavaria_macrophytes",
                "site_00662_date_00114_germany_bavaria_macrophytes",
                "site_00792_date_00398_germany_bavaria_macrophytes",
                "site_00794_date_00398_germany_bavaria_macrophytes",
                "site_00795_date_00169_germany_bavaria_macrophytes",
                "site_00801_date_00339_germany_bavaria_macrophytes",
                "site_00238_date_00268_germany_bavaria_macrophytes",
                "site_00376_date_00354_germany_bavaria_macrophytes",
                "site_00789_date_00144_germany_bavaria_macrophytes", 
                "site_00789_date_00400_germany_bavaria_macrophytes",
                "site_00187_date_00064_germany_bavaria_macrophytes", 
                "site_00528_date_00057_germany_bavaria_macrophytes",
                
                "site_00430_date_00030_uk_ee_macrophytes", 
                "site_00431_date_00036_uk_ee_macrophytes", 
                
                "site_01119_date_00008_france_naiades_macrophytes",
                "site_01458_date_00010_france_naiades_macrophytes",
                "site_01566_date_00524_france_naiades_macrophytes",
                "site_01450_date_00204_france_naiades_macrophytes",
                "site_01466_date_00153_france_naiades_macrophytes",
                
                "site_00016_date_00008_germany_brandenburg_macrophytes", 
                "site_00065_date_00005_germany_brandenburg_macrophytes", 
                "site_00100_date_00017_germany_brandenburg_macrophytes", 
                "site_00328_date_00045_germany_brandenburg_macrophytes", 
                "site_00611_date_00077_germany_brandenburg_macrophytes", 
                "site_00297_date_00086_germany_brandenburg_macrophytes", 
                
                "site_00003_date_00002_germany_lowersaxony_macrophytes",
                "site_00171_date_00059_germany_lowersaxony_macrophytes",
                "site_00214_date_00061_germany_lowersaxony_macrophytes",
                "site_01224_date_00284_germany_lowersaxony_macrophytes",
                "site_00325_date_00441_germany_lowersaxony_macrophytes",
                "site_01349_date_00490_germany_lowersaxony_macrophytes",
                "site_01391_date_00473_germany_lowersaxony_macrophytes",
                "site_00973_date_00241_germany_lowersaxony_macrophytes",
                "site_01599_date_00571_germany_lowersaxony_macrophytes", 
                "site_01278_date_00436_germany_lowersaxony_macrophytes",
                
                "site_00158_date_00060_germany_mecklenburg_macrophytes",
                "site_00159_date_00059_germany_mecklenburg_macrophytes", 
                
                "site_00021_date_00015_germany_saxony_macrophytes",
                "site_00035_date_00015_germany_saxony_macrophytes",
                "site_00441_date_00159_germany_saxony_macrophytes", 
                
                "site_00138_date_00053_germany_schleswig_holstein_macrophytes",
                "site_00195_date_00047_germany_schleswig_holstein_macrophytes", 
                
                "site_01178_date_00369_neherlands_monitoring_macrophytes", 
                
                "site_00346_date_00001_slovakia_monitoring_macrophytes",
                "site_00349_date_00001_slovakia_monitoring_macrophytes",
                "site_00359_date_00001_slovakia_monitoring_macrophytes",
                
                "site_00006_date_00002_spain_duero_macrophytes",
                "site_00059_date_00097_spain_duero_macrophytes",
                "site_00067_date_00065_spain_duero_macrophytes",
                "site_00107_date_00156_spain_duero_macrophytes",
                "site_00115_date_00038_spain_duero_macrophytes",
                "site_00128_date_00163_spain_duero_macrophytes",
                "site_00132_date_00163_spain_duero_macrophytes",
                "site_00133_date_00047_spain_duero_macrophytes",
                "site_00186_date_00235_spain_duero_macrophytes",
                "site_00219_date_00061_spain_duero_macrophytes",
                "site_00234_date_00074_spain_duero_macrophytes",
                "site_00244_date_00090_spain_duero_macrophytes",
                "site_00260_date_00008_spain_duero_macrophytes",
                "site_00261_date_00235_spain_duero_macrophytes",
                "site_00262_date_00008_spain_duero_macrophytes",
                "site_00264_date_00097_spain_duero_macrophytes",
                "site_00266_date_00097_spain_duero_macrophytes",
                "site_00278_date_00102_spain_duero_macrophytes",
                "site_00292_date_00102_spain_duero_macrophytes",
                "site_00296_date_00053_spain_duero_macrophytes",
                "site_00302_date_00043_spain_duero_macrophytes",
                "site_00303_date_00235_spain_duero_macrophytes",
                "site_00305_date_00092_spain_duero_macrophytes",
                "site_00314_date_00090_spain_duero_macrophytes",
                "site_00339_date_00092_spain_duero_macrophytes",
                "site_00340_date_00002_spain_duero_macrophytes",
                "site_00394_date_00083_spain_duero_macrophytes",
                "site_00412_date_00092_spain_duero_macrophytes",
                "site_00418_date_00047_spain_duero_macrophytes",
                "site_00033_date_00058_spain_duero_macrophytes",
                "site_00097_date_00144_spain_duero_macrophytes",
                "site_00187_date_00237_spain_duero_macrophytes",
                "site_00007_date_00505_spain_duero_macrophytes",
                "site_00472_date_00621_spain_duero_macrophytes",
                "site_00139_date_00624_spain_duero_macrophytes",
                "site_00477_date_00631_spain_duero_macrophytes",
                "site_00202_date_00675_spain_duero_macrophytes",
                "site_00208_date_00681_spain_duero_macrophytes",
                "site_00342_date_00806_spain_duero_macrophytes",
                "site_00535_date_00818_spain_duero_macrophytes",
                "site_00414_date_00862_spain_duero_macrophytes",
                "site_00017_date_00027_spain_duero_macrophytes",
                "site_00007_date_00046_spain_duero_macrophytes",
                "site_00068_date_00109_spain_duero_macrophytes",
                "site_00094_date_00141_spain_duero_macrophytes",
                "site_00095_date_00072_spain_duero_macrophytes",
                "site_00102_date_00150_spain_duero_macrophytes",
                "site_00129_date_00178_spain_duero_macrophytes",
                "site_00144_date_00194_spain_duero_macrophytes",
                "site_00155_date_00207_spain_duero_macrophytes",
                "site_00179_date_00230_spain_duero_macrophytes",
                "site_00187_date_00236_spain_duero_macrophytes",
                "site_00193_date_00241_spain_duero_macrophytes",
                "site_00194_date_00243_spain_duero_macrophytes",
                "site_00202_date_00255_spain_duero_macrophytes",
                "site_00253_date_00319_spain_duero_macrophytes",
                "site_00267_date_00339_spain_duero_macrophytes",
                "site_00268_date_00253_spain_duero_macrophytes",
                "site_00307_date_00093_spain_duero_macrophytes",
                "site_00310_date_00390_spain_duero_macrophytes",
                "site_00329_date_00141_spain_duero_macrophytes",
                "site_00139_date_00190_spain_duero_macrophytes",
                "site_00194_date_00244_spain_duero_macrophytes",
                "site_00023_date_00501_spain_duero_macrophytes",
                "site_00436_date_00533_spain_duero_macrophytes",
                "site_00442_date_00550_spain_duero_macrophytes",
                "site_00068_date_00552_spain_duero_macrophytes",
                "site_00448_date_00559_spain_duero_macrophytes",
                "site_00449_date_00560_spain_duero_macrophytes",
                "site_00095_date_00590_spain_duero_macrophytes",
                "site_00109_date_00600_spain_duero_macrophytes",
                "site_00466_date_00605_spain_duero_macrophytes",
                "site_00117_date_00609_spain_duero_macrophytes",
                "site_00144_date_00626_spain_duero_macrophytes",
                "site_00267_date_00729_spain_duero_macrophytes",
                "site_00520_date_00760_spain_duero_macrophytes",
                "site_00548_date_00586_spain_duero_macrophytes", 
                "site_00375_date_00053_spain_duero_macrophytes", 
                "site_00336_date_00411_spain_duero_macrophytes", 
                "site_00429_date_00520_spain_duero_macrophytes", 
                
                "site_00099_date_00245_spain_ebro_macrophytes",
                "site_00204_date_00383_spain_ebro_macrophytes",
                "site_00267_date_00245_spain_ebro_macrophytes",
                "site_00348_date_00353_spain_ebro_macrophytes",
                "site_00356_date_00450_spain_ebro_macrophytes",
                "site_00301_date_00385_spain_ebro_macrophytes",
                "site_00036_date_00087_spain_ebro_macrophytes",
                "site_00120_date_00270_spain_ebro_macrophytes",
                "site_00142_date_00296_spain_ebro_macrophytes",
                "site_00147_date_00043_spain_ebro_macrophytes",
                "site_00154_date_00269_spain_ebro_macrophytes",
                "site_00164_date_00318_spain_ebro_macrophytes",
                "site_00166_date_00330_spain_ebro_macrophytes",
                "site_00177_date_00339_spain_ebro_macrophytes",
                "site_00186_date_00026_spain_ebro_macrophytes",
                "site_00010_date_00365_spain_ebro_macrophytes",
                "site_00204_date_00382_spain_ebro_macrophytes",
                "site_00231_date_00280_spain_ebro_macrophytes",
                "site_00261_date_00430_spain_ebro_macrophytes",
                "site_00268_date_00268_spain_ebro_macrophytes",
                "site_00270_date_00346_spain_ebro_macrophytes",
                "site_00276_date_00441_spain_ebro_macrophytes",
                "site_00279_date_00111_spain_ebro_macrophytes",
                "site_00316_date_00068_spain_ebro_macrophytes",
                "site_00343_date_00201_spain_ebro_macrophytes",
                "site_00301_date_00359_spain_ebro_macrophytes",
                "site_00006_date_00016_spain_ebro_macrophytes",
                "site_00066_date_00198_spain_ebro_macrophytes",
                "site_00099_date_00036_spain_ebro_macrophytes",
                "site_00123_date_00274_spain_ebro_macrophytes",
                "site_00154_date_00060_spain_ebro_macrophytes",
                "site_00164_date_00324_spain_ebro_macrophytes",
                "site_00167_date_00198_spain_ebro_macrophytes",
                "site_00240_date_00414_spain_ebro_macrophytes",
                "site_00261_date_00429_spain_ebro_macrophytes",
                "site_00276_date_00440_spain_ebro_macrophytes",
                "site_00278_date_00121_spain_ebro_macrophytes", 
                "site_00155_date_00315_spain_ebro_macrophytes", 
                
                "site_00623_date_00107_poland_monitoring_macrophytes",
                "site_00140_date_00288_poland_monitoring_macrophytes", 
                "site_00749_date_00156_poland_monitoring_macrophytes",
                "site_00117_date_00049_poland_monitoring_macrophytes"
        )
data3 <- data3[!gr_sample_id %in% drop_id]

sample_counter2 <- data3[,uniqueN(gr_sample_id), by = "data.set"]
sample_counter2[, after.removing.outliers := V1]
sample_counter2[, V1 := NULL]
sample_counter <- sample_counter[sample_counter2, on = "data.set"]

data3[brt12 =="\"RT06\"", brt12 := "RT06"] 
data3[brt12 =="\"RT01\"", brt12 := "RT01"] 
data3[brt12 =="\"RT11\"", brt12 := "RT11"]
data3[brt12 =="\"RT12\"", brt12 := "RT12"] 
data3[brt12 =="\"RT02\"", brt12 := "RT02"] 
data3[brt12 =="\"RT03\"", brt12 := "RT03"] 
data3[brt12 =="\"RT07\"", brt12 := "RT07"] 
data3[brt12 =="\"RT04\"", brt12 := "RT04"]
data3[brt12 =="\"RT09\"", brt12 := "RT09"]
data3[brt12 ==     "RT1", brt12 := "RT01"]
data3[brt12 == "RT6"  , brt12 := "RT06"]
data3[brt12 == "RT8"  , brt12 := "RT08"]
data3[brt12 == "RT9"  , brt12 := "RT09"]
data3[brt12 == "RT4"  , brt12 := "RT04"]
data3[brt12 == "RT5"  , brt12 := "RT05"]
data3[brt12 == "RT3"  , brt12 := "RT03"]
data3[brt12 == "RT2"  , brt12 := "RT02"]
data3[brt12 == "RT7"  , brt12 := "RT07"]

data3[, unique(brt12)]



# ——— Harmonize Taxonomy ——— # 

#- Here I want to make sure that the taxonomy is harmonized. The taxontable is constantly 
#- evolving so potentially errors can occur if data sets are combined with different
#- versions of the taxontable. To avoid this, I join the data with the most recent version
#- of the taxontable here again. 

#- Load taxontable and drop "clean" variable 

taxontable[, clean := NULL]

#- Drop taxon variables except "original_name"
data3 %<>% select( - (species:kingdom))
#- Join data and taxontable
data4 <- taxontable[data3, on = "original_name"]
data4 <- data4[phylum != "Porifera"]

data4[, unique(taxon_state)]

# - fix typo
data4[taxon_state == "hydrophyte", taxon_state := "hydrophytes" ]

# - in how many data sets do which life forms occur? 
check_taxa <- copy(data4)
check_taxa <- unique(data4, by = c("taxon_state", "data.set"))
table(check_taxa$taxon_state)
which(!data4[, unique(data.set)] %in% data4[taxon_state == "moss", unique(data.set)])
data4[, unique(data.set)][c(4,14)]
# - Denmark and Ecosurv are missing mosses. Hence I drop them here. 
data4 <- data4[!data.set %in% c("denmark_monitoring_macrophytes","hungery_ecosurv_macrophytes")]

data4 <- data4[taxon_state %in% c("hydrophytes", "helophytes", "moss")]

# - check remaining kingdoms 
data4[, unique(kingdom)]
# - count phyla per data set
data4[, uniqueN(phylum), by = "data.set"]
# - All have two or three phyla. 
data4[, uniqueN(data.set), by = "phylum"]
# - the difference is machantiophyta which does not occur in seven of the data sets
## combine entries of same taxon 
data4[, lowest.taxon := ifelse(!is.na(species), species,
                               ifelse(!is.na(genus), genus,
                                      ifelse(!is.na(family), family,
                                             ifelse(!is.na(order), order,
                                                    ifelse(!is.na(subclass), subclass,
                                                           ifelse(!is.na(class), class,
                                                                  ifelse(!is.na(phylum), phylum, kingdom)))))))]


# #- fix duplicate sites 
# data4[site_id %in% c("00380", "00376") & data.set == "germany_brandenburg_macrophytes", `:=` (site_id = "00380", 
#                                                                                               gr_sample_id = "site_00380_date_00050_germany_brandenburg_macrophytes",
#                                                                                               date = lubridate::ymd("2020-08-26"),
#                                                                                               date_id = "00049")]
# data4[site_id %in% c("00385", "00383") & data.set == "germany_brandenburg_macrophytes", `:=` (site_id = "00385", 
#                                                                                               gr_sample_id = "site_00385_date_00053_germany_brandenburg_macrophytes",
#                                                                                               date = lubridate::ymd("2020-08-24"),
#                                                                                               date_id = "00053")]
# data4[site_id %in% c("02069", "02070") & data.set == "uk_ee_macrophytes", `:=` (site_id = "02070")]
# data4[site_id %in% c("00009", "00008") & data.set == "spain_ebro_macrophytes", `:=` (site_id = "00008")]
# data4[site_id %in% c("00189", "00190") & data.set == "spain_ebro_macrophytes", `:=` (site_id = "00189")]
# data4[site_id %in% c("00196", "00195") & data.set == "spain_ebro_macrophytes", `:=` (site_id = "00195")]
# data4[site_id %in% c("00002", "00003") & data.set == "spain_ebro_macrophytes", `:=` (site_id = "00002")]
# data4[site_id %in% c("00100", "00101") & data.set == "spain_ebro_macrophytes", `:=` (site_id = "00100")]
# 
# data4 <- data4[!gr_sample_id %in% c(
#         "site_01401_date_00360_germany_lowersaxony_macrophytes", 
#         "site_01478_date_00420_germany_lowersaxony_macrophytes", 
#         "site_01401_date_00360_germany_lowersaxony_macrophytes",
#         "site_00091_date_00031_germany_mecklenburg_macrophytes",
#         "site_00095_date_00030_germany_mecklenburg_macrophytes",
#         ## -- SPAIN EBRO 
#         "site_00009_date_00028_spain_ebro_macrophytes",
#         "site_00189_date_00362_spain_ebro_macrophytes",
#         "site_00196_date_00371_spain_ebro_macrophytes",
#         "site_00002_date_00010_spain_ebro_macrophytes",
#         "site_00061_date_00089_spain_ebro_macrophytes",
#         "site_00075_date_00206_spain_ebro_macrophytes",
#         "site_00101_date_00197_spain_ebro_macrophytes",
#         "site_00185_date_00206_spain_ebro_macrophytes",
#         ## -- UK EE 
#         "site_01745_date_00169_uk_ee_macrophytes",
#         "site_01989_date_00195_uk_ee_macrophytes",
#         "site_01680_date_00199_uk_ee_macrophytes",
#         "site_01681_date_00193_uk_ee_macrophytes",
#         "site_01761_date_00238_uk_ee_macrophytes"
#                                     )]
# 
# data4 <- data4[!original_site_name %in% c("DOSSE_3")]
# 
# data4 <- unique(data4, by = c("gr_sample_id", "lowest.taxon"))



#- check for dupicate sites 
sites <- unique(data4, by = c("data.set", "site_id"))
sites %<>% st_as_sf()
distances <- st_distance(sites)
distances2 <- as.matrix(distances)
diag(distances2) <- 666
which(distances2< units::as_units(1, "m"))

### ——— remove sites with only one taxon ——— ### 
richness <- copy(data4)
richness[, richness := uniqueN(lowest.taxon), by = "gr_sample_id"]
richness <- unique(richness, by = "gr_sample_id")
table(richness$richness)
drop_id <- richness[richness == 1, unique(gr_sample_id)]
data4 <- data4[! gr_sample_id %in% drop_id]

rm(drop_id, richness)

# ——— Seasons ——— # 
unique(data4$brt12)
data4[, month := lubridate::month(date)]
data4[data.set == "sweden_macrophytes_leo", month := 5]
data4 <- data4[month %in% 5:9]

sample_counter2 <- data4[,uniqueN(gr_sample_id), by = "data.set"]
sample_counter2[, after.focal.month := V1]
sample_counter2[, V1 := NULL]
sample_counter <- sample_counter[sample_counter2, on = "data.set"]

data4[, c("month", "least.impacted","distance","site_id", "date_id", "abundance", "original_site_name", "season", "date", "kingdom", "phylum" ,"original_name", "class", "lowest.taxon", "date.set", "sampling.method", "sampling.events", "subclass") := NULL]

#- new richness
data4[, richness := uniqueN(species), by = "gr_sample_id"]

library(ggplot2)
data4 |> 
        unique(by = "gr_sample_id") |> 
        ggplot(aes(x=data.set, y = richness)) + 
        geom_boxplot()

# SAVE TO FILE ----------------------------------------------------------------------
saveRDS(data4, paste0("data/macrophytes/combined_data/01_",Sys.Date(),"_combined_data_aggregated.rds"))

