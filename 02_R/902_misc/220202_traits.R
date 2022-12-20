### ------------------------- ###
### --- Trait for Diatoms --- ### 
### ------------------------- ###

# -------------------------------
# date written: 02.02.2022
# date last modified:  15.02.2022
# Project: GETREAL
# Purpose: Add Trait information to diatom data 
# Notes: 
# -------------------------------

# TEST 
# BOX 
# Dont Repeat Yourself  
# AUTOMATE 
# DOCUMENT 
# NO BORKEN WINDOWS 
# Design by Contract 
# Fail fast/ defensive programming 

# setup -----------------------------------------------

library(data.table)
library(ggplot2)
library(magrittr)
options("box.path" = "~/R/box_modules/")

box::use(d = dplyr, 
         s = stringr,
         readxl[read_excel],
         stringdist[amatch],
         box/box1)

# load data -------------------------------------------

taxontable <- readRDS("data/diatoms/2022-02-21_taxontable_diatoms.rds")
trait_data <- read_excel("data/traits/Rimet-KMAE-2012-appendix1.xls", sheet = 2)
diatoms    <- readRDS("data/diatoms/combined_data/01_2022-01-25_combined_data_aggregated.rds")

fwb <- readRDS("data/diatoms/fwb_table.rds")

# prepare data ----------------------------------------

## combined all three seasons 
diatoms <- rbindlist(diatoms)

## extract unique taxa from diatom data 
TU <- unique(diatoms$lowest.taxon)

trait_data %<>% d$select(- c('Code Omnidia':'Division', "name + authorities"))
trait_data %<>% d$select(- d$starts_with("Reference"))
trait_data %<>% d$mutate("genus_species" = paste(Genus, Species))

## Not all taxa are directly represented 
all(diatoms$lowest.taxon %in% trait_data$genus_species)

## Create a new column "trait_name" in diatoms. This column holds the name by which the
## taxon will be assigend traits.
diatoms$trait_name = character()
diatoms[lowest.taxon %in% trait_data$genus_species, trait_name := lowest.taxon]

TU <- 
        diatoms[is.na(trait_name), unique(lowest.taxon)] |> 
        sort()

setDT(trait_data)

trait_data[genus_species == "Cymbella affinis"]

## look for most similar name 
strdist_id  <- amatch(TU, trait_data$genus_species, maxDist = 100000)
strdist_tbl <- 
        data.table(taxontable_name = trait_data$genus_species[strdist_id], TU) |> 
        d$filter(!is.na(taxontable_name)) 

for (i in 1:nrow(strdist_tbl)){
        
        print (paste("old:", strdist_tbl[i, TU]))
        print (paste("new:", strdist_tbl[i, taxontable_name]))
        i.bool <- readline()
        if (i.bool == "y"){
                diatoms[lowest.taxon == strdist_tbl[i, TU], trait_name := strdist_tbl[i, taxontable_name]]
        }
        rm(list = ls()[grepl("^i\\.", ls())])
}

## errors 
saveRDS(diatoms, "R/misc/220215_quicksave_traits.rds")
taxontable[fixed_name == "Nitzschia heufleuriana", c("fixed_name", "species") := "Nitzschia heufleriana"]
taxontable[fixed_name == "Nitzschia homburgensis", c("fixed_name", "species") := "Nitzschia homburgiensis"]
taxontable[fixed_name == "Eolimna minima-seminulum-atomoidess", c("fixed_name", "species") := "Eolimna minima-seminulum-atomoides"]
taxontable[fixed_name == "Eunotia Complex", species := NA]
taxontable[fixed_name == "Kobayasiella Complex", species := NA]
taxontable[fixed_name == "Luticola Complex", species := NA]
taxontable[fixed_name == "Mayamaea Complex", species := NA]
taxontable[fixed_name == "Placoneis Complex", species := NA]

saveRDS(taxontable, paste0("data/diatoms/",Sys.Date(),"_taxontable_diatoms.rds"))


## extract taxa that are still missing 
TU <- 
        diatoms[is.na(trait_name), unique(lowest.taxon)] |> 
        sort()

## look for taxa that are in complexes manually 
setorderv(trait_data, "genus_species")
trait_data[s$str_detect(genus_species, "Synedra ")]

diatoms[lowest.taxon == "Achnanthes acares/ricula/carissima", trait_name := "Achnanthes acares"]
diatoms[lowest.taxon == "Achnanthidium exigua/ziegleri/subexigua", trait_name := "Achnanthes exigua"]
diatoms[lowest.taxon == "Cymbopleura angustata-descripta++", trait_name := "Cymbopleura sp."]
diatoms[lowest.taxon == "Encyonema gracile-luna", trait_name := "Encyonema lunatum"]
diatoms[lowest.taxon == "Eolimna minima-seminulum-atomoides", trait_name := "Eolimna minima"]
diatoms[lowest.taxon == "Fragilaria construens-pseudoconstruens", trait_name := "Fragilaria construens"]
diatoms[lowest.taxon == "Navicula cari-recens-erifuga++", trait_name := "Navicula cari"]
diatoms[lowest.taxon == "Navicula tenelloides-salinicola-incertata++", trait_name := "Navicula tenelloides"]
diatoms[lowest.taxon == "Nitzschia pura-linearis Complex"  , trait_name := "Nitzschia pura"]
diatoms[lowest.taxon == "Synedra cyclopum/Hannaea arcus", trait_name := "Synedra fasciculata"]

TU.complex = TU[which(s$str_detect(TU, "Complex"))]
fwb[taxon_new == "Staurosirella leptostauron complex", taxon_old]
trait_data[s$str_detect(genus_species, "Staurosirella"), genus_species]

diatoms[lowest.taxon == "Aulacoseira distans complex", trait_name := "Aulacoseira alpigena"]
diatoms[lowest.taxon == "Aulacoseira italica complex", trait_name := "Aulacoseira sp."]
diatoms[lowest.taxon == "Aulacoseira lirata complex" , trait_name := "Aulacoseira sp."]
diatoms[lowest.taxon == "Cymbopleura incerta complex", trait_name := "Cymbopleura sp."]
diatoms[lowest.taxon == "Gomphonema constrictum complex" , trait_name := "Gomphonema capitatum"]
diatoms[lowest.taxon == "Lindavia bodanica complex", trait_name := "Cyclotella distinguenda-unipunctata" ]
diatoms[lowest.taxon == "Lindavia rossii complex" , trait_name := "Cyclotella rossii"]
diatoms[lowest.taxon == "Pinnularia divergens complex", trait_name := "Pinnularia divergentissima"]
diatoms[lowest.taxon == "Pinnularia viridis complex" , trait_name := "Pinnularia viridis"]
diatoms[lowest.taxon == "Stauroneis complex small", trait_name := "Stauroneis thermicola"]
diatoms[lowest.taxon == "Stauroneis complex small capitate" , trait_name := "Stauroneis kriegeri"]
diatoms[lowest.taxon == "Staurosirella leptostauron complex", trait_name := "Staurosirella martyi"]
diatoms[lowest.taxon == "Chamaepinnularia soehrensis Complex", trait_name := "Chamaepinnularia submuscicola"]
diatoms[lowest.taxon == "Eunotia parallela Complex", trait_name := "Eunotia sp."]
diatoms[lowest.taxon == "Eunotia serra Complex", trait_name := "Eunotia sp."]
diatoms[lowest.taxon == "Gomphonema micropus Complex", trait_name := "Gomphonema micropus"]
diatoms[lowest.taxon == "Neidium iridis Complex", trait_name := "Neidium ampliatum"]
diatoms[lowest.taxon == "Nitzschia acicularis Complex", trait_name := "Nitzschia acicularis"]
diatoms[lowest.taxon == "Nitzschia bavarica Complex", trait_name := "Nitzschia sp."]
diatoms[lowest.taxon == "Nitzschia sigma Complex", trait_name := "Nitzschia sigma"]
diatoms[lowest.taxon == "Pinnularia abaujensis Complex", trait_name := "Pinnularia sp."]
diatoms[lowest.taxon == "Pinnularia biceps Complex", trait_name := "Pinnularia sp."]
diatoms[lowest.taxon == "Pinnularia interrupta Complex", trait_name := "Pinnularia sp."]
diatoms[lowest.taxon == "Pinnularia maior Complex", trait_name := "Pinnularia sp."]
diatoms[lowest.taxon == "Pinnularia mesolepta Complex", trait_name := "Pinnularia sp."]
diatoms[lowest.taxon == "Pinnularia nodosa Complex", trait_name := "Pinnularia sp."]
diatoms[lowest.taxon == "Sellaphora submuralis Complex", trait_name := "Sellaphora sp."]
diatoms[lowest.taxon == "Stauroneis amphicephala Complex", trait_name := "Stauroneis sp."]
diatoms[lowest.taxon == "Tabellaria fenestrata Complex", trait_name := "Tabellaria sp."]
diatoms[lowest.taxon == "Tabellaria quadriseptata Complex", trait_name := "Tabellaria sp."]


## extract taxa that are still missing 
TU <- 
        diatoms[is.na(trait_name), unique(lowest.taxon)] |> 
        sort()


for (i in seq_along(TU)){
        print(paste(i, "/", length(TU)))
        ## extract taxon for this loop 
        i.tu <- TU[i]
        
        # ## what taxonomic resolution is it at? 
        i.bool1 <- all(diatoms[lowest.taxon == i.tu, species == lowest.taxon])
        if (is.na(i.bool1)) i.bool1 <-  FALSE
        i.bool2 <- all(diatoms[lowest.taxon == i.tu, genus == lowest.taxon])
        if (is.na(i.bool2)) i.bool2 <-  FALSE
        i.bool3 <- all(diatoms[lowest.taxon == i.tu, family == lowest.taxon])
        if (is.na(i.bool3)) i.bool3 <-  FALSE
        i.bool4 <- all(diatoms[lowest.taxon == i.tu, order == lowest.taxon])
        if (is.na(i.bool4)) i.bool4 <-  FALSE
        
        i.bool.res <- d$case_when(i.bool1 ~ 1, i.bool2 ~ 2, i.bool3 ~ 3, i.bool4 ~ 4)
        
        ## check current level 
        if (i.bool.res == 1){
                
                if (i.tu %in% trait_data$genus_species){
                        i.trait <- trait_data[genus_species == i.tu]
                } 
        }
        if (i.bool.res == 2){
                
                if (i.tu %in% trait_data$Genus){
                        i.trait <- trait_data[Genus == i.tu]
                        ## check for species level entry
                        if (any(i.trait$Species == "sp.")){
                                sp.id <- which(i.trait$Species == "sp.")
                                i.trait <- i.trait[sp.id,]
                                i.trait <- unique(i.trait, by = "Genus")
                        }
                }
                
        }
        if (i.bool.res == 3){
                
                if (i.tu %in% trait_data$family){
                        i.trait <- trait_data[family == i.tu]
                }
                
        }
        if (i.bool.res == 4){
                
                if (i.tu %in% trait_data$order){
                        i.trait <- trait_data[order == i.tu]
                }
                
        }
        
        ## next highest taxon 
        i.tu.nxt <- 
                d$case_when(
                        i.bool.res == 1 ~ unique(diatoms[lowest.taxon == i.tu, genus]),
                        i.bool.res == 2 ~ unique(diatoms[lowest.taxon == i.tu, family]),
                        i.bool.res == 3 ~ unique(diatoms[lowest.taxon == i.tu, order]),
                        i.bool.res == 4 ~ unique(diatoms[lowest.taxon == i.tu, class]),
                )
        
        ## do we already have a i.trait in this run? 
        i.traitmissing <- (!"i.trait" %in% ls())
        
        if (i.bool.res == 1 & i.traitmissing){
                i.check.gen <- i.tu.nxt %in% trait_data$Genus  
                if (!i.check.gen) {
                        i.bool.res = 2 
                        i.tu.nxt <- unique(diatoms[lowest.taxon == i.tu, family])
                } else {
                        i.trait <- trait_data[Genus == i.tu.nxt]
                        if (any(i.trait$Species == "sp.")){
                                sp.id <- which(i.trait$Species == "sp.")
                                i.trait <- i.trait[sp.id,]
                                i.trait <- unique(i.trait, by = "Genus")
                        }
                }
        }
        if (i.bool.res == 2 & i.traitmissing) {
                i.check.fam <- i.tu.nxt %in% trait_data$Family
                
                if (!i.check.fam & i.traitmissing) {
                        i.bool.res = 3
                        i.tu.nxt <-
                                unique(diatoms[lowest.taxon == i.tu, order])
                }  else {
                        i.trait <- trait_data[Family == i.tu.nxt]
                }
        }
        if (i.bool.res == 3 & i.traitmissing){
        
                i.check.ord <- i.tu.nxt %in% trait_data$Order  

                if (!i.check.ord) {
                        i.bool.res = 4
                        i.tu.nxt <- unique(diatoms[lowest.taxon == i.tu, class])
                } else {
                        i.trait <- trait_data[Order == i.tu.nxt]
                }
        }
        if (i.bool.res == 4 & i.traitmissing){
                print(paste(i.tu, "only available at class level"))
        }
        
        ## now add i.trait to trait data 
        
        ## First however, skip if taxon is still missing 
        if (!"i.trait" %in% ls()){
                rm(list = ls()[grepl(pattern = "^i\\.", x = ls())])
                next()
        }
                
        
        ## if there is only one row, attach it with new "taxa_adjusted" column. 
        if (nrow(i.trait) == 1){
                diatoms[lowest.taxon == i.tu, trait_name := i.trait$genus_species]
        } else {
                
                i.trait2 <- copy(i.trait)
                ## drop non-trait columns
                i.trait2[, c(1:9, 36) := NULL]
                ## numerical traits
                i.trait3 <- copy(i.trait2)
                i.trait3 <- i.trait3[, 1:4]
                i.trait3 <- apply(i.trait3, 2, mean, na.rm = T)
                i.trait3 <-
                        i.trait3 |> data.frame() |> t() |> data.table()

                ## categorical traits
                i.trait4 <- copy(i.trait2)
                i.trait4 <- i.trait4[, -(1:4)]
                i.trait4 <- apply(i.trait4, 2, box1$vector_mode)
                i.trait4 <- i.trait4 |> data.frame() |> t() |>  data.table()

                i.trait5 <- setDT(
                        unlist(list(i.trait3, i.trait4), recursive = FALSE),
                        check.names = TRUE
                )
                i.trait5[, c("Subdivision","Class", "Order","Family", "Genus","Species", "infra sp1","infra sp2", "genus + species + var") := NA]
                i.trait5[, genus_species := i.tu]

                i.trait5 <-
                        i.trait5 |>
                        d$relocate( "genus + species + var",   .before = "length..µm.") |>
                        d$relocate("infra sp2",   .before = "genus + species + var") |>
                        d$relocate("infra sp1",   .before = "infra sp2") |>
                        d$relocate(Species,       .before = "infra sp1") |>
                        d$relocate(Genus,         .before = Species) |>
                        d$relocate(Family,        .before = Genus) |>
                        d$relocate(Order,         .before = Family) |>
                        d$relocate(Class,         .before = Order) |>
                        d$relocate(Subdivision,   .before = Class)
                
                names(i.trait5) <- names(i.trait)

                trait_data <- rbind(trait_data, i.trait5)
        }
        
        
        ## clean 
        rm(list = ls()[grepl(pattern = "^i\\.", x = ls())])
}
rm(i)

## update diatoms as trait_data has new genus_species entries. 
diatoms[lowest.taxon %in% trait_data$genus_species, trait_name := lowest.taxon]

## several taxa are only available at class level and are thus omitted: Brevilinea;
#Brevilinea kevei, Chaetoceros, Cymatosira belgica, Delphineis, Delphineis minutissima,
#Myriactula pulvinata, Navigeia tagensis, Orthoseira, Rhaphoneis amphiceros, Rhaphoneis
#surirelloides, Rhizosolenia, Thalassionema

diatoms[is.na(trait_name), unique(lowest.taxon)]
diatoms <- diatoms[!is.na(trait_name)]

## rename trait_name back to genus species for join. 
diatoms <- d$rename(diatoms, genus_species = trait_name)
## sefety copy 
trait_data2 <- copy(trait_data)
## unique entry in trait_data2 for each genus_species entry. Multiple entries would cause an 
## error. 
trait_data2 <- unique(trait_data2, by = "genus_species")
## join trait data and diatom data 
diatom2 <- trait_data2[diatoms, on = c("genus_species")]

diatom2[genus_species == "Cymbella affinis"]

# drop columns 
diatom2[, c("water_body", "sampling.events",  "richness", "bgr", "ife",
            "distance", "brtXife", "brtXbgr", "Subdivision","Class", "Order", "Family", 
            "Genus","Species", "infra sp1", "infra sp2", "genus + species + var") := NULL]

# rename columns
diatom2 <- d$rename(
        diatom2,
        length =  "length (µm)" ,
        width =  "width (µm)" ,
        thickness =  "thickness (µm)" ,
        biovolume =  "Biovolume (µm3)"
)

saveRDS(diatom2, "data/diatoms/220224_dfm_traits.rds")


## summarize categorical variables 

diatom2[, guild := d$case_when(`High profile guild` == 1 ~ "high", 
                               `Low profile guild` == 1 ~ "low", 
                               `Motile guild` == 1 ~ "motile",
                               Planktonic == 1 ~ "planktonic")]


diatom2$brt12 |> unique()
diatom2[brt12 == "\"RT11\"", brt12 := "RT11"]
diatom2[brt12 == "\"RT02\"", brt12 := "RT02"]
diatom2[brt12 == "\"RT04\"", brt12 := "RT04"]
diatom2[brt12 == "\"RT07\"", brt12 := "RT07"]
diatom2[brt12 == "\"RT05\"", brt12 := "RT05"]
diatom2[brt12 == "\"RT03\"", brt12 := "RT03"]
diatom2[brt12 == "\"RT06\"", brt12 := "RT06"]
diatom2[brt12 == "\"RT10\"", brt12 := "RT10"]
diatom2[brt12 == "\"RT01\"", brt12 := "RT01"]
diatom2[brt12 == "\"RT08\"", brt12 := "RT08"]
diatom2[brt12 == "\"RT09\"", brt12 := "RT09"]

diatom2[, Planktonic := as.numeric(Planktonic)]


# analyze --------------------------------------------

#prelim plots 


length_data <- diatom2 |> 
        d$select(-geometry) |> 
        d$group_by(gr_sample_id, brt12) |> 
        d$summarise(mean_value = median(length, na.rm = TRUE))
volume_data <- diatom2 |> 
        d$select(-geometry) |> 
        d$group_by(gr_sample_id, brt12) |> 
        d$summarise(mean_value = median(biovolume, na.rm = TRUE))
        d$summarise(mean_value = median(length, na.rm = TRUE))
volume_data <- diatom2 |> 
        d$select(-geometry) |> 
        d$group_by(gr_sample_id, brt12) |> 
        d$summarise(mean_value = mean(Planktonic, na.rm = TRUE))

#length_data |> 
volume_data |> 
        ggplot(aes(x = brt12, y = mean_value)) + 
        geom_boxplot()
        #geom_violin(aes(fill = brt12), draw_quantiles = c(0.25, .5, .75)) # +
        stat_summary(geom = "point", fun = mean, na.rm = T) 

saveRDS(diatom2, "data/diatoms/220215_dfm_traits.rds")
