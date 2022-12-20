library(data.table)
library(stringr)
library(taxize)


data <- readRDS("data/diatoms/harmonization_lookup_table.rds")
data[species == "Achnanthes impexa", species_update := "Achnanthes impexa"]
data[species == "Gompohonema", c("species_update") := "Gomphonema"]
data[, genus := word(species_update,1)]
TU <- unique(data$genus) |> sort()

for (i in 104:193){
        ##setup 
        if (i == 1){
                out.dt <- data.table(
                        genus = TU,
                        family =   character(193),
                        order =    character(193),
                        subclass = character(193),
                        class =    character(193),
                        phylum =   character(193),
                        kingdom =  character(193)
                )
        }
        
        i.tu <- TU[i]
        i.cl <- classification(i.tu, "gbif")[[1]]
        if ("family"   %in% i.cl[,2]) out.dt[genus == i.tu, family := i.cl[which(i.cl[,2] == "family")  ,1]]
        if ("order"    %in% i.cl[,2]) out.dt[genus == i.tu, order := i.cl[which(i.cl[,2] == "order" )  ,1]]
        if ("subclass" %in% i.cl[,2]) out.dt[genus == i.tu, subclass := i.cl[which(i.cl[,2] == "subclass"),1]]
        if ("class"    %in% i.cl[,2]) out.dt[genus == i.tu, class := i.cl[which(i.cl[,2] == "class" )  ,1]]
        if ("phylum"   %in% i.cl[,2]) out.dt[genus == i.tu, phylum := i.cl[which(i.cl[,2] == "phylum" ) ,1]]
        if ("kingdom"  %in% i.cl[,2]) out.dt[genus == i.tu, kingdom := i.cl[which(i.cl[,2] == "kingdom" ),1]]
        
        
}

out.dt[genus == "Khakista"]
out.dt[kingdom == "Animalia"]

unique(out.dt$phylum)

out.dt[genus == "Amicula", family := "Naviculales incertae sedis"]
out.dt[genus == "Centronella", family := "Fragilariaceae"]
out.dt[genus == "Ctenophora", family := "Ulnariaceae"]

out.dt[genus == "Amicula", order  := "Naviculales"]
out.dt[genus == "Centronella", order := "Fragilariales"]
out.dt[genus == "Ctenophora", order := "Licmophorales"]

out.dt[genus %in% c("Amicula","Centronella" ,"Ctenophora"), class := "Bacillariophyceae"]
out.dt[class == "Bacillariophyceae", phylum := "Bacillariophyta"]
out.dt[class == "Bacillariophyceae", kingdom := "Chromista"]


data <- out.dt[data, on = "genus"]

data <- dplyr::rename(data, "original_name" = "species",
       species = species_update)



saveRDS(data, paste0("data/diatoms/",Sys.Date(),"_taxon_table.rds"))
