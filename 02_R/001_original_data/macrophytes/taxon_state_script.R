## assign taxon state 

# - on phylum level 

taxontable[phylum == "Marchantiophyta", taxon_state := "moss"]
taxontable[phylum == "Bryophyta"      , taxon_state := "moss"]
taxontable[phylum == "Bryozoa"        , taxon_state := "bryozoa"]
taxontable[phylum == "Charophyta"     , taxon_state := "green_algae"]
taxontable[phylum == "Chlorophyta"    , taxon_state := "green_algae"]
taxontable[phylum == "Cyanobacteria"  , taxon_state := "cyanobacteria"]
taxontable[phylum == "Euglenozoa"     , taxon_state := "protozoa"]
taxontable[phylum == "Ochrophyta"     , taxon_state := "yellow_green_algae"]
taxontable[phylum == "Oomycota"       , taxon_state := "Oomycota"]
taxontable[phylum == "Proteobacteria" , taxon_state := "bacteria"]
taxontable[phylum == "Rhodophyta"     , taxon_state := "red_algae"]
taxontable[phylum == "Ascomycota"     , taxon_state := "lichen"]
taxontable[phylum == "Rotifera"       , taxon_state := "Rotifer"]
taxontable[phylum == "Ciliophora"     , taxon_state := "Ciliophora"]
taxontable[phylum == "Cryptophyta"     , taxon_state := "algae"]
taxontable[phylum == "Foraminifera"     , taxon_state := "Foraminifera"]
taxontable[phylum == "Myzozoa"     , taxon_state := "algae"]


#- class 
taxontable[class == "Polypodiopsida", taxon_state := "fern"]
taxontable[class == "Insecta", taxon_state := "Insect"]
taxontable[class == "Polypodiopsida", taxon_state := "fern"]
# - on order level 
taxontable[order == "Verrucariales", taxon_state := "lichen"]
taxontable[order == "Polypodiales", taxon_state := "fern"]



taxontable[is.na(species) & is.na(genus) & family == "Poaceae", taxon_state := "drop"]

taxontable[original_name =="Agrostis tenuis", taxon_state := "drop"]
taxontable[original_name =="Calystegia sp." , taxon_state := "drop"]                 
taxontable[original_name =="Carex ovalis"   , taxon_state := "helophytes"]
taxontable[original_name =="Festuca sp."    , taxon_state := "drop"]
taxontable[original_name =="Galium sp."     , taxon_state := "drop"]
taxontable[original_name =="Petasites sp.ruttojuuri", taxon_state := "drop"]       
taxontable[original_name =="Pinguicula sp." , taxon_state := "drop"]                  
taxontable[original_name =="Sparganium microcarpum.Neuman.", taxon_state := "helophytes"] 
