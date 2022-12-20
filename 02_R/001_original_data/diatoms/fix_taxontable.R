library(data.table)

tt <- readRDS("data/diatoms/2022-02-28_taxontable_diatoms.rds")
d1 <- readRDS("data/diatoms/fwb_table.rds")
d2 <- readRDS("data/diatoms/omn_table.rds")

fix_by_fixed <- function(fixed, new_fix, spe = NULL, gen = NULL, data = tt){
        
        if (is.null(spe))
                if(!stringr::str_detect(new_fix, "\\ ")) 
                {
                        spe <- NA
                } else {
                        spe <- new_fix        
                        }
                
        if (is.null(gen))
                gen <- stringr::word(new_fix)
        
        nf <- data[genus == gen, unique(family)]
        no <- data[genus == gen, unique(order)]
        nc <- data[genus == gen, unique(class)]
        
        if (length(nf) != 1){
                print("multiple families")
                break()
        }
        if (length(no) != 1){
                print("multiple orders")
                break()
        }
        if (length(nc) != 1){
                print("multiple classes")
                break()
        }
        
        row_id <- which(data$fixed_name %in% fixed)
        data$fixed_name[row_id] <- new_fix
        data$species[row_id] <- spe
        data$genus[row_id] <- gen
        data$family[row_id] <- nf
        data$order[row_id] <- no
        data$class[row_id] <- nc
        return(data)
}



tt <- fix_by_fixed(fixed = c("Achnanthes pyrenaica"), new_fix = c("Achnanthidium minutissimum"))
tt <- fix_by_fixed(fixed = "Achnanthes thienemannii",  new_fix = "Achnanthidium thienemannii")
tt <- fix_by_fixed(fixed = c("Achnanthes daui var.alaskaensis","Achnanthes daui"),  new_fix = "Planothidium daui-granum")
tt[species == "Kobayasiella ", c("fixed", "species") := .("Kobayasiella", NA)]
tt[species == "Adlafia ", c("fixed", "species") := .("Adlafia", NA)]
tt[species == "Craticula ", c("fixed", "species") := .("Craticula", NA)]
tt[original_name %in% c("Cyclotella", "Cyclotella sp"), c("fixed_name", "species", "genus") := .("Cyclotella", NA, "Cyclotella")]
tt <- fix_by_fixed(fixed = "Cyclotella atomus var.gracilis", new_fix = "Cyclotella atomus")
tt <- fix_by_fixed(fixed = "Cyclotella comta var.pliocaenica", new_fix = "Lindavia bodanica complex")
tt <- fix_by_fixed(fixed = c("Cymbella excisa var.angusta", "Cymbella excisa var.procera"), new_fix = "Cymbella excisa")
tt <- fix_by_fixed(fixed = c("Cymbella mexicana var.kamtschatica"), new_fix = "Cymbella mexicana")
tt <- fix_by_fixed(fixed = c("Cymbopleura rhomboidea var.angusta"), new_fix = "Cymbopleura rhomboidea")
tt <- fix_by_fixed(fixed = c("Denticula kuetzingii var.rumrichae"), new_fix = "Denticula kuetzingii")
tt <- fix_by_fixed(fixed = c("Diadesmis confervacea var.rostrata"), new_fix = "Diadesmis confervacea")
tt <- fix_by_fixed(fixed = c("Diatoma anceps var.linearis"), new_fix = "Diatoma anceps")
tt <- fix_by_fixed(fixed = c("Diatoma ehrenbergii var.capitulata"), new_fix = "Diatoma ehrenbergii")
tt <- fix_by_fixed(fixed = c("Diatoma vulgaris var.brevis", 
                             "Diatoma vulgaris var.lineare", 
                             "Diatoma vulgaris var.producta"), new_fix = "Diatoma ehrenbergii")
tt[fixed_name == "Discostella  ", c("fixed_name", "species") := .("Dicostella", NA)]
tt <- fix_by_fixed(fixed = "Encyonema caespitosum var.comensis", new_fix = "Encyonema caespitosum")
tt <- fix_by_fixed(fixed = "Encyonopsis cesatii var.undulata", new_fix = "Encyonopsis cesatii")
tt[fixed_name == "Eunotia ", c("fixed_name", "species") := .("Eunotia", NA)]
tt <- fix_by_fixed(fixed = "Eunotia eruca var.sibirica", new_fix = "Eunotia eruca")
tt[original_name == "Achnanthes lanceolata ssp. robusta var. abbreviata", 
   c("fixed_name", "species") := "Planothidium robustius"]
tt <- fix_by_fixed(fixed = "Achnanthes minutissima", new_fix = "Achnanthidium minutissimum")
tt <- fix_by_fixed(fixed = "Rossithidium petersennii", new_fix = "Rossithidium petersenii")
tt <- fix_by_fixed(fixed = "Psammothidium ventrale", new_fix = "Psammothidium ventrale Complex")
tt <- fix_by_fixed(fixed = "Adlafia Complex", new_fix = "Adlafia")
tt <- fix_by_fixed(fixed = c("Caloneis amphisbaena var.aequata", 
                             "Caloneis amphisbaena var.fenzlii",
                             "Caloneis amphisbaena var.aequata"),
                   new_fix = "Caloneis amphisbaena")
tt <- fix_by_fixed(fixed = "Caloneis patagonica var.alaskaensis", new_fix = "Caloneis patagonica")
tt <- fix_by_fixed(fixed = "Synedra tabulata var.obtusa", new_fix = "Tabularia tabulata")
tt <- fix_by_fixed(fixed = "Ctenophora pulchella var.lanceolata", new_fix = "Ctenophora pulchella")
tt[original_name == "Cymbella affinis Kützing", c("fixed_name", "species") := 
           "Cymbella affinis"]
tt <- fix_by_fixed(fixed = "Cymbella austriaca var.alaskaensis", new_fix = "Cymbopleura incerta complex")
tt <- fix_by_fixed(fixed = "Cymbella tropica var.tenuipunctata", new_fix = "Cymbella tropica")
tt[original_name == "Cymbopleura naviculiformis (Auerswald) Krammer var. naviculiformis",
   c("fixed_name", "species") := 
           "Cymbopleura amphicephala-naviculiformis-anglica"]

tt <- fix_by_fixed(fixed = "Encyonopsis cesatii var.undulata", new_fix = "Encyonopsis cesatii")
tt <- fix_by_fixed(fixed = "Eunotia arcus", new_fix = "Eunotia arcus/mucophila/bilunaris Complex")
tt <- fix_by_fixed(fixed = "Eunotia incisa", new_fix = "Eunotia incisa Complex")
tt <- fix_by_fixed(fixed = "Fragilaria capucina", new_fix = "Fragilaria capucina complex")
tt <- fix_by_fixed(fixed = "Fragilaria pulchella var.constricta", new_fix = "Ctenophora pulchella")
tt <- fix_by_fixed(fixed = "Fragilaria tabulata var.truncata", new_fix = "Tabularia tabulata")
tt <- fix_by_fixed(fixed = "Ulnaria ulna var.acus", new_fix = "Ulnaria ulna complex")
tt <- fix_by_fixed(fixed = "Fragilariforma virescens", new_fix = "Fragilaria virescens complex")
tt <- fix_by_fixed(fixed = "Navicula mutica var.binodis", new_fix = "Luticola")
tt <- fix_by_fixed(fixed = "Navicula viridula", new_fix = "Navicula viridula complex")
tt <- fix_by_fixed(fixed = "Nitzschia angustata var.producta", new_fix = "Tryblionella angustata Complex")
tt <- fix_by_fixed(fixed = c("Nitzschia levidensis group salinarum", 
                             "Nitzschia levidensis var. salinarum", 
                             "Nitzschia levidensis var.salinarum"),
                   new_fix = "Tryblionella gracilis")
tt <- fix_by_fixed(fixed = "Nitzschia littoralis", new_fix = "Tryblionella")
tt <- fix_by_fixed(fixed = "Nitzschia palea", new_fix = "Nitzschia palea-paleacea")
tt <- fix_by_fixed(fixed = "Nitzschia subcohaerens var.scotica", new_fix = "Nitzschia subcohaerens")
tt <- fix_by_fixed(fixed = "Pinnularia biceps", new_fix = "Pinnularia biceps Complex")
tt <- fix_by_fixed(fixed = "Pinnularia brebissonii Complex", new_fix = "Pinnularia microstauron Complex")
tt <- fix_by_fixed(fixed = "Pinnularia subcapitata var. semicruciata", new_fix = "Pinnularia subcapitata Complex")
tt <- fix_by_fixed(fixed = "Pinnularia subgibba", new_fix = "Pinnularia gibba complex")
tt <- fix_by_fixed(fixed = "Placoneis clementis var. linearis", new_fix = "Placoneis")
tt <- fix_by_fixed(fixed = "Achnanthidium bioretii", new_fix = "Psammothidium helveticum/chlidanos/daonense")
tt <- fix_by_fixed(fixed = "Psammothidium grishunum", new_fix = "Psammothidium marginulatum/scoticum/lacus-vulcani/levenderi")
tt <- fix_by_fixed(fixed = c("Pseudostaurosira parasitica", "Pseudostaurosira microstriata"), new_fix = "Pseudostaurosira parasitica complex")
tt <- fix_by_fixed(fixed = c("Stauroneis smithii"), new_fix = "Stauroneis smithii Complex")
tt <- fix_by_fixed(fixed = c("Ulnaria ulna"), new_fix = "Ulnaria ulna complex")
tt <- fix_by_fixed(fixed = c("Thalassiosira guillardii"), new_fix = "Conticribra guillardii")


tt[original_name %in% c(
        "Staurosira construens (Ehr.) var. binodis (Ehr.) Hamilton",
        "Staurosira construens f. binodis (Ehr.) Grunow f. anormale",
        "Staurosira construens f. subsalina",
        "Staurosira construens fo. binodis abnormal form",
        "Staurosira construens var. binodis"
), c("fixed_name", "species") := "Fragilaria construens-pseudoconstruens"]


tt[original_name == "Gomphonema acidoclinatum", c("fixed_name", "species") := "Gomphonema acidoclinatum"]
tt[original_name == "Gomphonema angustum/pumilum type", c("fixed_name", "species") := "Gomphonema angustatum Complex"]
tt[species == "Gomphonema", species := NA]
tt[species == "Mastogloia ", species := NA]
tt[species == "Mastogloia Complex", species := NA]
tt[species == "Navicula complex", species := NA]
tt[species == "Navicula ", c("fixed_name", "species") := .("Navicula", NA)]
tt[species == "Orthoseira ", c("fixed_name", "species") := .("Orthoseira", NA)]
tt[species == "Pseudostaurosiropsis", species := NA]
tt[species == "Thalassiosira ", c("fixed_name", "species") := .("Thalassiosira", NA)]
tt[species == "Thalassiosira Complex", species := NA]
tt[species == "Ulnaria", species := NA]


tt[original_name %in% c(
        "Nitzschia palea-Sippen",
        "Nitzschia palea - Sippen",
        "Nitzschia palea tenuirostris-Sippen",
        "Nitzschia palea var. tenuirostris",
        "Nitzschia palea var. tenuirostris sensu"
), c("fixed_name", "species") := "Nitzschia palea-paleacea"]
tt[original_name %in% c(
        "Nitzschia sinuata (Thwaites) Grunow var.delognei (Grunow)Lange-Bertalot",
        "Nitzschia sinuata var. delognei",
        "Nitzschia sinuata var.delognei"
), c("fixed_name", "species") := "Nitzschia sinuata Complex"]



tt <- fix_by_fixed(fixed = "Navicula arvensis", new_fix = "Navicula arvensis-difficillima++")
tt[species == "Vikingea promunturii", spe := NA]


tt <- fix_by_fixed(fixed = "Entomoneis paludosa var.subsalina", new_fix = "Entomoneis paludosa")
tt <- fix_by_fixed(fixed = "Epithemia sorex var.acuta", new_fix = "Epithemia sorex")
tt <- fix_by_fixed(fixed = "Gomphoneis eriense var.variabilis", new_fix = "Gomphoneis eriense")
tt <- fix_by_fixed(fixed = "Gomphonemopsis exigua var.platypus", new_fix = "Gomphonemopsis exigua")
tt <- fix_by_fixed(fixed = "Lemnicola hungarica var.pusilla", new_fix = "Lemnicola hungarica")
tt <- fix_by_fixed(fixed = "Mastogloia acutiuscula var.elliptica", new_fix = "Mastogloia acutiuscula")
tt <- fix_by_fixed(fixed = "Navicula cryptocephala var.terrestris", new_fix = "Navicula cryptocephala")
tt <- fix_by_fixed(fixed = "Nitzschia filiformis var.conferta", new_fix = "Nitzschia filiformis")
tt <- fix_by_fixed(fixed = "Nitzschia distans var.tumescens", new_fix = "Nitzschia distans")
tt <- fix_by_fixed(fixed = "Pinnularia marchica var.subrostrata", new_fix = "Pinnularia marchica")
tt <- fix_by_fixed(fixed = "Pinnularia substreptoraphe var.bornholmiana", new_fix = "Pinnularia substreptoraphe")
tt <- fix_by_fixed(fixed = "Pinnularia viridiformis var.minor", new_fix = "Pinnularia viridiformis")
tt <- fix_by_fixed(fixed = "Pleurosira laevis var.paludosa", new_fix = "Pleurosira laevis")
tt <- fix_by_fixed(fixed = "Nitzschia panduriformis var.panduriformis", new_fix = "Nitzschia panduriformis")
tt <- fix_by_fixed(fixed = "Psammothidium confusum var.atomoides", new_fix = "Psammothidium confusum")
tt <- fix_by_fixed(fixed = "Tryblionella hungarica var.linearis", new_fix = "Tryblionella hungarica")

saveRDS(tt, "data/diatoms/2022-03-02_taxontable_diatoms.rds")
