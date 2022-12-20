## -- Clean Swedish Fish Data

# created : 15.06.22
# modified: 15.06.22
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: Clean fish data provided by Leonard Sandin (14.06.22) for Sweden
# CRS: 


# setup -----------------------------------------------------------------------------

library(pacman)
p_load(
        data.table, 
        dplyr,
        ggplot2,
        lubridate,
        magrittr,
        mapview,
        readxl,
        sf,
        stringr,
        tidyr
)

source("R/functions/add_typologies.R")

# load data -------------------------------------------------------------------------

data           <- read_excel("data/fish/original_data/sweden_leo/raw/Fish_Sweden_LEO.xlsx")
taxontable     <- readRDS("data/fish/2022-04-27_taxontable_fish.rds")
typologies <- readRDS("data/all_typologies.rds")


# prepare data ----------------------------------------------------------------------

# - remove N and E letters from coordiantes 
data$Latitude %<>% str_remove("N")
data$Longitude %<>% str_remove("E")

# - format date as lubridate 
data%<>%mutate(Date = ymd(Date))

# - format abundance as number 
data%<>%mutate(abundance1 = as.numeric(Run1_number_all), 
               abundance2 = as.numeric(Run2_number_all),
               abundance3 = as.numeric(Run3_number_all),
               abundance4 = as.numeric(Run4_number_all)
               
               )

# - abundances are provided for four "runs". How different are the values? 
run_data <- select(data, Site_code, Species, starts_with("abundance", ignore.case = FALSE))
any(!is.na(run_data$abundance4))
# - all run4 values are NA -> drop column 
run_data %<>% 
        select(-abundance4) %>%
        mutate(id = paste0(Site_code, "_", Species)) %>%
        select(-c("Site_code", "Species")) %>%
        pivot_longer(cols = !c("id"))

ggplot(run_data, aes(x=id, y = value)) + 
        geom_boxplot()

# - For some taxa we find considerable differences between runs. However, they present the
# minority and typically the highly abundant taxa. I will aggregate the abundances thorugh
# the median value across the three runs

# - I use a placeholder name so I can use abundance to remove all other columns in the next step. 
data %<>% mutate(placeholder = median(abundance1, abundance2, abundance3, na.rm = TRUE)) 

# - drop all other abundance columns
data%<>%select(!contains("Total"))
data%<>%select(!contains("Run"))
data%<>%select(!contains("abundance"))
data%<>%select(!contains("Biomass"))
data%<>%select(!contains("estimated"))
data%<>%select(!c("Fished_area", "Formula"))

# - test on map 
sites <- unique(data, by = "Site_code")
sites %<>% st_as_sf(coords = c("Longitude", "Latitude"), crs = "EPSG:4326")
mapview(sites)

data %<>% mutate(EPSG = 4326, 
                 data.set = "sweden_leo_fish") %>%
        rename(abundance = placeholder, 
               original_site_name = Site_code,
               date    = Date,
               taxon = Species)

# - add season and year 
data %<>% mutate (season = case_when(month(date) %in% c(12,1,2)  ~ "winter",
                                             month(date) %in% c(3,4,5)   ~ "spring",
                                             month(date) %in% c(6,7,8)   ~ "summer",
                                             month(date) %in% c(9,10,11) ~ "autumn"), 
                  year = year(date))

# - are all taxa in the taxontable? -> yes
TU <- unique(data$taxon)
TU <- setdiff(TU, taxontable$original_name)

# - add taxonomic information
data %<>% rename("original_name" = taxon)
setDT(data)
data2 <- taxontable[data, on = "original_name"]

# - add site and date ids 
data2[, site_id := .GRP, by = "original_site_name"]
data2[, date_id := .GRP, by = "date"]

# - add leading zeros
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

# - add gr_sample_id
data2[,gr_sample_id := paste0("site_", site_id, "_date_", date_id, "_sweden_leo_fish")]

# - reshape data
data4 <- data2[, list(
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
        x.coord = Longitude,
        y.coord = Latitude,
        EPSG,
        data.set
)]

# - combine entries of same taxon
data4[, lowest.taxon := ifelse(!is.na(species), species,
                               ifelse(!is.na(genus), genus,
                                      ifelse(!is.na(family), family,
                                             ifelse(!is.na(order), order,
                                                    ifelse(!is.na(class), class,
                                                           ifelse(!is.na(phylum), phylum, kingdom))))))]

data4[, abundance := as.numeric(abundance)]
data4[, abundance := sum(abundance), by = c("gr_sample_id", "lowest.taxon")]
data5 <- unique(data4, by = c("gr_sample_id", "lowest.taxon"))
data5 <- add_typologies(data5)

# - visual checks
sites <- unique(data5, by = "site_id") |> st_as_sf(coords = c("x.coord", "y.coord"), crs = data5$EPSG[1])

mapview(sites, zcol = "brt12")
mapview(sites, zcol = "ife")
mapview(sites, zcol = "bgr")
mapview(sites, zcol = "few")
mapview(sites, zcol = "enz")
mapview(sites, zcol = "least.impacted")

# - subset to least impacted catchments 
data6 <- data5[least.impacted == TRUE]
sites <- unique(data6, by = "site_id") |> st_as_sf(coords = c("x.coord", "y.coord"), crs = data5$EPSG[1])

# - look for sites with different ID but same coordinates 
distances  <- st_distance(sites)
distances2 <- as.matrix(distances)
diag(distances2) <- 999
(duplicate_sites <- which(distances2 < units::as_units(1, "m")))

data6[, richness := uniqueN(lowest.taxon), by = "gr_sample_id"]
summary(data6$richness)
hist(data6$richness)
data7 <- data6[richness > 2]

# - drop sites far removed from ECRINS river network 
data8 <- data7[distance < 300]

# - visually check the assignment of sites 
rt <-
        data8 |>
        unique(by = "site_id") |>
        st_as_sf(coords = c("x.coord", "y.coord"),
                    crs = data5$EPSG[1])

plot_typology <- st_crop(typologies, st_transform(sites, crs = st_crs(typologies)))
updated_type  <- data.table(site_id = rt$site_id)
options(warn = -1)

for (i in 1:nrow(rt)){
        i.percent <- i/nrow(rt) * 100
        i.rt <- rt[i, ]
        i.plot_typology <- st_crop(plot_typology, st_buffer(st_transform(i.rt, crs = st_crs(typologies)), dist =  2000))
        x <- mapview(i.plot_typology, zcol = "brt", map.type = "OpenStreetMap.DE") + mapview(i.rt, popup = "waterbody", color = "red")
        print(x)
        i.bool <- "n"
        i.bool <- readline(paste0(i,"/", nrow(rt)))
        if (i.bool == "break")
                break()
        if (i.bool == "n"){
                updated_type[site_id == i.rt$site_id, new_type := "drop"]
        } else if (i.bool == "c"){
                i.towhat <- readline("change to:")
                updated_type[site_id == i.rt$site_id, new_type := i.towhat]
        } else {
                updated_type[site_id == i.rt$site_id, new_type := i.rt$brt12]
        }
        rm(list = ls()[grepl("i\\.", ls())])
}

# - save the update_type 
saveRDS(updated_type, paste0("data/fish/original_data/sweden_leo/", Sys.Date(), "_updated_type.rds"))

data9 <- left_join(data8, 
                      updated_type, 
                      by = "site_id")

#- drop "drop" rows determined in for-loop      
data9 <- data9[new_type != "drop"]
data9[, brt12 := NULL]
data9 <- rename(data9, brt12 = new_type)

# temporal aggregation --------------------------------------------------------------
agg <- data9 |> unique(by = "gr_sample_id")
unique(table(agg$site_id))
# - no 
data10 <- data9[month(date) %in% 5:9]

# - save to file 
saveRDS(data10, paste0("data/fish/original_data/sweden_leo/",Sys.Date(),"_final_aggregated.rds"))


# statistics -------------------------------------------------------------------------
# time span
summary(data2$year)
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
unique(data10, by = "gr_sample_id") |> pull(richness) |> mean()
# histogram richness
unique(data10, by = "gr_sample_id") |> pull(richness) |> hist()