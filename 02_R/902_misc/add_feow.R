## add fewo


box::use(s = sf)
library(data.table)

feow <- s$st_read("~/../Desktop/data_from_external/freshwater_ecoregions_of_the_world/feow_hydrosheds.shp")


feow <- s$st_set_crs(feow, 4326)

data <- s$st_as_sf(data)


## subset to europe 
feow_europe <- 
        feow |> 
        dplyr::mutate(FEOW_ID = as.character(FEOW_ID)) |> 
        dplyr::filter(stringr::str_detect(FEOW_ID, "^4"))

feow_europe <- s$st_make_valid(feow_europe)

sites <- unique(data, by = "gr_sample_id")
sites <- s$st_transform(sites, crs = 4326)

sites <- s$st_join(sites, 
                 feow_europe)
setDT(sites)
sites <- sites[, c("gr_sample_id", "FEOW_ID")]
data <- sites[data, on = "gr_sample_id"]
rm(sites, feow, feow_europe)
