## add ez


box::use(s = sf)
library(data.table)

ez <- s$st_read("~/../Desktop/data_from_external/environmental_zones/EnSv8/EnSv8/ens_v8.shp")
data <- s$st_as_sf(data)
#feow_europe <- s$st_make_valid(feow_europe)
sites <- unique(data, by = "gr_sample_id")
sites <- s$st_transform(sites, crs = s$st_crs(ez))

sites <- s$st_join(sites, 
                   ez)
setDT(sites)
sites <- sites[, c("gr_sample_id", "EnZ_name")]
data <- sites[data, on = "gr_sample_id"]
rm(sites, ez)
