### ------------------------- ###
### --- Create maps       --- ###
### --- Macrophytes       --- ### 
### ------------------------- ###

# -------------------------------
# date written: 28.01.2022
# date last modified: 24.03.2022
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: Create macrophyte maps.
# Notes: 
# -------------------------------

# setup -----------------------------------------------------------------------------
pacman::p_load(
        sf,
        tmap,
        data.table,
        maptiles,
        terra,
        dplyr,
        sfheaders
)

# load data -------------------------------------------------------------------------
data <- readRDS("data/macrophytes/combined_data/01_2022-12-12_combined_data_aggregated.rds")

# prepare data ----------------------------------------------------------------------
sites <- unique(data, by = "gr_sample_id")
sites <- st_as_sf(sites)

# static maps -----------------------------------------------------------------------
custom.color.palette <- c("#EC6B4F", "#65F78D")
tmap_mode("plot")
## -- load basemap from hard drive or web 
bbox <- 
        sf_bbox(sites) |> 
        st_as_sfc() |> 
        st_as_sf(crs = "EPSG:3035")

# - download new basemap 
basemap.tile <-
        get_tiles(bbox,
                provider = "Stamen.TerrainBackground",
                crop = TRUE,
                zoom = 5,
                forceDownload = TRUE)

# - load basemap from file 
#basemap.tile <- terra::rast("fig/basemap.tif")
## -- create map 
map.static <-
        tm_shape(basemap.tile) +
        tm_rgb () +
        tm_shape(sites) +
        tm_symbols(col = "#CD5C5C",
                   #col = "data.set.id",
                   shape = 21,
                   size = .05) 
map.static
## -- save map to file 
tmap_save(tm = map.static, 
          filename = paste0("fig/maps/", Sys.Date(), "_map_macrophytes.png"))


# dynamic maps ----------------------------------------------------------------------
tmap_mode("view")
map <- sites |> tm_shape() + tm_dots()
tmap_save(tm = map, filename = "../../../Blog/blog/static/sfe/macrophytes.html")
