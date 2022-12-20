#  — Diatom Maps  — # 

# setup -----------------------------------------------------------------------------
pacman::p_load(
        sf,
        tmap,
        data.table,
        fs,
        lubridate,
        stringr,
        dplyr,
        terra,
        mapview,
        maptiles,
        magrittr
)
tmap_mode("view")


# options ---------------------------------------------------------------------------
# 
# opt <- list()
# #- create static png maps for paper? 
# opt$static <- TRUE
# #- create dynamic seasonal maps for paper
# opt$season <- FALSE


# load data -------------------------------------------------------------------------
data <- readRDS("data/diatoms/combined_data/01_2022-06-15_combined_data_aggregated.rds")
#brt12 <- st_read("E://Arbeit/Data/broad_river_types/m_river_fec_broad_type.shp")


#- extract sites from data for each season
sites     <- st_as_sf(unique(data, by = "gr_sample_id"), crs = 3035)
#- define custom color palette
custom.color.palette <- c("#EC6B4F", "#65F78D")
    
mapview(sites)
    
tmap_mode("plot")

coords <- st_coordinates(sites)
x.min <- min(coords[1,])
x.max <- max(coords[1,])
y.min <- min(coords[2,])
y.max <- max(coords[2,])
coords <- st_bbox(c(xmin = x.min, xmax = x.max, ymin = y.min, ymax = y.max), crs = 3035)

## -- load basemap from hard drive or web 
basemap.tile <-
        get_tiles(coords,
                provider = "Esri.OceanBasemap",
                zoom = 7)
#writeRaster(basemap.tile, "fig/basemap.tif", overwrite=TRUE)
## -- create map 
map.static <-
        tm_shape(basemap.tile) +
        tm_rgb () +
        tm_shape(sites2) +
        tm_symbols(col = "#CD5C5C",
                   #col = "data.set.id",
                   shape = 21,
                   size = .05) #+
        #tm_facets(by = "season", free.coords = FALSE, nrow = 1)
## -- save map to file 
tmap_save(tm = map.static, 
          filename = paste0("fig/", Sys.Date(), "_map_diatom.png"))


tmap_mode("view")
map <- sites |> tm_shape() + tm_dots()

tmap_save(tm = map, filename = "../../../Blog/blog/static/sfe/diatoms.html")
