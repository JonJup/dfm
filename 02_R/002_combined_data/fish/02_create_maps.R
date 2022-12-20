#  — Fish Maps  — # 

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
        maptiles,
        magrittr,
        mapview
)
tmap_mode("view")


# load data -------------------------------------------------------------------------
data <- readRDS("data/fish/combined_data/01_2022-06-15_combined_data_aggregated.rds")

#- extract sites from data for each season
sites <- unique(data, by = "gr_sample_id")
sites <- st_as_sf(sites, crs = 3035)



# static ----------------------------------------------------------------------------


#- define custom color palette
custom.color.palette <- c("#EC6B4F", "#65F78D")
tmap_mode("plot")
## -- load basemap from hard drive or web
## -- load basemap from hard drive or web 
bbox <- 
        sfheaders::sf_bbox(sites) |> 
        st_as_sfc() |> 
        st_as_sf(crs = "EPSG:3035")

basemap.tile <-
        get_tiles(bbox,
                  provider = "Esri.OceanBasemap",
                  crop = TRUE,
                  zoom = 7,
                  forceDownload = TRUE)
}
basemap.tile <- terra::rast("fig/basemap.tif")
## -- create map
map.static <-
        # tm_shape(basemap.tile) +
        # tm_rgb () +
        tm_shape(sites) +
        tm_symbols(col = "#CD5C5C",
                   shape = 21,
                   size = .05)
## -- save map to file
tmap_save(tm = map.static,
          filename = paste0("fig/maps/", Sys.Date(), "_map_fish.png"))



## test other combintion of month 
sites2 <- mutate(sites2, month = lubridate::month(date))

tm_shape(basemap.tile) +
        tm_rgb () +
        tm_shape(filter(sites2, month %in% 5:9)) +
        tm_symbols(col = "#CD5C5C",
                   #col = "data.set.id",
                   shape = 21,
                   size = .05) 

#-- map brt12 

brt12.static <-
        tm_shape(basemap.tile) +
        tm_rgb () +
        tm_shape(brt12, legend.show = FALSE) +
        tm_lines(col = "m_btype12")  
tmap_save(tm = brt12.static, 
          filename = paste0("fig/", Sys.Date(), "_brt12.png"))


# animated --------------------------------------------------------------------------


tmap_mode("view")
map <- sites |> tm_shape() + tm_dots()

tmap_save(tm = map, filename = "../../../Blog/blog/static/sfe/fish.html")
