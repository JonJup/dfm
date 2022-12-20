## -- new least distrubed delineation

# As a reaction to the review process of the invertebrate paper, I changed the delineation of least disturbed sites. 
# In this script I add these changes to the all_typologies file for dfm. 

#   date writte: 24.05.22
# last modified: 24.05.22
#       Project: dfm

library(pacman)
p_load(sf, dplyr, magrittr, mapview, rstudioapi)

all_typologies <- readRDS("data/all_typologies.rds")
least_disturbed_new <- st_read("data/lemm_least_impacted.gpkg")

# prep ------------------------------------------------------------------------------
st_crs(all_typologies)
st_crs(least_disturbed_new)

least_disturbed_new %<>% st_transform(crs = st_crs(all_typologies))

all_typologies %<>% select(m_zhyd, brt12, bgr, illies)

test_all_typologies <- all_typologies[1:10, ]

mapview(test_all_typologies)
test2 <- st_join(test_all_typologies, 
        least_disturbed_new)
mapview(test2)
