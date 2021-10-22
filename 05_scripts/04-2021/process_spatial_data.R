#==============================================================================#
#                                                                              #
#                          Process the spatial data                            #
#                                                                              #
#==============================================================================#

library(tidyverse)
library(rgdal)
library (raster)
library(broom)
Sys.setenv(LANG = "en")



# A. Water =====================================================================

# ~ 1. Waterbodies -------------------------------------------------------------

waterbodies <- readOGR(dsn = "04_raw_data/water_tanziania/tanzania_waterholes_shp/gis_osm_water_a_free_1.shp")

# ~~~ a. The snp buffer --------------------------------------------------------
snp.buffer <- readOGR(dsn = "06_processed_data/borders/serengeti_buffer_10km.shp")

waterbodies.snp <- raster::crop(waterbodies, snp.buffer)

writeOGR(obj = waterbodies.snp,
         dsn = "06_processed_data/water",
         layer = "waterbodies_snp_buffer_2021-07-04",
         driver="ESRI Shapefile")

plot(waterbodies.snp)


# ~~~ b. The 65km wide circle merged with snp boundary (no buffer) -------------

circle.snp <-  readOGR(dsn = "06_processed_data/borders/circle_65km_serengeti_combined.shp")

waterbodies.circle.snp <- raster::crop(waterbodies, circle.snp)

writeOGR(obj = waterbodies.circle.snp,
         dsn = "06_processed_data/water",
         layer = "waterbodies_circle_snp_2021-07-04",
         driver="ESRI Shapefile")

plot(waterbodies.circle.snp)



# ~ 2. Waterways, rivers -------------------------------------------------------

rivers <- readOGR(dsn = "04_raw_data/water_tanziania/tanzania_waterways_lines_shp/hotosm_tza_waterways_lines.shp")


# ~~~ a. The snp buffer --------------------------------------------------------
snp.buffer <- readOGR(dsn = "06_processed_data/borders/serengeti_buffer_10km.shp")

rivers.snp <- raster::crop(rivers, snp.buffer)

writeOGR(obj = rivers.snp,
         dsn = "06_processed_data/water",
         layer = "rivers_snp_buffer_2021-07-04",
         driver = "ESRI Shapefile")

plot(rivers.snp)


# ~~~ b. The 65km wide circle merged with snp boundary (no buffer) -------------

circle.snp <-  readOGR(dsn = "06_processed_data/borders/circle_65km_serengeti_combined.shp")

rivers.circle.snp <- raster::crop(rivers, circle.snp)

writeOGR(obj = rivers.circle.snp,
         dsn = "06_processed_data/water",
         layer = "rivers_circle_snp_2021-07-04",
         driver="ESRI Shapefile")

plot(rivers.circle.snp)
plot(circle.snp, add = TRUE)





# B. Amenity =====================================================================

# ~ 1. Amenity points ----------------------------------------------------------

amenity.points <- readOGR(dsn = "04_raw_data/water_tanziania/tanzania_waterholes_shp/gis_osm_water_a_free_1.shp")

snp.buffer <- readOGR(dsn = "06_processed_data/borders/serengeti_buffer_10km.shp")

amenity.points.snp <- raster::crop(amenity.points, snp.buffer)

writeOGR(obj = waterbodies.snp,
         dsn = "06_processed_data/amenity",
         layer = "amenity_points_snp_buffer_2021-07-04",
         driver="ESRI Shapefile")

plot(waterbodies.snp)


# ~ 2. Amenity polygons ------------------------------------------------

amenity.polygons <- readOGR(dsn = "04_raw_data/water_tanziania/tanzania_waterways_lines_shp/hotosm_tza_waterways_lines.shp")

snp.buffer <- readOGR(dsn = "06_processed_data/borders/serengeti_buffer_10km.shp")

amenity.polygons.snp <- raster::crop(amenity.polygons, snp.buffer)

writeOGR(obj = amenity.polygons.snp,
         dsn = "06_processed_data/amenity",
         layer = "amenity_polygons_snp_buffer_2021-07-04",
         driver = "ESRI Shapefile")

plot(rivers.snp)