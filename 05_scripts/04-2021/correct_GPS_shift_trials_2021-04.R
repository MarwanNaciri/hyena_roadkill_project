#==============================================================================#
#                                                                              #      
#              Try to correct the wrong GPS coordinates (04-2021)              #
#                                                                              #
#==============================================================================#

library(tidyverse)
library(rgdal)
library(raster)
library(tmap)

# 1. Try to correct graphically ------------------------------------------------

# Plot the wrong carcasses -----------------------------------------------------
boundary <- rgdal::readOGR(dsn  = "6_processed_data/borders",
                           layer = "serengeti.mara.ecosystem.09-18")

shp.hy.before.2000 <- readOGR(dsn = "6_processed_data/carcasses",
                              layer = "hy.before.2000.04-2021")



(map_hy_bef_2000 <- 
    tm_shape(shp.hy.before.2000) +
    tm_symbols(col  =  "#E69F00", alpha = 1, size = 0.75)
  
)

tmap_save(tm = map_hy_bef_2000,
          filename = "6_processed_data/carcasses/correct_GPS_shift_04-2021/points_bef_2000.svg")

(map_hy_bef_2000_w_bound <- 
    tm_shape(boundary) +
    tm_polygons(col  =  "#d6e9d6", alpha = 1, boundary.col = "#5ea65c") +
    tm_shape(shp.hy.before.2000) +
    tm_symbols(col  =  "#E69F00", alpha = 1, size = 0.18)
  
)

tmap_save(tm = map_hy_bef_2000_w_bound,
          filename = "6_processed_data/carcasses/correct_GPS_shift_04-2021/points_bef_2000_with_boundary.svg")



# Plot various points to have references ----------------------------------------

spatial_references <- data.frame(name = c("simba kopjes", "1.8km from hippo pool", 
                                          "school", "S airstrip", "12.2km south of hippo pool"),
                                 long = c(34.9286, 34.8052, 34.8218, 34.8208, 34.8417),
                                 lat = c(-2.6847, -2.4136, -2.4349, -2.4580, -2.5183))

# Create a crs object
crs_A <- crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


#convert into a SpatialPointDataFrame
shp.spatial.references<- SpatialPointsDataFrame(coords = spatial_references[,2:3], #the coordinates
                                                data = spatial_references,    #the R object to convert
                                                proj4string = crs_A)   # assigns a CRS 



#save the shapefile
writeOGR(shp.spatial.references, 
         dsn = "6_processed_data/carcasses/correct_GPS_shift_04-2021",
         layer = "spatial_references_04-2021",
         driver = "ESRI Shapefile", overwrite=TRUE)



# Plot the roads to have a reference -------------------------------------------

major.roads <- rgdal::readOGR(dsn  =  "6_processed_data/roads/SNP.roads.QGIS", 
                              layer = "major_roads")

roads <- rgdal::readOGR(dsn  =  "6_processed_data/roads/SNP.roads.QGIS",
                        layer = "snp.roads.acc.to.map.road.w.carcass.del")

sp_references <- readOGR(dsn = "6_processed_data/carcasses/correct_GPS_shift_04-2021",
                         layer = "spatial_references_04-2021")


tmap_mode("view")
(map_roads <- tm_shape(boundary) +
    tm_polygons(col  =  "#d6e9d6", alpha = 1, boundary.col = "#5ea65c") +
    tm_shape(roads) +
    tm_lines(col  =  "black", lwd  =  0.75) +
    tm_shape(major.roads) +
    tm_lines(lty = "solid", col  =  "black", lwd  =  1.5) +
    tm_shape(sp_references) +
    tm_symbols(col  =  "darkred", alpha = 1, size = 0.05)
  
)

tmap_save(tm = map_roads,
          filename = "6_processed_data/carcasses/correct_GPS_shift_04-2021/roads.svg")


# Plot various points to have references ----------------------------------------

spatial_references <- data.frame(name = c("simba kopjes", "1.8km from hippo pool", 
                                          "school", "S airstrip", "12.2km south of hippo pool"),
                                 long = c(34.9286, 34.8052, 34.8218, 34.8208, 34.8417),
                                 lat = c(-2.6847, -2.4136, -2.4349, -2.4580, -2.5183))

# Create a crs object
crs_A <- crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


#convert into a SpatialPointDataFrame
shp.spatial.references<- SpatialPointsDataFrame(coords = spatial_references[,2:3], #the coordinates
                                                data = spatial_references,    #the R object to convert
                                                proj4string = crs_A)   # assigns a CRS 



#save the shapefile
writeOGR(shp.hy.before.2000, 
         dsn = "6_processed_data/carcasses",
         layer = "hy.before.2000.04-2021",
         driver = "ESRI Shapefile", overwrite=TRUE)


# Try with another crs
crs_B <- crs("+proj=geocent +ellps=WGS72 +units=m +no_defs")

#convert into a SpatialPointDataFrame
shp.hy.before.2000_B <- SpatialPointsDataFrame(coords = hy.before.2000[,2:3], #the coordinates
                                               data = hy.before.2000,    #the R object to convert
                                               proj4string = crs_B)   # assigns a CRS 


# save the shapefile
writeOGR(shp.hy.before.2000_B, 
         dsn = "6_processed_data/carcasses",
         layer = "hy.before.2000.WSG72.04-2021",
         driver = "ESRI Shapefile", overwrite=TRUE)