#------------------------------------------------------------------------------#
#                                                                              #
#               Create circes with a given radius or rectangles                #        
#                                                                              #
#------------------------------------------------------------------------------#

library(rgdal)
library(raster)
library(tmap)
library(sp)
library(sf)
library(tidyverse)
library(rgeos)
Sys.setenv(LANG = "en")

# A. Circles ===================================================================

# Load the point corresponding to the visitor center
visitor <- readOGR(dsn = "06_processed_data/amenity",
                   layer = "visitor.center")

# Project to planar coordinates
visitor.planar <- spTransform(visitor, 
                              CRS("+proj=utm +zone=36 +south +a=6378249.145 +rf=293.46500000303 +towgs84=-160,-6,-302,0,0,0,0 +units=m +no_defs")) 


# Create the buffer
buffer.size <- 47500
visitor.buffer.planar <- gBuffer(visitor.planar,
                                        byid = TRUE, id = NULL, 
                                        width = buffer.size, 
                                        quadsegs = 5, 
                                        capStyle = "ROUND",
                                        joinStyle = "ROUND",
                                        mitreLimit = 1.0)

# Project to spherical coordinates 
visitor.buffer <- spTransform(visitor.buffer.planar, 
                              crs(visitor)) 

writeOGR(obj = visitor.buffer,
         dsn = "06_processed_data/borders",
         layer = "circle_47.5km",
         driver = "ESRI Shapefile", overwrite=TRUE)




# B. Squares ===================================================================

# ~ 1. From Ikoma gate to Naabi gate -------------------------------------------

square.ikoma.naabi <- extent(34.7252727, 34.9974738, 
                      -2.8320276, -2.1890186)

square.ikoma.naabi.poly <- as(square.ikoma.naabi, 
                              'SpatialPolygons') 

crs(square.ikoma.naabi.poly) <- "+proj=longlat +datum=WGS84 +no_defs"


df <- data.frame(ID = 1)

# Try to coerce to SpatialPolygonsDataFrame (will throw error)
square.ikoma.naabi.poly.2 <- SpatialPolygonsDataFrame(square.ikoma.naabi.poly, df) 


writeOGR(obj = square.ikoma.naabi.poly.2,
         dsn = "06_processed_data/borders",
         layer = "square.ikoma.naabi",
         driver = "ESRI Shapefile", overwrite=TRUE)


# ~ 2. Crop square around the roads that I keep (2021-09) ----------------------

crop.square <- extent(34.52791, 35.45, 
                      -3.055874, -1.982601)

crop.square.poly <- as(crop.square, 
                              'SpatialPolygons') 

crs(crop.square.poly) <- "+proj=longlat +datum=WGS84 +no_defs"


df <- data.frame(ID = 1)

# Try to coerce to SpatialPolygonsDataFrame (will throw error)
crop.square.poly.2 <- SpatialPolygonsDataFrame(crop.square.poly, df) 


writeOGR(obj = crop.square.poly.2,
         dsn = "06_processed_data/borders",
         layer = "crop.square.2021-09",
         driver = "ESRI Shapefile", overwrite=TRUE)



# ~ 2. Crop square around the roads that I keep WITH MARGIN (2021-09) ----------

boundary <- readOGR(dsn  = "06_processed_data/borders",
                    layer = "SNP.lastest.cropped.2021-09")

range.x <- boundary@bbox[1, 2] - boundary@bbox[1, 1]
range.y <- boundary@bbox[2, 2] - boundary@bbox[2, 1]

A <- 0.25 # factor determining the width of the margin left and right of figure
B <- 0.1 # factor determining the width of the margin above and below figure

crop.square <- extent(boundary@bbox[1, 1] - A*range.x, # xmin
                      boundary@bbox[1, 2] + A*range.x, # xmax
                      boundary@bbox[2, 1] - B*range.y, # ymin
                      boundary@bbox[2, 2] + B*range.y) # ymax

crop.square.poly <- as(crop.square, 
                       'SpatialPolygons') 

crs(crop.square.poly) <- "+proj=longlat +datum=WGS84 +no_defs"

df <- data.frame(ID = 1)

# Try to coerce to SpatialPolygonsDataFrame (will throw error)
crop.square.poly.2 <- SpatialPolygonsDataFrame(crop.square.poly, df) 


writeOGR(obj = crop.square.poly.2,
         dsn = "06_processed_data/borders",
         layer = "crop.square.margin.2021-09",
         driver = "ESRI Shapefile", overwrite=TRUE)
