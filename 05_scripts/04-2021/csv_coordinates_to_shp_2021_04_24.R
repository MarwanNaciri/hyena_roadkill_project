#==============================================================================#
#                                                                              #      
#              From CSV coordinates of carcasses to shapefile                  #
#                                                                              #
#==============================================================================#

library(tidyverse)
library(rgdal)
library(raster)

# 1. Hyenas with wrong GPS coordiantes (~before 2000) --------------------------

# Import csv
hy.before.2000 <- read_delim("6_processed_data/carcasses/13_hy.before.2000.csv", 
                             ";", escape_double = FALSE, col_types = cols(lat = col_character(), 
                             long = col_character()), trim_ws = TRUE)
hy.before.2000$lat = as.numeric(str_replace(hy.before.2000$lat,
                                            pattern = ",",
                                            replacement = "."))
hy.before.2000$long = as.numeric(str_replace(hy.before.2000$long,
                                             pattern = ",",
                                             replacement = "."))


# Create a crs object
crs_A <- crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


#convert into a SpatialPointDataFrame
shp.hy.before.2000 <- SpatialPointsDataFrame(coords = hy.before.2000[,2:3], #the coordinates
                                              data = hy.before.2000,    #the R object to convert
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

# reproject
roads <- readOGR(dsn  =  "6_processed_data/roads/SNP.roads.QGIS",
                 layer = "snp.roads.acc.to.map.road.w.carcass.del")

shp.hy.before.2000_B_reprojected <- spTransform(shp.hy.before.2000_B, 
                                                CRSobj = crs(roads)) # R

writeOGR(shp.hy.before.2000_B_reprojected, 
         dsn = "6_processed_data/carcasses",
         layer = "hy.before.2000.WSG72.reprojected.04-2021",
         driver = "ESRI Shapefile", overwrite=TRUE)



# 2. All hyenas ----------------------------------------------------------------

hy_carcasses <- read_delim("6_processed_data/carcasses/12_hy.carcasses.certainty.formatted.spatial_updated_04-2021.csv", 
                             ";", escape_double = FALSE, col_types = cols(lat = col_character(), 
                                                                          long = col_character()), trim_ws = TRUE) %>%
  mutate(clan_members = ifelse(ID_OG %in% c("Z112", "Z141", "Z142", "Z147", "Z156", "Z160", "Z164",
                                "Z166","Z175", "Z178", "P518", "Z193", "Z196"), "yes", "no"))
hy_carcasses$lat = as.numeric(str_replace(hy_carcasses$lat,
                                            pattern = ",",
                                            replacement = "."))
hy_carcasses$long = as.numeric(str_replace(hy_carcasses$long,
                                             pattern = ",",
                                             replacement = "."))


# Create a crs object
crs_A <- crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


#convert into a SpatialPointDataFrame
shp_hy_carcasses <- SpatialPointsDataFrame(coords = hy_carcasses[,4:5], #the coordinates
                                             data = hy_carcasses,    #the R object to convert
                                             proj4string = crs_A)   # assigns a CRS 



#save the shapefile
writeOGR(shp_hy_carcasses, 
         dsn = "6_processed_data/carcasses",
         layer = "hy_carcasses_04-2021",
         driver = "ESRI Shapefile", overwrite=TRUE)



# 3. Clan members --------------------------------------------------------------

shp_hy_carcasses_clan_members <- shp_hy_carcasses[shp_hy_carcasses@data$ID_OG %in% c("Z112", "Z141", "Z142", "Z147", "Z156", "Z160", "Z164",
                                                                 "Z166","Z175", "Z178", "P518", "Z193", "Z196"), ]

writeOGR(shp_hy_carcasses_clan_members, 
         dsn = "6_processed_data/carcasses",
         layer = "hy_carcasses_clan_members_04-2021",
         driver = "ESRI Shapefile", overwrite=TRUE)
