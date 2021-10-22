#==============================================================================#
#                                                                              #      
#              From CSV coordinates of carcasses to shapefile                  #
#                                                                              #
#==============================================================================#

library(tidyverse)
library(rgdal)
library(raster)



# 1. All hyenas ----------------------------------------------------------------
hy_carcasses <- read_delim("06_processed_data/carcasses/12_hy.carcasses.certainty.formatted.spatial_updated_10-2021.csv", 
           ";", escape_double = FALSE, trim_ws = TRUE)  %>%
  mutate(clan_members = ifelse(ID_OG %in% c("Z112", "Z141", "Z142", "Z147", "Z156", "Z160", "Z164",
                                            "Z166","Z175", "Z178", "P518", "Z193", "Z196"), "yes", "no")) %>%
  filter(lat != 0)



# Create a crs object
crs_A <- crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


#convert into a SpatialPointDataFrame
shp_hy_carcasses <- SpatialPointsDataFrame(coords = hy_carcasses[, c("long", "lat")], #the coordinates
                                             data = hy_carcasses,    #the R object to convert
                                             proj4string = crs_A)   # assigns a CRS 

plot(shp_hy_carcasses)

#save the shapefile
writeOGR(shp_hy_carcasses, 
         dsn = "06_processed_data/carcasses",
         layer = "hy_carcasses_10-2021",
         driver = "ESRI Shapefile", overwrite = TRUE)









# 2. Hyenas with high location certainty ---------------------------------------

# Import csv
hy_carcasses_high_certainty <- read_delim("06_processed_data/carcasses/12_hy.carcasses.certainty.formatted.spatial_updated_10-2021.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE)  %>%
  mutate(clan_members = ifelse(ID_OG %in% c("Z112", "Z141", "Z142", "Z147", "Z156", "Z160", "Z164",
                                            "Z166","Z175", "Z178", "P518", "Z193", "Z196"), "yes", "no")) %>%
  filter(location_certainty_score >= 0.75)


# Create a crs object
crs_A <- crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


#convert into a SpatialPointDataFrame
shp_hy_carcasses_high_certainty <- SpatialPointsDataFrame(coords = hy_carcasses_high_certainty[, c("long", "lat")], #the coordinates
                                           data = hy_carcasses_high_certainty,    #the R object to convert
                                           proj4string = crs_A)   # assigns a CRS 

plot(shp_hy_carcasses_high_certainty)

#save the shapefile
writeOGR(shp_hy_carcasses_high_certainty, 
         dsn = "06_processed_data/carcasses",
         layer = "hy_carcasses_high_certainty_10-2021",
         driver = "ESRI Shapefile", overwrite = TRUE)







# 3. Clan members --------------------------------------------------------------

shp_hy_carcasses_clan_members <- shp_hy_carcasses[shp_hy_carcasses@data$ID_OG %in% c("Z112", "Z141", "Z142", "Z147", "Z156", "Z160", "Z164",
                                                                 "Z166","Z175", "Z178", "P518", "Z193", "Z196"), ]

writeOGR(shp_hy_carcasses_clan_members, 
         dsn = "06_processed_data/carcasses",
         layer = "hy_carcasses_clan_members_10-2021",
         driver = "ESRI Shapefile", overwrite=TRUE)
