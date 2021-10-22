#------------------------------------------------------------------------------#
#                                                                              #
#                       Plot the maps of the study area                        #        
#                                                                              #
#------------------------------------------------------------------------------#

library(rgdal)
library(raster)
library(tmap)
library(tmaptools)
library(leaflet)
library(shinyjs)
library(sp)
library(sf)
library(tidyverse)


# See https://rdrr.io/cran/tmap/man/tmap_arrange.html to arrange the maps


### Map Serengeti migration --------

boundary <- rgdal::readOGR(dsn  = "06_processed_data/borders",
                           layer = "serengeti.mara.ecosystem.09-18")

boundary.kenya.tenzania <- rgdal::readOGR(dsn  =  "06_processed_data/migration",
                                          layer = "frontier.kenya")


tmap_mode("view")
(map_migration <- tm_shape(boundary) +
    tm_polygons(col  =  "#d6e9d6", alpha = 1, boundary.col = "#5ea65c") +
    tm_shape(boundary.kenya.tenzania) +
    tm_lines(col  =  "black", lwd  =  2) +
    tm_scale_bar(position = c("right", "bottom"),
                 text.size = 1,
                 breaks = c(0, 25, 50, 100)) +
    tm_compass(position = c("left", "top"),
               size = 4) +
    tm_grid(lines = FALSE,
            labels.size = 1)
  
)
?tm_grid

tmap_save(tm = map_migration,
          # height = 3.525674,
          # width = 3.474513,
          # units = "in",
          filename = "07_intermediate_results/maps/09-08_snp.migration.svg")


### Map roads with main roads thicker ---------

boundary <- rgdal::readOGR(dsn  = "06_processed_data/borders",
                           layer = "serengeti.mara.ecosystem.09-18")

major.roads <- rgdal::readOGR(dsn  =  "06_processed_data/roads/SNP.roads.QGIS", 
                              layer = "major_roads")

roads <- rgdal::readOGR(dsn  =  "06_processed_data/roads/SNP.roads.QGIS",
                        layer = "snp_roads_for_analyses_25-05-2021")


tmap_mode("view")
(map_roads <- tm_shape(boundary) +
    tm_polygons(col  =  "#d6e9d6", alpha = 1, boundary.col = "#5ea65c") +
    tm_shape(roads) +
    tm_lines(col  =  "black", lwd  =  1) +
    tm_shape(major.roads) +
    tm_lines(lty = "solid", col  =  "black", lwd  =  2.5) +
    tm_scale_bar(position = c("left", "bottom"),
                 text.size = 1.2,
                 breaks = c(0, 25, 50, 100)) +
    tm_compass(position = c("left", "top"),
               size = 4) +
    tm_grid(lines = FALSE,
            labels.size = 1)
  
)

tmap_save(tm = map_roads,
          filename = "07_intermediate_results/maps/09-08_roads.major.roads.thick.svg")


### Map roads with main roads thicker + carcasses ---------

boundary <- rgdal::readOGR(dsn  = "06_processed_data/borders",
                           layer = "serengeti.mara.ecosystem.09-18")

major.roads <- rgdal::readOGR(dsn  =  "06_processed_data/roads/SNP.roads.QGIS", 
                              layer = "major_roads")

roads <- rgdal::readOGR(dsn  =  "06_processed_data/roads/SNP.roads.QGIS",
                        layer = "snp_roads_for_analyses_25-05-2021")

hy.carcasses <- rgdal::readOGR(dsn  = "06_processed_data/carcasses",
                               layer = "hy_carcasses_04-2021")



tmap_mode("view")
(map_roads <- tm_shape(boundary) +
    tm_polygons(col  =  "#d6e9d6", alpha = 1, boundary.col = "#5ea65c") +
    tm_shape(roads) +
    tm_lines(col  =  "black", lwd  =  1) +
    tm_shape(major.roads) +
    tm_lines(lty = "solid", col  =  "black", lwd  =  2.5) +
    tm_shape(hy.carcasses) +
    tm_symbols(size = 0.18, col = "#E69F00" , border.lwd = 0.3, border.col = "black") +
    tm_scale_bar(position = c("left", "bottom"),
                 text.size = 1.2,
                 breaks = c(0, 25, 50, 100)) +
    tm_compass(position = c("left", "top"),
               size = 4) +
    tm_grid(lines = FALSE,
            labels.size = 1)
  
)

tmap_save(tm = map_roads,
          filename = "07_intermediate_results/maps/10-26_roads.major.roads.thick.carcasses.svg")



### Map with rivers and waterbodies cropped and zoomed in ----

boundary <- rgdal::readOGR(dsn  = "06_processed_data/borders",
                           layer = "serengeti.mara.ecosystem.09-18")

waterbodies <- rgdal::readOGR(dsn  = "06_processed_data/water",
                              layer = "SNP.waterbodies")

rivers <- rgdal::readOGR(dsn  = "06_processed_data/water",
                         layer = "rivers2")

waterbodies <- rgdal::readOGR(dsn  = "06_processed_data/water",
                              layer = "test.better")


circle_65km <- rgdal::readOGR(dsn  =  "06_processed_data/roads",
                              layer = "visitor.center.buffer.65km")

rivers.65km <- raster::crop(rivers, circle_65km)
waterbodies.65km <- raster::crop(waterbodies, circle_65km)


crop.square <- extent(34.18437, 35.45, -3.0831, -1.8)
boundary2 <- raster::crop(boundary, crop.square)

tmap_mode("view")
(map_rivers_waterbodies <- tm_shape(boundary2) +
    tm_polygons(col  =  "#d6e9d6", alpha = 1, boundary.col = "#5ea65c") +
    tm_shape(waterbodies.65km) +
    tm_polygons(col  =  "#00ace6", boundary.col = "#00ace6") +
    tm_shape(rivers.65km) +
    tm_lines(col  =  "#00ace6", lwd  =  1) +
    tm_shape(circle_65km) +
    tm_polygons(col  =  "#56B4E9", alpha = 0, boundary.col = "#56B4E9", lwd = 2.5) +
    tm_scale_bar(position=c("left", "bottom"),
                 text.size = 0.75,
                 breaks = c(0, 20, 40)) +
    tm_compass(position = c("left", "top")) +
    tm_grid(lines = FALSE)
)

tmap_save(tm = map_rivers_waterbodies,
          height = 3.525674,
          width = 3.474513,
          units = "in",
          filename = "07_intermediate_results/maps/09-08_water_65km_zoomed.svg")



### Map with buildings cropped and zoomed in -----------

boundary <- rgdal::readOGR(dsn  = "06_processed_data/borders",
                           layer = "serengeti.mara.ecosystem.09-18")

building.point <- rgdal::readOGR(dsn  =  "06_processed_data/buildings",
                                 layer = "building.points.09-11")

circle_65km <- rgdal::readOGR(dsn  =  "06_processed_data/roads",
                              layer = "visitor.center.buffer.65km")


building.point.65km <- raster::crop(building.point, circle_65km)


building.polygon.65km <- rgdal::readOGR(dsn  =  "06_processed_data/buildings",
                                        layer = "buildings.polygons.cropped.65km.circle.09-11")

crop.square <- extent(34.18437, 35.45, -3.0831, -1.8)
boundary2 <- raster::crop(boundary, crop.square)

tmap_mode("view")
(map_buildings <- tm_shape(boundary2) +
    tm_polygons(col  =  "#d6e9d6", alpha = 1, boundary.col = "#5ea65c") +
    tm_shape(building.polygon.65km) +
    tm_polygons(col  =  "#333333", boundary.col = "black") +
    tm_shape(building.point.65km) +
    tm_symbols(size = 0.01, col = "black") + #, boundary.lwd = 0.3) +
    tm_shape(circle_65km) +
    tm_polygons(col  =  "#56B4E9", alpha = 0, boundary.col = "#56B4E9", lwd = 2.5) +
    tm_scale_bar(position=c("left", "bottom"),
                 text.size = 0.75,
                 breaks = c(0, 20, 40)) +
    tm_compass(position = c("left", "top")) +
    
    tm_grid(lines = FALSE)
)

tmap_save(tm = map_buildings,
          height = 3.525674,
          width = 3.474513,
          units = "in",
          filename = "07_intermediate_results/maps/09-03_buildings_65km_zoomed.svg")



### Map with roads within 65km and carcasses and zoomed ----------------

boundary <- rgdal::readOGR(dsn  = "06_processed_data/borders",
                           layer = "serengeti.mara.ecosystem.09-18")

major.roads <- rgdal::readOGR(dsn  =  "06_processed_data/roads/SNP.roads.QGIS", 
                              layer = "major_roads")

roads <- rgdal::readOGR(dsn  =  "06_processed_data/roads/SNP.roads.QGIS",
                        layer = "snp_roads_for_analyses_25-05-2021")


hy.carcasses <- rgdal::readOGR(dsn  = "06_processed_data/carcasses",
                               layer = "hy_carcasses_04-2021")

# hy.carcasses <- rgdal::readOGR(dsn  = "06_processed_data/carcasses",
#                                layer = "hy_carcasses_clan_members_04-2021")

circle_65km <- rgdal::readOGR(dsn  =  "06_processed_data/roads",
                              layer = "visitor.center.buffer.65km")

major.roads.65km <- raster::crop(major.roads, circle_65km)
roads.65km <- raster::crop(roads, circle_65km)

crop.square <- extent(34.18437, 35.45, -3.0831, -1.8)
boundary2 <- raster::crop(boundary, crop.square)




tmap_mode("view")
(map3 <- 
    tm_shape(boundary2) +
    tm_polygons(col  =  "#d6e9d6", alpha = 1, boundary.col = "#5ea65c") +
    tm_shape(roads.65km) +
    tm_lines(col  =  "black", lwd  =  1) +
    tm_shape(major.roads.65km) +
    tm_lines(lty = "solid", col  =  "black", lwd  =  2.5) +
    tm_shape(circle_65km) +
    tm_polygons(col  =  "#56B4E9", alpha = 0, boundary.col = "#56B4E9", lwd = 2.5) +
    tm_shape(hy.carcasses) +
    tm_symbols(size = 0.12, col = "#E69F00") +
    #tm_symbols(size  =  0.03, col  =  "#E69F00")
    
    tm_scale_bar(position=c("left", "bottom"),
                 text.size = 0.75,
                 breaks = c(0, 20, 40)) +
    tm_compass(position = c("left", "top")) +
    tm_grid(lines = FALSE)
)

tmap_save(tm = map3,
          height = 5, #3.525674,
          width = 5, #3.474513,
          units = "in",
          filename = "07_intermediate_results/2021-04/maps/roads.carcasses.cropped.and.zoomed.in.png")


### Map with segmented roads cropped and zoomed in -----------------

# CreateDataFrameRoads <- function(Shp) { # Converts the road shp into a df 
#   roads.df <- c()
#   for (i in 1:length(Shp@lines)) {
#     temp.i <- c()
#     for (j in 1:length(Shp@lines[[i]]@Lines)) {
#       
#       temp.j <- data.frame(ID = Shp@data[["osm_id"]][i],
#                            class = Shp@data[["fclass"]][i],
#                            long = Shp@lines[[i]]@Lines[[j]]@coords[,1],
#                            lat = Shp@lines[[i]]@Lines[[j]]@coords[,2],
#                            type = ifelse(Shp@data[["fclass"]][i] %in% c("primary", "secondary"), "major_road", "minor_road"))
#       temp.i <- rbind(temp.i, temp.j)
#     }
#     roads.df <- rbind(roads.df,temp.i)
#   }
#   return(roads.df)
# }
# 
# CreateSegment <- function(coords, from, to) {
#   distance <- 0
#   coordsOut <- c()
#   biggerThanFrom <- FALSE
#   for (i in 1:(nrow(coords) - 1)) {
#     d <- sqrt((coords[i, 1] - coords[i + 1, 1])^2 + (coords[i, 2] - coords[i + 1, 2])^2)   #distance between two successive points
#     distance <- distance + d
#     if (!biggerThanFrom && (distance > from)) {
#       w <- 1 - (distance - from)/d
#       x <- coords[i, 1] + w * (coords[i + 1, 1] - coords[i, 1])
#       y <- coords[i, 2] + w * (coords[i + 1, 2] - coords[i, 2])
#       coordsOut <- rbind(coordsOut, c(x, y))
#       biggerThanFrom <- TRUE
#     }
#     if (biggerThanFrom) {
#       if (distance > to) {
#         w <- 1 - (distance - to)/d
#         x <- coords[i, 1] + w * (coords[i + 1, 1] - coords[i, 1])
#         y <- coords[i, 2] + w * (coords[i + 1, 2] - coords[i, 2])
#         coordsOut <- rbind(coordsOut, c(x, y))
#         break
#       }
#       coordsOut <- rbind(coordsOut, c(coords[i + 1, 1], coords[i + 1, 2]))
#     }
#   }
#   return(coordsOut)
# }
# 
# CreateSegments <- function(coords, length = 0, n.parts = 0) {
#   stopifnot((length > 0 || n.parts > 0))
#   # calculate total length line
#   total_length <- 0
#   for (i in 1:(nrow(coords) - 1)) {
#     d <- sqrt((coords[i, 1] - coords[i + 1, 1])^2 + (coords[i, 2] - coords[i + 1, 2])^2)
#     total_length <- total_length + d
#   }
#   
#   # calculate the extremities of each segment
#   if (length > 0) {
#     stationing <- c(seq(from = 0, to = total_length, by = length), total_length)
#   } else {
#     stationing <- c(seq(from = 0, to = total_length, length.out = n.parts), 
#                     total_length)
#   }   # e.g. for c(seq(from = 0, to = 100, by = 25), 100), it creates:  (0 25 50 75 100 100)
#   
#   # calculate segments and store them in list
#   newlines <- list()
#   for (i in 1:(length(stationing) - 1)) {
#     newlines[[i]] <- CreateSegment(coords, stationing[i], stationing[i + 1])
#   }
#   return(newlines)
# }
# 
# SegmentationShp <- function(df) {
#   list.ID.new <- c(levels(df$ID_new)) #Création d'une liste contenant tous les nouveaux ID
#   segment.length <- 0.0226
#   coord.all.roads.segmented.df <- c() #sera le dataframe qui recevra les segments de toutes les routes
#   for (i in 1:length(list.ID.new)) {
#     df.i <- df %>%     #extraction des points correspondant à une seule route (en ne gardant que les points partageant le ID-new[i])
#       filter(ID_new == list.ID.new[i])
#     coord.one.road <- df.i %>%   
#       dplyr::select(long,lat)      #ne conserve que les coordonnées de la route
#     coord.one.road.segmented <- CreateSegments(coord.one.road, 
#                                                length = segment.length) #segmentation de la route qui correspond au subset
#     
#     for (j in 1:length(coord.one.road.segmented)) {
#       #fait un dataframe avec une colonne ID de la route + une colonne ID du segment de la route et les coordonnées GPS
#       coord.one.road.segmented.df <- data.frame(ID = list.ID.new[i], 
#                                                 ID_seg = paste0(list.ID.new[i],"_",j),
#                                                 long = coord.one.road.segmented[[j]][,1], 
#                                                 lat = coord.one.road.segmented[[j]][,2],
#                                                 osm_ID = df$ID[i],
#                                                 class = df.i$class[j])  
#       coord.all.roads.segmented.df <- rbind(coord.all.roads.segmented.df,coord.one.road.segmented.df) # rajoute temp_2 a un dataframe contenant les segments de toutes les routes
#     }
#   }
#   coordinates(coord.all.roads.segmented.df) =~ long+lat # Transformation du df en SpatialObject (Shp) grace à la spécification des coordonnées (colonnes long et lat)
#   proj4string(coord.all.roads.segmented.df) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#   x <- lapply(split(coord.all.roads.segmented.df, coord.all.roads.segmented.df$ID_seg), #classe les points en segments grace à l'ID_seg dans le Shp
#               function(x) Lines(list(Line(coordinates(x))),
#                                 x$ID_seg[1L]))
#   lines <- SpatialLines(x)
#   data <- data.frame(coord.all.roads.segmented.df) %>%
#     dplyr::select(ID = ID_seg,
#                   osm_ID,
#                   class) %>%    # only keeps the column ID_seg (i_j, with i the road number and j the segment number)
#     distinct() %>%
#     mutate(ID_road = sub("_.*","",ID),   # ID_road = i (in i_j)
#            nb_seg = as.numeric(sub("[^_]*_","",ID)),   # nb_seg = j (in i_j)
#            color = ifelse((nb_seg %% 2) == 0, "black","#b30000"),
#            type = ifelse(class %in% c("primary", "secondary"), "major_road", "minor_road"))   # if j is even, the segment has the color black, else the color red
#   rownames(data) <- data$ID
#   l <- SpatialLinesDataFrame(lines, data)
#   return(l)
# }
# 
# 
# roads <- rgdal::readOGR(dsn  =  "06_processed_data/roads/SNP.roads.QGIS",
#                         layer = "snp_roads_for_analyses_25-05-2021")
# 
# roads.df <- CreateDataFrameRoads(roads)
# 
# roads.df1 <- roads.df %>%
#   mutate(ID_new = as.factor(as.numeric(ID) + 0)) 
# 
# segmented.roads <- SegmentationShp(roads.df1)

segmented.roads <- rgdal::readOGR(dsn = "06_processed_data/roads/Segmented",
                                  layer = "roads.segmented_09-21")

boundary <- rgdal::readOGR(dsn  = "06_processed_data/borders",
                           layer = "serengeti.mara.ecosystem.09-18")

circle_65km <- rgdal::readOGR(dsn  =  "06_processed_data/roads",
                              layer = "visitor.center.buffer.65km")

segmented.roads.65km <- raster::crop(segmented.roads, circle_65km)

crop.square <- extent(34.18437, 35.45, -3.0831, -1.8)
boundary2 <- raster::crop(boundary, crop.square)


tmap_mode("view")
(map_seg_roads <- tm_shape(boundary2) +
    tm_polygons(col  =  "#d6e9d6", alpha = 1, boundary.col = "#5ea65c") +
    tm_shape(segmented.roads.65km) +
    tm_lines(col  =  "color", lwd  =  2.5) +
    tm_shape(circle_65km) +
    tm_polygons(col  =  "#56B4E9", alpha = 0, boundary.col = "#56B4E9", lwd = 2.5) +
    tm_scale_bar(position = c("left", "bottom"),
                 text.size = 0.75,
                 breaks = c(0, 20, 40)) +
    tm_compass(position = c("left", "top")) +
    tm_grid(lines = FALSE)
)

values.land.cover <- as.data.frame(values(land.cover))
distinct <- values.land.cover %>%
  distinct(`values(land.cover)`)

tmap_save(tm = map_seg_roads,
          height = 3.525674,
          width = 3.474513,
          units = "in",
          filename = "07_intermediate_results/maps/09-03_segmented_roads_65km_zoomed.svg")






### Map with land cover cropped and zoomed in ----------------

land.cover <- raster("06_processed_data/land_cover/Reed_2009/serengeti.land.cover.Reed.2009.cropped.zoomed.in.2.tif")

land.cover.trimmed <- trim(x = land.cover, padding = 0, values = 0) # This removes the cells with value 0
# For some reason, need to do that or otherwise I get white cells even in the SNP although their true value
# is not 0


legend.land.cover <- read_delim("06_processed_data/land_cover/Reed_2009/land.cover.types.in.SNP.csv", 
                                ";", escape_double = FALSE, trim_ws = TRUE) # Contains the color palette.
legend.land.cover <- read_delim("06_processed_data/land_cover/Reed_2009/land.cover.types.in.SNP_less_detailed.csv", 
                                ";", escape_double = FALSE, trim_ws = TRUE) # Contains the color palette.

boundary <- rgdal::readOGR(dsn  = "06_processed_data/borders",
                           layer = "serengeti.mara.ecosystem.09-18")

circle_65km <- rgdal::readOGR(dsn  =  "06_processed_data/roads",
                              layer = "visitor.center.buffer.65km")

crop.square <- extent(34.18437, 35.45, -3.0831, -1.8)
square.shp <- as(crop.square, 'SpatialPolygons')

boundary2 <- raster::crop(boundary, crop.square)


tmap_mode("view")
(map.land.cover <- tm_shape(square.shp) +
    tm_polygons(col  =  "white", alpha = 0, boundary.col = "white") +
    
    tm_shape(land.cover.trimmed) +
    tm_raster(palette = legend.land.cover$color_brewer,
              legend.show = FALSE) +
    tm_shape(boundary2) +
    tm_polygons(col  =  "#5ea65c", alpha = 0, boundary.col = "#5ea65c") +
    tm_shape(circle_65km) +
    tm_polygons(col  =  "#56B4E9", alpha = 0, boundary.col = "#56B4E9", lwd = 2.5) +
    tm_scale_bar(position=c("left", "bottom"),
                 width = 0.25,
                 text.size = 0.75,
                 breaks = c(0, 20, 40)) +
    tm_legend(show = FALSE) +
    tm_grid(lines = FALSE)
)

tmap_save(tm = map.land.cover,
          height = 3.525674,
          width = 3.474513,
          units = "in",
          filename = "07_intermediate_results/maps/10-23_land.cover.cropped.and.zoomed.in.svg")



### Map with land cover  + roads cropped and zoomed in ----------------

# Land cover ++++++++++++++
land.cover <- raster("06_processed_data/land_cover/Reed_2009/serengeti.land.cover.Reed.2009.cropped.zoomed.in.2.tif")

land.cover.trimmed <- trim(x = land.cover, padding = 0, values = 0) # This removes the cells with value 0
# For some reason, need to do that or otherwise I get white cells even in the SNP although their true value
# is not 0
legend.land.cover <- read_delim("06_processed_data/land_cover/Reed_2009/land.cover.types.in.SNP.csv", 
                                ";", escape_double = FALSE, trim_ws = TRUE) # Contains the color palette.
legend.land.cover <- read_delim("06_processed_data/land_cover/Reed_2009/land.cover.types.in.SNP_less_detailed.csv", 
                                ";", escape_double = FALSE, trim_ws = TRUE) # Contains the color palette.

# Roads ++++++++++++++++
major.roads <- rgdal::readOGR(dsn  =  "06_processed_data/roads/SNP.roads.QGIS", 
                              layer = "major_roads")

roads <- rgdal::readOGR(dsn  =  "06_processed_data/roads/SNP.roads.QGIS",
                        layer = "snp_roads_for_analyses_25-05-2021")


circle_65km <- rgdal::readOGR(dsn  =  "06_processed_data/roads",
                              layer = "visitor.center.buffer.65km")
# Crop + reproject
major.roads.65km <- spTransform(raster::crop(major.roads, circle_65km),
                                crs(land.cover.trimmed))
roads.65km <- spTransform(raster::crop(roads, circle_65km),
                          crs(land.cover.trimmed))




boundary <- rgdal::readOGR(dsn  = "06_processed_data/borders",
                           layer = "serengeti.mara.ecosystem.09-18")

crop.square <- extent(34.18437, 35.45, -3.0831, -1.8)
square.shp <- as(crop.square, 'SpatialPolygons')

boundary2 <- raster::crop(boundary, crop.square)


tmap_mode("view")
(map.land.cover <- tm_shape(square.shp) +
    tm_polygons(col  =  "white", alpha = 0, boundary.col = "white") +
    
    tm_shape(land.cover.trimmed) +
    tm_raster(palette = legend.land.cover$color_brewer,
              legend.show = FALSE) +
    
    tm_shape(roads.65km) +
    tm_lines(col  =  "black", lwd  =  1) +
    tm_shape(major.roads.65km) +
    tm_lines(lty = "solid", col  =  "black", lwd  =  2.5) +
    
    tm_shape(boundary2) +
    tm_polygons(col  =  "#5ea65c", alpha = 0, boundary.col = "#5ea65c") +
    tm_shape(circle_65km) +
    tm_polygons(col  =  "#56B4E9", alpha = 0, boundary.col = "#56B4E9", lwd = 2.5) +
    tm_scale_bar(position=c("left", "bottom"),
                 width = 0.25,
                 text.size = 0.75,
                 breaks = c(0, 20, 40)) +
    tm_legend(show = FALSE) +
    tm_grid(lines = FALSE)
)

tmap_save(tm = map.land.cover,
          height = 5, #3.525674,
          width = 5, # 3.474513,
          units = "in",
          filename = "07_intermediate_results/2021-04/maps/land.cover.roads.cropped.and.zoomed.in.png")




### Map east Africa -------------
library(tmap)

east.africa.borders <- rgdal::readOGR(dsn  =  "06_processed_data/borders/borders_african_countries",
                                      layer = "10-26.borders.east.africa")
square <- extent(33.8, 36, -3.7, -1.1)
square.shp <- as(square, 'SpatialPolygons')


tmap_mode("view")
(east.africa <- tm_shape(east.africa.borders) +
    # tm_polygons(col = "#fcfaa7", alpha = 0.5, boundary.col = "black", lwd = 2) +
    tm_polygons(col  =  "#f2efe9", boundary.col = "black", lwd = 2) +
    tm_shape(square.shp) +
    tm_polygons(col  =  "black", alpha = 0.70, boundary.col = "black") +
    tm_grid(lines = FALSE) 
)

tmap_save(tm = east.africa,
          # height = 20,
          # width = 8,
          filename = "07_intermediate_results/maps/10-26_east.africa.svg")

######
crop.square.shp <- as(crop.square, 'SpatialPolygons')
shapefile(crop.square.shp, "06_processed_data/roads/square.shp", overwrite=TRUE)