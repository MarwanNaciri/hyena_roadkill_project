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
Sys.setenv(LANG = "en")

# See https://rdrr.io/cran/tmap/man/tmap_arrange.html to arrange the maps

# A. Create the panels ---------------------------------------------------------

# ~ 1. Map Serengeti migration --------

boundary <- readOGR(dsn  = "06_processed_data/borders",
                    layer = "serengeti.mara.ecosystem.09-18")

boundary.kenya.tenzania <- readOGR(dsn  =  "06_processed_data/migration",
                                   layer = "frontier.kenya")


tmap_mode("view")
(map_great_migration <- tm_shape(boundary) +
    tm_polygons(col  =  "#d6e9d6", alpha = 1, boundary.col = "#5ea65c") +
    tm_shape(boundary.kenya.tenzania) +
    tm_lines(col  =  "black", lwd  =  2) +
    tm_scale_bar(position = c("left", "bottom"),
                 text.size = 1,
                 breaks = c(0, 25, 50, 100)) +
    tm_compass(position = c("right", "top"),
               size = 2) +
    tm_grid(lines = FALSE) +
    tm_layout(title= "(b)", 
              title.position = c('left', 'top'))
  
)
save(map_great_migration, 
     file = "07_intermediate_results/2021-04/maps/map_great_migration.RData")

tmap_save(tm = map_great_migration,
          # height = 3.525674,
          # width = 3.474513,
          # units = "in",
          filename = "07_intermediate_results/maps/09-08_snp.migration.svg")



# ~ 2. Map east Africa -------------

serengeti.square <- extent(33.8, 36, -3.7, -1.1)
serengeti.square.shp <- as(serengeti.square, 'SpatialPolygons')

background <- extent(28.5, 42.6, -11, 4.3)
background.shp <- as(background, 'SpatialPolygons')

east.africa <- readOGR(dsn  =  "06_processed_data/borders",
                       layer = "east_africa_no_islands")

tmap_mode("view")
(map_east_africa <- tm_shape(background.shp) +
    tm_polygons(col = "#00ace6") +
    
    tm_shape(east.africa) +
    # tm_polygons(col = "#fcfaa7", alpha = 0.5, boundary.col = "black", lwd = 2) +
    tm_polygons(col  = "white",  #"#f2efe9", 
                boundary.col = "black") +
    
    tm_shape(serengeti.square.shp) +
    tm_polygons(col  =  "black", alpha = 0.70, boundary.col = "black") +
    tm_grid(lines = FALSE) +
    tm_layout(title= "(a)", 
              title.position = c('left', 'top'))
)

save(map_east_africa, file = "07_intermediate_results/2021-04/maps/map_east_africa.RData")



# ~ 3. Map roads with main roads thicker + carcasses -----------------------------

boundary <- readOGR(dsn  = "06_processed_data/borders",
                    layer = "SNP.lastest")

roads <- readOGR(dsn  =  "06_processed_data/roads/SNP.roads.QGIS",
                 layer = "snp_roads_for_analyses_25-05-2021")
major.roads <- readOGR(dsn  =  "06_processed_data/roads/SNP.roads.QGIS", 
                       layer = "major_roads")

hy.carcasses <- readOGR(dsn  = "06_processed_data/carcasses",
                        layer = "hy_carcasses_04-2021")

# # bbox
# range.x <- crop.square@xmax - crop.square@xmin
# range.y <- crop.square@ymax - crop.square@ymin
# 
# bbox <-  st_bbox(crop.square)
# A <- 0.
# bbox[] <- bbox[] + c(-A*range.x, - A*range.y, A*range.x, A*range.y)


tmap_mode("view")
(map_roads_carcasses <- 
    tm_shape(boundary) +
    tm_polygons(col  =  "#d6e9d6", alpha = 1, boundary.col = "#5ea65c") +
    
    tm_shape(roads) +
    tm_lines(col  =  "black", lwd  =  1) +
    tm_shape(major.roads) +
    tm_lines(lty = "solid", col  =  "black", lwd  =  2.5) +
    
    tm_shape(hy.carcasses) +
    tm_symbols(size = 0.08, col = "#E69F00", border.lwd = 0.25) +
    
    tm_scale_bar(position=c("left", "bottom"),
                 text.size = 0.75,
                 breaks = c(0, 25, 50, 100)) +
    # tm_compass(position = c("left", "top")) +
    tm_grid(lines = FALSE) +
    tm_layout(title= "(b)", 
              title.position = c('left', 'top'))
)


save(map_roads_carcasses, 
     file = "07_intermediate_results/2021-04/maps/map_roads_carcasses.RData")
tmap_save(tm = map_roads_carcasses,
          filename = "07_intermediate_results/")




# ~ 4. Map with roads within 65km and carcasses, zoomed ----------------

boundary <- readOGR(dsn  = "06_processed_data/borders",
                    layer = "SNP.lastest")

roads <- readOGR(dsn  =  "06_processed_data/roads/SNP.roads.QGIS",
                 layer = "snp_roads_for_analyses_25-05-2021")
major.roads <- readOGR(dsn  =  "06_processed_data/roads/SNP.roads.QGIS", 
                       layer = "major_roads")

hy.carcasses <- readOGR(dsn  = "06_processed_data/carcasses",
                        layer = "hy_carcasses_04-2021")

# Crop
circle_65km <- readOGR(dsn  =  "06_processed_data/borders",
                       layer = "circle_65km")

roads.65km <- raster::crop(roads, circle_65km)
major.roads.65km <- raster::crop(major.roads, circle_65km)

crop.square <- extent(34.18437, 35.45, -3.0831, -1.8)
boundary2 <- raster::crop(boundary, crop.square)

# bbox
range.x <- crop.square@xmax - crop.square@xmin
range.y <- crop.square@ymax - crop.square@ymin

bbox <-  st_bbox(crop.square)
A <- 0.
bbox[] <- bbox[] + c(-A*range.x, - A*range.y, A*range.x, A*range.y)

tmap_mode("view")
(map_roads_carcasses_zoom <- 
    tm_shape(boundary2, bbox = bbox) +
    tm_polygons(col  =  "#d6e9d6", alpha = 1, boundary.col = "#5ea65c") +
    
    tm_shape(roads.65km) +
    tm_lines(col  =  "black", lwd  =  1) +
    tm_shape(major.roads.65km) +
    tm_lines(lty = "solid", col  =  "black", lwd  =  2.5) +
    
    tm_shape(circle_65km) +
    tm_polygons(col  =  "#56B4E9", alpha = 0, boundary.col = "#56B4E9", lwd = 2.5) +
    
    tm_shape(hy.carcasses) +
    tm_symbols(size = 0.08, col = "#E69F00", border.lwd = 0.3) +

    tm_scale_bar(position=c("left", "bottom"),
                 text.size = 0.75,
                 breaks = c(0, 20, 40)) +
    # tm_compass(position = c("left", "top")) +
    tm_grid(lines = FALSE) +
    tm_layout(title= "(c)", 
              title.position = c('left', 'top'))
)

save(map_roads_carcasses_zoom, 
     file = "07_intermediate_results/2021-04/maps/map_roads_carcasses_zoom.RData")



(map_roads_carcasses_zoom <- 

    tm_shape(hy.carcasses) +
    tm_symbols(size = 0.08, col = "#E69F00", border.lwd = 0.3)
)



# ~ 5. Map with amenity cropped, zoomed -----------

boundary <- readOGR(dsn = "06_processed_data/borders",
                    layer = "SNP.lastest")

# amenity.polygons <- readOGR(dsn  =  "06_processed_data/amenity",
#                             layer = "buildings_polygons_for_analyses_2021-05-25")
amenity.polygons <- readOGR(dsn  =  "06_processed_data/amenity",
                            layer = "amenity_polygons_snp_circle_2021-07-04")

amenity.points <- readOGR(dsn  =  "06_processed_data/amenity",
                          layer = "building_points_for analyses_2020-09-11")

# Crop
circle_65km <- readOGR(dsn  =  "06_processed_data/borders",
                       layer = "circle_65km")



crop.square <- extent(34.18437, 35.45, -3.0831, -1.8)
boundary2 <- raster::crop(boundary, crop.square)

amenity.polygons.65km <- raster::crop(amenity.polygons, crop.square)# circle_65km)
amenity.points.65km <- raster::crop(amenity.points, circle_65km)

# bbox
range.x <- crop.square@xmax - crop.square@xmin
range.y <- crop.square@ymax - crop.square@ymin

bbox <-  st_bbox(crop.square)
A <- 0.
bbox[] <- bbox[] + c(-A*range.x, - A*range.y, A*range.x, A*range.y)


tmap_mode("view")
(map_amenity_zoom <- tm_shape(boundary2, bbox = bbox) +
    tm_polygons(col  =  "#d6e9d6", alpha = 1, boundary.col = "#5ea65c") +
    tm_shape(amenity.polygons.65km) +
    tm_polygons(col  =  "#333333", boundary.col = "black") +
    tm_shape(amenity.points.65km) +
    tm_symbols(size = 0.01, col = "black") + #, boundary.lwd = 0.3) +
    tm_shape(circle_65km) +
    tm_polygons(col  =  "#56B4E9", alpha = 0, boundary.col = "#56B4E9", lwd = 2.5) +
    tm_scale_bar(position=c("left", "bottom"),
                 text.size = 0.75,
                 breaks = c(0, 20, 40)) +
    # tm_compass(position = c("left", "top")) +
    
    tm_grid(lines = FALSE) +
    tm_layout(title= "(e)", 
              title.position = c('left', 'top'))
)

save(map_amenity_zoom, 
     file = "07_intermediate_results/2021-04/maps/map_amenity_zoom.RData")

# tmap_save(tm = map_buildings,
#           height = 3.525674,
#           width = 3.474513,
#           units = "in",
#           filename = "07_intermediate_results/maps/09-03_buildings_65km_zoomed.svg")




# ~ 6. Map with rivers & waterbodies cropped, zoomed ----------------------

boundary <- readOGR(dsn  = "06_processed_data/borders",
                    layer = "SNP.lastest")

circle_65km <- readOGR(dsn  =  "06_processed_data/borders",
                       layer = "circle_65km")

crop.square <- extent(34.18437, 35.45, -3.0831, -1.8)
boundary2 <- raster::crop(boundary, crop.square)


rivers <- readOGR(dsn  = "06_processed_data/water",
                  layer = "rivers_circle_snp_2021-07-04")
waterbodies <- readOGR(dsn  = "06_processed_data/water",
                       layer = "waterbodies_circle_snp_2021-07-04")

rivers.cropped <- raster::crop(rivers, crop.square)
waterbodies.cropped <- raster::crop(waterbodies, crop.square)

# bbox
range.x <- crop.square@xmax - crop.square@xmin
range.y <- crop.square@ymax - crop.square@ymin

bbox <-  st_bbox(crop.square)
A <- 0.
bbox[] <- bbox[] + c(-A*range.x, - A*range.y, A*range.x, A*range.y)

tmap_mode("view")
(map_rivers_waterbodies_zoom <- tm_shape(boundary2, bbox = bbox) +
    tm_polygons(col  =  "#d6e9d6", alpha = 1, boundary.col = "#5ea65c") +
    tm_shape(waterbodies.cropped) +
    tm_polygons(col  =  "#00ace6", boundary.col = "#00ace6") +
    tm_shape(rivers.cropped) +
    tm_lines(col  =  "#00ace6", lwd  =  1) +
    tm_shape(circle_65km) +
    tm_polygons(col  =  "#56B4E9", alpha = 0, boundary.col = "#56B4E9", lwd = 2.5) +
    tm_scale_bar(position=c("left", "bottom"),
                 text.size = 0.75,
                 breaks = c(0, 20, 40)) +
    # tm_compass(position = c("left", "top")) +
    tm_grid(lines = FALSE) +
    tm_layout(title= "(f)", 
              title.position = c('left', 'top'))
)

save(map_rivers_waterbodies_zoom, 
     file = "07_intermediate_results/2021-04/maps/map_rivers_waterbodies_zoom.RData")

# tmap_save(tm = map_rivers_waterbodies,
#           height = 3.525674,
#           width = 3.474513,
#           units = "in",
#           filename = "07_intermediate_results/maps/09-08_water_65km_zoomed.svg")


# ~ 7. Map with land cover cropped and zoomed ----------------------------------

land.cover <- raster("06_processed_data/land_cover/Reed_2009/serengeti.land.cover.Reed.2009.cropped.zoomed.in.tif")
land.cover.trimmed <- trim(x = land.cover, padding = 0, values = 0) # This removes the cells with value 0

boundary <- readOGR(dsn  = "06_processed_data/borders",
                    layer = "SNP.lastest")

circle_65km <- readOGR(dsn  =  "06_processed_data/borders",
                       layer = "circle_65km")

crop.square <- extent(34.18437, 35.45, -3.0831, -1.8)
square.shp <- as(crop.square, 'SpatialPolygons')


range.x <- crop.square@xmax - crop.square@xmin
range.y <- crop.square@ymax - crop.square@ymin

bbox <-  st_bbox(crop.square)
A <- 0.015
bbox[] <- bbox[] + c(-A*range.x, - A*range.y, A*range.x, A*range.y)
                      

boundary2 <- raster::crop(boundary, crop.square)

legend.land.cover <- read_delim("06_processed_data/land_cover/Reed_2009/land.cover.types.in.SNP.csv", 
                                ";", escape_double = FALSE, trim_ws = TRUE) # Contains the palette.

tmap_mode("view")
(map_land_cover_zoom <- 
    # tm_shape(square.shp) +
    # tm_polygons(col  =  "white", alpha = 0, boundary.col = "white") +

    tm_shape(land.cover.trimmed, bbox = bbox) +
    tm_raster(palette = legend.land.cover$color_brewer,
              legend.show = FALSE) +
    tm_shape(boundary2) +
    tm_polygons(col  =  "#5ea65c", alpha = 0, boundary.col = "#5ea65c") +
    tm_shape(circle_65km) +
    tm_polygons(col  =  "#56B4E9", alpha = 0, boundary.col = "#56B4E9", lwd = 2.5) +

    tm_scale_bar(position = c("left", "bottom"),
                 width = 0.25,
                 text.size = 0.75,
                 breaks = c(0, 20, 40)) +
    tm_legend(show = FALSE) +
    tm_grid(lines = FALSE) +
    tm_layout(title= "(g)", 
              title.position = c('left', 'top'))
)

save(map_land_cover_zoom, 
     file = "07_intermediate_results/2021-04/maps/map_land_cover_zoom.RData")

tmap_save(tm = map_land_cover_zoom,
          # height = 3.525674,
          # width = 3.474513,
          # units = "in",
          filename = "07_intermediate_results/2021-04/maps/map_land_cover_zoom.png")


# ~ 8. Map with segmented roads cropped and zoomed in --------------------------

segmented.roads.65km.df <- read_csv("06_processed_data/glm_data_2021-04/roads.segmented.cropped.no.short.df.csv")

# Convert the data frame into SpatialLines
coordinates(segmented.roads.65km.df) =~ long+lat # Transformation du df en SpatialObject (Shp) grace à la spécification des coordonnées (colonnes long et lat)
proj4string(segmented.roads.65km.df) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
x <- lapply(split(segmented.roads.65km.df,
                  segmented.roads.65km.df$ID_road_seg), #classe les points en segments grace à l'ID_seg dans le Shp
            function(x) Lines(list(Line(coordinates(x))),
                              x$ID_road_seg[1L]))
lines <- SpatialLines(x)


# Convert the SpatialLines into SpatialLinesDataFrame
data <- data.frame(segmented.roads.65km.df) %>%
  dplyr::select(ID_road_seg,
                ID_seg,
                osm_ID,
                class) %>%    # only keeps the column ID_seg (i_j, with i the road number and j the segment number)
  distinct() %>%
  mutate(ID_road = sub("_.*", "", ID_seg),   # ID_road = i (in i_j)
         nb_seg = as.numeric(sub("[^_]*_", "", ID_seg)),   # nb_seg = j (in i_j)
         color = ifelse((nb_seg %% 2) == 0, "black","#b30000"),
         type = ifelse(class %in% c("primary", "secondary"), "major_road", "minor_road"))   # if j is even, the segment has the color black, else the color red
rownames(data) <- data$ID_road_seg
segmented.roads.65km.shp <- SpatialLinesDataFrame(lines, data)



boundary <- readOGR(dsn  = "06_processed_data/borders",
                    layer = "SNP.lastest")

circle_65km <- readOGR(dsn  =  "06_processed_data/borders",
                       layer = "circle_65km")

# Crop
crop.square <- extent(34.18437, 35.45, -3.0831, -1.8)
boundary2 <- raster::crop(boundary, crop.square)

range.x <- crop.square@xmax - crop.square@xmin
range.y <- crop.square@ymax - crop.square@ymin

bbox <-  st_bbox(crop.square)
A <- 0.
bbox[] <- bbox[] + c(-A*range.x, - A*range.y, A*range.x, A*range.y)

tmap_mode("view")
(map_segmented_roads_zoom <- tm_shape(boundary2, bbox = bbox) +
    tm_polygons(col  =  "#d6e9d6", alpha = 1, boundary.col = "#5ea65c") +
    tm_shape(segmented.roads.65km.shp) +
    tm_lines(col  =  "color", lwd  =  2.5) +
    tm_shape(circle_65km) +
    tm_polygons(col  =  "#56B4E9", alpha = 0, boundary.col = "#56B4E9", lwd = 2.5) +
    tm_scale_bar(position = c("left", "bottom"),
                 text.size = 0.75,
                 breaks = c(0, 20, 40)) +
    # tm_compass(position = c("left", "top")) +
    tm_grid(lines = FALSE) +
    tm_layout(title= "(h)", 
              title.position = c('left', 'top'))
)


save(map_segmented_roads_zoom, 
     file = "07_intermediate_results/2021-04/maps/map_segmented_roads_zoom.RData")

# tmap_save(tm = map_seg_roads,
#           height = 3.525674,
#           width = 3.474513,
#           units = "in",
#           filename = "07_intermediate_results/maps/09-03_segmented_roads_65km_zoomed.svg")


# ~ 9. Temporary map for figure building ---------------------------------------

boundary <- readOGR(dsn  = "06_processed_data/borders",
                    layer = "SNP.lastest")

circle_65km <- readOGR(dsn  =  "06_processed_data/borders",
                       layer = "circle_65km")

crop.square <- extent(34.18437, 35.45, -3.0831, -1.8)
boundary2 <- raster::crop(boundary, crop.square)


# bbox
range.x <- crop.square@xmax - crop.square@xmin
range.y <- crop.square@ymax - crop.square@ymin

bbox <-  st_bbox(crop.square)
A <- 0.
bbox[] <- bbox[] + c(-A*range.x, - A*range.y, A*range.x, A*range.y)

tmap_mode("view")
(map_temp <- 
    tm_shape(boundary2, bbox = bbox) +
    tm_polygons(col  =  "#d6e9d6", alpha = 1, boundary.col = "#5ea65c") +
    tm_shape(circle_65km) +
    tm_polygons(col  =  "#56B4E9", alpha = 0, boundary.col = "#56B4E9", lwd = 2.5) +
    tm_scale_bar(position=c("left", "bottom"),
                 text.size = 0.75,
                 breaks = c(0, 20, 40)) +
    # tm_compass(position = c("left", "top")) +
    tm_grid(lines = FALSE) +
    tm_layout(title= "(d)", 
              title.position = c('left', 'top'))
)

save(map_temp, 
     file = "07_intermediate_results/2021-04/maps/map_temp.RData")


# B. Build the figure ----------------------------------------------------------

# Load the data
load("07_intermediate_results/2021-04/maps/map_east_africa.RData")
load("07_intermediate_results/2021-04/maps/map_great_migration.RData")
load("07_intermediate_results/2021-04/maps/map_roads_carcasses.RData")
load("07_intermediate_results/2021-04/maps/map_roads_carcasses_zoom.RData")
load("07_intermediate_results/2021-04/maps/map_amenity_zoom.RData")
load("07_intermediate_results/2021-04/maps/map_rivers_waterbodies_zoom.RData")
load("07_intermediate_results/2021-04/maps/map_land_cover_zoom.RData")
load("07_intermediate_results/2021-04/maps/map_segmented_roads_zoom.RData")
load("07_intermediate_results/2021-04/maps/map_temp.RData")

# Plot
# With east africa as the first panel
figure <- tmap_arrange(map_east_africa, map_great_migration, map_roads_carcasses_zoom,
                       map_temp, map_amenity_zoom, map_rivers_waterbodies_zoom,
                       map_land_cover_zoom, map_temp, map_segmented_roads_zoom,
                       widths = c(0.33, 0.33, 0.33,
                                  0.33, 0.33, 0.33,
                                  0.33, 0.33, 0.33),
                       ncol = 3,
                       nrow = 3)
tmap_save(tm = figure,
          # height = 5,
          # width = 8,
          # units = "in",
          filename = "07_intermediate_results/2021-04/maps/figure.test.2.png")
tmap_save(tm = figure,
          filename = "07_intermediate_results/2021-04/maps/figure.test.2.svg")


# figure <- tmap_arrange(map_great_migration, map_roads_carcasses, map_roads_carcasses_zoom,
#                        map_rivers_waterbodies_zoom, map_amenity_zoom, map_rivers_waterbodies_zoom,
#                        map_land_cover_zoom, map_segmented_roads_zoom, map_segmented_roads_zoom,
#                        widths = c(0.33, 0.33, 0.33,
#                                   0.33, 0.33, 0.33,
#                                   0.33, 0.33, 0.33),
#                        ncol = 3,
#                        nrow = 3)
# 
# tmap_save(tm = figure,
#           # height = 5,
#           # width = 8,
#           # units = "in",
#           filename = "07_intermediate_results/2021-04/maps/figure.test.png")
# tmap_save(tm = figure,
#           filename = "07_intermediate_results/2021-04/maps/figure.test.png")



# Temporary figure -------------------

boundary <- readOGR(dsn  = "06_processed_data/borders",
                    layer = "SNP.lastest")

roads <- readOGR(dsn  =  "06_processed_data/roads/SNP.roads.QGIS",
                 layer = "snp_roads_for_analyses_25-05-2021")
major.roads <- readOGR(dsn  =  "06_processed_data/roads/SNP.roads.QGIS", 
                       layer = "major_roads")

hy.carcasses <- readOGR(dsn  = "06_processed_data/carcasses",
                        layer = "hy_carcasses_04-2021")

circle_65km <- readOGR(dsn  =  "06_processed_data/borders",
                       layer = "circle_65km")

circle_47.5km <- readOGR(dsn  =  "06_processed_data/borders",
                       layer = "circle_47.5km")



tmap_mode("view")
(map_roads_carcasses <- 
    tm_shape(boundary) +
    tm_polygons(col  =  "#d6e9d6", alpha = 1, boundary.col = "#5ea65c") +
    
    tm_shape(roads) +
    tm_lines(col  =  "black", lwd  =  1) +
    tm_shape(major.roads) +
    tm_lines(lty = "solid", col  =  "black", lwd  =  2.5) +
    
    # tm_shape(hy.carcasses) +
    # tm_symbols(size = 0.08, col = "#E69F00", border.lwd = 0.25) +
    tm_shape(hy.carcasses) +
    tm_symbols(size = 0.08, col = age, border.lwd = 0.25) +
    tm_shape(circle_65km) +
    tm_polygons(col  =  "#56B4E9", alpha = 0, boundary.col = "#56B4E9", lwd = 2.5) +
    tm_shape(circle_47.5km) +
    tm_polygons(col  =  "#56B4E9", alpha = 0, boundary.col = "#56B4E9", lwd = 2.5) +
    
    tm_scale_bar(position=c("left", "bottom"),
                 text.size = 0.75,
                 breaks = c(0, 25, 50, 100)) +
    # tm_compass(position = c("left", "top")) +
    tm_grid(lines = FALSE) +
    tm_layout(title= "(b)", 
              title.position = c('left', 'top'))
)




boundary <- readOGR(dsn  = "06_processed_data/borders",
                    layer = "SNP.lastest")

roads <- readOGR(dsn  =  "06_processed_data/roads/SNP.roads.QGIS",
                 layer = "snp_roads_for_analyses_25-05-2021")
major.roads <- readOGR(dsn  =  "06_processed_data/roads/SNP.roads.QGIS", 
                       layer = "major_roads")

hy.carcasses <- readOGR(dsn  = "06_processed_data/carcasses",
                        layer = "hy_carcasses_04-2021")

# Crop
circle_65km <- readOGR(dsn  =  "06_processed_data/borders",
                       layer = "circle_65km")

roads.65km <- raster::crop(roads, circle_65km)
major.roads.65km <- raster::crop(major.roads, circle_65km)

boundary.df <- tidy(boundary)
roads.df <- tidy(roads.65km)
major.roads.df <- tidy(major.roads.65km)
hy.carcasses.df <- as.data.frame(hy.carcasses) %>%
  filter(long != 0)
circle_65km_df <- tidy(circle_65km)

hy.carcasses.df <- hy_carcasses %>%
  filter(age == "subadult",
         collision_certainty_score_NEW > 0)

(map_roads_carcasses_zoom <- ggplot() +
    geom_path(data = roads.df, 
              aes(x = long, y = lat, group = id), size = 0.25) +
    geom_path(data = major.roads.df, 
              aes(x = long, y = lat, group = id), size = 0.75) +
    
    geom_point(data = hy.carcasses.df,
               aes(x = long, y = lat, color = sex), size = 1.5)
)    

ggsave(plot = map_roads_carcasses_zoom, filename = "07_intermediate_results/2021-04/maps/temp.png",
       height = 6.5, width = 6.5)
