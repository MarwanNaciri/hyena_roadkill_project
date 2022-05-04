#==============================================================================#
#                                                                              #
#                       Plot the maps of the study area                        #        
#                                                                              #
#==============================================================================#

library(rgdal)
library(raster)
library(tmap)
library(tmaptools)
# library(leaflet)
# library(shinyjs)
library(sp)
library(sf)
library(tidyverse)
Sys.setenv(LANG = "en")

# See https://rdrr.io/cran/tmap/man/tmap_arrange.html to arrange the maps

# A. Create the panels ---------------------------------------------------------

# ~ 1. Map Serengeti migration -------------------------------------------------

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
     file = "07_intermediate_results/2021-04/maps/map_great_migration_2021-09.RData")

tmap_save(tm = map_great_migration,
          # height = 3.525674,
          # width = 3.474513,
          # units = "in",
          filename = "07_intermediate_results/maps/09-08_snp.migration.svg")



# ~ 2. Map east Africa -------------

east.africa <- readOGR(dsn  =  "06_processed_data/borders",
                       layer = "east_africa_no_islands")

boundary <- readOGR(dsn  = "06_processed_data/borders",
                    layer = "serengeti.mara.ecosystem.09-18")

serengeti.square <- extent(33.8, 36, -3.7, -1.1)
serengeti.square.shp <- as(serengeti.square, 'SpatialPolygons')
crs(serengeti.square.shp) <- crs(east.africa)

background <- extent(28.5, 42.6, -11, 4.3)
background.shp <- as(background, 'SpatialPolygons')
crs(background.shp) <- crs(east.africa)



tmap_mode("view")
(map_east_africa <- tm_shape(background.shp) + # The blue background
    tm_polygons(col = "#00ace6") +
    
    tm_shape(east.africa) +                   # Country borders
    tm_polygons(col  = "white",  #"#f2efe9", 
                boundary.col = "black") +
    
    tm_shape(boundary) +
    tm_polygons(col  =  "#d6e9d6", alpha = 1, boundary.col = "#5ea65c", lwd = 0.5) +
    
    tm_shape(serengeti.square.shp) +
    tm_polygons(col  =  "black", alpha = 0.50, boundary.col = "black") +
    tm_grid(lines = FALSE) +
    tm_layout(title= "(a)", 
              title.position = c('left', 'top'))
)

save(map_east_africa, file = "07_intermediate_results/2021-04/maps/map_east_africa_2021-09.RData")




# ~ 4. Map with roads within study area and carcasses --------------------------

# boundary <- readOGR(dsn  = "06_processed_data/borders",
#                     layer = "SNP.lastest.cropped.margin.2021-09")
boundary <- readOGR(dsn  = "06_processed_data/borders",
                    layer = "serengeti.mara.ecosystem.cropped.margin.2021-09")

roads.cropped <- readOGR(dsn  =  "06_processed_data/roads/SNP.roads.QGIS",
                         layer = "snp_roads_according_to_FZS_map_cropped")

major.roads.cropped <- readOGR(dsn  =  "06_processed_data/roads/SNP.roads.QGIS", 
                               layer = "major_roads_cropped")

hy.carcasses.high.certainty <- readOGR(dsn  = "06_processed_data/carcasses",
                        layer = "hy_carcasses_high_certainty_2022-04")


crop.square.margin <- readOGR(dsn  = "06_processed_data/borders",
                              layer = "crop.square.margin.2021-09")
bbox <-  st_bbox(crop.square.margin)


tmap_mode("view")
(map_roads_carcasses_zoom <- 
    tm_shape(boundary, bbox = bbox) +
    tm_polygons(col  =  "#d6e9d6", alpha = 1, boundary.col = "#5ea65c") +
    
    tm_shape(roads.cropped, bbox = bbox) +
    tm_lines(col  =  "black", lwd  =  1) +
    tm_shape(major.roads.cropped, bbox = bbox) +
    tm_lines(lty = "solid", col  =  "black", lwd  =  2.5) +
    
    tm_shape(hy.carcasses.high.certainty, bbox = bbox) +
    tm_symbols(size = 0.065, col = "#E69F00", border.lwd = 0.4) +
    
    tm_scale_bar(position=c("left", "bottom"),
                 text.size = 0.6,
                 breaks = c(0, 20, 40)) +
    # tm_compass(position = c("left", "top")) +
    tm_grid(lines = FALSE) +
    tm_layout(title= "(c)", 
              title.position = c('left', 'top'))
)

save(map_roads_carcasses_zoom, 
     file = "07_intermediate_results/2021-04/maps/map_roads_carcasses_zoom_2021-09.RData")



# ~ 5. Map with amenity cropped, zoomed -----------

# boundary <- readOGR(dsn  = "06_processed_data/borders",
#                     layer = "SNP.lastest.cropped.margin.2021-09")
boundary <- readOGR(dsn  = "06_processed_data/borders",
                    layer = "serengeti.mara.ecosystem.cropped.margin.2021-09")

amenity.polygons <- readOGR(dsn  =  "06_processed_data/amenity",
                            layer = "amenity_polygon_plot_2021-09")

amenity.points <- readOGR(dsn  =  "06_processed_data/amenity",
                          layer = "building_points_for analyses_2020-09-11")


crop.square.margin <- readOGR(dsn  = "06_processed_data/borders",
                              layer = "crop.square.margin.2021-09")

bbox <-  st_bbox(crop.square.margin)

tmap_mode("view")
(map_amenity_zoom <- 
    tm_shape(boundary, bbox = bbox) + 
    tm_polygons(col  =  "#d6e9d6", alpha = 1, boundary.col = "#5ea65c") +
    
    tm_shape(amenity.polygons,bbox = bbox) +
    tm_polygons(col  =  "#333333", boundary.col = "black") +
    
    tm_scale_bar(position=c("left", "bottom"),
                 text.size = 0.6,
                 breaks = c(0, 20, 40)) +
    # tm_compass(position = c("left", "top")) +
    tm_grid(lines = FALSE,
            labels.show = FALSE) +
    tm_layout(title= "(e)", 
              title.position = c('left', 'top'))
)

tmap_save(tm = map_amenity_zoom,
          filename = "07_intermediate_results/maps/amenity_2022-04.png")
save(map_amenity_zoom, 
     file = "07_intermediate_results/2021-04/maps/map_amenity_zoom_2021-09_no_axis.RData")

# save(map_amenity_zoom, 
#      file = "07_intermediate_results/2021-04/maps/map_amenity_zoom_2021-09.RData")

# tmap_save(tm = map_buildings,
#           height = 3.525674,
#           width = 3.474513,
#           units = "in",
#           filename = "07_intermediate_results/maps/09-03_buildings_65km_zoomed.svg")




# ~ 6. Map with rivers & waterbodies cropped, zoomed ----------------------

# boundary <- readOGR(dsn  = "06_processed_data/borders",
#                     layer = "SNP.lastest.cropped.margin.2021-09")
boundary <- readOGR(dsn  = "06_processed_data/borders",
                    layer = "serengeti.mara.ecosystem.cropped.margin.2021-09")

rivers <- readOGR(dsn  = "06_processed_data/water",
                  layer = "rivers_crop_square_margin_2021-09")
waterbodies <- readOGR(dsn  = "06_processed_data/water",
                       layer = "waterbodies_crop_square_margin_2021-09")

bbox <-  st_bbox(extent(rivers@bbox[1, 1], rivers@bbox[1, 2],
                        rivers@bbox[2, 1], rivers@bbox[2, 2]))

tmap_mode("view")
(map_rivers_waterbodies_zoom <- tm_shape(boundary, bbox = bbox) +
    tm_polygons(col  =  "#d6e9d6", alpha = 1, boundary.col = "#5ea65c") +
    tm_shape(waterbodies, bbox = bbox) +
    tm_polygons(col  =  "#00ace6", boundary.col = "#00ace6") +
    tm_shape(rivers, bbox = bbox) +
    tm_lines(col  =  "#00ace6", lwd  =  1) +
    tm_scale_bar(position=c("left", "bottom"),
                 text.size = 0.6,
                 breaks = c(0, 20, 40)) +
    # tm_compass(position = c("left", "top")) +
    tm_grid(lines = FALSE,
            labels.show = FALSE) +
    tm_layout(title= "(f)", 
              title.position = c('left', 'top'))
)
save(map_rivers_waterbodies_zoom, 
     file = "07_intermediate_results/2021-04/maps/map_rivers_waterbodies_zoom_2021-09_no_labels.RData")
# save(map_rivers_waterbodies_zoom, 
#      file = "07_intermediate_results/2021-04/maps/map_rivers_waterbodies_zoom_2021-09.RData")


# ~ 7. Map with land cover cropped and zoomed ----------------------------------

land.cover <- raster("06_processed_data/land_cover/Reed_2009/serengeti.land.cover.Reed.2009.cropped.greater.serengeti.zoomed.in.2021-09.tif")
land.cover.coarse <- raster::aggregate(land.cover, fact = 2) # <10 sec. This reduces the resolution
land.cover.coarse.trimmed <- trim(x = land.cover.coarse, padding = 0, values = 0) # This removes the cells with value 0

legend.land.cover <- read_delim("06_processed_data/land_cover/Reed_2009/land.cover.types.in.SNP_less_detailed.csv", 
                                ";", escape_double = FALSE, trim_ws = TRUE) # Contains the palette.
  
correspondance.land.cover.code <- legend.land.cover %>%
  dplyr::select(cell_value, color_code)

df_cell_values <- data.frame(cell_value = land.cover.coarse.trimmed[])

df_cell_values_new <- df_cell_values %>%
  left_join(x = .,
            y = correspondance.land.cover.code,
            by = "cell_value")

land.cover.coarse.trimmed.plot <- land.cover.coarse.trimmed
land.cover.coarse.trimmed.plot[] <- as.factor(df_cell_values_new$color_code) # replace land cover numerical codes by letters

plot.legend <- legend.land.cover %>%
  distinct(color_code, color_brewer_2) %>%
  arrange(color_code)

crop.square.margin <- readOGR(dsn  = "06_processed_data/borders",
                              layer = "crop.square.margin.2021-09")
bbox <-  st_bbox(crop.square.margin)
boundary <- readOGR(dsn = "06_processed_data/borders",
                    layer = "serengeti.mara.ecosystem.cropped.margin.2021-09")

tmap_mode("view")
(map_land_cover_zoom <- 
    tm_shape(land.cover.coarse.trimmed.plot, bbox = bbox) +
    tm_raster(palette = plot.legend$color_brewer_2,
              legend.show = FALSE) +
    tm_shape(boundary, bbox = bbox) +
    tm_polygons(col  =  "#5ea65c", alpha = 0, boundary.col = "#5ea65c") +
    
    tm_scale_bar(position = c("left", "bottom"),
                 width = 0.25,
                 text.size = 0.6,
                 breaks = c(0, 20, 40)) +
    tm_grid(lines = FALSE) +
    tm_layout(title = "(g)", 
              title.position = c('left', 'top'))
)

save(map_land_cover_zoom, 
     file = "07_intermediate_results/2021-04/maps/map_land_cover_zoom_2021-09.RData")

tmap_save(tm = map_land_cover_zoom,
          # height = 3.525674, width = 3.474513, units = "in",
          filename = "07_intermediate_results/2021-04/maps/map_land_cover_zoom_legend.png")



# ~ 8. Map with segmented roads cropped and zoomed in --------------------------

segmented.roads.df <- read_csv("06_processed_data/glm_data_2021-09/roads.segmented.cropped.no.short.df.csv")

# Convert the data frame into SpatialLines
coordinates(segmented.roads.df) =~ long+lat # Transformation du df en SpatialObject (Shp) grace à la spécification des coordonnées (colonnes long et lat)
proj4string(segmented.roads.df) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
x <- lapply(split(segmented.roads.df,
                  segmented.roads.df$ID_road_seg), #classe les points en segments grace à l'ID_seg dans le Shp
            function(x) Lines(list(Line(coordinates(x))),
                              x$ID_road_seg[1L]))
lines <- SpatialLines(x)


# Convert the SpatialLines into SpatialLinesDataFrame
data <- data.frame(segmented.roads.df) %>%
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
segmented.roads.shp <- SpatialLinesDataFrame(lines, data)


# boundary <- readOGR(dsn  = "06_processed_data/borders",
#                     layer = "SNP.lastest.cropped.margin.2021-09")
boundary <- readOGR(dsn  = "06_processed_data/borders",
                    layer = "serengeti.mara.ecosystem.cropped.margin.2021-09")

crop.square.margin <- readOGR(dsn  = "06_processed_data/borders",
                              layer = "crop.square.margin.2021-09")

bbox <-  st_bbox(crop.square.margin)

tmap_mode("view")
(map_segmented_roads_zoom <- 
    tm_shape(boundary, bbox = bbox) +
    tm_polygons(col  =  "#d6e9d6", alpha = 1, 
                boundary.col = "#5ea65c") +
    
    tm_shape(segmented.roads.shp, bbox = bbox) +
    tm_lines(col  =  "color", lwd  =  2.15) +
    tm_scale_bar(position = c("left", "bottom"),
                 text.size = 0.6,
                 breaks = c(0, 20, 40)) +
    # tm_compass(position = c("left", "top")) +
    tm_grid(lines = FALSE) +
            # labels.show = c(TRUE, FALSE)) +
    tm_layout(title= "(h)", 
              title.position = c('left', 'top'))
)


save(map_segmented_roads_zoom, 
     file = "07_intermediate_results/2021-04/maps/map_segmented_roads_zoom_2021-09.RData")

# tmap_save(tm = map_seg_roads,
#           height = 3.525674,
#           width = 3.474513,
#           units = "in",
#           filename = "07_intermediate_results/maps/09-03_segmented_roads_65km_zoomed.svg")


# ~ 9. Temporary map for figure building ---------------------------------------

# boundary <- readOGR(dsn  = "06_processed_data/borders",
#                     layer = "SNP.lastest.cropped.margin.2021-09")
boundary <- readOGR(dsn  = "06_processed_data/borders",
                    layer = "serengeti.mara.ecosystem.cropped.margin.2021-09")

crop.square.margin <- readOGR(dsn  = "06_processed_data/borders",
                              layer = "crop.square.margin.2021-09")

bbox <-  st_bbox(crop.square.margin)

tmap_mode("view")
(map_temp <- 
    tm_shape(boundary, bbox = bbox) +
    tm_polygons(col  =  "#d6e9d6", alpha = 1, boundary.col = "#5ea65c") +
    tm_scale_bar(position = c("left", "bottom"),
                 text.size = 0.6,
                 breaks = c(0, 20, 40)) +
    # tm_compass(position = c("left", "top")) +
    tm_grid(lines = FALSE) +
    tm_layout(title= "(d)", 
              title.position = c('left', 'top'))
)

save(map_temp, 
     file = "07_intermediate_results/2021-04/maps/map_temp_2021-09.RData")


# B. Build the figure ----------------------------------------------------------

# Load the data
load("07_intermediate_results/2021-04/maps/map_east_africa_2021-09.RData")
load("07_intermediate_results/2021-04/maps/map_great_migration_2021-09.RData")
load("07_intermediate_results/2021-04/maps/map_roads_carcasses_2021-09.RData")
load("07_intermediate_results/2021-04/maps/map_roads_carcasses_zoom_2021-09.RData")
# load("07_intermediate_results/2021-04/maps/map_amenity_zoom_2021-09_no_axis.RData")
load("07_intermediate_results/2021-04/maps/map_amenity_zoom_2021-09.RData")
# load("07_intermediate_results/2021-04/maps/map_rivers_waterbodies_zoom_2021-09_no_labels.RData")
load("07_intermediate_results/2021-04/maps/map_rivers_waterbodies_zoom_2021-09.RData")
load("07_intermediate_results/2021-04/maps/map_land_cover_zoom_2021-09.RData")
load("07_intermediate_results/2021-04/maps/map_segmented_roads_zoom_2021-09.RData")
load("07_intermediate_results/2021-04/maps/map_temp_2021-09.RData")

# Plot
# With east africa as the first panel
figure <- tmap_arrange(map_east_africa, map_great_migration, map_roads_carcasses_zoom,
                       map_temp, map_amenity_zoom, map_rivers_waterbodies_zoom,
                       map_land_cover_zoom, map_temp, map_segmented_roads_zoom, # replace the first map-temp of this line
                       widths = c(0.33, 0.33, 0.33,
                                  0.33, 0.33, 0.33,
                                  0.33, 0.33, 0.33),
                       ncol = 3, nrow = 3)
tmap_save(tm = figure,
          # height = 5, width = 8, units = "in",
          filename = "11_manuscript/V4 Figures/figure 1 (map) raw.png")
tmap_save(tm = figure,
          filename = "11_manuscript/V4 Figures/figure 1 (map) raw.svg")



