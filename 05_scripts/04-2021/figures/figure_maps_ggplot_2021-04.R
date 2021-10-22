#------------------------------------------------------------------------------#
#                                                                              #
#                       Plot the maps of the study area                        #        
#                                                                              #
#------------------------------------------------------------------------------#

library(tidyverse)
library(broom)
library(rgdal)
library(raster)
library(ggsn)
library(tmaptools)
library(leaflet)
library(shinyjs)
library(sp)
library(sf)
Sys.setenv(LANG = "en")

# A. Prepare complementary shapes ----------------------------------------------

# Buffer around Serengeti
serengeti <- readOGR(dsn  = "06_processed_data/borders",
                            layer = "SNP.lastest")

serengeti.planar <- spTransform(serengeti,
                                CRS("+proj=utm +zone=36 +datum=WGS84"))
buffer.size <- 10000
serengeti.buffer.planar <- rgeos::gBuffer(serengeti.planar, # generate the buffer around Svalbard
                                   byid = FALSE, 
                                   id = NULL, 
                                   width = buffer.size, 
                                   quadsegs = 10, 
                                   capStyle = "ROUND",
                                   joinStyle = "ROUND",
                                   mitreLimit = 1.0)
serengeti.buffer <- spTransform(serengeti.buffer.planar,
                                crs(serengeti))

shapefile(serengeti.buffer, "06_processed_data/borders/serengeti_buffer_10km")


# Buffer around 65km-wide circle
circle_65km <- readOGR(dsn  =  "06_processed_data/roads",
                       layer = "visitor.center.buffer.65km")

circle.planar <- spTransform(circle_65km,
                             CRS("+proj=utm +zone=36 +datum=WGS84"))
buffer.size <- 5000
circle.buffer.planar <- rgeos::gBuffer(circle.planar, # generate the buffer around Svalbard
                                          byid = FALSE, 
                                          id = NULL, 
                                          width = buffer.size, 
                                          quadsegs = 10, 
                                          capStyle = "ROUND",
                                          joinStyle = "ROUND",
                                          mitreLimit = 1.0)
circle.buffer <- spTransform(circle.buffer.planar,
                            crs(circle_65km))

shapefile(circle.buffer, "06_processed_data/borders/circle_70km")
shapefile(circle_65km, "06_processed_data/borders/circle_65km")

# Reproj the land cover data

land.cover <- raster("06_processed_data/land_cover/Reed_2009/serengeti.land.cover.Reed.2009.cropped.zoomed.in.2.tif")
land.cover.trimmed <- trim(x = land.cover, padding = 0, values = 0) # This removes the cells with value 0
# For some reason, need to do that or otherwise I get white cells even in the SNP although their true value
# is not 0

land.cover.trimmed.reproj <- projectRaster(land.cover.trimmed,
                                           crs = crs(boundary))
land.cover.trimmed.reproj.coarse <- raster::aggregate(land.cover.trimmed.reproj, 
                                                     fact = 5)
writeRaster(land.cover.trimmed.reproj, "06_processed_data/land_cover/Reed_2009/serengeti.cropped.zoom.trimmed.reproj.tif")

# Reduce resolution
land.cover.trimmed.reproj.coarse <- raster::aggregate(land.cover.trimmed, 
                                                      fact = 8)
writeRaster(land.cover.trimmed.reproj.coarse, "06_processed_data/land_cover/Reed_2009/serengeti.cropped.zoom.trimmed.reproj.coarse.x8.tif")

# B. Plot ----------------------------------------------------------------------

# ~ 1. Map Serengeti migration --------

boundary <- readOGR(dsn  = "06_processed_data/borders",
                           layer = "serengeti.mara.ecosystem.09-18")

boundary.kenya.tenzania <- readOGR(dsn  =  "06_processed_data/migration",
                                          layer = "frontier.kenya")

boundary.df <- tidy(boundary)
boundary.kenya.tenzania.df <- tidy(boundary.kenya.tenzania)

(map_great_migration <- ggplot() +
    geom_polygon(data = boundary.df, 
                 aes(x = long, y = lat, group = id),
                 color = "#5ea65c", fill = "#d6e9d6") +
    geom_path(data = boundary.kenya.tenzania, 
              aes(x = long, y = lat, group = id), size = 0.5) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    ggsn::scalebar(data = boundary.df, 
                   location = "bottomleft",
                   dist = 50, dist_unit = "km", transform = TRUE, model = 'WGS84',
                   height = 0.02,
                   st.size = 4, border.size = 0.5,
                   st.bottom = TRUE) +
    ggsn::north(data = boundary.df,
                location = "topleft",
                scale = 0.1,
                symbol = 12) +
    labs(x = "",
         y = "")
)

ggsave(plot = map_great_migration, filename = "07_intermediate_results/2021-04/maps/1_map_great_migration.png",
       height = 5, width = 4.35)

save(map_great_migration, file = "07_intermediate_results/2021-04/maps/1_map_great_migration.RData")



# ~ 2. Map east Africa -------------

serengeti.square <- extent(33.8, 36, -3.7, -1.1)
serengeti.square.shp <- as(serengeti.square, 'SpatialPolygons')

background <- extent(28.5, 42.6, -11, 4.3)
background.shp <- as(background, 'SpatialPolygons')

east.africa <- readOGR(dsn  =  "06_processed_data/borders",
       layer = "east_africa_no_islands")

# Convert to df
east.africa.df <- tidy(east.africa)
serengeti.square.df <- tidy(serengeti.square.shp)
background.df <- tidy(background.shp)



(map_east_africa <- ggplot() +
    geom_polygon(data = background.df, 
                 aes(x = long, y = lat, group = id),
                 color = "black", fill = "#00ace6") +
    geom_polygon(data = east.africa.df, 
                 aes(x = long, y = lat, group = id),
                 color = "black", fill = "white") +
    geom_polygon(data = serengeti.square.df, 
                 aes(x = long, y = lat, group = id),
                 color = "black", fill = "black",
                 alpha = 0.70, size = 1) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank()) +
    # ggsn::scalebar(data = east.africa.df, 
    #                location = "bottomleft",
    #                dist = 250, dist_unit = "km", transform = TRUE, model = 'WGS84',
    #                height = 0.02,
    #                st.size = 4, border.size = 0.5,
    #                st.bottom = TRUE) +
    # ggsn::north(data = east.africa.df,
    #             location = "topleft",
    #             scale = 0.1,
    #             symbol = 12) +
    labs(x = "",
         y = "")
)

ggsave(plot = map_east_africa, filename = "07_intermediate_results/2021-04/maps/2_map_east_africa.png",
       height = 3.5, width = 3) # ratio = 1.18

save(map_east_africa, file = "07_intermediate_results/2021-04/maps/2_map_east_africa.RData")




# ~ 3. Map roads with main roads thicker + carcasses -----------------------------

boundary <- readOGR(dsn  = "06_processed_data/borders",
                           layer = "SNP.lastest")

roads <- readOGR(dsn  =  "06_processed_data/roads/SNP.roads.QGIS",
                        layer = "snp_roads_for_analyses_25-05-2021")
major.roads <- readOGR(dsn  =  "06_processed_data/roads/SNP.roads.QGIS", 
                              layer = "major_roads")

hy.carcasses <- readOGR(dsn  = "06_processed_data/carcasses",
                               layer = "hy_carcasses_04-2021")

boundary.df <- tidy(boundary)
roads.df <- tidy(roads)
major.roads.df <- tidy(major.roads)
hy.carcasses.df <- as.data.frame(hy.carcasses) %>%
  filter(long != 0)

(map_roads_carcasses <- ggplot() +
    geom_polygon(data = boundary.df, 
                 aes(x = long, y = lat, group = id),
                 color = "#5ea65c", fill = "#d6e9d6") +
    geom_path(data = roads.df, 
              aes(x = long, y = lat, group = id), size = 0.25) +
    geom_path(data = major.roads.df, 
              aes(x = long, y = lat, group = id), size = 0.75) +
    
    geom_point(data = hy.carcasses.df,
               aes(x = long, y = lat), shape = 21, size = 1.5,
               color = "black", fill = "#E69F00", stroke = 0.2) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    ggsn::scalebar(data = boundary.df, 
                   location = "bottomleft",
                   dist = 50, dist_unit = "km", transform = TRUE, model = 'WGS84',
                   height = 0.02,
                   st.size = 4, border.size = 0.5,
                   st.bottom = TRUE) +
    ggsn::north(data = boundary.df,
                location = "topleft",
                scale = 0.1,
                symbol = 12) +
    labs(x = "",
         y = "")
)

ggsave(plot = map_roads_carcasses, filename = "07_intermediate_results/2021-04/maps/3_map_roads_carcasses.png",
       height = 5, width = 3.7)

save(map_roads_carcasses, file = "07_intermediate_results/2021-04/maps/3_map_roads_carcasses.RData")



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

boundary.df <- tidy(boundary2)
roads.df <- tidy(roads.65km)
major.roads.df <- tidy(major.roads.65km)
hy.carcasses.df <- as.data.frame(hy.carcasses) %>%
  filter(long != 0)
circle_65km_df <- tidy(circle_65km)


(map_roads_carcasses_zoom <- ggplot() +
    geom_polygon(data = boundary.df, 
                 aes(x = long, y = lat, group = id),
                 color = "#5ea65c", fill = "#d6e9d6") +
    geom_path(data = roads.df, 
              aes(x = long, y = lat, group = id), size = 0.25) +
    geom_path(data = major.roads.df, 
              aes(x = long, y = lat, group = id), size = 0.75) +
    
    geom_point(data = hy.carcasses.df,
               aes(x = long, y = lat), shape = 21, size = 1.5,
               color = "black", fill = "#E69F00", stroke = 0.2) +
    
    geom_polygon(data = circle_65km_df,
                 aes(x = long, y = lat), 
                 color = "#56B4E9", alpha = 0, size = 1.25) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    ggsn::scalebar(data = boundary.df, 
                   location = "bottomleft",
                   dist = 25, dist_unit = "km", transform = TRUE, model = 'WGS84',
                   height = 0.02,
                   st.size = 4, border.size = 0.5,
                   st.bottom = TRUE) +
    ggsn::north(data = boundary.df,
                location = "topleft",
                scale = 0.1,
                symbol = 12) +
    labs(x = "",
         y = "")
)

ggsave(plot = map_roads_carcasses_zoom, filename = "07_intermediate_results/2021-04/maps/4_map_roads_carcasses_zoom.png",
       height = 4.5, width = 4.5) # ratio is 0.87

save(map_roads_carcasses_zoom, file = "07_intermediate_results/2021-04/maps/4_map_roads_carcasses_zoom.RData")



# ~ 5. Map with amenity cropped, zoomed -----------

boundary <- readOGR(dsn = "06_processed_data/borders",
                    layer = "SNP.lastest")
amenity.polygons <- readOGR(dsn  =  "06_processed_data/amenity",
                            layer = "buildings_polygons_for_analyses_2021-05-25")

amenity.points <- readOGR(dsn  =  "06_processed_data/amenity",
                          layer = "building_points_for analyses_2020-09-11")

# Crop
circle_65km <- readOGR(dsn  =  "06_processed_data/borders",
                       layer = "circle_65km")



crop.square <- extent(34.18437, 35.45, -3.0831, -1.8)
boundary2 <- raster::crop(boundary, crop.square)

amenity.polygons.65km <- raster::crop(amenity.polygons, crop.square)# circle_65km)
amenity.points.65km <- raster::crop(amenity.points, circle_65km)

# Convert to df
boundary.df <- tidy(boundary2)
amenity.polygons.df <- tidy(amenity.polygons.65km)
amenity.points.df <- as.data.frame(amenity.points.65km) %>%
  rename(long = coords.x1,
         lat = coords.x2)
circle_65km_df <- tidy(circle_65km)


ggplot() +
  geom_polygon(data = boundary.df, 
               aes(x = long, y = lat, group = id),
               color = "#5ea65c", fill = "#d6e9d6") +
  geom_polygon(data = amenity.polygons.df, 
               aes(x = long, y = lat, group = id),
               color = "black", fill = "black") 


(map_amenity_zoom <- ggplot() +
    geom_polygon(data = boundary.df, 
                 aes(x = long, y = lat, group = id),
                 color = "#5ea65c", fill = "#d6e9d6") +
    geom_polygon(data = amenity.polygons.df, 
                 aes(x = long, y = lat, group = id),
                 color = "black", fill = "black") +
    geom_point(data = amenity.points.df,
               aes(x = long, y = lat), size = 0.5) +
    geom_polygon(data = circle_65km_df,
                 aes(x = long, y = lat), 
                 color = "#56B4E9", alpha = 0, size = 1.25) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    ggsn::scalebar(data = boundary.df, 
                   location = "bottomleft",
                   dist = 25, dist_unit = "km", transform = TRUE, model = 'WGS84',
                   height = 0.02,
                   st.size = 4, border.size = 0.5,
                   st.bottom = TRUE) +
    ggsn::north(data = boundary.df,
                location = "topleft",
                scale = 0.1,
                symbol = 12) +
    labs(x = "",
         y = "")
)

ggsave(plot = map_amenity_zoom, filename = "07_intermediate_results/2021-04/maps/5_map_amenity_zoom.png",
       height = 4.5, width = 4.5)

save(map_amenity_zoom, file = "07_intermediate_results/2021-04/maps/5_map_amenity_zoom.RData")



# ~ 6. Map with rivers & waterbodies cropped, zoomed ----------------------

boundary <- readOGR(dsn  = "06_processed_data/borders",
                           layer = "SNP.lastest")

circle_65km <- readOGR(dsn  =  "06_processed_data/borders",
                       layer = "circle_65km")

# waterbodies <- readOGR(dsn  = "06_processed_data/water",
#                               layer = "waterbodies_for_analyses_2021-05-25")
# 
# rivers <- readOGR(dsn  = "06_processed_data/water",
#                          layer = "rivers_for_analyses_2021-05-25")
# 
# 
# # Crop
# circle_70km <- readOGR(dsn  =  "06_processed_data/borders",
#                        layer = "circle_70km")
# 
# rivers.70km <- raster::crop(rivers, circle_70km)
# waterbodies.70km <- raster::crop(waterbodies, circle_70km)

crop.square <- extent(34.18437, 35.45, -3.0831, -1.8)
boundary2 <- raster::crop(boundary, crop.square)


rivers <- readOGR(dsn  = "06_processed_data/water",
                       layer = "rivers_circle_snp_2021-07-04")
waterbodies <- readOGR(dsn  = "06_processed_data/water",
                            layer = "waterbodies_circle_snp_2021-07-04")

rivers.cropped <- raster::crop(rivers, crop.square)
waterbodies.cropped <- raster::crop(waterbodies, crop.square)


# Convert to df
boundary.df <- tidy(boundary2)
rivers.df <- tidy(rivers.cropped) 
waterbodies.df <- tidy(waterbodies.cropped)
circle_65km_df <- tidy(circle_65km)

(map_rivers_waterbodies_zoom <- ggplot() +
    geom_polygon(data = boundary.df, 
                 aes(x = long, y = lat, group = id),
                 color = "#5ea65c", fill = "#d6e9d6") +
    
    geom_path(data = rivers.df,
              aes(x = long, y = lat, group = id),
              color = "#00ace6") +
    geom_polygon(data = waterbodies.df, 
              aes(x = long, y = lat, group = id),
              size = 0.75, fill = "#00ace6") +
    
    geom_polygon(data = circle_65km_df,
                 aes(x = long, y = lat), 
                 color = "#56B4E9", alpha = 0, size = 1.25) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    ggsn::scalebar(data = boundary.df, 
                   location = "bottomleft",
                   dist = 25, dist_unit = "km", transform = TRUE, model = 'WGS84',
                   height = 0.02,
                   st.size = 4, border.size = 0.5,
                   st.bottom = TRUE) +
    ggsn::north(data = boundary.df,
                location = "topleft",
                scale = 0.1,
                symbol = 12) +
    labs(x = "",
         y = "")
)

ggsave(plot = map_rivers_waterbodies_zoom, filename = "07_intermediate_results/2021-04/maps/6_map_rivers_waterbodies_zoom.png",
       height = 4.5, width = 4.5)


save(map_rivers_waterbodies_zoom, file = "07_intermediate_results/2021-04/maps/6_map_rivers_waterbodies_zoom.RData")



# ~ 7. Map with land cover cropped and zoomed in ----------------

boundary <- readOGR(dsn  = "06_processed_data/borders",
                    layer = "SNP.lastest")

circle_65km <- readOGR(dsn  =  "06_processed_data/borders",
                       layer = "circle_65km")

# Crop
crop.square <- extent(34.18437, 35.45, -3.0831, -1.8)
crop.square.poly <- as(crop.square, 'SpatialPolygons') 
crs(crop.square.poly) <- crs(boundary)


boundary2 <- raster::crop(boundary, crop.square)


# Land cover
land.cover.all <- raster("06_processed_data/land_cover/Reed_2009/serengeti.land.cover.Reed.2009.tif")

crop.square.planar <- spTransform(crop.square.poly,
                                  crs(land.cover.all))

land.cover.all.1st.crop <- raster::crop(land.cover.all,
                                        crop.square.planar) # extent(625000, 760000,
# 9650000, 9810000))

boundary.planar <- spTransform(boundary2,
                               crs(land.cover.all.1st.crop))

land.cover.all.2nd.crop <- raster::mask(land.cover.all.1st.crop, 
                                        boundary.planar)

rm(land.cover.all,
   land.cover.all.1st.crop,
   crop.square.poly,
   crop.square)

# Convert to df
circle_65km.planar <- spTransform(circle_65km,
                                  crs(land.cover.all.2nd.crop))
circle_65km.df <- tidy(circle_65km.planar)
boundary.df <- tidy(boundary.planar)



land.cover.df <- as.data.frame(land.cover.all.2nd.crop,
                               xy = TRUE)
colnames(land.cover.df)[3] <- "value"


land.cover.df <- land.cover.df %>%
  mutate(cover = ifelse(value %in% c(0, 1, 3, NA), "bare gorund, unclassified, cloud",
                        ifelse(value == 2, "water",
                               ifelse(value %in% c(6:9), "grassland",
                                      ifelse(value %in% c(10:17), "shrubbed grassland, treed grassland",
                                             
                                             ifelse(value %in% c(16:29), "shrubland",
                                                    ifelse(value %in% c(35, 36, 39), "woodland",
                                                           ifelse(value %in% c(32, 40, 41), "forest", "missing"))))))))



# legend.land.cover <- read_delim("06_processed_data/land_cover/Reed_2009/land.cover.types.in.SNP.csv", 
#                                 ";", escape_double = FALSE, trim_ws = TRUE) # Contains the color palette.
land.covers <- c("bare gorund, unclassified, cloud", 
                 "water",
                 "grassland", 
                 "shrubbed grassland, treed grassland", 
                 "shrubland",
                 "woodland",
                 "forest")

colors <- c("white", 
            "#716ea2",  # water
            "#ffffd4",  # grassland
            "#fec44f",  # shrubbed grassland, tree grassland
            "#fe9929",  # shrubland
            "#cc4c02",  # woodland
            "#8c2d04")  # forest

rm(boundary,
   boundary2,
   boundary.planar,
   circle_65km, 
   circle_65km.planar,
   crop.square.planar)



map_land_cover_zoom <- ggplot() +
  geom_raster(data = land.cover.df, 
              aes(x = x, y = y, fill = as.factor(cover))) +
  scale_fill_manual(limits = land.covers,
                    values = colors) +
  geom_polygon(data = boundary.df,
               aes(x = long, y = lat, group = id),
               color = "#5ea65c", fill = "#d6e9d6",
               alpha = 0) +
  
  geom_polygon(data = circle_65km.df,
               aes(x = long, y = lat),
               color = "#56B4E9", alpha = 0, size = 1.25) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  # legend.position = "none") +
  ggsn::scalebar(data = boundary.df,
                 location = "bottomleft",
                 dist = 25, dist_unit = "km", transform = FALSE, # model = 'WGS84',
                 height = 0.02,
                 st.size = 4, border.size = 0.5,
                 st.bottom = TRUE) +
  ggsn::north(data = boundary.df,
              location = "topleft",
              scale = 0.1,
              symbol = 12) +
  labs(x = "",
       y = "",
       fill = "")

ggsave(plot = map_land_cover_zoom, filename = "07_intermediate_results/2021-04/maps/7_map_land_cover_zoom_with_legend.png",
       height = 4.5, width = 6.8541) #4.5) # The legend is 6.8541 - 4.5 long

ggsave(plot = map_land_cover_zoom, filename = "07_intermediate_results/2021-04/maps/7_map_land_cover_zoom_with_legend2.png",
       height = 3.5, width = 5.5) #4.5) # The legend is 6.8541 - 4.5 long

save(map_land_cover_zoom, file = "07_intermediate_results/2021-04/maps/7_map_land_cover_zoom.RData")



# ~ 8. Map with segmented roads cropped and zoomed in --------------------------

segmented.roads.65km.df <- read_csv("06_processed_data/glm_data_2021-04/roads.segmented.cropped.no.short.df.csv")

boundary <- readOGR(dsn  = "06_processed_data/borders",
                    layer = "SNP.lastest")

circle_65km <- readOGR(dsn  =  "06_processed_data/borders",
                              layer = "circle_65km")

# Crop
crop.square <- extent(34.18437, 35.45, -3.0831, -1.8)
boundary2 <- raster::crop(boundary, crop.square)

# Convert to df
boundary.df <- tidy(boundary2)
circle_65km_df <- tidy(circle_65km)


(map_segmented_roads_zoom <- ggplot() +
    geom_polygon(data = boundary.df,
                 aes(x = long, y = lat, group = id),
                 color = "#5ea65c", fill = "#d6e9d6") +
    geom_path(data = segmented.roads.65km.df, 
              aes(x = long, y = lat, group = ID_road_seg, color = color), 
              size = 0.75) +
    scale_color_manual(values = c("red", "black")) +
    geom_polygon(data = circle_65km_df,
                 aes(x = long, y = lat),
                 color = "#56B4E9", alpha = 0, size = 1.25) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "none") +
    ggsn::scalebar(data = boundary.df,
                   location = "bottomleft",
                   dist = 25, dist_unit = "km", transform = TRUE, model = 'WGS84',
                   height = 0.02,
                   st.size = 4, border.size = 0.5,
                   st.bottom = TRUE) +
    ggsn::north(data = boundary.df,
                location = "topleft",
                scale = 0.1,
                symbol = 12) +
    labs(x = "",
         y = "",
         fill = "")
)

ggsave(plot = map_segmented_roads_zoom, filename = "07_intermediate_results/2021-04/maps/8_map_segmented_roads_zoom.png",
       height = 4.5, width = 4.5)

save(map_segmented_roads_zoom, file = "07_intermediate_results/2021-04/maps/8_map_segmented_roads_zoom.RData")





# C. Gather all the plots ------------------------------------------------------

library(cowplot)
load("07_intermediate_results/2021-04/maps/1_map_great_migration.RData")
load("07_intermediate_results/2021-04/maps/2_map_east_africa.RData")
load("07_intermediate_results/2021-04/maps/3_map_roads_carcasses.RData")
load("07_intermediate_results/2021-04/maps/4_map_roads_carcasses_zoom.RData")
load("07_intermediate_results/2021-04/maps/5_map_amenity_zoom.RData")
load("07_intermediate_results/2021-04/maps/6_map_rivers_waterbodies_zoom.RData")
load("07_intermediate_results/2021-04/maps/7_map_land_cover_zoom.RData")
load("07_intermediate_results/2021-04/maps/8_map_segmented_roads_zoom.RData")

# Serengeti ecosystem map: ratio is 1.15 (0.87)
# SNP map: ratio is 1.35 (0.74)
# study area zoomed in: ratio is 1.144 (0.873)

figure <- ggdraw() +
    draw_plot(map_great_migration, x = 0, y = 0.66, width = 0.29, height = 0.33) +  
    draw_plot(map_roads_carcasses, x = 0.32, y = 0.66, width = 0.2444, height = 0.33) +
    draw_plot(map_roads_carcasses_zoom, x = 0.6, y = 0.66, width = 0.29, height = 0.33) +
    
    
    draw_plot(map_amenity_zoom, x = 0.30, y = 0.33, width = 0.29, height = 0.33) +
    draw_plot(map_rivers_waterbodies_zoom, x = 0.6, y = 0.33, width = 0.29, height = 0.33) +
    
    # draw_plot(map_land_cover_zoom, x = 0.0, y = 0, width = 0.5, height = 0.33) +
    draw_plot(map_segmented_roads_zoom, x = 0.6, y = 0, width = 0.29, height = 0.33) +
    
    draw_plot_label(label = c("(a)", "(b)", "(c)",
                              "(d)", "(e)", "(f)",
                              "(g)", "(h)"),
                    x = c(0, 0.3, 0.6,
                          0, 0.3, 0.6,
                          0, 0.6), 
                    y = c(1, 1, 1,
                          0.66, 0.66, 0.66, 
                          0.33, 0.33), 
                    size = 17) 
  


save_plot("07_intermediate_results/2021-04/maps/figure.maps.2.svg", 
          plot = figure,
          ncol = 3,
          nrow = 3,
          base_asp = 1)










### Map roads with main roads thicker ---------

# boundary <- readOGR(dsn  = "06_processed_data/borders",
#                            layer = "serengeti.mara.ecosystem.09-18")
boundary <- readOGR(dsn  = "06_processed_data/borders",
                    layer = "SNP.lastest")
roads <- readOGR(dsn  =  "06_processed_data/roads/SNP.roads.QGIS",
                 layer = "snp_roads_for_analyses_25-05-2021")
major.roads <- readOGR(dsn  =  "06_processed_data/roads/SNP.roads.QGIS", 
                       layer = "major_roads")


boundary.df <- tidy(boundary)
roads.df <- tidy(roads)
major.roads.df <- tidy(major.roads)

(map_roads <- ggplot() +
    geom_polygon(data = boundary.df, 
                 aes(x = long, y = lat, group = id),
                 color = "#5ea65c", fill = "#d6e9d6") +
    geom_path(data = roads.df, 
              aes(x = long, y = lat, group = id), size = 0.25) +
    geom_path(data = major.roads.df, 
              aes(x = long, y = lat, group = id), size = 0.75) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    ggsn::scalebar(data = boundary.df, 
                   location = "bottomleft",
                   dist = 50, dist_unit = "km", transform = TRUE, model = 'WGS84',
                   height = 0.02,
                   st.size = 4, border.size = 0.5,
                   st.bottom = TRUE) +
    ggsn::north(data = boundary.df,
                location = "topleft",
                scale = 0.1,
                symbol = 12) +
    labs(x = "",
         y = "")
)

ggsave(plot = map_roads, filename = "07_intermediate_results/2021-04/maps/map_roads.png",
       height = 5, width = 3.7)


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
major.roads <- readOGR(dsn  =  "06_processed_data/roads/SNP.roads.QGIS", 
                              layer = "major_roads")

roads <- readOGR(dsn  =  "06_processed_data/roads/SNP.roads.QGIS",
                        layer = "snp_roads_for_analyses_25-05-2021")


circle_65km <- readOGR(dsn  =  "06_processed_data/roads",
                              layer = "visitor.center.buffer.65km")
# Crop + reproject
major.roads.65km <- spTransform(raster::crop(major.roads, circle_65km),
                                crs(land.cover.trimmed))
roads.65km <- spTransform(raster::crop(roads, circle_65km),
                          crs(land.cover.trimmed))




boundary <- readOGR(dsn  = "06_processed_data/borders",
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


0.33*6.85/4.5
