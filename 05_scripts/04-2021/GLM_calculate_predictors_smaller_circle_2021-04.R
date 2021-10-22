#==============================================================================#
#                                                                              #
#                  Linear model of spatial roadkill predictors                 #
#                                                                              #
#==============================================================================#

library(tidyverse)
library(rgdal)
library(raster)
library(sp)
library(sf)
library(tmap)
library(geosphere)
library(rgeos)
library(effects)
library(glmmTMB)

Sys.setenv(LANG = "en")

source(file = "05_scripts/04-2021/GLM_functions_calculate_predictors_2021-04.R")

#==============================================================================#
# STEP 1: Import and format road data ------------------------------------------

roads <- readOGR(dsn  =  "06_processed_data/roads/SNP.roads.QGIS",
                 layer = "snp_roads_for_analyses_25-05-2021")

# roads <- readOGR(dsn  =  "06_processed_data/roads/SNP.roads.QGIS/old",
#                         layer = "snp.roads.acc.to.map.road.w.carcass.del")


# Convert the shapefile into a data frame (because the road segmentation function
# takes data frames as arguments)
roads.df <- CreateDataFrameRoads(roads) %>%
  mutate(ID = as.factor(ID))



#==============================================================================#
# STEP 2: Segment the roads ----------------------------------------------------

roads.segmented.df <- SegmentationDf(roads.df,     # ~20 seconds
                                     segment.length = 2500) # for some reason, the length that must be specified
# here must be proportional to the desired real
# distance. If I used 2.5 or 2500 as arguments
# I don't get 2.5km-long segments.
# y = -1.382e-06 + 8.989e-06 * desired_real_length





write_csv(roads.segmented.df, "06_processed_data/glm_data_2021-04/47.5km/roads.segmented.df.csv")


#==============================================================================#
# STEP 3: Find the midpoint and measure the length of each segment ---------------------------------------

start <- Sys.time()
midpoint.and.length <- MidpointAndLengthAllSegments(roads.segmented.df) # ~20 sec
end <- Sys.time()
end-start

write_csv(midpoint.and.length, "06_processed_data/glm_data_2021-04/47.5km/midpoint.and.length.csv")

# Create table with all the info
table.glm_3 <- roads.segmented.df %>%
  distinct(ID_road_seg,
           road_importance) %>%
  mutate(nbr_carcasses = 0) # This column will be filled in step 6

# Add the midpoint and length of each segment
table.glm_3.2 <- table.glm_3 %>%
  left_join(x = .,
            y = midpoint.and.length,
            by = "ID_road_seg")

write_csv(table.glm_3.2, "06_processed_data/glm_data_2021-04/47.5km/table.glm_3.2.csv")


#==============================================================================#
# STEP 4: Delete roads that are too far -----------------------------------

table.glm_3.2 <- read_csv("06_processed_data/glm_data_2021-04/47.5km/table.glm_3.2.csv")


coord.center = c(34.818275, -2.4459913) # These are the coordinates of the center

table.glm_4 <- DeleteSegmentFarFromCenter(table.glm_3.2, # Instantaneous
                                          coord.center = coord.center, 
                                          lim.dist = 47500) 
# Save
write_csv(table.glm_4, "06_processed_data/glm_data_2021-04/47.5km/table.glm_4.csv")

# Remove the coordinates of the points of the roads that are outside of the 
# 65km-radius circle
roads.segmented.cropped.df <- roads.segmented.df %>%
  filter(ID_road_seg %in% table.glm_4$ID_road_seg)

write_csv(roads.segmented.cropped.df, "06_processed_data/glm_data_2021-04/47.5km/roads.segmented.cropped.df.csv")


#==============================================================================#
# STEP 5: Delete segments that are too short -----------------------------------------

table.glm_4 <- read_csv("06_processed_data/glm_data_2021-04/47.5km/table.glm_4.csv")

table.glm_5 <- table.glm_4 %>%
  filter(length > 2000)

write_csv(table.glm_5, "06_processed_data/glm_data_2021-04/47.5km/table.glm_5.csv")


# Remove the coordinates of the points of the road segments that are shorter than
# 2000 meters
roads.segmented.cropped.no.short.df <- roads.segmented.df %>%
  filter(ID_road_seg %in% table.glm_5$ID_road_seg)
write_csv(roads.segmented.cropped.no.short.df, "06_processed_data/glm_data_2021-04/47.5km/roads.segmented.cropped.no.short.df.csv")


#==============================================================================#
# STEP 6: Count the number of carcasses on each segment ------------------------

table.glm_5 <- read_csv("06_processed_data/glm_data_2021-04/47.5km/table.glm_5.csv")
roads.segmented.cropped.no.short.df <- read_csv("06_processed_data/glm_data_2021-04/47.5km/roads.segmented.cropped.no.short.df.csv")


# Import the file with the coordinates of the carcasses
hy_carcasses_formatted_spatial <- read_delim("06_processed_data/carcasses/12_hy.carcasses.certainty.formatted.spatial_updated_04-2021.csv", 
                                             ";", escape_double = FALSE, trim_ws = TRUE) %>%
  filter(GPS_certainty_score > 0,
         lat < -2.016429,
         lat > -2.875551)

start <- Sys.time()
closest.segment <- GetClosestRoadSegment(carcasses.df = hy_carcasses_formatted_spatial, # ~ 40 sec
                                         roads.df = roads.segmented.cropped.no.short.df)
end <- Sys.time()
end-start

table.glm_6 <- AssignCarcassesToSegments(table.glm_5, 
                                         closest.segment) # 5-10 seconds


sum(table.glm_6$nbr_carcasses)
write_csv(table.glm_6, "06_processed_data/glm_data_2021-04/47.5km/table.glm_6.csv")



#==============================================================================#
# STEP 7: Calculate distance to the closest water source -----------------------

### Load the data
snp.rivers <- readOGR("06_processed_data/water", 
                      layer = "rivers_for_analyses_2021-05-25")
snp.waterbodies <- readOGR("06_processed_data/water", 
                           layer = "waterbodies_for_analyses_2021-05-25")

table.glm_6 <- read_csv("06_processed_data/glm_data_2021-04/47.5km/table.glm_6.csv")

start <- Sys.time()
closest.water <- ShortestDistanceToRiverAndWaterbody(table.glm_6, 
                                                     snp.waterbodies, 
                                                     snp.rivers) # ~ 20min
end <- Sys.time()
end - start 


write_csv(closest.water, "06_processed_data/glm_data_2021-04/47.5km/closest.water.csv")

### Add the columns to the table
table.glm_7 <- table.glm_6 %>%
  left_join(x = table.glm_6,
            y = closest.water[ , c(1, 3)],
            by = "segment_ID") %>%
  mutate(distance_water_km = as.numeric(distance)/1000) %>%
  dplyr::select(-distance)

write_csv(table.glm_7, "06_processed_data/glm_data_2021-04/47.5km/table.glm_7.csv")


#==============================================================================#
# STEP 8: Calculate distance to the closest amenity ----------------------------

### Load the data
snp.amenity.polygons <- readOGR("06_processed_data/amenity", 
                                layer = "buildings_polygons_for_analyses_2021-05-25")

snp.amenity.points <- readOGR("06_processed_data/amenity", 
                              layer = "building_points_for analyses_2020-09-11")

table.glm_7 <- read_csv("06_processed_data/glm_data_2021-04/47.5km/table.glm_7.csv")


start <- Sys.time()
closest.amenity <- ShortestDistanceAmenity(table.glm_7, # ~37min
                                           snp.amenity.polygons, 
                                           snp.amenity.points) 
end <- Sys.time()
end - start 

write_csv(closest.amenity, "06_processed_data/glm_data_2021-04/47.5km/closest.amenity.csv")


### Add the columns to the table
table.glm_8 <- table.glm_7 %>%
  left_join(x = table.glm_7,
            y = closest.amenity[ , c(1, 3)],
            by = "segment_ID") %>%
  mutate(distance_amenity_km = as.numeric(distance)/1000) %>%
  dplyr::select(-distance)

write_csv(table.glm_8, "06_processed_data/glm_data_2021-04/47.5km/table.glm_8.csv")



#==============================================================================#
# STEP 9: Extract land cover percentages --------------------------------------

table.glm_8 <- read_csv("06_processed_data/glm_data_2021-04/47.5km/table.glm_8.csv")

snp.land.cover <- raster("06_processed_data/land_cover/Reed_2009/serengeti.land.cover.Reed.2009.tif")


# ~ 1. Crop the land cover data to reduce computational burden -----------------
crop.square <- extent(635158.5, 769056.5, # Determined using the 65km circle (+ a margin)
                      9662876, 9796770) 
snp.land.cover.cropped <- raster::crop(snp.land.cover, crop.square)

tmap_mode("view")
tm_shape(snp.land.cover.cropped) +
  tm_raster(legend.show = FALSE)



# ~ 2. Run the function --------------------------------------------------------

roads.segmented.cropped.no.short.df <- read_csv("06_processed_data/glm_data_2021-04/47.5km/roads.segmented.cropped.no.short.df.csv")
# roads.segmented.partial <- roads.segmented.cropped.no.short.df[1:484,]

buffer.size <- 500
start <- Sys.time()
land.cover.seg <- ExtractLandCover(roads.segmented.cropped.no.short.df, # ~ 4-8min
                                   snp.land.cover.cropped, 
                                   buffer.size) 
percentage.land.cover <- PercentageLandCover(land.cover.seg)
end <- Sys.time() 
end - start

# Convert columns to numeric
percentage.land.cover <- percentage.land.cover %>%
  mutate_at(c("unclassified", "bare_ground","water","clouds", "sparse_grassland", 
              "open_grassland", "dense_grassland", "closed_grassland",
              "sparse_shrubbed_grassland", "open_shrubbed_grassland", 
              "dense_shrubbed_grassland", "closed_shrubbed_grassland", 
              "open_treed_grassland", "dense_treed_grassland","closed_treed_grassland", 
              "dense_shrubland","open_grassed_shrubland", "dense_grassed_shrubland",
              "closed_grassed_shrubland","open_treed_shrubland","dense_treed_shrubland", 
              "closed_treed_shrubland", "dense_forest", "open_grassed_woodland", 
              "dense_grassed_woodland", "closed_grassed_woodland", "dense_shrubbed_forest", 
              "closed_shrubbed_forest"), as.numeric)


# Merge some land cover types
percentage.land.cover.processed <- percentage.land.cover %>%
  mutate(sparse_open_grassland = sparse_grassland + open_grassland + sparse_shrubbed_grassland + 
           open_shrubbed_grassland + open_treed_grassland,
         
         dense_closed_grassland = dense_grassland + closed_grassland + dense_shrubbed_grassland + 
           closed_shrubbed_grassland + dense_treed_grassland + closed_treed_grassland,
         
         sparse_open_shrubland = open_grassed_shrubland + open_treed_shrubland,
         
         dense_closed_shrubland = dense_shrubland + dense_grassed_shrubland + closed_grassed_shrubland 
         + dense_treed_shrubland + closed_treed_shrubland,
         
         woodland = closed_grassed_woodland + open_grassed_woodland + dense_grassed_woodland,
         
         forest = dense_shrubbed_forest + closed_shrubbed_forest + dense_forest) %>%
  
  dplyr::select(segment_ID, sparse_open_grassland, dense_closed_grassland, sparse_open_shrubland, dense_closed_shrubland, 
                woodland, forest, water, bare_ground) %>%
  # Combine all the grassland covers, and all the shrubland covers
  mutate(grassland = sparse_open_grassland + dense_closed_grassland,
         shrubland = sparse_open_shrubland + dense_closed_shrubland)



# Save
save(land.cover.seg, 
     file = paste0("06_processed_data/glm_data_2021-04/47.5km/extracted_cell_cover_values_", buffer.size, "m", ".RData"))
write_csv(percentage.land.cover, 
          path = paste0("06_processed_data/glm_data_2021-04/47.5km/percentage.land.cover.", buffer.size, "m", ".csv"))
write_csv(percentage.land.cover.processed, 
          path = paste0("06_processed_data/glm_data_2021-04/47.5km/percentage.land.cover.", buffer.size, "m.processed.csv"))

percentage.land.cover.processed <- read_csv(paste0("06_processed_data/glm_data_2021-04/47.5km/percentage.land.cover.", 
                                                   buffer.size, "m.processed.csv"))

table.glm_9 <- table.glm_8 %>%
  left_join(x = table.glm_8,
            y = percentage.land.cover.processed,
            by = "segment_ID")

write_csv(table.glm_9, 
          path = paste0("06_processed_data/glm_data_2021-04/47.5km/table.glm_9.", buffer.size, "m.csv"))



#==============================================================================#
# STEP 10: Scale the variables -------------------------------------------------
buffer.size <- 500
table.glm_9 <- read_csv(paste0("06_processed_data/glm_data_2021-04/47.5km/table.glm_9.", buffer.size, "m.csv"))


colnames(table.glm_9)
table.glm_10 <- table.glm_9 %>% 
  mutate_at(c("length", "distance_water_km", "distance_amenity_km", "sparse_open_grassland",
              "dense_closed_grassland", "sparse_open_shrubland", "dense_closed_shrubland",
              "woodland", "forest", "grassland", "shrubland", "water", "bare_ground"), ~ (scale(.) %>% as.vector))

write_csv(table.glm_10, 
          path = paste0("06_processed_data/glm_data_2021-04/47.5km/table.glm_10.", buffer.size, "m.csv"))


# Save the mean and sd in a csv
table.glm_9 <- table.glm_9 %>%
  mutate_at(c("length", "distance_water_km", "distance_amenity_km", "sparse_open_grassland",
              "dense_closed_grassland", "sparse_open_shrubland", "dense_closed_shrubland",
              "woodland", "forest", "grassland", "shrubland", "water", "bare_ground"), as.numeric)

predictor_mean_sd <- cbind(colnames(table.glm_9[sapply(table.glm_9, is.numeric)]),
                           as.data.frame(sapply(table.glm_9[sapply(table.glm_9, is.numeric)], 
                                                mean, 
                                                na.rm = T)),
                           as.data.frame(sapply(table.glm_9[sapply(table.glm_9, is.numeric)], 
                                                sd, 
                                                na.rm = T)))
rownames(predictor_mean_sd) <- NULL
colnames(predictor_mean_sd) <- c("variable", "mean", "sd")

write_csv(predictor_mean_sd, 
          path = paste0("06_processed_data/glm_data_2021-04/47.5km/predictor_mean_sd_", buffer.size, "m.csv"))


#==============================================================================#
# STEP 11: Tables with carcasses with 0.5 or 0.75 certainty scores--------------

buffer.size <- 500

table.glm_5 <- read_csv("06_processed_data/glm_data_2021-04/47.5km/table.glm_5.csv")
table.glm_10 <- read_csv(paste0("06_processed_data/glm_data_2021-04/47.5km/table.glm_10.", 
                                buffer.size, "m.csv"))
roads.segmented.cropped.no.short.df <- read_csv("06_processed_data/glm_data_2021-04/47.5km/roads.segmented.cropped.no.short.df.csv")

# ~ 1. For carcasses with >0.5 -------------------------------------------------

# Here we only consider carcasses with mid to high certainty in their GPS location and in 
# their cause of death


hy_carcasses_formatted_spatial_0.5 <- read_delim("06_processed_data/carcasses/12_hy.carcasses.certainty.formatted.spatial_updated_04-2021.csv", 
                                             ";", escape_double = FALSE, trim_ws = TRUE) %>%
  filter(GPS_certainty_score > 0,
         lat < -2.016429,
         lat > -2.875551,
         GPS_certainty_score >= 0.5,
         collision_certainty_score_NEW >= 0.5)

closest.segment_0.5 <- GetClosestRoadSegment(carcasses.df = hy_carcasses_formatted_spatial_0.5, 
                                             roads.df = roads.segmented.cropped.no.short.df)

table.glm_6_0.5 <- AssignCarcassesToSegments(table.glm_5, 
                                             closest.segment_0.5) #10 seconds


table.glm_10_0.5 <- table.glm_6_0.5 %>%
  left_join(x = table.glm_6_0.5,
            y = table.glm_10[, -c(2:7)],
            by = "segment_ID")

sum(table.glm_10_0.5$nbr_carcasses)

write_csv(table.glm_10_0.5, 
          path = paste0("06_processed_data/glm_data_2021-04/47.5km/table.glm_10.", buffer.size, "m.0.5.csv"))


# ~ 2. For carcasses with >=0.75 ------------------------------------------------

# Here we only consider carcasses with high certainty in their GPS location and in 
# their cause of death


hy_carcasses_formatted_spatial_0.75 <- read_delim("06_processed_data/carcasses/12_hy.carcasses.certainty.formatted.spatial_updated_04-2021.csv", 
                                                 ";", escape_double = FALSE, trim_ws = TRUE) %>%
  filter(GPS_certainty_score > 0,
         lat < -2.016429,
         lat > -2.875551,
         GPS_certainty_score >= 0.75,
         collision_certainty_score_NEW >= 0.75)

closest.segment_0.75 <- GetClosestRoadSegment(carcasses.df = hy_carcasses_formatted_spatial_0.75, 
                                              roads.df = roads.segmented.cropped.no.short.df)

table.glm_6_0.75 <- AssignCarcassesToSegments(table.glm_5, 
                                              closest.segment_0.75) #10 seconds

table.glm_10_0.75 <- table.glm_6_0.75 %>%
  left_join(x = table.glm_6_0.75,
            y = table.glm_10[, -c(2:7)],
            by = "segment_ID")

sum(table.glm_10_0.75$nbr_carcasses)

write_csv(table.glm_10_0.75, 
          path = paste0("06_processed_data/glm_data_2021-04/47.5km/table.glm_10.", buffer.size, "m.0.75.csv"))







ggplot(data = table.glm_10, aes(x = road_importance, y = grassland)) + 
  geom_boxplot()

wilcox.test(formula = grassland ~ road_importance, data = table.glm_11, alternative = "two.sided", exact = TRUE)

ggplot(data = table.glm_10, aes(x = road_importance, y = woodland)) + 
  geom_boxplot()

wilcox.test(formula = woodland ~ road_importance, data = table.glm_11, alternative = "two.sided", exact = TRUE)





