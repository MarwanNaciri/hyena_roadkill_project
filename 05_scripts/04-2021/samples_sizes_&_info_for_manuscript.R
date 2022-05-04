#==============================================================================#
#                                                                              #
#                 Samples sizes & other info for manuscript                    #
#                                                                              #
#==============================================================================#

# In this script, I calculate the samples sizes, the size effects, 
# the total length of roads, the lengths of the segments, etc.

library(tidyverse)
library(lubridate)
library(broom)
library(rgdal)
library(raster)
library(sp)
library(sf)
library(geosphere)
library(rgeos)

# A. Sample sizes ==============================================================

hy_carcasses <- read_delim("06_processed_data/carcasses/12_hy.carcasses.certainty.formatted.spatial_updated_10-2021.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE) %>%
  mutate(death_obs_new = paste0(substr(date_obs, 1, 4), "-", substr(date_obs, 5, 6), "-", substr(date_obs, 7, 8))) %>%
  mutate(date_obs_new = as.Date(death_obs_new, format = "%Y - %m - %d")) %>%
  dplyr::select(-date_obs) %>%
  rename(date_obs = date_obs_new)

# ~ 1. Age categories ---------
hy_carcasses %>%
  count(age)


# ~ 2. Sex classes ---------
hy_carcasses %>%
  count(sex)

# When considering adults only
hy_carcasses %>%
  filter(age %in% c("adult", "unknown")) %>%
  count(sex)

hy_carcasses %>%
  filter(age == "adult") %>%
  count(sex)


# ~ 3. GPS coordinates ---------
hy_carcasses %>%
  count(location_certainty_score)


# ~ 4. Temporal analysis --------

# Serengeti IV seson:
hy_carcasses %>%
  mutate(year = year(date_obs),
         herds_position = ifelse(date_obs %within% interval(ymd(paste0(year, "-08-01")), 
                                                            ymd(paste0(year, "-10-31"))), 
                                 "north west", 
                                 ifelse(date_obs %within% interval(ymd(paste0(year, "-05-11")), 
                                                                   ymd(paste0(year, "-07-31"))),
                                        "transit",
                                        ifelse(date_obs %within% interval(ymd(paste0(year, "-11-01")), 
                                                                          ymd(paste0(year, "-12-20"))), 
                                               "transit", "south east")))) %>%
  filter(age %in% c("adult", "unknown", "subadult"),
         herds_position %in% c("north west", "south east"),
         year < 2020) %>%
  count(herds_position)


# ~ 5. Spatiotemporal analysis --------

# Serengeti IV seson:
hy_carcasses %>%
  filter(location_certainty_score > 0) %>%
  mutate(year = year(date_obs)) %>%

  mutate(herds_position = ifelse(date_obs %within% interval(ymd(paste0(year, "-08-01")), 
                                                                     ymd(paste0(year, "-10-31"))), 
                                          "north west", 
                                          ifelse(date_obs %within% interval(ymd(paste0(year, "-05-11")), 
                                                                            ymd(paste0(year, "-07-31"))),
                                                 "transit",
                                                 ifelse(date_obs %within% interval(ymd(paste0(year, "-11-01")), 
                                                                                   ymd(paste0(year, "-12-20"))), 
                                                        "transit", "south east")))) %>%
  filter(age %in% c("adult", "unknown", "subadult"),
         herds_position %in% c("north west", "south east")) %>%
  count(herds_position)



# ~ 6. Certainty/confidence scores ---------------------------------------------

# Cd = Cause of Death certainty score
# Cl = Location certainty score

all.carcasses <-  read_delim("06_processed_data/carcasses/12_hy.carcasses.certainty.formatted.spatial_updated_10-2021.csv", 
                             ";", escape_double = FALSE, trim_ws = TRUE)

all.carcasses.indications <- all.carcasses %>%
  dplyr::select(ID_OG, ID, date_obs, exact, location_certainty_score, Comment)

all.carcasses.indications %>%
  filter(location_certainty_score == 0.25)


# Number of carcasses with Cd >= 0.75
carcasses_0.75_Cd <- all.carcasses %>%
  filter(collision_certainty_score_NEW >= 0.75)


# Number of carcasses with Cd >= 0.75 AND Cl >= 0.75
carcasses_0.75 <- all.carcasses %>%
  filter(collision_certainty_score_NEW >= 0.75,
         location_certainty_score >= 0.75)


# Number of carcasses with Cd >= 0.75 of each age class 
carcasses_0.75_Cd_age <- all.carcasses %>%
  filter(!collision_certainty_score_NEW < 0.75) %>%
  count(age)


# Number of carcasses with each Cd 
all.carcasses %>%
  count(collision_certainty_score_NEW)


# Number of carcasses with each Cl 
all.carcasses %>%
  count(location_certainty_score)


# B. Other measures/figures ====================================================

# ~ 1. Total road length -------------------------------------------------------

source(file = "05_scripts/04-2021/GLM_2021-10/GLM_functions_calculate_predictors_2021-10.R")

roads <- rgdal::readOGR(dsn  =  "06_processed_data/roads/SNP.roads.QGIS",
                        layer = "snp_roads_according_to_FZS_map_cropped")
roads@data[["osm_id"]] <- seq(from = 1, to = length(roads@data[["osm_id"]]))

# Convert the shapefile into a data frame
roads.df <- CreateDataFrameRoads(roads) %>%
  mutate(ID = as.factor(ID))

# ~~~ a. All roads, before segmentation ----------------------------------------

# Measure each road (~ 5-10sec)
new.column <- c() 
list.ID <- roads.df %>%
  distinct(ID)
for (i in 1:nrow(list.ID)) {              # For each segment
  coords.i <- roads.df %>%            # Get coordinates of segment i
    filter(ID == list.ID$ID[i]) %>%
    dplyr::select(long, lat)
  L <- 0
  for (i in 1:(nrow(coords.i) - 1)) {
    d <- distGeo(coords.i[i, ], 
                 coords.i[i+1, ])  # distGeo calculates the shortest distance  
                                   # between two points assuming the WGS84 earth. 
    L <- L + d        # Here we sum the distances between each two successive 
                      # points of a segment to get the total segment length
  }  
  new.column <- rbind(new.column, L)
}
length_df <- data.frame(ID = list.ID$ID,
                        length = new.column)

total.length <- sum(length_df$length)/1000
total.length
# 1443,957 m
# Not exactly the same result when I sum the lengths of the segments 
# (5km difference = 0.3%)


# ~~~ b. Main roads, before segmentation ---------------------------------------

roads.df.main <- roads.df %>%
  filter(class %in% c("primary", "secondary"))

# Measure each road (~ 5-10sec)
new.column <- c() 
list.ID <- roads.df.main %>%
  distinct(ID)
for (i in 1:nrow(list.ID)) {              # For each segment
  coords.i <- roads.df.main %>%            # Get coordinates of segment i
    filter(ID == list.ID$ID[i]) %>%
    dplyr::select(long, lat)
  L <- 0
  for (i in 1:(nrow(coords.i) - 1)) {
    d <- distGeo(coords.i[i, ], 
                 coords.i[i+1, ])  # distGeo calculates the shortest distance  
    # between two points assuming the WGS84 earth. 
    L <- L + d        # Here we sum the distances between each two successive 
    # points of a segment to get the total segment length
  }  
  new.column <- rbind(new.column, L)
}
length_df <- data.frame(ID = list.ID$ID,
                        length = new.column)

total.length.main <- sum(length_df$length)/1000
total.length.main
# 201,1076 km

# ~~~ b. Tracks, before segmentation ---------------------------------------

`%notin%` <- Negate(`%in%`)
roads.df.tracks <- roads.df %>%
  filter(class %notin% c("primary", "secondary"))

# Measure each road (~ 5-10sec)
new.column <- c() 
list.ID <- roads.df.tracks %>%
  distinct(ID)
for (i in 1:nrow(list.ID)) {              # For each segment
  coords.i <- roads.df.tracks %>%            # Get coordinates of segment i
    filter(ID == list.ID$ID[i]) %>%
    dplyr::select(long, lat)
  L <- 0
  for (i in 1:(nrow(coords.i) - 1)) {
    d <- distGeo(coords.i[i, ], 
                 coords.i[i+1, ])  # distGeo calculates the shortest distance  
    # between two points assuming the WGS84 earth. 
    L <- L + d        # Here we sum the distances between each two successive 
    # points of a segment to get the total segment length
  }  
  new.column <- rbind(new.column, L)
}
length_df <- data.frame(ID = list.ID$ID,
                        length = new.column)

total.length.tracks <- sum(length_df$length)/1000
total.length.tracks
# 1242.85 km


# ~ 2. Proportion/length of segments <2km --------------------------------------
table.glm_3.2 <- read_csv("06_processed_data/glm_data_2021-09/table.glm_3.2.csv")

total.length <- sum(table.glm_3.2$length) # Length in m
total.length
# 1448724


prop.short <- table.glm_3.2 %>%
  mutate(short = ifelse(length < 2000, "yes", "no")) %>%
  group_by(short) %>%
  summarise(summed_length = sum(length)) %>%
  mutate(proportion = summed_length/(total.length))


ggplot(table.glm_3.2, aes(x = length)) +
  geom_histogram() +
  theme_classic()

# ~ 3. Median position during wet VS dry season --------------------------------

# ~~~ a. All carcasses ---------------------------------------------------------
hy_carcasses_formatted <- read_delim("06_processed_data/carcasses/12_hy.carcasses.certainty.formatted.spatial_updated_10-2021.csv", 
                                     ";", escape_double = FALSE, trim_ws = TRUE)

median_per_season <- hy_carcasses_formatted %>%
  filter(location_certainty_score > 0) %>%
  mutate(date_obs = as.Date(x = as.character(date_obs), 
                            format = "%Y%m%d",
                            origin = "1970-01-01"),
         year = year(date_obs)) %>%
  mutate(herds_position = ifelse(date_obs %within% interval(ymd(paste0(year, "-08-01")), 
                                                            ymd(paste0(year, "-10-31"))), 
                                 "north west", 
                                 ifelse(date_obs %within% interval(ymd(paste0(year, "-05-11")), 
                                                                   ymd(paste0(year, "-07-31"))),
                                        "transit",
                                        ifelse(date_obs %within% interval(ymd(paste0(year, "-11-01")), 
                                                                          ymd(paste0(year, "-12-20"))), 
                                               "transit", "south east")))) %>%
  filter(age %in% c("adult", "unknown", "subadult"),
         herds_position %in% c("north west", "south east")) %>%
  group_by(herds_position) %>%
  summarise(median_long = median(long),
            median_lat = median(lat))

# The difference in long and lat
(diff_long <- median_per_season$median_long[1] - median_per_season$median_long[2])
(diff_lat <- median_per_season$median_lat[1] - median_per_season$median_lat[2])



# The total distance
geosphere::distGeo(median_per_season[1, 2:3], median_per_season[2, 2:3])

# The longitudinal distance only
coords1 <- unlist(c(median_per_season[1, 2:3]))
coords2 <- unlist(c(median_per_season[2, 2], median_per_season[1, 3]))
geosphere::distGeo(coords1, coords2)

# The latitudinal distance only
coords1 <- unlist(c(median_per_season[1, 2:3]))
coords2 <- unlist(c(median_per_season[1, 2], median_per_season[2, 3]))
geosphere::distGeo(coords1, coords2)




# ~~~ b. High certainty --------------------------------------------------------
hy_carcasses_formatted <- read_delim("06_processed_data/carcasses/12_hy.carcasses.certainty.formatted.spatial_updated_10-2021.csv", 
                                     ";", escape_double = FALSE, trim_ws = TRUE)

median_per_season_high_certainty <- hy_carcasses_formatted %>%
  filter(location_certainty_score >= 0.75) %>%
  mutate(date_obs = as.Date(x = as.character(date_obs), 
                            format = "%Y%m%d",
                            origin = "1970-01-01"),
         year = year(date_obs)) %>%
  mutate(herds_position = ifelse(date_obs %within% interval(ymd(paste0(year, "-08-01")), 
                                                            ymd(paste0(year, "-10-31"))), 
                                 "north west", 
                                 ifelse(date_obs %within% interval(ymd(paste0(year, "-05-11")), 
                                                                   ymd(paste0(year, "-07-31"))),
                                        "transit",
                                        ifelse(date_obs %within% interval(ymd(paste0(year, "-11-01")), 
                                                                          ymd(paste0(year, "-12-20"))), 
                                               "transit", "south east")))) %>%
  filter(age %in% c("adult", "unknown", "subadult"),
         herds_position %in% c("north west", "south east")) %>%
  group_by(herds_position) %>%
  summarise(median_long = median(long),
            median_lat = median(lat))

# The difference in long and lat
(diff_long <- median_per_season_high_certainty$median_long[1] - median_per_season_high_certainty$median_long[2])
(diff_lat <- median_per_season_high_certainty$median_lat[1] - median_per_season_high_certainty$median_lat[2])



# The total distance
geosphere::distGeo(median_per_season_high_certainty[1, 2:3], median_per_season_high_certainty[2, 2:3])

# The longitudinal distance only
coords1 <- unlist(c(median_per_season_high_certainty[1, 2:3]))
coords2 <- unlist(c(median_per_season_high_certainty[2, 2], median_per_season_high_certainty[1, 3]))
geosphere::distGeo(coords1, coords2)

# The latitudinal distance only
coords1 <- unlist(c(median_per_season_high_certainty[1, 2:3]))
coords2 <- unlist(c(median_per_season_high_certainty[1, 2], median_per_season_high_certainty[2, 3]))
geosphere::distGeo(coords1, coords2)




# ~ 4. Number of roadkill per 100km per year -----------------------------------

# ~~~ a. All roads taken together ----------------------------------------------

# here, no need for the carcasses to be located by GPS, we know they were found in
# the study area.

hy_carcasses <- read_delim("06_processed_data/carcasses/12_hy.carcasses.certainty.formatted.spatial_updated_10-2021.csv", 
                                     ";", escape_double = FALSE, trim_ws = TRUE) %>%
  mutate(date_obs = as.Date(x = as.character(date_obs), 
                            format = "%Y%m%d",
                            origin = "1970-01-01"),
         year = year(date_obs)) 


yearly.roadkills <- hy_carcasses %>%
  filter(year < 2020) %>%
  count(year) %>%
  mutate(rate = n/total.length,
         rate_100km = 100*n/total.length) # Get total.length from ~ 1.

# Need to add the years during which no carcasses were found
years <- seq(1989, 2019, by = 1)

remove <- c()
for (k in 1:length(years)) {
  if (length(which(yearly.roadkills$year == years[k]))) {
    remove <- c(remove, k)
  }
}

add <- data.frame(year = years[-remove],
                  n = 0,
                  rate = seq(0.00000001, 0.00000007, by = 0.00000001), # to avoid ties
                  rate_100km = seq(0.000001, 0.000007, by = 0.000001))

x2 <- which(yearly.roadkills$n == 2)
yearly.roadkills$rate_100km[x2] <- yearly.roadkills$rate_100km[x2] + 
  seq(0.00000001, (0.00000001 + (length(x2) - 1)*0.00000001), by = 0.00000001)
x3 <- which(yearly.roadkills$n == 3)
yearly.roadkills$rate_100km[x3] <- yearly.roadkills$rate_100km[x3] + 
  seq(0.00000001, (0.00000001 + (length(x3) - 1)*0.00000001), by = 0.00000001)
x4 <- which(yearly.roadkills$n == 4)
yearly.roadkills$rate_100km[x4] <- yearly.roadkills$rate_100km[x4] + 
  seq(0.00000001, (0.00000001 + (length(x4) - 1)*0.00000001), by = 0.00000001)
x10 <- which(yearly.roadkills$n == 10)
yearly.roadkills$rate_100km[x10] <- yearly.roadkills$rate_100km[x10] + 
  seq(0.00000001, (0.00000001 + (length(x10) - 1)*0.00000001), by = 0.00000001)


yearly.roadkills <- rbind(yearly.roadkills,
                          add) %>%
  arrange(year)

# Get median with 95% CI interval
wilcox.test(yearly.roadkills$rate_100km, conf.int = TRUE, conf.level = 0.95)






# ~~~ b. Only on major roads ---------------------------------------------------

source(file = "05_scripts/04-2021/GLM_2021-10/GLM_functions_calculate_predictors_2021-10.R")

roads <- rgdal::readOGR(dsn  =  "06_processed_data/roads/SNP.roads.QGIS",
                        layer = "snp_roads_according_to_FZS_map_cropped")
roads@data[["osm_id"]] <- seq(from = 1, to = length(roads@data[["osm_id"]]))

# Convert the shapefile into a data frame
roads.df <- CreateDataFrameRoads(roads) %>%
  mutate(ID = as.factor(ID))


hy_carcasses <- read_delim("06_processed_data/carcasses/12_hy.carcasses.certainty.formatted.spatial_updated_10-2021.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE) %>%
  mutate(date_obs = as.Date(x = as.character(date_obs), 
                            format = "%Y%m%d",
                            origin = "1970-01-01"),
         year = year(date_obs)) %>%
  filter(year < 2020,
         location_certainty_score > 0)

dist.min <- c()
for (i in 1:nrow(hy_carcasses)) {
  roads.df.i <- roads.df %>%
    filter(abs(long - as.numeric(hy_carcasses$long[i])) < 0.05,
           abs(lat - as.numeric(hy_carcasses$lat[i])) < 0.05)
  dist.carcass.i <- c()
  d.min <- 1000000000
  ID <- hy_carcasses$ID[i]
  for (j in 1:nrow(roads.df.i)) {
    d <- distGeo(hy_carcasses[i , c('long', 'lat')], 
                 roads.df.i[j, c('long', 'lat')])
    if (d < d.min) { # if the distance between carcass j and road i is smaller 
                    # than the minimum  distance until now for carcass i
      ID_closest_seg <- as.numeric(roads.df.i$ID[j])
      d.min <- d
      long <- roads.df.i$long[j]
      lat <- roads.df.i$lat[j]
      type <- roads.df.i$type[j]
    }
  }
  new.row <- c(ID, ID_closest_seg, d.min, long, lat, type)
  dist.min <- rbind(dist.min, 
                    new.row)
}

dist.min <- as.data.frame(dist.min)
colnames(dist.min) <- c("ID", "ID_closest_seg", "distance", "long", "lat", "type")
write_csv(dist.min, "06_processed_data/carcasses/14_closest.road.carcass.csv")


dist.min <- read_csv("06_processed_data/carcasses/14_closest.road.carcass.csv")
hyena_major_roads <- dist.min %>%
  filter(type == "major_road")

yearly.roadkills.major <- hy_carcasses %>%
  filter(ID %in% hyena_major_roads$ID) %>%
  count(year) %>%
  mutate(rate = n/total.length.main,
         rate_100km = 100*n/total.length.main) # Get total.length from ~ 1.



# Need to add the years during which no carcasses were found
years <- seq(1989, 2019, by = 1)

remove <- c()
for (k in 1:length(years)) {
  if (length(which(yearly.roadkills.major$year == years[k]))) {
    remove <- c(remove, k)
  }
}

add <- data.frame(year = years[-remove],
                  n = 0,
                  rate = 0, # to avoid ties
                  rate_100km = 0)

yearly.roadkills.major <- rbind(yearly.roadkills.major,
                                add) %>%
  arrange(year)

# Mean and confidence interval _________________________________________________
rate <- mean(yearly.roadkills.major$rate_100km)

n <- nrow(yearly.roadkills.major)
sd <- sd(yearly.roadkills.major$rate_100km)

#calculate margin of error
margin <- qt(0.975, df = n-1)*sd/sqrt(n)


# Median and confidence interval _______________________________________________

# add <- data.frame(year = years[-remove],
#                   n = 0,
#                   rate = seq(0.00000001, 0.00000010, by = 0.00000001), # to avoid ties
#                   rate_100km = seq(0.000001, 0.000010, by = 0.000001))
# 
# x1 <- which(yearly.roadkills.major$n == 1)
# yearly.roadkills.major$rate_100km[x1] <- yearly.roadkills.major$rate_100km[x1] + 
#   seq(0.000001, (0.000001 + (length(x1) - 1)*0.000001), by = 0.000001)
# x2 <- which(yearly.roadkills.major$n == 2)
# yearly.roadkills.major$rate_100km[x2] <- yearly.roadkills.major$rate_100km[x2] + 
#   seq(0.000001, (0.000001 + (length(x2) - 1)*0.000001), by = 0.000001)
# x3 <- which(yearly.roadkills.major$n == 3)
# yearly.roadkills.major$rate_100km[x3] <- yearly.roadkills.major$rate_100km[x3] + 
#   seq(0.000001, (0.000001 + (length(x3) - 1)*0.000001), by = 0.000001)
# x4 <- which(yearly.roadkills.major$n == 4)
# yearly.roadkills.major$rate_100km[x4] <- yearly.roadkills.major$rate_100km[x4] + 
#   seq(0.000001, (0.000001 + (length(x4) - 1)*0.000001), by = 0.000001)
# 
# yearly.roadkills.major <- rbind(yearly.roadkills.major,
#                           add) %>%
#   arrange(year)
# 
# # Get median with 95% CI interval
# wilcox.test(yearly.roadkills.major$rate_100km, conf.int = TRUE, conf.level = 0.95)




# ~~~ c. Only on minor roads ---------------------------------------------------

hy_carcasses <- read_delim("06_processed_data/carcasses/12_hy.carcasses.certainty.formatted.spatial_updated_10-2021.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE) %>%
  mutate(date_obs = as.Date(x = as.character(date_obs), 
                            format = "%Y%m%d",
                            origin = "1970-01-01"),
         year = year(date_obs)) %>%
  filter(year < 2020,
         location_certainty_score > 0)

dist.min <- read_csv("06_processed_data/carcasses/14_closest.road.carcass.csv")
hyena_minor_roads <- dist.min %>%
  filter(type == "minor_road")

yearly.roadkills.minor <- hy_carcasses %>%
  filter(ID %in% hyena_minor_roads$ID) %>%
  count(year) %>%
  mutate(rate = n/total.length.tracks,
         rate_100km = 100*n/total.length.tracks) # Get total.length from ~ 1.


# Need to add the years during which no carcasses were found
years <- seq(1989, 2019, by = 1)

remove <- c()
for (k in 1:length(years)) {
  if (length(which(yearly.roadkills.minor$year == years[k]))) {
    remove <- c(remove, k)
  }
}

# Mean and confidence interval _________________________________________________
rate <- mean(yearly.roadkills.minor$rate_100km)

n <- nrow(yearly.roadkills.minor)
sd <- sd(yearly.roadkills.minor$rate_100km)

#calculate margin of error
margin <- qt(0.975, df = n-1)*sd/sqrt(n)


# Median and confidence interval _______________________________________________

# add <- data.frame(year = years[-remove],
#                   n = 0,
#                   rate = seq(0.00000001, 0.00000015, by = 0.00000001), # to avoid ties
#                   rate_100km = seq(0.000001, 0.000015, by = 0.000001))
# 
# x1 <- which(yearly.roadkills.minor$n == 1)
# yearly.roadkills.minor$rate_100km[x1] <- yearly.roadkills.minor$rate_100km[x1] + 
#   seq(0.000001, (0.000001 + (length(x1) - 1)*0.000001), by = 0.000001)
# x2 <- which(yearly.roadkills.minor$n == 2)
# yearly.roadkills.minor$rate_100km[x2] <- yearly.roadkills.minor$rate_100km[x2] + 
#   seq(0.000001, (0.000001 + (length(x2) - 1)*0.000001), by = 0.000001)
# x3 <- which(yearly.roadkills.minor$n == 3)
# yearly.roadkills.minor$rate_100km[x3] <- yearly.roadkills.minor$rate_100km[x3] + 
#   seq(0.000001, (0.000001 + (length(x3) - 1)*0.000001), by = 0.000001)
# yearly.roadkills.minor <- rbind(yearly.roadkills.minor,
#                                 add) %>%
#   arrange(year)
# 
# # Get median with 95% CI interval
# wilcox.test(yearly.roadkills.minor$rate_100km, conf.int = TRUE, conf.level = 0.95)





library(boot) 

# function to obtain the mean
Bmean <- function(data, indices) {
  d <- data[indices] # allows boot to select sample 
  return(mean(d))
} 

# bootstrapping with 1000 replications 
results <- boot(data=data0, statistic=Bmean, R=1000)


# view results
results 
plot(results)

# get 95% confidence interval 
boot.ci(results, type=c("norm", "basic", "perc", "bca"))


# C. Other visualisations ------------------------------------------------------

# ~ 1. Marion's VS Serengeti IV seasons ----------------------------------------


seasons <- data.frame(source = c(rep("Marion", times = 4), c(rep("Serengeti_IV", times = 4))),
                      position_herds = c(rep(c("south-east", "transit", "north-west", "transit"), times = 2)),
                      start = as.Date(c("1989-11-16", "1990-04-26",
                                          "1990-06-26", "1990-10-26",
                                          
                                          "1989-12-21", "1990-05-11",
                                          "1990-08-01", "1990-11-01")),
                              
                      
                      end = as.Date(c("1990-04-25", "1990-06-25",
                                      "1990-10-25", "1990-11-15",
                                      
                                      "1990-05-10", "1990-07-31",
                                      "1990-10-31", "1990-12-20")))
                              

ggplot(seasons, aes(x = start, xend = end, 
                    y = source, yend = source, 
                    color = position_herds)) +
  geom_segment(size = 6) +
  scale_colour_manual(values = c('#e69f00', '#336600', '#333333')) +
  theme_bw()

ggsave("07_intermediate_results/2021-04/plots/Marion's season VS Serengeti IV seasons.png",
       width = 6, height = 2.5)
