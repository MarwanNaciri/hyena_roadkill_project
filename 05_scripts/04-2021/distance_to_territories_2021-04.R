#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
#                         Distance to territories 2.0
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(tidyverse)
library(rgdal)
library(geosphere)
Sys.setenv(LANG = "en")


# Load the average position of the each clan's den
clans <- rgdal::readOGR("04_raw_data/hyenas", layer = "clan_centroids")

clans.df <- c()
for (i in 1:3) { 
  row.i <- data.frame(ID = clans@data[["clan"]][i], 
                      long = clans@coords[i,1], 
                      lat = clans@coords[i,2])
  clans.df <- rbind(clans.df, row.i)
}
colnames(clans.df)[1] <- "clan"
# clans.df <- clans.df %>%
#   mutate(clan = ifelse(ID == "I", "Isiaka",
#                        ifelse(ID == "M", "Mamba", "Pool"))) %>%
#   dplyr::select(clan, long, lat)



# Load the carcasses of hyenas belonging to the studied clans
hy.carcasses.clan.members <- read_delim("06_processed_data/carcasses/3_hy.clan.members.csv", 
                                        ";", escape_double = FALSE, trim_ws = TRUE)[1:13,] # Need to select first 13 rows because not
                                                                                           # there are empty rows


# Calculate the distance between each hyena carcass and the average position of the den 
# of the clan to which the hyena belonged
distance.from.den <- c()
for (k in 1:nrow(hy.carcasses.clan.members)) {
  x <- which(clans.df$clan == hy.carcasses.clan.members$clan[k])
  distance.from.den[k] <- distGeo(hy.carcasses.clan.members[k, 4:5], clans.df[x, 2:3]) #in meters
  
  # distance.from.den[k] <- sqrt((hy.carcasses.clan.members$long[k] - clans.df$long[x])^2 + 
  #                             (hy.carcasses.clan.members$lat[k] - clans.df$lat[x])^2)
}
hy.carcasses.clan.members <- data.frame(hy.carcasses.clan.members, 
                                        distance_den = distance.from.den)

hy.carcasses.clan.members[hy.carcasses.clan.members$sex == "F", 
                          c("initial_ID", "ID_clan", "date_death", "sex", 
                            "age_category", "standardized_rank", "distance_den")]


# Get prey level
table_01 <- read_delim("06_processed_data/carcasses/table_01.csv", 
                       ";", escape_double = FALSE, trim_ws = TRUE)[,1:33]

# WARNING: in table_01, there are carcasses that are excluded from the analysis (uncertain cause of death, etc.)
prey.levels_i <- c()
prey.levels_m <- c()
prey.levels_p <- c()
for (k in 1:nrow(hy.carcasses.clan.members)) {
  x <- which(table_01$other_id == hy.carcasses.clan.members$ID_clan[k])
  prey.levels_i[k] <- table_01$prey_level_i[x]
  prey.levels_m[k] <- table_01$prey_level_m[x]
  prey.levels_p[k] <- table_01$prey_level_p[x]
}

hy.carcasses.clan.members <- data.frame(hy.carcasses.clan.members, 
                                        prey.levels_i,
                                        prey.levels_m,
                                        prey.levels_p)

hy.carcasses.clan.members <- hy.carcasses.clan.members %>%
  mutate(relevant_prey_level = ifelse(clan == "I", prey.levels_i,
                                      ifelse(clan == "M", prey.levels_m, prey.levels_p)))


# Outside their territory ?
hy.carcasses.clan.members <- hy.carcasses.clan.members %>%
  mutate(killed_outside_territoriy = ifelse(distance_den > 4203, "yes", "no")) %>%
  dplyr::select(-collision_certainty_score_NEW, -GPS_certainty_score, #☻ Remove useless columns
                -den_long, -den_lat, -den_long_1, -den_lat_2,
                -prey.levels_i, -prey.levels_m, -prey.levels_p)


# PLOT
hy.carcasses.clan.members.2 <- hy.carcasses.clan.members[-4,]

ggplot(hy.carcasses.clan.members.2, 
       aes(x = as.factor(relevant_prey_level), y = distance_den)) +
  geom_boxplot() +
  geom_point() +
  theme_classic()




wilcox.test(formula = distance.from.den ~ relevant_prey_level, 
            data = hy.carcasses.clan.members.2, alternative = "two.sided", exact = TRUE)


hy.carcasses.clan.members.F <- hy.carcasses.clan.members%>%
  filter(sex == "F")

hy.carcasses.clan.members.M <- hy.carcasses.clan.members%>%
  filter(sex == "M")

ggplot(hy.carcasses.clan.members.F, aes(x = standardized_rank, y = distance.from.den)) +
  geom_point()

