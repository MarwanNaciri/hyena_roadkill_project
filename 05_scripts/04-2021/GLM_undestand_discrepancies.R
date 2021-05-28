#==============================================================================#
#                                                                              #
#                    Understand the discrepancies in the GLM                   #
#                                                                              #
#==============================================================================#

library(tidyverse)
library(glmmTMB)
library(pscl)
library(DHARMa)
library(MASS)
library(EMT)
library(fmsb)
library(broom)
Sys.setenv(LANG = "en")


# 1. Try old predictors with new response variable -----------------------

# Old dataset ----------------------
table.glm.old <- read_delim("6_processed_data/glm_data/f_table.glm.final.csv",
                            ";", escape_double = FALSE, col_types = cols(X1 = col_skip()),
                            trim_ws = TRUE) %>%
  mutate(grassland = sparse_open_grassland + dense_closed_grassland,
         shrubland = sparse_open_shrubland + dense_closed_shrubbland)
glm_nb_all_old <- MASS::glm.nb(formula = nbr_carcasses ~ road_importance * distance_building_km
                               + distance_water_km + woodland,
                               data = table.glm.old,
                               link = log)
summary(glm_nb_all_old)


# New dataset ---------------------

buffer.size <- 500

table.glm <- read_csv(paste0("6_processed_data/glm_data_2021-04/table.glm_11.", buffer.size, "m.csv"))

glm_nb_all <- MASS::glm.nb(formula = nbr_carcasses ~ road_importance * distance_amenity_km
                           + distance_water_km + woodland,
                           data = table.glm,
                           link = log)
# glm_nb_all <- MASS::glm.nb(formula = nbr_carcasses ~ road_importance * distance_amenity_km
#                            + distance_water_km + grassland, # Woodland
#                            data = table.glm,
#                            link = log)

summary(glm_nb_all)



# Old with new hyena carcass number ---------------
table.glm.old.w.new <- table.glm.old %>%
  mutate(nbr_carcasses == table.glm$nbr_carcasses,
         grassland = sparse_open_grassland + dense_closed_grassland,
         shrubland = sparse_open_shrubland + dense_closed_shrubbland)

glm_nb_all_old_w_new <- MASS::glm.nb(formula = nbr_carcasses ~ road_importance * distance_building_km
                                     + distance_water_km + woodland,
                                     data = table.glm.old.w.new,
                                     link = log)
summary(glm_nb_all_old_w_new)


# New with old hyena carcass number ---------------
table.glm.new.w.old <- table.glm %>%
  mutate(nbr_carcasses == table.glm.old$nbr_carcasses)

glm_nb_all_new_w_old <- MASS::glm.nb(formula = nbr_carcasses ~ road_importance * distance_amenity_km
                                     + distance_water_km + woodland,
                                     data = table.glm.new.w.old,
                                     link = log)
summary(glm_nb_all_new_w_old)

# Without scaling
table.glm.new.w.old <- table.glm_10 %>%
  mutate(nbr_carcasses == table.glm.old$nbr_carcasses)

glm_nb_all_new_w_old <- MASS::glm.nb(formula = nbr_carcasses ~ road_importance * distance_amenity_km
                                     + distance_water_km + woodland,
                                     data = table.glm.new.w.old,
                                     link = log)
summary(glm_nb_all_new_w_old)

hist(table.glm.old$distance_building_km)
hist(table.glm_10$distance_amenity_km)

# 2. Try to scale old predictors with old/new response variable ----------------

table.glm.old.w.new.scaled <- table.glm.old.w.new %>%
  mutate_at(c("distance_water_km", "distance_building_km", "sparse_open_grassland",
              "dense_closed_grassland", "sparse_open_shrubland", "dense_closed_shrubbland",
              "woodland", "forest", "grassland", "shrubland", "water", "bare_ground"), ~ (scale(.) %>% as.vector))

glm_nb_all_old_w_new_scaled <- MASS::glm.nb(formula = nbr_carcasses ~ road_importance * distance_building_km
                                     + distance_water_km + woodland,
                                     data = table.glm.old,
                                     link = log)
summary(glm_nb_all_old_w_new_scaled)


# 3. Check whether row values match for predictors

options(scipen = 999) 

table.glm_10 <- read_csv(paste0("6_processed_data/glm_data_2021-04/table.glm_10.", buffer.size, "m.csv"))

(table.glm_10$distance_water_km - table.glm.old.w.new$distance_water_km) < 0.001

(table.glm_10$distance_amenity_km - table.glm.old.w.new$distance_building_km) < 0.001

(table.glm_10$woodland - table.glm.old.w.new$woodland) < 0.001

x <- table.glm_10$woodland - table.glm.old.w.new$woodland
x1 <- table.glm_10$distance_amenity_km - table.glm.old.w.new$distance_building_km
x2 <- table.glm_10$distance_water_km - table.glm.old.w.new$distance_water_km

sum((table.glm_10$distance_water_km - table.glm.old.w.new$distance_water_km) < 0.01)
sum((table.glm_10$woodland - table.glm.old.w.new$woodland) < 0.01)


index_corres <- c()
for (k in 1:nrow(table.glm_10)) {
  index_corres <- c(index_corres, which(table.glm.old.w.new$woodland == table.glm_10$woodland[k]))
}


y[2]
x2[2]

table.new.with.old.woodland <- table.glm %>%
  mutate(woodland = table.glm.old$woodland)

glm_nb_old_woodland <- MASS::glm.nb(formula = nbr_carcasses ~ road_importance * distance_amenity_km
                           + distance_water_km + woodland,
                           data = table.new.with.old.woodland,
                           link = log)
summary(glm_nb_old_woodland)


table.new.with.old.water <- table.glm %>%
  mutate(distance_water_km = table.glm.old$distance_water_km)

glm_nb_old_water <- MASS::glm.nb(formula = nbr_carcasses ~ road_importance * distance_amenity_km
                                    + distance_water_km + woodland,
                                    data = table.new.with.old.water,
                                    link = log)
summary(glm_nb_old_water)

table.new.with.old.amenity <- table.glm %>%
  mutate(distance_amenity_km = table.glm.old$distance_building_km)

glm_nb_old_amenity <- MASS::glm.nb(formula = nbr_carcasses ~ road_importance * distance_amenity_km
                                 + distance_water_km + woodland,
                                 data = table.new.with.old.amenity,
                                 link = log)
summary(glm_nb_old_amenity)
