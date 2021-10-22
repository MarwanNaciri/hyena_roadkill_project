#==============================================================================#
#                                                                              #
#                   Likelyhood ratio test for model selection                  #
#                                                                              #
#==============================================================================#

library(tidyverse)
library(MASS)
library(mdscore) # This package is for lr.test but this function only works for
# glm objects (not the object returned by glm.nb)



# A. All carcasses ============================================================

buffer.size <- 500
# buffer.size <- 1000

table.glm <- read_csv(paste0("06_processed_data/glm_data_2021-09/table.glm_9.", 
                             buffer.size, "m.csv"))


# ~~~ a. Full model ------------------------------------------------------------

glm_nb_all <- MASS::glm.nb(formula = nbr_carcasses ~ road_importance + distance_amenity_km
                           + distance_water_km + woodland,
                           data = table.glm,
                           link = log)
summary(glm_nb_all)




# ~~~ b. Reduced model: no woodland --------------------------------------------

glm_nb_reduced_1 <- MASS::glm.nb(formula = nbr_carcasses ~ road_importance + distance_amenity_km
                           + distance_water_km,
                           data = table.glm,
                           link = log)

summary(glm_nb_reduced_1)


# LRT
anova(glm_nb_all, glm_nb_reduced_1, test = "LRT")



# ~~~ c. Reduced model: no water ----------------------------------------------

glm_nb_reduced_2 <- MASS::glm.nb(formula = nbr_carcasses ~ road_importance + distance_amenity_km
                                 + woodland,
                                 data = table.glm,
                                 link = log)

summary(glm_nb_reduced_2)


# LRT
anova(glm_nb_all, glm_nb_reduced_2, test = "LRT")



# ~~~ d. Reduced model: no amenity ---------------------------------------------

glm_nb_reduced_3 <- MASS::glm.nb(formula = nbr_carcasses ~ road_importance + distance_water_km
                                 + woodland,
                                 data = table.glm,
                                 link = log)

summary(glm_nb_reduced_3)


# LRT
anova(glm_nb_all, glm_nb_reduced_3, test = "LRT")



# ~~~ e. Reduced model: no road type  ------------------------------------------

glm_nb_reduced_4 <- MASS::glm.nb(formula = nbr_carcasses ~ distance_amenity_km + distance_water_km
                                 + woodland,
                                 data = table.glm,
                                 link = log)

summary(glm_nb_reduced_4)


# LRT
anova(glm_nb_all, glm_nb_reduced_4, test = "LRT")





# B. >0.5 carcasses ===========================================================

buffer.size <- 500
# buffer.size <- 1000

table.glm.0.5 <- read_csv(paste0("06_processed_data/glm_data_2021-09/table.glm_9.", 
                                  buffer.size, "m.0.5.csv"))

# ~~~ a. Full model ------------------------------------------------------------

glm_nb_0.5 <- MASS::glm.nb(formula = nbr_carcasses ~ road_importance + distance_amenity_km
                           + distance_water_km + woodland,
                           data = table.glm.0.5,
                           link = log)
summary(glm_nb_0.5)




# ~~~ b. Reduced model: no woodland --------------------------------------------

glm_nb_reduced_1 <- MASS::glm.nb(formula = nbr_carcasses ~ road_importance + distance_amenity_km
                                 + distance_water_km,
                                 data = table.glm.0.5,
                                 link = log)

summary(glm_nb_reduced_1)


# LRT
anova(glm_nb_0.5, glm_nb_reduced_1, test = "LRT")



# ~~~ c. Reduced model: no water ----------------------------------------------

glm_nb_reduced_2 <- MASS::glm.nb(formula = nbr_carcasses ~ road_importance + distance_amenity_km
                                 + woodland,
                                 data = table.glm.0.5,
                                 link = log)

summary(glm_nb_reduced_2)


# LRT
anova(glm_nb_0.5, glm_nb_reduced_2, test = "LRT")



# ~~~ d. Reduced model: no amenity ---------------------------------------------

glm_nb_reduced_3 <- MASS::glm.nb(formula = nbr_carcasses ~ road_importance + distance_water_km
                                 + woodland,
                                 data = table.glm.0.5,
                                 link = log)

summary(glm_nb_reduced_3)


# LRT
anova(glm_nb_0.5, glm_nb_reduced_3, test = "LRT")



# ~~~ e. Reduced model: no road type  ------------------------------------------

glm_nb_reduced_4 <- MASS::glm.nb(formula = nbr_carcasses ~ distance_amenity_km + distance_water_km
                                 + woodland,
                                 data = table.glm.0.5,
                                 link = log)

summary(glm_nb_reduced_4)


# LRT
anova(glm_nb_0.5, glm_nb_reduced_4, test = "LRT")


# B. >0.75 carcasses ===========================================================

buffer.size <- 500
# buffer.size <- 1000

table.glm.0.75 <- read_csv(paste0("06_processed_data/glm_data_2021-09/table.glm_9.", 
                                 buffer.size, "m.0.75.csv"))

# ~~~ a. Full model ------------------------------------------------------------

glm_nb_0.75 <- MASS::glm.nb(formula = nbr_carcasses ~ road_importance + distance_amenity_km
                           + distance_water_km + woodland,
                           data = table.glm.0.75,
                           link = log)
summary(glm_nb_0.75)




# ~~~ b. Reduced model: no woodland --------------------------------------------

glm_nb_reduced_1 <- MASS::glm.nb(formula = nbr_carcasses ~ road_importance + distance_amenity_km
                                 + distance_water_km,
                                 data = table.glm.0.75,
                                 link = log)

summary(glm_nb_reduced_1)


# LRT
anova(glm_nb_0.75, glm_nb_reduced_1, test = "LRT")



# ~~~ c. Reduced model: no water ----------------------------------------------

glm_nb_reduced_2 <- MASS::glm.nb(formula = nbr_carcasses ~ road_importance + distance_amenity_km
                                 + woodland,
                                 data = table.glm.0.75,
                                 link = log)

summary(glm_nb_reduced_2)


# LRT
anova(glm_nb_0.75, glm_nb_reduced_2, test = "LRT")



# ~~~ d. Reduced model: no amenity ---------------------------------------------

glm_nb_reduced_3 <- MASS::glm.nb(formula = nbr_carcasses ~ road_importance + distance_water_km
                                 + woodland,
                                 data = table.glm.0.75,
                                 link = log)

summary(glm_nb_reduced_3)


# LRT
anova(glm_nb_0.75, glm_nb_reduced_3, test = "LRT")



# ~~~ e. Reduced model: no road type  ------------------------------------------

glm_nb_reduced_4 <- MASS::glm.nb(formula = nbr_carcasses ~ distance_amenity_km + distance_water_km
                                 + woodland,
                                 data = table.glm.0.75,
                                 link = log)

summary(glm_nb_reduced_4)


# LRT
anova(glm_nb_0.75, glm_nb_reduced_4, test = "LRT")
