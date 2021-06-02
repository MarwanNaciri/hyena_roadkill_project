#==============================================================================#
#                                                                              #
#                 Test GLM with negative binomial distribution                 #
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



# A. All carcasses --------------------------------------------------------------

# Chose the buffer size  before loading the corresponding dataframe containing
# all the data required to run the model
buffer.size <- 500 
buffer.size <- 1000

table.glm <- read_csv(paste0("06_processed_data/glm_data_2021-04/table.glm_10.", 
                             buffer.size, "m.csv"))

# ~~~ Fit the model -----------------
glm_nb_all <- MASS::glm.nb(formula = nbr_carcasses ~ road_importance + distance_amenity_km
                           + distance_water_km + woodland,
                           data = table.glm,
                           link = log)
summary(glm_nb_all)
AICcmodavg::AICc(glm_nb_all, return.K = FALSE, second.ord = TRUE,
                 nobs = NULL, c.hat = 1)



# ~~~ Check the assumptions of the model using DHARMA --------------------------------

simulationOutput <- DHARMa::simulateResiduals(fittedModel = glm_nb_all, n= 5000)
plot(simulationOutput)

DHARMa::testDispersion(glm_nb_all)
DHARMa::testZeroInflation(glm_nb_all)
DHARMa::testQuantiles(glm_nb_all) 
DHARMa::testUniformity(glm_nb_all)
DHARMa::testResiduals(glm_nb_all)

DHARMa::plotResiduals(glm_nb_all, table.glm$road_importance) # No quantile deviation and no quadratic effect
DHARMa::plotResiduals(glm_nb_all, table.glm$distance_amenity_km)
DHARMa::plotResiduals(glm_nb_all, table.glm$distance_water_km) # No quantile deviation and no quadratic effect
DHARMa::plotResiduals(glm_nb_all, table.glm$woodland)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# B. Carcasses with certainty scores >= 0.5 ------------------------------------

# Chose the buffer size  before loading the corresponding dataframe containing all
# the data to run the model
buffer.size <- 500
buffer.size <- 1000

table.glm.0.5 <- read_csv(paste0("06_processed_data/glm_data_2021-04/table.glm_10.", 
                                 buffer.size, "m.0.5.csv"))



# ~~~ Fit the model -----------------

glm_nb <- MASS::glm.nb(formula = nbr_carcasses ~ road_importance + distance_amenity_km
                       + distance_water_km + woodland,
                       data = table.glm.0.5,
                       link = log)
summary(glm_nb)
AICcmodavg::AICc(glm_nb, return.K = FALSE, second.ord = TRUE,
                 nobs = NULL, c.hat = 1)


# ~~~ Check the assumptions of the model using DHARMa -----------------------------------

simulationOutput <- DHARMa::simulateResiduals(fittedModel = glm_nb, n= 5000)
plot(simulationOutput)

DHARMa::testDispersion(glm_nb)
DHARMa::testZeroInflation(glm_nb)
DHARMa::testQuantiles(glm_nb)
DHARMa::testUniformity(glm_nb)
DHARMa::testResiduals(glm_nb)

DHARMa::plotResiduals(glm_nb, table.glm.0.5$road_importance) # No quantile deviation and no quadratic effect
DHARMa::plotResiduals(glm_nb, table.glm.0.5$distance_amenity_km) # No quantile deviation and no quadratic effect
DHARMa::plotResiduals(glm_nb, table.glm.0.5$distance_water_km) # No quantile deviation and no quadratic effect
DHARMa::plotResiduals(glm_nb, table.glm.0.5$woodland) # No quantile deviation and no quadratic effect



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# C. Carcasses with certainty scores >= 0.75 -----------------------------------

# Chose the buffer size  before loading the corresponding dataframe containing all
# the data to run the model
buffer.size <- 500
buffer.size <- 1000

table.glm.0.75 <- read_csv(paste0("06_processed_data/glm_data_2021-04/table.glm_10.", 
                                  buffer.size, "m.0.75.csv"))


# ~~~ Fit the model -----------------

glm_nb_0.75 <- MASS::glm.nb(formula = nbr_carcasses ~ road_importance + distance_amenity_km
                            + distance_water_km + woodland,
                            data = table.glm.0.75,
                            link = log)
summary(glm_nb_0.75)
AICcmodavg::AICc(glm_nb_0.75, return.K = FALSE, second.ord = TRUE,
                 nobs = NULL, c.hat = 1)


# ~~~ Check the assumptions of the model using DHARMa -----------------------------------

simulationOutput <- DHARMa::simulateResiduals(fittedModel = glm_nb_0.75, n= 5000)
plot(simulationOutput)

DHARMa::testDispersion(glm_nb_0.75)
DHARMa::testZeroInflation(glm_nb_0.75)
DHARMa::testQuantiles(glm_nb_0.75)
DHARMa::testUniformity(glm_nb_0.75)
DHARMa::testResiduals(glm_nb_0.75)

DHARMa::plotResiduals(glm_nb_0.75, table.glm.0.75$road_importance) # No quantile deviation and no quadratic effect
DHARMa::plotResiduals(glm_nb_0.75, table.glm.0.75$distance_amenity_km) 
DHARMa::plotResiduals(glm_nb_0.75, table.glm.0.75$distance_water_km) 
DHARMa::plotResiduals(glm_nb_0.75, table.glm.0.75$woodland) 




