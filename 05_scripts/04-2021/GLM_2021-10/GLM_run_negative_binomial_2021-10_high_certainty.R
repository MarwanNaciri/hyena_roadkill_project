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


# Chose the buffer size  before loading the corresponding dataframe containing 
# all the data required to run the model
buffer.size <- 500
# buffer.size <- 1000

table.glm <- read_csv(paste0("06_processed_data/glm_data_2021-10_high_certainty/table.glm_9.", 
                             buffer.size, "m.csv"))

# ~~~ Fit the model -----------------
glm_nb_all <- MASS::glm.nb(formula = nbr_carcasses ~ road_importance + distance_amenity_km
                           + distance_water_km + woodland,
                           data = table.glm,
                           link = log)
# glm_nb_all <- MASS::glm.nb(formula = nbr_carcasses ~ road_importance * distance_amenity_km
#                            + distance_water_km + woodland + length,
#                            data = table.glm,
#                            link = log)

summary(glm_nb_all)
anova(glm_nb_all)
(AIC <- AICcmodavg::AICc(glm_nb_all, 
                         return.K = FALSE, 
                         second.ord = TRUE,
                         nobs = NULL, 
                         c.hat = 1))

plot(glm_nb_all)

# ~~~ Check the assumptions of the model using DHARMA --------------------------------
residuals_norm.all <- (glm_nb_all[["residuals"]] - min(glm_nb_all[["residuals"]]))/(max(glm_nb_all[["residuals"]])-min(glm_nb_all[["residuals"]]))
qqnorm(residuals_norm.all, pch = 1, frame = FALSE)
qqline(residuals_norm.all, col = "steelblue", lwd = 2)
shapiro.test(residuals_norm.all) # p <2.2e-16


simulationOutput <- DHARMa::simulateResiduals(fittedModel = glm_nb_all, n = 5000)
plot(simulationOutput)

DHARMa::testDispersion(glm_nb_all)
DHARMa::testZeroInflation(glm_nb_all)
DHARMa::testQuantiles(glm_nb_all) 
DHARMa::testUniformity(glm_nb_all)
DHARMa::testResiduals(glm_nb_all)

DHARMa::plotResiduals(glm_nb_all, as.factor(table.glm$road_importance))
DHARMa::plotResiduals(glm_nb_all, table.glm$distance_amenity_km)
DHARMa::plotResiduals(glm_nb_all, table.glm$distance_water_km) # A bit of quantile deviation but combined adjused quantile test n.s.
DHARMa::plotResiduals(glm_nb_all, table.glm$woodland)





