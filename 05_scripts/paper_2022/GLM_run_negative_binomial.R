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

table.glm <- read_csv(paste0("06_processed_data/glm_data_2022-04/table.glm_9.", 
                             buffer.size, "m.csv")) %>%
  mutate(road_importance = factor(road_importance, levels = c("minor_road", "major_road")),
         woodland2 = woodland^2,
         distance_amenity_km2 = distance_amenity_km^2)

# ~~~ Fit the models -----------------
glm_nb_all <- MASS::glm.nb(formula = nbr_carcasses ~ road_importance + distance_amenity_km
                           + distance_water_km + woodland,
                           data = table.glm,
                           link = log)
(AIC <- AICcmodavg::AICc(glm_nb_all, return.K = FALSE, second.ord = TRUE,
                         nobs = NULL,  c.hat = 1))
summary(glm_nb_all)
anova(glm_nb_all)
plot(glm_nb_all)

# Without road imp
glm_nb_0 <- MASS::glm.nb(formula = nbr_carcasses ~ distance_amenity_km
                         + distance_water_km + woodland,
                         data = table.glm,
                         link = log)
(AIC_0 <- AICcmodavg::AICc(glm_nb_0, return.K = FALSE, second.ord = TRUE,
                           nobs = NULL,  c.hat = 1))
anova(glm_nb_all, glm_nb_0, test = "LRT")

# Without distance_amenity_km
glm_nb_3 <- MASS::glm.nb(formula = nbr_carcasses ~ road_importance + distance_water_km
                         + woodland,
                         data = table.glm,
                         link = log)
(AIC_3 <- AICcmodavg::AICc(glm_nb_3, return.K = FALSE, second.ord = TRUE,
                           nobs = NULL,  c.hat = 1))
anova(glm_nb_all, glm_nb_3, test = "LRT")

# Without distance_water_km
glm_nb_2 <- MASS::glm.nb(formula = nbr_carcasses ~ road_importance + distance_amenity_km
                         + woodland,
                         data = table.glm,
                         link = log)
(AIC_2 <- AICcmodavg::AICc(glm_nb_2, return.K = FALSE, second.ord = TRUE,
                           nobs = NULL,  c.hat = 1))
anova(glm_nb_all, glm_nb_2, test = "LRT")

# Without woodland
glm_nb_1 <- MASS::glm.nb(formula = nbr_carcasses ~ road_importance + distance_amenity_km
                           + distance_water_km,
                           data = table.glm,
                           link = log)
(AIC_1 <- AICcmodavg::AICc(glm_nb_1, return.K = FALSE, second.ord = TRUE,
                           nobs = NULL,  c.hat = 1))
anova(glm_nb_all, glm_nb_1, test = "LRT")



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
DHARMa::plotResiduals(glm_nb_all, table.glm$distance_water_km)
DHARMa::plotResiduals(glm_nb_all, table.glm$woodland)





