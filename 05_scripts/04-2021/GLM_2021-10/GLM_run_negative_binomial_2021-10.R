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



# ALL AVAILABLE CARCASSES ------------------------------------------------------

# Chose the buffer size  before loading the corresponding dataframe containing 
# all the data required to run the model
buffer.size <- 500
# buffer.size <- 1000

table.glm <- read_csv(paste0("06_processed_data/glm_data_2021-10/table.glm_9.", 
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


#___________________________________________________________________________####
# CARCASSES >= 0.5 -------------------------------------------------------------

buffer.size <- 500
# buffer.size <- 1000

table.glm.0.5 <- read_csv(paste0("06_processed_data/glm_data_2021-09/table.glm_9.", 
                                 buffer.size, "m.0.5.csv"))


# ~~~ Fit the model -----------------

glm_nb <- MASS::glm.nb(formula = nbr_carcasses ~ road_importance + distance_amenity_km
                       + distance_water_km + woodland,
                       data = table.glm.0.5,
                       link = log)
summary(glm_nb)
AICcmodavg::AICc(glm_nb, return.K = FALSE, second.ord = TRUE,
                 nobs = NULL, c.hat = 1)


# # POISSON
# glm_poisson <- glm_poisson <- glm(formula = nbr_carcasses ~ road_importance + distance_amenity_km 
#                                   + distance_water_km,
#                                   family = poisson(link = "log"),
#                                   data = table.glm.0.5)
# AICcmodavg::AICc(glm_poisson, return.K = FALSE, second.ord = TRUE,
#                  nobs = NULL, c.hat = 1)
# # AICc = 347.08
# 
# 
# # Compare the model to Poisson
# (likelyhood.ratio.test <- pchisq(2*(logLik(glm_nb) - logLik(glm_poisson)), df = 1, lower.tail = FALSE)
# ) # 1.87e-05


# ~~~ Check the significance of parameters --------------------

# glm_nb2 <- update(glm_nb, . ~ . - distance_water_km) 
# #summary(glm_nb2)
# anova(glm_nb, glm_nb2) # Distance to water seems non significant, as evidenced in the model output
# 
# 
# # confidence interval for the coefficients
# (est <- cbind(Estimate = coef(glm_nb), confint(glm_nb)))



# ~~~ Check the assumptions of the model using DHARMa -----------------------------------

residuals_norm.0.5 <- (glm_nb[["residuals"]] - min(glm_nb[["residuals"]]))/(max(glm_nb[["residuals"]])-min(glm_nb[["residuals"]]))
qqnorm(residuals_norm.0.5, pch = 1, frame = FALSE)
qqline(residuals_norm.0.5, col = "steelblue", lwd = 2)
shapiro.test(residuals_norm.0.5) # p <2.2e-16

hist(scale(residuals_norm.0.5), breaks = 25)

x <- data.frame(residuals = residuals_norm.0.5)

ggplot(data = x, aes(x = residuals)) +
  geom_histogram() 


simulationOutput <- DHARMa::simulateResiduals(fittedModel = glm_nb, n = 5000)
plot(simulationOutput)

DHARMa::testDispersion(glm_nb)
DHARMa::testZeroInflation(glm_nb)
DHARMa::testQuantiles(glm_nb)
DHARMa::testUniformity(glm_nb)
DHARMa::testResiduals(glm_nb)

DHARMa::plotResiduals(glm_nb, as.factor(table.glm.0.5$road_importance)) # No quantile deviation and no quadratic effect
DHARMa::plotResiduals(glm_nb, table.glm.0.5$distance_amenity_km)
DHARMa::plotResiduals(glm_nb, table.glm.0.5$distance_water_km) # No quantile deviation and no quadratic effect
DHARMa::plotResiduals(glm_nb, table.glm.0.5$woodland)


# # ~~~ Add quadratic effect for distance water -------------------------------
# 
# glm_nb3<- MASS::glm.nb(formula = nbr_carcasses ~ road_importance * distance_amenity_km
#                        + distance_water_km + I(distance_water_km^2),
#                        data = table.glm.0.5,
#                        link = log)
# summary(glm_nb3)
# 
# AICcmodavg::AICc(glm_nb3, return.K = FALSE, second.ord = TRUE,
#                  nobs = NULL, c.hat = 1)
# 
# simulationOutput <- DHARMa::simulateResiduals(fittedModel = glm_nb)
# plot(simulationOutput)
# 
# DHARMa::plotResiduals(glm_nb3, table.glm.0.5$road_importance) # No quantile deviation and no quadratic effect
# DHARMa::plotResiduals(glm_nb3, table.glm.0.5$distance_amenity_km)
# DHARMa::plotResiduals(glm_nb3, table.glm.0.5$distance_water_km)
# table.glm.0.5 <- table.glm.0.5 %>%
#   mutate(distance_water_km_2 = distance_water_km^2)
# DHARMa::plotResiduals(glm_nb3, table.glm.0.5$distance_water_km_2)
# 
# 
# residuals_norm.0.5 <- (glm_nb3[["residuals"]] - min(glm_nb3[["residuals"]]))/(max(glm_nb3[["residuals"]])-min(glm_nb3[["residuals"]]))
# qqnorm(residuals_norm.0.5, pch = 1, frame = FALSE)
# qqline(residuals_norm.0.5, col = "steelblue", lwd = 2)
# shapiro.test(residuals_norm.0.5) # p <2.2e-16
# 
# # The residuals look perfect now (except for normality but maybe the model is robust.  
# # But are we over fitting? A likelihood ratio test tells us if the quadratic effect 
# # is significantly supported (https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html)
# 
# anova(glm_nb, glm_nb3)
# 
# # It seems like it's not  significantly supported and that we might be over-fitting.
# # Not sure what to do then. Aimara suggests to not overthink it.
# 
# 
# # ~~~ Check colinearity between predictors -----------------------------------------
# 
# ggplot(data = table.glm.0.5, aes(x = distance_water_km, y = distance_amenity_km)) +
#   geom_point()
# 
# ggplot(data = table.glm.0.5, aes(x = road_importance, y = distance_amenity_km)) +
#   geom_boxplot()
# 
# ggplot(data = table.glm.0.5, aes(x = road_importance, y = distance_water_km)) +
#   geom_boxplot()
# 
# wilcox.test(formula = distance_water_km ~ road_importance, data = table.glm.0.5, alternative = "two.sided", exact = TRUE)
# 
# 
# # Check significance of the interaction
# 
# 
# glm_nb2<- MASS::glm.nb(formula = nbr_carcasses ~ road_importance + distance_amenity_km
#                        + distance_water_km,
#                        data = table.glm.0.5,
#                        link = log)
# lmtest::lrtest(glm_nb, glm_nb2)
# 


#___________________________________________________________________________####
# CARCASSES >= 0.75 ------------------------------------------------------------

buffer.size <- 500
# buffer.size <- 1000

table.glm.0.75 <- read_csv(paste0("06_processed_data/glm_data_2021-09/table.glm_9.", 
                                  buffer.size, "m.0.75.csv"))


# ~~~ Fit the model -----------------

glm_nb_0.75 <- MASS::glm.nb(formula = nbr_carcasses ~ road_importance + distance_amenity_km
                            + distance_water_km + woodland,
                            data = table.glm.0.75,
                            link = log)
summary(glm_nb_0.75)
AICcmodavg::AICc(glm_nb_0.75, return.K = FALSE, second.ord = TRUE,
                 nobs = NULL, c.hat = 1)


# # POISSON
# glm_poisson <- glm_poisson <- glm(formula = nbr_carcasses ~ road_importance + distance_amenity_km 
#                                   + distance_water_km,
#                                   family = poisson(link = "log"),
#                                   data = table.glm.0.75)
# AICcmodavg::AICc(glm_poisson, return.K = FALSE, second.ord = TRUE,
#                  nobs = NULL, c.hat = 1)
# # AICc = 310.75
# 
# 
# # Compare the model to Poisson
# (likelyhood.ratio.test <- pchisq(2*(logLik(glm_nb_0.75) - logLik(glm_poisson)), df = 1, lower.tail = FALSE)
# ) # 1.87e-05


# ~~~ Check the significance of parameters --------------------

# glm_nb2 <- update(glm_nb, . ~ . - distance_water_km) 
# #summary(glm_nb2)
# anova(glm_nb, glm_nb2) # Distance to water seems non significant, as evidenced in the model output
# 
# 
# # confidence interval for the coefficients
# (est <- cbind(Estimate = coef(glm_nb), confint(glm_nb)))

# ~~~ Check the assumptions of the model using DHARMa -----------------------------------

residuals_norm.0.5 <- (glm_nb_0.75[["residuals"]] - min(glm_nb_0.75[["residuals"]]))/(max(glm_nb_0.75[["residuals"]])-min(glm_nb_0.75[["residuals"]]))
qqnorm(residuals_norm.0.5, pch = 1, frame = FALSE)
qqline(residuals_norm.0.5, col = "steelblue", lwd = 2)
shapiro.test(residuals_norm.0.5) # p <2.2e-16

simulationOutput <- DHARMa::simulateResiduals(fittedModel = glm_nb_0.75, n= 5000)
plot(simulationOutput)

DHARMa::testDispersion(glm_nb_0.75)
DHARMa::testZeroInflation(glm_nb_0.75)
DHARMa::testQuantiles(glm_nb_0.75)
DHARMa::testUniformity(glm_nb_0.75)
DHARMa::testResiduals(glm_nb_0.75)

DHARMa::plotResiduals(glm_nb_0.75, as.factor(table.glm.0.75$road_importance)) # No quantile deviation and no quadratic effect
DHARMa::plotResiduals(glm_nb_0.75, table.glm.0.75$distance_amenity_km) # No quantile deviation and no quadratic effect
DHARMa::plotResiduals(glm_nb_0.75, table.glm.0.75$distance_water_km) # No quantile deviation and no quadratic effect
DHARMa::plotResiduals(glm_nb_0.75, table.glm.0.75$woodland) # No quantile deviation and no quadratic effect




