#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                              #
#                         Test distributions for the GLM                       #        
#                                                                              #
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(tidyverse)
library(glmmTMB)
library(pscl) # For zero inflated Poisson, and hurdle
library(VGAM) # For truncated Poisson
library(DHARMa)
library(MASS)
library(EMT)
library(fmsb)
library(broom)
library(raster)

Sys.setenv(LANG = "en")


# A. ORIGINAL DATASET ----------------------------------------------------------

# Chose the buffer size  before loading the corresponding dataframe containing
# all the data required to run the model
buffer.size <- 500 
buffer.size <- 1000

table.glm <- read_csv(paste0("06_processed_data/glm_data_2021-04/table.glm_10.", 
                             buffer.size, "m.csv"))

# ~ 1. Poisson -----------------------------------------------------------------
glm_pois_all <- glm(formula = nbr_carcasses ~ road_importance + distance_amenity_km
                    + distance_water_km + woodland,
                    data = table.glm,
                    family = "poisson")
summary(glm_pois_all)


# Check overdispersion
resid <- residuals(glm_pois_all, type = "pearson") 
N <- nrow(table.glm)
p <- length(coef(glm_pois_all))
sum (resid^2) / (N-p) # This is superior to 1 => overdispersion.


1 - pchisq(summary(glm_pois_all)$deviance, summary(glm_pois_all)$df.residual)



# ~ 2. Negative binomial -------------------------------------------------------
glm_nb_all <- glm.nb(formula = nbr_carcasses ~ road_importance + distance_amenity_km
                     + distance_water_km + woodland,
                     data = table.glm,
                     link = log)
summary(glm_nb_all)
AICcmodavg::AICc(glm_nb_all, return.K = FALSE, second.ord = TRUE,
                 nobs = NULL, c.hat = 1)
plot(glm_nb_all)
# Check overdispersion
resid <- residuals(glm_nb_all, type = "pearson") 
N <- nrow(table.glm)
p <- length(coef(glm_nb_all))
sum (resid^2) / (N-p) # This is superior to 1 => overdispersion.

1 - pchisq(summary(glm_nb_all)$deviance, summary(glm_nb_all)$df.residual)


# ~~~  Observed VS predicted ---------------------------------------------------

estobsk_1 <- function(k){
  sum(dnbinom(k, mu = fitted(glm_nb_all),
              size = glm_nb_all$theta))
}
# Calculate the absolute frequency of segments with 0:4 carcasses
estobs0to5 <- sapply(X = 0:5,
                     FUN = estobsk_1)
# Calculate the absolute frequency of segments with >4 carcasses
estobs6 <- length(table.glm$nbr_carcasses) - sum(estobs0to5)

estobs <- as.data.frame(append(estobs0to5, estobs6))

# Extract the observed absolute frequency
observed <- table.glm %>%
  count(nbr_carcasses)
observed <- rbind(observed, c(5, 0))


est_obs <- cbind(observed, estobs)
colnames(est_obs) <- c("nbr_carcasses", "observed", "negbin_fit")


# PLOT ---

(est_obs_frequencies <- ggplot() +
    geom_col(data = est_obs, aes(x = nbr_carcasses, y = observed), 
             color = "black",
             fill = "white") +
    geom_point(data = est_obs, aes(x = nbr_carcasses, y = negbin_fit), shape = 16) +
    geom_line(data = est_obs, aes(x = nbr_carcasses, y = negbin_fit)) +
    theme_classic() +
    xlab("Number of carcasses per segment") +
    ylab("Numer of road segments")
)

# ~~~ Check the assumptions of the model using DHARMA --------------------------
simulationOutput <- DHARMa::simulateResiduals(fittedModel = glm_nb_all, n= 5000)
plot(simulationOutput)

DHARMa::testDispersion(glm_nb_all)
DHARMa::testZeroInflation(glm_nb_all)
DHARMa::testQuantiles(glm_nb_all) 
DHARMa::testUniformity(glm_nb_all)
DHARMa::testResiduals(glm_nb_all)
DHARMa::testOutliers(glm_nb_all)

DHARMa::plotResiduals(glm_nb_all, table.glm$road_importance) # No quantile deviation and no quadratic effect
DHARMa::plotResiduals(glm_nb_all, table.glm$distance_amenity_km)
DHARMa::plotResiduals(glm_nb_all, table.glm$distance_water_km) # No quantile deviation and no quadratic effect
DHARMa::plotResiduals(glm_nb_all, table.glm$woodland)



# ~ 3. Zero inflated poisson -----------------

zeroinfl_all <- zeroinfl(formula = nbr_carcasses ~ road_importance + distance_amenity_km
                         + distance_water_km + woodland,
                         data = table.glm)
summary(zeroinfl_all)

# ~~~ Check overdispersion ----------------------------------------------------
resid <- residuals(zeroinfl_all, type = "pearson") 
N <- nrow(table.glm)
p <- length(coef(zeroinfl_all))
sum (resid^2) / (N-p) # This is superior to 1 => overdispersion.

major_roads <- table.glm %>%
  filter(road_importance == "major_road") 

mean(table.glm$nbr_carcasses[table.glm$road_importance == "major_road"])
mean(table.glm$nbr_carcasses[table.glm$road_importance == "minor_road"])

nd <- data.frame(road_importance = c("major_road","minor_road"),
                 distance_amenity_km = mean(table.glm$distance_amenity_km),
                 distance_water_km = mean(table.glm$distance_water_km),
                 woodland = mean(table.glm$woodland))

cbind(nd, 
      Count = predict(zeroinfl_all, newdata = nd, type = "count"), 
      Zero = predict(zeroinfl_all, newdata = nd, type = "zero")) 

major_roads <- table.glm[table.glm$road_importance == "major_road",]
minor_raods <- table.glm[table.glm$road_importance == "minor_road",]

table.glm %>%
  filter(road_importance == "major_road") %>%
  count(nbr_carcasses) %>%
  mutate(prop = n/94) %>%
  ggplot(aes(x = nbr_carcasses, y = prop)) + 
  geom_col()


table.glm %>%
  filter(road_importance == "minor_road") %>%
  count(nbr_carcasses) %>%
  mutate(prop = n/474) %>%
  ggplot(aes(x = nbr_carcasses, y = prop)) + 
  geom_col()


table.glm %>%
  filter(nbr_carcasses != 0) %>%
  group_by(road_importance) %>%
  summarise(mean_nbr = mean(nbr_carcasses))


# ~~~  Observed VS predicted ---------------------------------------------------

estobsk <- function(k){
  sum(predict(zeroinfl_all, type = "prob")[, 1+k])
}

# Calculate the absolute frequency of segments with 0:4 carcasses
estobs0to5 <- sapply(X = 0:5,
                     FUN = estobsk)
# Calculate the absolute frequency of segments with >4 carcasses
estobs6 <- length(table.glm$nbr_carcasses) - sum(estobs0to5)

estobs <- as.data.frame(append(estobs0to5, estobs6))

# Extract the observed absolute frequency
observed <- table.glm %>%
  count(nbr_carcasses)
observed <- rbind(observed, c(5, 0))


est_obs <- cbind(observed, estobs)
colnames(est_obs) <- c("nbr_carcasses", "observed", "negbin_fit")


# PLOT ---

(est_obs_frequencies <- ggplot() +
    geom_col(data = est_obs, aes(x = nbr_carcasses, y = observed), 
             color = "black",
             fill = "white") +
    geom_point(data = est_obs, aes(x = nbr_carcasses, y = negbin_fit), shape = 16) +
    geom_line(data = est_obs, aes(x = nbr_carcasses, y = negbin_fit)) +
    theme_classic() +
    labs(x = "number of carcasses",
         y = "absolute frequency",
         title = "Zero inflated poisson with all covariates in the zero part")
)



ggsave("10_meetings/2021-06-16 Meeting with Sarah, Aimara & Morgane/zero_inflated_all_covariates_in_zero_model.png",
       width = 4, height = 3)


# ~ 4. Zero inflated poisson 2 -----------------

zeroinfl_all_2 <- zeroinfl(formula = nbr_carcasses ~ road_importance + distance_amenity_km
                           + distance_water_km + woodland | 1,
                           data = table.glm)
summary(zeroinfl_all_2)

# ~~~ Check overdispersion ----------------------------------------------------
resid <- residuals(zeroinfl_all_2, type = "pearson") 
N <- nrow(table.glm)
p <- length(coef(zeroinfl_all_2))
sum (resid^2) / (N-p) # This is superior to 1 => overdispersion.



major_roads <- table.glm %>%
  filter(road_importance == "major_road") 

mean(table.glm$nbr_carcasses[table.glm$road_importance == "major_road"])
mean(table.glm$nbr_carcasses[table.glm$road_importance == "minor_road"])

nd <- data.frame(road_importance = c("major_road","minor_road"),
                 distance_amenity_km = mean(table.glm$distance_amenity_km),
                 distance_water_km = mean(table.glm$distance_water_km),
                 woodland = mean(table.glm$woodland))

cbind(nd, 
      Count = predict(zeroinfl_all_2, newdata = nd, type = "count"), 
      Zero = predict(zeroinfl_all_2, newdata = nd, type = "zero")) 

major_roads <- table.glm[table.glm$road_importance == "major_road",]
minor_raods <- table.glm[table.glm$road_importance == "minor_road",]

table.glm %>%
  filter(road_importance == "major_road") %>%
  count(nbr_carcasses) %>%
  mutate(prop = n/94) %>%
  ggplot(aes(x = nbr_carcasses, y = prop)) + 
  geom_col()


table.glm %>%
  filter(road_importance == "minor_road") %>%
  count(nbr_carcasses) %>%
  mutate(prop = n/474) %>%
  ggplot(aes(x = nbr_carcasses, y = prop)) + 
  geom_col()


table.glm %>%
  filter(nbr_carcasses != 0) %>%
  group_by(road_importance) %>%
  summarise(mean_nbr = mean(nbr_carcasses))

# ~~~  Observed VS predicted ---------------------------------------------------

estobsk <- function(k){
  sum(predict(zeroinfl_all_2, type = "prob")[, 1+k])
}

# Calculate the absolute frequency of segments with 0:4 carcasses
estobs0to5 <- sapply(X = 0:5,
                     FUN = estobsk)
# Calculate the absolute frequency of segments with >4 carcasses
estobs6 <- length(table.glm$nbr_carcasses) - sum(estobs0to5)

estobs <- as.data.frame(append(estobs0to5, estobs6))

# Extract the observed absolute frequency
observed <- table.glm %>%
  count(nbr_carcasses)
observed <- rbind(observed, c(5, 0))


est_obs <- cbind(observed, estobs)
colnames(est_obs) <- c("nbr_carcasses", "observed", "negbin_fit")


# PLOT ---

(est_obs_frequencies <- ggplot() +
    geom_col(data = est_obs, aes(x = nbr_carcasses, y = observed), 
             color = "black",
             fill = "white") +
    geom_point(data = est_obs, aes(x = nbr_carcasses, y = negbin_fit), shape = 16) +
    geom_line(data = est_obs, aes(x = nbr_carcasses, y = negbin_fit)) +
    theme_classic() +
    labs(x = "number of carcasses",
         y = "absolute frequency",
         title = "Zero inflated poisson with no covariates in the zero part")
)

ggsave("10_meetings/2021-06-16 Meeting with Sarah, Aimara & Morgane/zero_inflated_no_covariates_in_zero_model.png",
       width = 4, height = 3)



# ~ 5. Zero inflated poisson 3 -----------------

zeroinfl_all_3 <- zeroinfl(formula = nbr_carcasses ~ road_importance + distance_amenity_km
                           + distance_water_km + woodland | road_importance,
                           data = table.glm)
summary(zeroinfl_all_3)


# ~~~ Check overdispersion ----------------------------------------------------
resid <- residuals(zeroinfl_all_3, type = "pearson") 
N <- nrow(table.glm)
p <- length(coef(zeroinfl_all_3))
sum (resid^2) / (N-p) # This is inferior and very close to 1 => no overdispersion


# ~~~ Check overdispersion -----------------------------------------------------

df <- data.frame(resid = resid,
                 fitted = fitted(zeroinfl_all_3))
ggplot(df,aes(x = fitted, y = resid)) +
  geom_point()



# ~~~ Plot residuals -----------------------------------------------------------
major_roads <- table.glm %>%
  filter(road_importance == "major_road") 

mean(table.glm$nbr_carcasses[table.glm$road_importance == "major_road"])
mean(table.glm$nbr_carcasses[table.glm$road_importance == "minor_road"])

nd <- data.frame(road_importance = c("major_road","minor_road"),
                 distance_amenity_km = mean(table.glm$distance_amenity_km),
                 distance_water_km = mean(table.glm$distance_water_km),
                 woodland = mean(table.glm$woodland))

cbind(nd, 
      Count = predict(zeroinfl_all_3, newdata = nd, type = "count"), 
      Zero = predict(zeroinfl_all_3, newdata = nd, type = "zero")) 

major_roads <- table.glm[table.glm$road_importance == "major_road",]
minor_raods <- table.glm[table.glm$road_importance == "minor_road",]




# ~~~  Observed VS predicted ---------------------------------------------------

estobsk <- function(k){
  sum(predict(zeroinfl_all_3, type = "prob")[, 1+k])
}

# Calculate the absolute frequency of segments with 0:4 carcasses
estobs0to5 <- sapply(X = 0:5,
                     FUN = estobsk)
# Calculate the absolute frequency of segments with >4 carcasses
estobs6 <- length(table.glm$nbr_carcasses) - sum(estobs0to5)

estobs <- as.data.frame(append(estobs0to5, estobs6))

# Extract the observed absolute frequency
observed <- table.glm %>%
  count(nbr_carcasses)
observed <- rbind(observed, c(5, 0))


est_obs <- cbind(observed, estobs)
colnames(est_obs) <- c("nbr_carcasses", "observed", "negbin_fit")

est_obs <- est_obs %>%
  mutate(diff_prop = 100*(observed - negbin_fit)/observed)

# PLOT ---

(est_obs_frequencies <- ggplot() +
    geom_col(data = est_obs, aes(x = nbr_carcasses, y = observed), 
             color = "black",
             fill = "white") +
    geom_point(data = est_obs, aes(x = nbr_carcasses, y = negbin_fit), shape = 16) +
    geom_line(data = est_obs, aes(x = nbr_carcasses, y = negbin_fit)) +
    geom_text(data = est_obs , 
              aes(x = nbr_carcasses, y = observed, 
                  label = round(diff_prop, 3)), nudge_y = 10, size = 3) +
    
    theme_classic() +
    labs(x = "number of carcasses",
         y = "absolute frequency",
         title = "Zero inflated poisson with RoadType in the zero part")
)
  


ggsave("10_meetings/2021-06-16 Meeting with Sarah, Aimara & Morgane/zero_inflated_only_road_imp_in_zero_model.png",
       width = 4, height = 3)





# B. NEW DATASET: only roads with carcasses ------------------------------------

buffer.size <- 500 
buffer.size <- 1000

table.glm <- read_csv(paste0("06_processed_data/glm_data_2021-04/table.glm_10.", 
                             buffer.size, "m.csv")) %>%
  mutate(dataset = "complete")

temp <- read_csv(paste0("06_processed_data/glm_data_2021-04/table.glm_10.", 
                        buffer.size, "m.csv")) %>%
  mutate(road = gsub("\\_.*","", segment_ID)) %>%
  group_by(road) %>%
  summarize(total_per_road = sum(nbr_carcasses)) %>%
  filter(total_per_road > 0)

table.glm.reduced <- table.glm %>%
  dplyr::select(-dataset) %>%
  mutate(road = gsub("\\_.*","", segment_ID)) %>% 
  filter(road %in% temp$road) %>%
  mutate(dataset = "reduced")



# ~ 1. Plot --------------------------------------------------------------------

# Plot absolute frequency
comparison.df <- rbind(data.frame(nbr_carcasses = table.glm$nbr_carcasses, 
                                  dataset = table.glm$dataset),
                       data.frame(nbr_carcasses = table.glm.reduced$nbr_carcasses,
                                  dataset = table.glm.reduced$dataset))
ggplot(comparison.df, aes(x = nbr_carcasses)) +
  geom_bar() +
  theme_bw() +
  labs(x = "Number of carcasses",
       y = "Absolute frequency") +
  facet_wrap(~dataset)

ggsave("10_meetings/2021-06-16 Meeting with Sarah, Aimara & Morgane/absolute_frequency_comparison.png",
       width = 6, height = 4)


# Plot the roads
roads.ggplot.map <- read_csv("06_processed_data/glm_data_2021-04/roads.segmented.cropped.no.short.df.csv")

roads.ggplot.map.reduced <- roads.ggplot.map %>%
  filter(ID_road_seg %in% table.glm.reduced$segment_ID)

ggplot() +
  geom_line(data = roads.ggplot.map, aes( x = long, y = lat, group = ID_road_seg),
            size = 0.75) +
  geom_line(data = roads.ggplot.map.reduced, aes( x = long, y = lat, group = ID_road_seg),
            size = 0.75, color = "red") +
  theme_classic() + #theme_minimal() + 
  ylab("Latitude") +
  xlab("Longitude")


ggsave("10_meetings/2021-06-16 Meeting with Sarah, Aimara & Morgane/roads_reduced.png",
       width = 4, height = 5)


# With the carcasses

hy_carcasses <- read_delim("06_processed_data/carcasses/12_hy.carcasses.certainty.formatted.spatial_updated_04-2021.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE)

hy_carcasses_spatial <- hy_carcasses %>%
  filter(GPS_certainty_score > 0)

ggplot() +
  geom_line(data = roads.ggplot.map, aes(x = long, y = lat, group = ID_road_seg),
            size = 0.75) +
  geom_line(data = roads.ggplot.map.reduced, aes( x = long, y = lat, group = ID_road_seg),
            size = 0.75, color = "red") +
  geom_point(data = hy_carcasses_spatial,
             aes(x = long, y = lat), 
             color = "blue",
             size = 1.5) +
  theme_classic() + 
  ylab("Latitude") +
  xlab("Longitude")


ggsave("10_meetings/2021-06-16 Meeting with Sarah, Aimara & Morgane/roads_reduced_w_carcasses.png",
       width = 4, height = 5)



# ~ 2. Poisson -------------------------------------------------------------------

glm_poisson_reduced <- glm(formula = nbr_carcasses ~ road_importance + distance_amenity_km
                           + distance_water_km + woodland,
                           data = table.glm.reduced,
                           family = "poisson")
summary(glm_poisson_reduced)
plot(glm_poisson_reduced)

# ~~~ Check overdispersion ----------------------------------------------------
resid <- residuals(glm_poisson_reduced, type = "pearson") 
N <- nrow(table.glm.reduced)
p <- length(coef(glm_poisson_reduced))
sum (resid^2) / (N-p) # This is superior to 1 => overdispersion.


1 - pchisq(summary(glm_poisson_reduced)$deviance, summary(glm_poisson_reduced)$df.residual)




# ~ 3. Negbin --------------------------------------------------------------------

glm_nb_all_reduced <- MASS::glm.nb(formula = nbr_carcasses ~ road_importance + distance_amenity_km
                             + distance_water_km + woodland,
                             data = table.glm.reduced,
                             link = log)
summary(glm_nb_all_reduced)
plot(glm_nb_all_reduced)

# ~~~ Check overdispersion ----------------------------------------------------
resid <- residuals(glm_nb_all_reduced, type = "pearson") 
N <- nrow(table.glm.reduced)
p <- length(coef(glm_nb_all_reduced))
sum (resid^2) / (N-p) # This is very close 1 and inferior to 1 => no overdispersion.

1 - pchisq(summary(glm_nb_all_reduced)$deviance, summary(glm_nb_all_reduced)$df.residual)


# ~~~ Check the assumptions of the model using DHARMA --------------------------
simulationOutput <- DHARMa::simulateResiduals(fittedModel = glm_nb_all_reduced, n= 5000)
plot(simulationOutput)

DHARMa::testDispersion(glm_nb_all_reduced)
DHARMa::testZeroInflation(glm_nb_all_reduced)
DHARMa::testQuantiles(glm_nb_all_reduced) 
DHARMa::testUniformity(glm_nb_all_reduced)
DHARMa::testResiduals(glm_nb_all_reduced)
DHARMa::testOutliers(glm_nb_all_reduced)

DHARMa::plotResiduals(glm_nb_all_reduced, table.glm.reduced$road_importance) # No quantile deviation and no quadratic effect
DHARMa::plotResiduals(glm_nb_all_reduced, table.glm.reduced$distance_amenity_km)
DHARMa::plotResiduals(glm_nb_all_reduced, table.glm.reduced$distance_water_km) # No quantile deviation and no quadratic effect
DHARMa::plotResiduals(glm_nb_all_reduced, table.glm.reduced$woodland)






# C. NEW DATASET: only main road -----------------------------------------------

buffer.size <- 500 
buffer.size <- 1000

table.glm <- read_csv(paste0("06_processed_data/glm_data_2021-04/table.glm_10.", 
                             buffer.size, "m.csv")) %>%
  mutate(dataset = "complete")


table.glm.reduced <- table.glm %>%
  dplyr::select(-dataset) %>%
  mutate(road = gsub("\\_.*","", segment_ID)) %>% 
  filter(road == 68)

ggplot(table.glm.reduced, aes(x = nbr_carcasses)) +
  geom_histogram()

ggplot(table.glm.reduced, aes(x = long_midpoint, y = lat_midpoint, color = nbr_carcasses)) +
  geom_point() +
  theme_classic()


# ~ 1. Poisson -----------------------------------------------------------------

glm_poisson_reduced <- glm(formula = nbr_carcasses ~ distance_amenity_km
                           + distance_water_km + woodland,
                           data = table.glm.reduced,
                           family = "poisson")
summary(glm_poisson_reduced)
# plot(glm_poisson_reduced)


# ~~~ Check overdispersion ----------------------------------------------------
resid <- residuals(glm_poisson_reduced, type = "pearson") 
N <- nrow(table.glm.reduced)
p <- length(coef(glm_poisson_reduced))
sum (resid^2) / (N-p) # This is superior to 1 => overdispersion.


1 - pchisq(summary(glm_poisson_reduced)$deviance, summary(glm_poisson_reduced)$df.residual)

# ~~~  Observed VS predicted ---------------------------------------------------

estobsk_1 <- function(k){
  sum(dpois(k, fitted(glm_poisson_reduced)))
}
# Calculate the absolute frequency of segments with 0:4 carcasses
estobs0to5 <- sapply(X = 0:5,
                     FUN = estobsk_1)
# Calculate the absolute frequency of segments with >4 carcasses
estobs6 <- length(table.glm.reduced$nbr_carcasses) - sum(estobs0to5)

estobs <- as.data.frame(append(estobs0to5, estobs6))

# Extract the observed absolute frequency
observed <- table.glm.reduced %>%
  count(nbr_carcasses)
observed <- rbind(observed, c(5, 0))


est_obs <- cbind(observed, estobs)
colnames(est_obs) <- c("nbr_carcasses", "observed", "negbin_fit")


# PLOT ---

(est_obs_frequencies <- ggplot() +
    geom_col(data = est_obs, aes(x = nbr_carcasses, y = observed), 
             color = "black",
             fill = "white") +
    geom_point(data = est_obs, aes(x = nbr_carcasses, y = negbin_fit), shape = 16) +
    geom_line(data = est_obs, aes(x = nbr_carcasses, y = negbin_fit)) +
    theme_classic() +
    xlab("Number of carcasses per segment") +
    ylab("Numer of road segments")
)



# ~ 2. Negbin --------------------------------------------------------------------

glm_nb_all_reduced <- glm.nb(formula = nbr_carcasses ~ distance_amenity_km
                             + distance_water_km + woodland,
                             data = table.glm.reduced,
                             link = log)
summary(glm_nb_all_reduced)
plot(glm_nb_all_reduced)

# ~~~ Check overdispersion ----------------------------------------------------
resid <- residuals(glm_nb_all_reduced, type = "pearson") 
N <- nrow(table.glm.reduced)
p <- length(coef(glm_nb_all_reduced))
sum (resid^2) / (N-p) # This is very close 1 and inferior to 1 => no overdispersion.

1 - pchisq(summary(glm_nb_all_reduced)$deviance, summary(glm_nb_all_reduced)$df.residual)


# ~~~ Check the assumptions of the model using DHARMA --------------------------
simulationOutput <- DHARMa::simulateResiduals(fittedModel = glm_nb_all_reduced, n= 5000)
plot(simulationOutput)

DHARMa::testDispersion(glm_nb_all_reduced)
DHARMa::testZeroInflation(glm_nb_all_reduced)
DHARMa::testQuantiles(glm_nb_all_reduced) 
DHARMa::testUniformity(glm_nb_all_reduced)
DHARMa::testResiduals(glm_nb_all_reduced)
DHARMa::testOutliers(glm_nb_all_reduced)

DHARMa::plotResiduals(glm_nb_all_reduced, table.glm.reduced$road_importance) # No quantile deviation and no quadratic effect
DHARMa::plotResiduals(glm_nb_all_reduced, table.glm.reduced$distance_amenity_km)
DHARMa::plotResiduals(glm_nb_all_reduced, table.glm.reduced$distance_water_km) # No quantile deviation and no quadratic effect
DHARMa::plotResiduals(glm_nb_all_reduced, table.glm.reduced$woodland)



# ~~~  Observed VS predicted ---------------------------------------------------

estobsk_1 <- function(k){
  sum(dnbinom(k, mu = fitted(glm_nb_all_reduced),
              size = glm_nb_all_reduced$theta))
}
# Calculate the absolute frequency of segments with 0:4 carcasses
estobs0to5 <- sapply(X = 0:5,
                     FUN = estobsk_1)
# Calculate the absolute frequency of segments with >4 carcasses
estobs6 <- length(table.glm.reduced$nbr_carcasses) - sum(estobs0to5)

estobs <- as.data.frame(append(estobs0to5, estobs6))

# Extract the observed absolute frequency
observed <- table.glm.reduced %>%
  count(nbr_carcasses)
observed <- rbind(observed, c(5, 0))


est_obs <- cbind(observed, estobs)
colnames(est_obs) <- c("nbr_carcasses", "observed", "negbin_fit")


# PLOT ---

(est_obs_frequencies <- ggplot() +
    geom_col(data = est_obs, aes(x = nbr_carcasses, y = observed), 
             color = "black",
             fill = "white") +
    geom_point(data = est_obs, aes(x = nbr_carcasses, y = negbin_fit), shape = 16) +
    geom_line(data = est_obs, aes(x = nbr_carcasses, y = negbin_fit)) +
    theme_classic() +
    xlab("Number of carcasses per segment") +
    ylab("Numer of road segments")
)





# D. NEW DATASET: only segments with carcasses ---------------------------------

buffer.size <- 500 
buffer.size <- 1000

table.glm <- read_csv(paste0("06_processed_data/glm_data_2021-04/table.glm_10.", 
                             buffer.size, "m.csv")) %>%
  mutate(dataset = "complete")

table.glm.reduced <- table.glm %>%
  filter(nbr_carcasses > 0)


# ~ 1. Plot --------------------------------------------------------------------

ggplot(table.glm.reduced, aes(x = long_midpoint, y = lat_midpoint, color = nbr_carcasses)) +
  geom_point() +
  theme_classic()


# Plot non-zero road segments with waterways 

rivers <- rgdal::readOGR(dsn  = "06_processed_data/water",
                         layer = "rivers_for_analyses_2021-05-25")

discarded_segments <- table.glm %>%
  filter(nbr_carcasses == 0)

rivers.df <- tidy(rivers)

ggplot() +
  geom_path(data = rivers.df, aes(x = long, y = lat, group = id)) +
  geom_point(data = discarded_segments, aes(x = long_midpoint, y = lat_midpoint), color = "darkred", size = 1) +
  geom_point(data = table.glm.reduced, aes(x = long_midpoint, y = lat_midpoint, color = nbr_carcasses)) +
  theme_classic()



# ~ 2. Truncated Poisson -----------------------------------------------------------------

glm_trunc_poisson <- vglm(formula = nbr_carcasses ~ road_importance + distance_amenity_km
                          + distance_water_km + woodland, 
                          family = pospoisson(),
                          data = table.glm.reduced)
summary(glm_trunc_poisson)


# ~~~ Check overdispersion ----------------------------------------------------
resid <- residuals(glm_trunc_poisson, type = "pearson") 
N <- nrow(table.glm.reduced)
p <- length(coef(glm_trunc_poisson))
sum (resid^2) / (N-p) # This is superior to 1 => overdispersion.



# ~~~  Observed VS predicted ---------------------------------------------------

estobsk_1 <- function(k){
  sum(dpois(k, fitted(glm_poisson_reduced)))
}
# Calculate the absolute frequency of segments with 0:4 carcasses
estobs0to5 <- sapply(X = 0:5,
                     FUN = estobsk_1)
# Calculate the absolute frequency of segments with >4 carcasses
estobs6 <- length(table.glm.reduced$nbr_carcasses) - sum(estobs0to5)

estobs <- as.data.frame(append(estobs0to5, estobs6))

# Extract the observed absolute frequency
observed <- table.glm.reduced %>%
  count(nbr_carcasses)
observed <- rbind(observed, c(5, 0))


est_obs <- cbind(observed, estobs)
colnames(est_obs) <- c("nbr_carcasses", "observed", "negbin_fit")


# PLOT ---

(est_obs_frequencies <- ggplot() +
    geom_col(data = est_obs, aes(x = nbr_carcasses, y = observed), 
             color = "black",
             fill = "white") +
    geom_point(data = est_obs, aes(x = nbr_carcasses, y = negbin_fit), shape = 16) +
    geom_line(data = est_obs, aes(x = nbr_carcasses, y = negbin_fit)) +
    theme_classic() +
    xlab("Number of carcasses per segment") +
    ylab("Numer of road segments")
)



# ~ 2. Negbin --------------------------------------------------------------------

glm_nb_all_reduced <- glm.nb(formula = nbr_carcasses ~ distance_amenity_km
                             + distance_water_km + woodland,
                             data = table.glm.reduced,
                             link = log)
summary(glm_nb_all_reduced)
plot(glm_nb_all_reduced)

# ~~~ Check overdispersion ----------------------------------------------------
resid <- residuals(glm_nb_all_reduced, type = "pearson") 
N <- nrow(table.glm.reduced)
p <- length(coef(glm_nb_all_reduced))
sum (resid^2) / (N-p) # This is very close 1 and inferior to 1 => no overdispersion.

1 - pchisq(summary(glm_nb_all_reduced)$deviance, summary(glm_nb_all_reduced)$df.residual)


# ~~~ Check the assumptions of the model using DHARMA --------------------------
simulationOutput <- DHARMa::simulateResiduals(fittedModel = glm_nb_all_reduced, n= 5000)
plot(simulationOutput)

DHARMa::testDispersion(glm_nb_all_reduced)
DHARMa::testZeroInflation(glm_nb_all_reduced)
DHARMa::testQuantiles(glm_nb_all_reduced) 
DHARMa::testUniformity(glm_nb_all_reduced)
DHARMa::testResiduals(glm_nb_all_reduced)
DHARMa::testOutliers(glm_nb_all_reduced)

DHARMa::plotResiduals(glm_nb_all_reduced, table.glm.reduced$road_importance) # No quantile deviation and no quadratic effect
DHARMa::plotResiduals(glm_nb_all_reduced, table.glm.reduced$distance_amenity_km)
DHARMa::plotResiduals(glm_nb_all_reduced, table.glm.reduced$distance_water_km) # No quantile deviation and no quadratic effect
DHARMa::plotResiduals(glm_nb_all_reduced, table.glm.reduced$woodland)



# ~~~  Observed VS predicted ---------------------------------------------------

estobsk_1 <- function(k){
  sum(dnbinom(k, mu = fitted(glm_nb_all_reduced),
              size = glm_nb_all_reduced$theta))
}
# Calculate the absolute frequency of segments with 0:4 carcasses
estobs0to5 <- sapply(X = 0:5,
                     FUN = estobsk_1)
# Calculate the absolute frequency of segments with >4 carcasses
estobs6 <- length(table.glm.reduced$nbr_carcasses) - sum(estobs0to5)

estobs <- as.data.frame(append(estobs0to5, estobs6))

# Extract the observed absolute frequency
observed <- table.glm.reduced %>%
  count(nbr_carcasses)
observed <- rbind(observed, c(5, 0))


est_obs <- cbind(observed, estobs)
colnames(est_obs) <- c("nbr_carcasses", "observed", "negbin_fit")


# PLOT ---

(est_obs_frequencies <- ggplot() +
    geom_col(data = est_obs, aes(x = nbr_carcasses, y = observed), 
             color = "black",
             fill = "white") +
    geom_point(data = est_obs, aes(x = nbr_carcasses, y = negbin_fit), shape = 16) +
    geom_line(data = est_obs, aes(x = nbr_carcasses, y = negbin_fit)) +
    theme_classic() +
    xlab("Number of carcasses per segment") +
    ylab("Numer of road segments")
)






# E. NEW DATASET: without outlier ----------------------------------------------

buffer.size <- 500 
buffer.size <- 1000

table.glm.no.outlier <- read_csv(paste0("06_processed_data/glm_data_2021-04/table.glm_10.", 
                             buffer.size, "m.csv")) %>%
  filter(nbr_carcasses < 5)


# ~ 1. Plot --------------------------------------------------------------------

ggplot(table.glm, aes(x = long_midpoint, y = lat_midpoint, color = nbr_carcasses)) +
  geom_point() +
  theme_classic()

# ~ 2. Negbin ------------------------------------------------------------------

glm_nb_no_outlier <- glm.nb(formula = nbr_carcasses ~ road_importance + distance_amenity_km
                             + distance_water_km + woodland,
                             data = table.glm.no.outlier,
                             link = log)
summary(glm_nb_no_outlier)
plot(glm_nb_no_outlier)

# ~~~ Check overdispersion ----------------------------------------------------
resid <- residuals(glm_nb_no_outlier, type = "pearson") 
N <- nrow(table.glm.no.outlier)
p <- length(coef(glm_nb_no_outlier))
sum (resid^2) / (N-p) # This is very close 1 and inferior to 1 => no overdispersion.

1 - pchisq(summary(glm_nb_no_outlier)$deviance, summary(glm_nb_no_outlier)$df.residual)


# ~~~ Check the assumptions of the model using DHARMA --------------------------
simulationOutput <- DHARMa::simulateResiduals(fittedModel = glm_nb_no_outlier, n= 5000)
plot(simulationOutput)

DHARMa::testDispersion(glm_nb_no_outlier)
DHARMa::testZeroInflation(glm_nb_no_outlier)
DHARMa::testQuantiles(glm_nb_no_outlier) 
DHARMa::testUniformity(glm_nb_no_outlier)
DHARMa::testResiduals(glm_nb_no_outlier)
DHARMa::testOutliers(glm_nb_no_outlier)

DHARMa::plotResiduals(glm_nb_no_outlier, table.glm.no.outlier$road_importance) # No quantile deviation and no quadratic effect
DHARMa::plotResiduals(glm_nb_no_outlier, table.glm.no.outlier$distance_amenity_km)
DHARMa::plotResiduals(glm_nb_no_outlier, table.glm.no.outlier$distance_water_km) # No quantile deviation and no quadratic effect
DHARMa::plotResiduals(glm_nb_no_outlier, table.glm.no.outlier$woodland)



# ~~~  Observed VS predicted ---------------------------------------------------

estobsk_1 <- function(k){
  sum(dnbinom(k, mu = fitted(glm_nb_no_outlier),
              size = glm_nb_no_outlier$theta))
}
# Calculate the absolute frequency of segments with 0:4 carcasses
estobs0to5 <- sapply(X = 0:5,
                     FUN = estobsk_1)
# Calculate the absolute frequency of segments with >4 carcasses
estobs6 <- length(table.glm.no.outlier$nbr_carcasses) - sum(estobs0to5)

estobs <- as.data.frame(append(estobs0to5, estobs6))

# Extract the observed absolute frequency
observed <- table.glm.no.outlier %>%
  count(nbr_carcasses)
observed <- rbind(observed, c(5, 0))


est_obs <- cbind(observed, estobs)
colnames(est_obs) <- c("nbr_carcasses", "observed", "negbin_fit")


# PLOT ---

(est_obs_frequencies <- ggplot() +
    geom_col(data = est_obs, aes(x = nbr_carcasses, y = observed), 
             color = "black",
             fill = "white") +
    geom_point(data = est_obs, aes(x = nbr_carcasses, y = negbin_fit), shape = 16) +
    geom_line(data = est_obs, aes(x = nbr_carcasses, y = negbin_fit)) +
    theme_classic() +
    xlab("Number of carcasses per segment") +
    ylab("Numer of road segments")
)



# F. NEW DATASET: in a rectangle Ikoma-Naabi -----------------------------------

# Chose the buffer size  before loading the corresponding dataframe containing
# all the data required to run the model
buffer.size <- 500 
buffer.size <- 1000

table.glm <- read_csv(paste0("06_processed_data/glm_data_2021-04/table.glm_10.", 
                             buffer.size, "m.csv"))


square.ikoma.naabi <- extent(34.7252727, 34.9974738, 
                             -2.8320276, -2.1890186)

table.glm.reduced <- table.glm %>% 
  filter(lat_midpoint >= -2.8320276,
         lat_midpoint <= -2.1890186,
         long_midpoint >= 34.7252727,
         long_midpoint <= 34.9974738)

sum(table.glm$nbr_carcasses)
sum(table.glm.reduced$nbr_carcasses)
# ~ 1. Plot --------------------------------------------------------------------

# Plot absolute frequency

table.glm.smaller.circle <- read_csv(paste0("06_processed_data/glm_data_2021-04/47.5km/table.glm_10.", 
                                            buffer.size, "m.csv"))

comparison.df <- rbind(data.frame(nbr_carcasses = table.glm$nbr_carcasses, 
                                  dataset = "65km circle (n = 84 carcasses)"),
                       data.frame(nbr_carcasses = table.glm.smaller.circle$nbr_carcasses, 
                                  dataset = "47.5km circle (n = 81)"),
                       data.frame(nbr_carcasses = table.glm.reduced$nbr_carcasses,
                                  dataset = "Ikoma-Naabi rectangle (n = 79)")) %>%
  mutate(dataset = factor(dataset, 
                          levels = c("65km circle (n = 84 carcasses)",  
                                     "47.5km circle (n = 81)", 
                                     "Ikoma-Naabi rectangle (n = 79)")))

ggplot(comparison.df, aes(x = nbr_carcasses)) +
  geom_bar() +
  theme_bw() +
  labs(x = "Number of carcasses",
       y = "Absolute frequency") +
  facet_wrap(~dataset)

ggsave("11_manuscript/V3 Corrections/Mail à Sarah/number_segments.png")

ggplot(table.glm.reduced, aes(x = long_midpoint, y = lat_midpoint, color = nbr_carcasses)) +
  geom_point() +
  theme_classic()



# ~ 2. Negative binomial -------------------------------------------------------

glm_nb_all <- glm.nb(formula = nbr_carcasses ~ road_importance + distance_amenity_km
                     + distance_water_km + woodland,
                     data = table.glm.reduced,
                     link = log)
summary(glm_nb_all)
AICcmodavg::AICc(glm_nb_all, return.K = FALSE, second.ord = TRUE,
                 nobs = NULL, c.hat = 1)
plot(glm_nb_all)

# Check overdispersion
resid <- residuals(glm_nb_all, type = "pearson") 
N <- nrow(table.glm.reduced)
p <- length(coef(glm_nb_all))
sum (resid^2) / (N-p) # This is superior to 1 => overdispersion.

1 - pchisq(summary(glm_nb_all)$deviance, summary(glm_nb_all)$df.residual)

DHARMa::testDispersion(glm_nb_all)
simulationOutput <- DHARMa::simulateResiduals(fittedModel = glm_nb_all, n= 5000)
plot(simulationOutput)

# ~~~  Observed VS predicted ---------------------------------------------------

estobsk_1 <- function(k){
  sum(dnbinom(k, mu = fitted(glm_nb_all),
              size = glm_nb_all$theta))
}

# Calculate the absolute frequency of segments with 0:4 carcasses
estobs0to5 <- sapply(X = 0:5,
                     FUN = estobsk_1)
# Calculate the absolute frequency of segments with >4 carcasses
estobs6 <- length(table.glm.reduced$nbr_carcasses) - sum(estobs0to5)

estobs <- as.data.frame(append(estobs0to5, estobs6))

# Extract the observed absolute frequency
observed <- table.glm.reduced %>%
  count(nbr_carcasses)
observed <- rbind(observed, c(5, 0))


est_obs <- cbind(observed, estobs)
colnames(est_obs) <- c("nbr_carcasses", "observed", "negbin_fit")


# PLOT ---

(est_obs_frequencies <- ggplot() +
    geom_col(data = est_obs, aes(x = nbr_carcasses, y = observed), 
             color = "black",
             fill = "white") +
    geom_point(data = est_obs, aes(x = nbr_carcasses, y = negbin_fit), shape = 16) +
    geom_line(data = est_obs, aes(x = nbr_carcasses, y = negbin_fit)) +
    theme_classic() +
    xlab("Number of carcasses per segment") +
    ylab("Numer of road segments")
)


# ~ 3. Zero inflated poisson -----------------

zeroinfl_all <- zeroinfl(formula = nbr_carcasses ~ road_importance + distance_amenity_km
                         + distance_water_km + woodland,
                         data = table.glm.reduced)
summary(zeroinfl_all)

# ~~~ Check overdispersion ----------------------------------------------------
resid <- residuals(zeroinfl_all, type = "pearson") 
N <- nrow(table.glm.reduced)
p <- length(coef(zeroinfl_all))
sum (resid^2) / (N-p) # This is superior to 1 => overdispersion.

# ~~~  Observed VS predicted ---------------------------------------------------

estobsk <- function(k){
  sum(predict(zeroinfl_all, type = "prob")[, 1+k])
}

# Calculate the absolute frequency of segments with 0:4 carcasses
estobs0to5 <- sapply(X = 0:5,
                     FUN = estobsk)
# Calculate the absolute frequency of segments with >4 carcasses
estobs6 <- length(table.glm.reduced$nbr_carcasses) - sum(estobs0to5)

estobs <- as.data.frame(append(estobs0to5, estobs6))

# Extract the observed absolute frequency
observed <- table.glm.reduced %>%
  count(nbr_carcasses)
observed <- rbind(observed, c(5, 0))


est_obs <- cbind(observed, estobs)
colnames(est_obs) <- c("nbr_carcasses", "observed", "negbin_fit")


# PLOT ---

(est_obs_frequencies <- ggplot() +
    geom_col(data = est_obs, aes(x = nbr_carcasses, y = observed), 
             color = "black",
             fill = "white") +
    geom_point(data = est_obs, aes(x = nbr_carcasses, y = negbin_fit), shape = 16) +
    geom_line(data = est_obs, aes(x = nbr_carcasses, y = negbin_fit)) +
    theme_classic() +
    labs(x = "number of carcasses",
         y = "absolute frequency",
         title = "Zero inflated poisson with all covariates in the zero part")
)
