#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                              #
#                   Plot correlations between covariates                       #        
#                                                                              #
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(tidyverse)
library(ggpubr)
Sys.setenv(LANG = "en")

# A. Plot response VS predictors ===============================================

# ~ 1. Load the data and run the model -----------------------------------------
buffer.size <- 500
buffer.size <- 1000

table.glm <- read_csv(paste0("06_processed_data/glm_data_2021-04/table.glm_10.", 
                             buffer.size, "m.csv"))


# ~ 2. Plots -----------------------------------------

# road_importance VS nbr carcasses
ggplot(table.glm, aes(x = road_importance, y = nbr_carcasses)) +
  geom_boxplot() +
  theme_bw()

# distance_amenity 
ggplot(table.glm, aes(x = distance_amenity_km)) +
  geom_histogram() +
  theme_bw()

# distance_amenity VS nbr carcasses
ggplot(table.glm, aes(x = distance_amenity_km, y = nbr_carcasses)) +
  geom_point() +
  theme_bw()

# distance_water
ggplot(table.glm, aes(x = distance_water_km)) +
  geom_histogram() +
  theme_bw()

# distance_water VS nbr carcasses
ggplot(table.glm, aes(x = distance_water_km, y = nbr_carcasses)) +
  geom_point() +
  theme_bw()

ggsave("10_meetings/2021-XX-XX Meeting with Sarah/plot_water_VS_nbr_carcasses_outlier.png",
       width = 3, height = 3)

# woodland VS nbr carcasses
ggplot(table.glm, aes(x = woodland, y = nbr_carcasses)) +
  geom_point() +
  theme_bw()

# dist_from_center VS nbr carcasses
ggplot(table.glm, aes(x = dist_from_center, y = nbr_carcasses)) +
  geom_point() +
  theme_bw()


# B. Plot correlations =========================================================

# ~ 1. Load the data and run the model -----------------------------------------
buffer.size <- 500
buffer.size <- 1000

table.glm <- read_csv(paste0("06_processed_data/glm_data_2021-04/table.glm_10.", 
                             buffer.size, "m.csv"))

# ~~~ Fit the model -----------------
glm_nb_all <- glm.nb(formula = nbr_carcasses ~ road_importance + distance_amenity_km
                     + distance_water_km + woodland,
                     data = table.glm,
                     link = log)
summary(glm_nb_all)





# ~ 2. Plot and save correlations ------------------------------------------------

# Road_importance VS distance_amenity

ggplot(table.glm, aes(x = road_importance, y = distance_amenity_km)) +
  geom_violin() +
  geom_boxplot(width = 0.2) +
  stat_compare_means(method="wilcox.test") +
  theme_bw() +
  labs(x = "Road type",
       y = "Distance to closest amenity")

ggsave("07_intermediate_results/2021-04/correlations_covariates/road_imp_VS_dist_amenity.png",
       width = 3, height = 3)


# Road_importance VS distance_water

ggplot(table.glm, aes(x = road_importance, y = distance_water_km)) +
  geom_violin() +
  geom_boxplot(width = 0.2) +
  stat_compare_means(method="wilcox.test") +
  theme_bw() +
  labs(x = "Road type",
       y = "Distance to closest water source")

ggsave("07_intermediate_results/2021-04/correlations_covariates/road_imp_VS_dist_water.png",
       width = 3, height = 3)

ggplot(table.glm, aes(x = long_midpoint, y = lat_midpoint, color = distance_water_km)) +
  geom_point()


# Road_importance VS woodland cover
  
ggplot(table.glm, aes(x = road_importance, y = woodland)) +
  geom_violin() +
  geom_boxplot(width = 0.2) +
  stat_compare_means(method="wilcox.test") +
  theme_bw() +
  labs(x = "Road type",
       y = paste0("Woodland cover (", buffer.size, "m)"))

ggsave(paste0("07_intermediate_results/2021-04/correlations_covariates/road_imp_VS_woodland_", 
              buffer.size, "m.png"),
       width = 4, height = 3)

ggplot(table.glm, aes(x = long_midpoint, y = lat_midpoint, color = woodland)) +
  geom_point()


# Distance_amenity VS distance_water
m <- lm(table.glm$distance_water_km ~ table.glm$distance_amenity_km)


ggplot(table.glm, aes(x = distance_amenity_km, y = distance_water_km)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  labs(x = "Distance to closest amenity",
       y = "Distance to closest water source") +
  # geom_text(data = table.glm, x = 0, y = 3, label = eq(table.glm$woodland, 
  #                                                      table.glm$distance_amenity_km), parse = TRUE) +
  geom_text(data = table.glm, x = 1, y = 3.5, label = paste0("y = ", round(coef(m)[1], 3), " + ", 
                                                             round(coef(m)[2], 3), "x, r2 = ",
                                                             round(summary(m)$r.squared, 3)))


ggsave("07_intermediate_results/2021-04/correlations_covariates/distance_amenity_VS_distance_water.png",
       width = 4, height = 3)


# Distance_amenity VS woodland cover

m <- lm(table.glm$woodland ~ table.glm$distance_amenity_km)


ggplot(table.glm, aes(x = distance_amenity_km, y = woodland)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  labs(x = "Distance to closest amenity",
       y = paste0("Woodland cover (", buffer.size, "m)")) +
  # geom_text(data = table.glm, x = 0, y = 3, label = eq(table.glm$woodland, 
  #                                                      table.glm$distance_amenity_km), parse = TRUE) +
  geom_text(data = table.glm, x = 1, y = 3.5, label = paste0("y = ", round(coef(m)[1], 3), " + ", 
                                                           round(coef(m)[2], 3), "x, r2 = ",
                                                           round(summary(m)$r.squared, 3)))


ggsave(paste0("07_intermediate_results/2021-04/correlations_covariates/distance_amenity_VS_woodland_", 
              buffer.size, "m.png"),
       width = 4, height = 3)


# Distance_water VS woodland cover

m <- lm(table.glm$woodland ~ table.glm$distance_water_km)


ggplot(table.glm, aes(x = distance_water_km, y = woodland)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  labs(x = "Distance to closest water source",
       y = paste0("Woodland cover (", buffer.size, "m)")) +
  # geom_text(data = table.glm, x = 0, y = 3, label = eq(table.glm$woodland, 
  #                                                      table.glm$distance_amenity_km), parse = TRUE) +
  geom_text(data = table.glm, x = 1, y = 3.5, label = paste0("y = ", round(coef(m)[1], 3), " + ", 
                                                             round(coef(m)[2], 3), "x, r2 = ",
                                                             round(summary(m)$r.squared, 3)))

ggsave(paste0("07_intermediate_results/2021-04/correlations_covariates/distance_water_VS_woodland_", 
              buffer.size, "m.png"),
       width = 4, height = 3)


