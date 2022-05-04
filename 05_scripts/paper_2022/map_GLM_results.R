#==============================================================================#
#                                                                              #
#               Maps observed and predicted number of carcasses                #        
#                                                                              #
#==============================================================================#

library(tidyverse)
library(ggplot2)
Sys.setenv(LANG = "en")

table.glm <- read_csv("06_processed_data/glm_data_2021-10_high_certainty/table.glm_9.500m.csv") %>%
  mutate(road_importance = factor(road_importance, levels = c("minor_road", "major_road")))

glm_nb_all<- MASS::glm.nb(formula = nbr_carcasses ~ road_importance + distance_amenity_km
                          + distance_water_km + woodland,
                          data = table.glm,
                          link = log)

# create new data
real_road_segments <- data.frame(road_importance = table.glm$road_importance,
                                 distance_amenity_km = table.glm$distance_amenity_km,
                                 distance_water_km = table.glm$distance_water_km,
                                 woodland = table.glm$woodland)

## add the fitted values by predicting from the model for the new data
real_road_segments <- add_column(real_road_segments, 
                                 fit = predict(glm_nb_all,
                                               newdata = real_road_segments,
                                               se = TRUE,
                                               type = "response")[["fit"]])

## add fit and se.fit on the **link** scale
real_road_segments <- bind_cols(real_road_segments, 
                                setNames(as_tibble(predict(glm_nb_all,
                                                           newdata = real_road_segments, 
                                                           se.fit = TRUE)[1:2]),
                                         c('fit_link','se_link')))
## grab the inverse link function 
ilink <- glm_nb_all[["family"]][["linkinv"]]

## create the interval and backtransform
real_road_segments <- mutate(segment_ID = table.glm$segment_ID,
                             nbr_carcasses = table.glm$nbr_carcasses,
                             real_road_segments,
                             right_upr = ilink(fit_link + (2 * se_link)),
                             right_lwr = ilink(fit_link - (2 * se_link)),
                             long_mid = table.glm$long_midpoint,
                             lat_mid = table.glm$lat_midpoint)


roads.segmented.cropped.no.short.df <- read_csv("06_processed_data/glm_data_2022-04/roads.segmented.cropped.no.short.df.csv") %>%
  dplyr::select(segment_ID = ID_road_seg,
                long, lat)

data_plot <- roads.segmented.cropped.no.short.df %>%
  left_join(x = .,
            y = real_road_segments,
            by = "segment_ID")

ggplot(data_plot, aes(x = long, y = lat, color = fit, group = segment_ID)) +
  geom_line(size = 1) +
  theme_classic() +
  viridis::scale_color_viridis() +
  labs(x = "longitue", y = "latitude", 
       color = "predicted number\nof carcasses\nper segment")

ggsave("11_manuscript/V4 Figures/figure 3 (map GLM results).png",
       width = 7, height = 7)


ggplot(data_plot, aes(x = long, y = lat, color = nbr_carcasses, group = segment_ID)) +
  geom_line(size = 1) +
  theme_classic() +
  viridis::scale_color_viridis() +
  labs(x = "longitue", y = "latitude", 
       color = "observed number\nof carcasses\nper segment")

ggsave("11_manuscript/V4 Figures/figure 3 (map observed carcasses).png",
       width = 7, height = 7)
