#==============================================================================#
#                                                                              #
#                             Figures for the paper                            #        
#                                                                              #
#==============================================================================#

library(tidyverse)
library(lubridate)
library(ggplot2)
library(patchwork)
library(cowplot)
Sys.setenv(LANG = "en")


# ~ 1. Figure 1: Map of the study area -----------------------------------------

# In a separate script because very long. See script 



# ~ 2. Figure 2: GLM results ---------------------------------------------------
buffer.size <- 500
table.glm <- read_csv("06_processed_data/glm_data_2022-04/table.glm_9.500m.csv") %>%
  mutate(road_importance = factor(road_importance, levels = c("minor_road", "major_road")))

glm_nb_all<- MASS::glm.nb(formula = nbr_carcasses ~ road_importance + distance_amenity_km
                          + distance_water_km + woodland,
                          data = table.glm,
                          link = log)
summary(glm_nb_all)


# ~~~ Road type ----------------------------------------------------------------

# create new data
newdata_road_importance <- expand.grid(road_importance = c("major_road", "minor_road"),
                                       distance_amenity_km = mean(table.glm$distance_amenity_km),
                                       distance_water_km = mean(table.glm$distance_water_km),
                                       woodland = mean(table.glm$woodland))

## add the fitted values by predicting from the model for the new data
newdata_road_importance <- add_column(newdata_road_importance, fit = predict(glm_nb_all,
                                                                             newdata = newdata_road_importance,
                                                                             se = TRUE,
                                                                             type = "response")[["fit"]])

## add fit and se.fit on the **link** scale
newdata_road_importance <- bind_cols(newdata_road_importance, setNames(as_tibble(predict(glm_nb_all, 
                                                                                         newdata = newdata_road_importance, 
                                                                                         se.fit = TRUE)[1:2]),
                                                                       c('fit_link','se_link')))
## grab the inverse link function 
ilink <- glm_nb_all[["family"]][["linkinv"]]

## create the interval and backtransform
newdata_road_importance <- mutate(newdata_road_importance,
                                  fit_resp  = ilink(fit_link),
                                  right_upr = ilink(fit_link + (2 * se_link)),
                                  right_lwr = ilink(fit_link - (2 * se_link)))

## plot
(plot.road.imp <- ggplot(newdata_road_importance, aes(x = road_importance, y = fit)) +
    geom_col(fill = "white",
             color = "black") +
    geom_errorbar(aes(x = newdata_road_importance[,1],
                      ymin = newdata_road_importance[,10], 
                      ymax = newdata_road_importance[,9], 
                      width=.2)) +
    theme_classic() +
    scale_x_discrete(limits = c("major_road", "minor_road"),
                     labels = c("main \nroad", "track")) +
    labs(x = 'Road type', 
         y = 'Estimated mean number \nof carcasses per segment')
)

# ~~~ Amenity ------------------------------------------------------------------

# Create some data
newdata_distance_amenity <- expand.grid(distance_amenity_km = seq(from = range(table.glm$distance_amenity_km)[1],
                                                                  to = range(table.glm$distance_amenity_km)[2],
                                                                  by = 0.05), 
                                        road_importance = c("major_road", "minor_road"),
                                        woodland = mean(table.glm$woodland),
                                        distance_water_km = mean(table.glm$distance_water_km))

## add the fitted values by predicting from the model for the new data
newdata_distance_amenity <- add_column(newdata_distance_amenity, fit = predict(glm_nb_all,
                                                                               newdata = newdata_distance_amenity,
                                                                               se = TRUE,
                                                                               type = "response")[["fit"]])
## add fit and se.fit on the **link** scale
newdata_distance_amenity <- bind_cols(newdata_distance_amenity, setNames(as_tibble(predict(glm_nb_all, 
                                                                                           newdata = newdata_distance_amenity, 
                                                                                           se.fit = TRUE)[1:2]),
                                                                         c('fit_link','se_link')))
## grab the inverse link function 
ilink <- glm_nb_all[["family"]][["linkinv"]]

## create the interval and backtransform
newdata_distance_amenity <- mutate(newdata_distance_amenity,
                                   fit_resp  = ilink(fit_link),
                                   right_upr = ilink(fit_link + (2 * se_link)),
                                   right_lwr = ilink(fit_link - (2 * se_link)))


# plot
N1 <- nrow(newdata_distance_amenity)

### To rescale the predictor
mean_sd <- read_csv(paste0("06_processed_data/glm_data_2021-10_high_certainty/predictor_mean_sd_", buffer.size, "m.csv"))
x <- which(mean_sd$variable == "distance_amenity_km")
y <- newdata_distance_amenity$distance_amenity_km * mean_sd$sd[x] + mean_sd$mean[x]
range(y)
(plot.dist.amenity <- ggplot() +
    geom_line(data = newdata_distance_amenity ,
              aes(x = newdata_distance_amenity$distance_amenity_km * mean_sd$sd[x] + mean_sd$mean[x],
                  y = newdata_distance_amenity$fit, #[, 5],
                  linetype = road_importance)) +
    geom_ribbon(aes(x = newdata_distance_amenity$distance_amenity_km[1:(N1/2)] * mean_sd$sd[x] + mean_sd$mean[x],
                    ymin = newdata_distance_amenity$right_lwr [1:(N1/2)],
                    ymax = newdata_distance_amenity$right_upr[1:(N1/2)]),
                alpha = 0.2) +
    geom_ribbon(aes(x = newdata_distance_amenity$distance_amenity_km[(N1/2 + 1):N1] * mean_sd$sd[x] + mean_sd$mean[x],
                    ymin = newdata_distance_amenity$right_lwr[(N1/2 + 1):N1],
                    ymax = newdata_distance_amenity$right_upr[(N1/2 + 1):N1]),
                alpha = 0.2) +
    theme_classic() +
    theme(legend.position = c(0.70, 0.8),
          legend.title = element_blank(),
          legend.background = element_rect(fill = "white",
                                           size = 0.5, linetype = "solid", 
                                           colour = "black")) +
    scale_linetype_discrete(labels = c("main road", "track")) +
    labs(x = 'Distance from the \nclosest amenity (km)', y = 'Estimated mean number \nof carcasses per segment')
) 

# ~~~ Water --------------------------------------------------------------------

# create some data
newdata_distance_water <- expand.grid(distance_water_km = seq(from = range(table.glm$distance_water_km)[1],
                                                              to = range(table.glm$distance_water_km)[2],
                                                              by = 0.025), 
                                      road_importance = c("major_road", "minor_road"),
                                      distance_amenity_km = mean(table.glm$distance_amenity_km),
                                      woodland = mean(table.glm$woodland))

## add the fitted values by predicting from the model for the new data
newdata_distance_water <- add_column(newdata_distance_water, fit = predict(glm_nb_all,
                                                                           newdata = newdata_distance_water,
                                                                           se = TRUE,
                                                                           type = "response")[["fit"]])
## add fit and se.fit on the **link** scale
newdata_distance_water <- bind_cols(newdata_distance_water, setNames(as_tibble(predict(glm_nb_all, 
                                                                                       newdata = newdata_distance_water, 
                                                                                       se.fit = TRUE)[1:2]),
                                                                     c('fit_link','se_link')))
## grab the inverse link function 
ilink <- glm_nb_all[["family"]][["linkinv"]]

## create the interval and backtransform
newdata_distance_water <- mutate(newdata_distance_water,
                                 fit_resp  = ilink(fit_link),
                                 right_upr = ilink(fit_link + (2 * se_link)),
                                 right_lwr = ilink(fit_link - (2 * se_link)))

# Plot
N <- nrow(newdata_distance_water)

### To rescale the predictor
mean_sd <- read_csv(paste0("06_processed_data/glm_data_2021-10_high_certainty/predictor_mean_sd_", buffer.size, "m.csv"))
x <- which(mean_sd$variable == "distance_water_km")
y <- newdata_distance_water$distance_water_km * mean_sd$sd[x] + mean_sd$mean[x]
range(y)
(plot.dist.water <- ggplot() +
    geom_line(data = newdata_distance_water ,
              aes(x = newdata_distance_water$distance_water_km * mean_sd$sd[x] + mean_sd$mean[x],
                  y = newdata_distance_water$fit, #[, 5],
                  linetype = road_importance)) +
    geom_ribbon(aes(x = newdata_distance_water$distance_water_km[1:(N/2)] * mean_sd$sd[x] + mean_sd$mean[x],
                    ymin = newdata_distance_water$right_lwr [1:(N/2)],
                    ymax = newdata_distance_water$right_upr[1:(N/2)]),
                alpha = 0.2) +
    geom_ribbon(aes(x = newdata_distance_water$distance_water_km[(N/2 + 1):N] * mean_sd$sd[x] + mean_sd$mean[x],
                    ymin = newdata_distance_water$right_lwr[(N/2 + 1):N],
                    ymax = newdata_distance_water$right_upr[(N/2 + 1):N]),
                alpha = 0.2) +
    theme_classic() +
    theme(legend.position = c(0.70, 0.8),
          legend.title = element_blank(),
          legend.background = element_rect(fill = "white",
                                           size = 0.5, linetype = "solid", 
                                           colour = "black")) +
    scale_linetype_discrete(labels = c("main road", "track")) +
    labs(x = 'Distance from the closest \nwater source/riverine habitat (km)', 
         y = 'Estimated mean number \nof carcasses per segment')
) 

# ~~~ Percentage woodland --------------------

# Create some data
newdata_woodland <- expand.grid(woodland = seq(from = range(table.glm$woodland)[1],
                                               to = range(table.glm$woodland)[2],
                                               by = 0.025), 
                                road_importance = c("major_road", "minor_road"),
                                distance_amenity_km = mean(table.glm$distance_amenity_km),
                                distance_water_km = mean(table.glm$distance_water_km))

## add the fitted values by predicting from the model for the new data
newdata_woodland <- add_column(newdata_woodland, fit = predict(glm_nb_all,
                                                               newdata = newdata_woodland,
                                                               se = TRUE,
                                                               type = "response")[["fit"]])

## add fit and se.fit on the **link** scale
newdata_woodland <- bind_cols(newdata_woodland, setNames(as_tibble(predict(glm_nb_all, 
                                                                           newdata = newdata_woodland, 
                                                                           se.fit = TRUE)[1:2]),
                                                         c('fit_link','se_link')))
## grab the inverse link function 
ilink <- glm_nb_all[["family"]][["linkinv"]]

## create the interval and backtransform
newdata_woodland <- mutate(newdata_woodland,
                           fit_resp  = ilink(fit_link),
                           right_upr = ilink(fit_link + (2 * se_link)),
                           right_lwr = ilink(fit_link - (2 * se_link)))


# Plot
N <- nrow(newdata_woodland)

### To rescale the predictor
mean_sd <- read_csv(paste0("06_processed_data/glm_data_2021-10_high_certainty/predictor_mean_sd_", buffer.size, "m.csv"))
x <- which(mean_sd$variable == "woodland")
y <- newdata_woodland$woodland * mean_sd$sd[x] + mean_sd$mean[x]
range(y)
(plot.per.woodland <- ggplot() +
    geom_line(data = newdata_woodland ,
              aes(x = newdata_woodland$woodland * mean_sd$sd[x] + mean_sd$mean[x],
                  y = newdata_woodland$fit, #[, 5],
                  linetype = road_importance)) +
    geom_ribbon(aes(x = newdata_woodland$woodland[1:(N/2)] * mean_sd$sd[x] + mean_sd$mean[x],
                    ymin = newdata_woodland$right_lwr [1:(N/2)],
                    ymax = newdata_woodland$right_upr[1:(N/2)]),
                alpha = 0.2) +
    geom_ribbon(aes(x = newdata_woodland$woodland[(N/2 + 1):N] * mean_sd$sd[x] + mean_sd$mean[x],
                    ymin = newdata_woodland$right_lwr[(N/2 + 1):N],
                    ymax = newdata_woodland$right_upr[(N/2 + 1):N]),
                alpha = 0.2) +
    theme_classic() +
    theme(legend.position = c(0.70, 0.8),
          legend.title = element_blank(),
          legend.background = element_rect(fill = "white",
                                           size = 0.5, linetype = "solid", 
                                           colour = "black")) +
    scale_linetype_discrete(labels = c("main road", "track")) +
    labs(x = 'Percentage of \nwoodland cover', 
         y = 'Estimated mean number \nof carcasses per segment')
) 


# ~~~ Construct the figure -----------------------------------------------------

(plot.road.imp + plot.dist.amenity) / (plot.dist.water + plot.per.woodland) + 
  plot_annotation(tag_levels = 'a', tag_prefix = '(',
                  tag_suffix = ')') 

ggsave("11_manuscript/V4 Figures/figure 2 (GLM results).png",
       width = 8, height = 7)



# ~ 3. Figure 3: Spatio-temporal patterns --------------------------------------

# ~~~ a. Count the roadkills per season/month ----------------------------------

hy_carcasses <- read_delim("06_processed_data/carcasses/12_hy.carcasses.certainty.formatted.spatial_updated_2022-04.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE) %>%
  mutate(date_obs_new = paste0(substr(date_obs, 1, 4), "-", substr(date_obs, 5, 6), "-", substr(date_obs, 7, 8))) %>%
  mutate(date_obs_new = as.Date(date_obs_new, format = "%Y - %m - %d")) %>%
  dplyr::select(-date_obs) %>%
  rename(date_obs = date_obs_new)
# %>%
#   filter(location_certainty_score >= 0.75)

# Season dates are from Serengeti IV 
# With transit
{ start_transit_1 <- as.Date("1989-05-11")
  end_transit_1 <- as.Date("1989-07-31")
  start_dry <- as.Date("1989-08-01")
  end_dry <- as.Date("1989-10-31")
  start_transit_2 <- as.Date("1989-11-01")
  end_transit_2 <- as.Date("1989-12-20")
  start_wet <- as.Date("1989-12-21")
  end_wet <- as.Date("1990-05-10")
  nbr_days <- 365
}

hy_carcasses_season_w_transit <- hy_carcasses %>%
  mutate(year = year(date_obs), 
         herds_position_w_transit = ifelse(date_obs %within% interval(ymd(paste0(year, "-08-01")), 
                                                                      ymd(paste0(year, "-10-31"))), 
                                           "north west", 
                                           ifelse(date_obs %within% interval(ymd(paste0(year, "-05-11")), 
                                                                             ymd(paste0(year, "-07-31"))),
                                                  "transit",
                                                  ifelse(date_obs %within% interval(ymd(paste0(year, "-11-01")), 
                                                                                    ymd(paste0(year, "-12-20"))), 
                                                         "transit", "south east")))) %>%
  filter(age %in% c("adult", "unknown", "subadult"),
         year < 2020) %>%
  count(herds_position_w_transit)


# By month
hy_carcasses_month <- read_delim("06_processed_data/carcasses/12_hy.carcasses.certainty.formatted.spatial_updated_2022-04.csv", 
                                                 ";", escape_double = FALSE, trim_ws = TRUE) %>%
  mutate(date_obs_new = paste0(substr(date_obs, 1, 4), "-", substr(date_obs, 5, 6), "-", substr(date_obs, 7, 8))) %>%
  mutate(date_obs_new = as.Date(date_obs_new, format = "%Y - %m - %d")) %>%
  dplyr::select(-date_obs) %>%
  rename(date_obs = date_obs_new) %>%
  mutate(month = month(date_obs),
         year = year(date_obs)) %>%
  filter(year < 2020) %>% # Remove the year 2020 as it was incomplete
  count(month)
sum(hy_carcasses_month$n)


# ~~~ b. Plot ------------------------------------------------------------------

# ~~~~~~ Panel a (by month) ----------------------------------------------------
(plot_month <- hy_carcasses_month %>%
   mutate(herds_position = ifelse(month %in% c(1, 2, 3, 4, 5, 12), "south_east",
                                  ifelse(month %in% c(8, 9, 10), "north_west", "transit")),
          month_name = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                         "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) %>%
   ggplot(., aes(x = month, y = n, fill = herds_position)) +
   geom_bar(stat = "identity",
            position = position_dodge(),
            color = "black",
            size = 0.5) +
   scale_fill_manual(limits = c("south_east", "north_west", "transit"),
                     values = c("#336600", "#E69F00", "#333333"),
                     labels = c("south- \neast", "north \n& MMR", "transit")) +  # c("north and north-west", "south-east", "in transit")) +
   theme_classic() +
   scale_x_discrete(limits = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
   scale_y_continuous(breaks = c(0, 3, 6, 9, 12, 15)) +
   labs(x = "month", y = "number of carcasses",
        fill = "migratory herds' position") +
   theme(legend.position = "top",
         legend.direction = "horizontal",
         axis.text.x = element_text(angle = 60, hjust = 1)) +
   guides(fill = guide_legend(title.position = "top", title.hjust = 0.5))
)

# ~~~~~~ Panel b (by season) ---------------------------------------------------
days.dry <- length(seq(start_dry, end_dry, by = "day"))
days.wet <- length(seq(start_wet, end_wet, by = "day"))
nbr.days <- days.dry + days.wet
proba <- c(days.dry/nbr.days, days.wet/nbr.days)

df_exp_VS_obs <- data.frame(type = c("observed", "observed", "expected", "expected"),
                            herds_position = c("north_west", "south_east", "north_west", "south_east"),
                            value = c(hy_carcasses_season_w_transit$n[1], 
                                      hy_carcasses_season_w_transit$n[2],
                                      proba[1] * sum(hy_carcasses_season_w_transit$n[1:2]),
                                      proba[2] * sum(hy_carcasses_season_w_transit$n[1:2]))) %>%
  mutate(herds_position = factor(herds_position, levels = c("south_east", "north_west")))


(plot_seasons <- ggplot(df_exp_VS_obs, aes(x = herds_position, y = value,
                                           fill = herds_position, alpha = type)) +
    geom_col(position = position_dodge(),
             color = "black",
             size = 0.5) +
    scale_fill_manual(values = c("#336600", "#E69F00"),
                      labels = c("south- \neast", "north \n& MMR")) +
    theme_classic() +
    theme(legend.title = element_blank()) +
          # legend.position = "none") +
    scale_alpha_discrete(limits = c("expected", "observed"),
                         range = c(0.3, 1)) +
    scale_x_discrete(labels = c("late Dec -\nearly May", "Aug - Oct")) + 
    # scale_x_discrete(labels = c("wet", "dry")) + #"north \n& MMR", "south- \neast")) +
    labs(y = "number of carcasses",
         x = "period"))
         
# ~~~~~~ Panel c  --------------------------------------------------------------

hy_carcasses_herds_position <- read_delim("06_processed_data/carcasses/12_hy.carcasses.certainty.formatted.spatial_updated_2022-04.csv", 
                                          ";", escape_double = FALSE, trim_ws = TRUE) %>%
  mutate(date_obs_new = paste0(substr(date_obs, 1, 4), "-", substr(date_obs, 5, 6), "-", substr(date_obs, 7, 8))) %>%
  mutate(date_obs_new = as.Date(date_obs_new, format = "%Y - %m - %d")) %>%
  dplyr::select(-date_obs) %>%
  rename(date_obs = date_obs_new) %>%
  filter(location_certainty_score > 0.75) %>%
  mutate(year = year(date_obs)) %>%
  mutate(herds_position = ifelse(date_obs %within% interval(ymd(paste0(year, "-08-01")), 
                                                            ymd(paste0(year, "-10-31"))), 
                                 "north west", 
                                 ifelse(date_obs %within% interval(ymd(paste0(year, "-01-01")),
                                                                   ymd(paste0(year, "-05-10"))),
                                        "south east", 
                                        ifelse(date_obs %within% interval(ymd(paste0(year, "-12-21")),
                                                                          ymd(paste0(year, "-12-31"))),
                                               "south east", NA)))) %>%
  filter(!is.na(herds_position)) %>%
  dplyr::select(ID, date_obs, herds_position, long, lat, location_certainty_score, age, sex)

hy_carcasses_herds_position_ad <- hy_carcasses_herds_position %>%
  filter(age %in% c("adult", "unknown", "subadult"))


roads.ggplot.map <- read_csv("06_processed_data/glm_data_2021-10/roads.segmented.cropped.no.short.df.csv") %>%
  filter(long > 34.6)

roads <- rgdal::readOGR(dsn  =  "06_processed_data/roads/SNP.roads.QGIS",
                        layer = "snp_roads_according_to_FZS_map_cropped")
roads@data[["osm_id"]] <- seq(from = 1, to = length(roads@data[["osm_id"]]))
source(file = "05_scripts/04-2021/GLM_2021-10/GLM_functions_calculate_predictors_2021-10.R")
roads.ggplot.map <- CreateDataFrameRoads(roads) %>%
  mutate(ID = as.factor(ID))

(long_lat_scatter <- ggplot() +
    geom_path(data = roads.ggplot.map, aes(x = long, y = lat, group = ID), # ID_road_seg),
              size = 0.75) +
    geom_point(data = hy_carcasses_herds_position_ad, aes(x = long, y = lat, color = herds_position),
               size = 2) +
    theme_classic() + #theme_minimal() + 
    scale_color_manual(limits = c("south east", "north west"),
                       labels = c("south- \neast", "north \n& MMR"),
                       values = c("#336600", "#E69F00")) +
    theme(legend.position = "top",
          legend.direction = "horizontal") +
    labs(y = "latitude", 
         x = "longitude",
         color = "migratory herds' position") +
    xlim(34.6, 35.3) 
  #  guides(color = guide_legend(title.position = "top", title.hjust = 0.5))
)

transparent_theme <- theme(
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks = element_blank(),
  panel.grid = element_blank(),
  axis.line = element_blank(),
  panel.background = element_rect(fill = "transparent",colour = NA),
  plot.background = element_rect(fill = "transparent",colour = NA))


(xlong_violin <- ggplot(hy_carcasses_herds_position_ad, 
                        aes(x = herds_position, y = long, color = herds_position,
                            fill = herds_position)) +
    geom_violin(size = 0.5, color = "black") +
    geom_boxplot(width=0.1, size = 0.5, color = "black") +
    scale_fill_manual(limits = c("south east", "north west"),
                      values = c("#336600", "#E69F00"),
                      labels = c("south east", "north & MMR")) +
    # scale_x_discrete(limits = c("south east", "north west"),
    #                  labels = c("wet", "dry")) +
    scale_x_discrete(limits = c("south east", "north west"),
                     labels = c("late Dec -\nearly May", "Aug - Oct")) +
    theme(axis.title.x = element_blank(),
          legend.position = "bottom") +
    labs(x = "period", # "Position of the \nmigratory herds",
         fill = "") +
    ylim(34.6, 35.2) +
    coord_flip() +
    transparent_theme
)

(ylat_violin <- ggplot(hy_carcasses_herds_position_ad, 
                       aes(x = herds_position, y = lat, fill = herds_position)) +
    geom_violin(size = 0.5, color = "black") +
    geom_boxplot(width=0.1, size = 0.5, color = "black") +
    scale_fill_manual(limits = c("south east", "north west"),
                      labels = c("south-east", "north west"),
                      values = c("#336600", "#E69F00")) +
    scale_x_discrete(limits = c("south east", "north west"),
                     labels = c("late Dec -\nearly May", "Aug - Oct")) +
    scale_y_continuous(breaks = c(-3, -2.7, -2.4, -2.1),
                       limits = c(-3.04, -1.9)) +
    # theme_classic() + 
    theme(axis.title.y = element_blank()) +
    #       axis.text.x = element_blank(),
    theme(legend.position = "") +
    labs(x = "period", # "Position of the \nmigratory herds",
         fill = "") +
    transparent_theme
)

xlong_violin_grob = ggplotGrob(xlong_violin)
ylat_violin_grob = ggplotGrob(ylat_violin)
xmin <- min(roads.ggplot.map$long); xmax <- max(roads.ggplot.map$long)
ymin <- min(roads.ggplot.map$lat); ymax <- max(roads.ggplot.map$lat)

long_lat_scatter_complete <- long_lat_scatter + 
  annotation_custom(grob = ylat_violin_grob, 
                    xmin = xmax - 0.20, xmax = xmax + 0.15,
                    ymin = ymin - 0.07, ymax = ymax + 0.15) +
  annotation_custom(grob = xlong_violin_grob, 
                    xmin = xmin + 0.02, xmax = xmax + 0.02,
                    # ymin = ymax - 0.25, ymax = ymax + 0.05)
                    ymin = ymax - 0.4, ymax = ymax + 0.05)


# ~~~ Construct the figure -----------------------------------------------------

(figure <- ggdraw() +
    draw_plot(plot_seasons, x = 0.04, y = 0, width = 0.39, height = 0.3) +
    draw_plot(plot_month, x = 0.04, y = 0.29, width = 0.36, height = 0.35) +
    draw_plot(long_lat_scatter_complete, x = 0.41,  y = 0, width = 0.59, height = 0.706) +
    draw_plot_label(label = c("(a)", "(b)", "(c)"),
                    x = c(0, 0, 0.36), y = c(0.67, 0.3, 0.67), size = 14)

)
save_plot("11_manuscript/V4 Figures/figure 3 (spatio temporal).png",
          plot = figure,
          ncol = 2, nrow = 2,
          base_asp = 1)
save_plot("11_manuscript/V4 Figures/figure 3 (spatio temporal).svg",
          plot = figure,
          ncol = 2, nrow = 2,
          base_asp = 1)

# With patchwork:
# layout <- "
# AACCC
# BBCCC
# "
# plot_month + plot_seasons + long_lat_scatter_complete + 
#   plot_annotation(tag_levels = 'a', tag_prefix = '(',
#                   tag_suffix = ')') + 
#   plot_layout(design = layout)
# 
# ggsave("11_manuscript/V4 Figures/figure 3 (spatio temporal).png",
#        width = 8, height = 6)




# ~ 4. Figure 4: Effect of age, sex and rank -----------------------------------

# ~~~ a. Effect of age ---------------------------------------------------------

# ~~~~~ Calculate the average age distribution ---------------------------------

clan_size_year_clan <- read_csv("06_processed_data/hyenas/Clan_size_year_clan.csv", 
                                col_types = cols(...1 = col_skip())) %>%
  na.omit() %>%
  group_by(age_class) %>%
  summarise(mean_nb = mean(nb))
proportion_age_classes_raw <- data.frame(age_class = clan_size_year_clan$age_class,
                                         proportion = clan_size_year_clan$mean_nb/sum(clan_size_year_clan$mean_nb))

proportions <- c(proportion_age_classes_raw[1, 2],    # Adult females
                            proportion_age_classes_raw[2, 2] + 
                              proportion_age_classes_raw[3, 2],  # Adult males
                            proportion_age_classes_raw[5, 2] + 
                              proportion_age_classes_raw[6, 2],  # Subadults
                            proportion_age_classes_raw[4, 2])    # Cubs
names(proportions) <- c("Adult females", "Adult males",
                   "Subadults", "Cubs")

# Fuse all adults together, and subadult males and females
proportions_1 <- c(proportion_age_classes_raw[1, 2] +
                                proportion_age_classes_raw[2, 2] + 
                                proportion_age_classes_raw[3, 2],  # Adults
                              proportion_age_classes_raw[5, 2] + 
                                proportion_age_classes_raw[6, 2],  # Subadults
                              proportion_age_classes_raw[4, 2])    # Cubs
names(proportions_1) <- c("Adults", "Subadults", "Cubs")

# ~~~~~ Count roadkills of each age class --------------------------------------

hy_carcasses <- read_delim("06_processed_data/carcasses/12_hy.carcasses.certainty.formatted.spatial_updated_2022-04.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE)
{counts <- hy_carcasses %>%
  count(age, sex)
nb.ad.F <- as.numeric(counts[1,3])
nb.ad.M <- as.numeric(counts[2,3])
nb.ad <- as.numeric(counts[1,3] + counts[2,3] + counts[3,3])
nb.sub <- as.numeric(counts[7,3] + counts[8,3] + counts[9,3])
nb.cub <- as.numeric(counts[4,3] + counts[5,3] + counts[6,3])
nb.unkn <- as.numeric(counts[10,3] + counts[11,3])

obs <- c(nb.ad.F, nb.ad.M, nb.sub, nb.cub) # adult males and adult females in separate categories
obs1 <- c(nb.ad, nb.sub, nb.cub) 
obs2 <- c(nb.ad + nb.unkn, nb.sub, nb.cub) # unknown age considered as adults.
}


# ~~~~~  Plot ------------------------------------------------------------------
# Adults and subadults VS cubs (with unknowns considered adults)
df.age <- data.frame(age = c("adults and subadults", "adults and subadults", "cubs", "cubs"),
                     type = c("observed", "expected", "observed", "expected"),
                     n =  c(obs2[1] + obs2[2], 
                            (proportions_1[1] + proportions_1[2]) * sum(counts$n), # Expected 
                            obs2[3], 
                            proportions_1[3]*sum(counts$n)))              # Expected

(plot.age <- ggplot(df.age, aes(x = age, y = n, fill = type)) +
    geom_bar(stat = "identity",
             position = position_dodge(),
             color = "black",
             size = 0.5) +
    # scale_fill_grey(start = 0.8, end = 0.2) +
    scale_fill_manual(values = c("#999999", "#E69F00")) +
    theme_classic() +
    theme(legend.title = element_blank()) +
    scale_x_discrete(limits = c("adults and subadults", "cubs"), 
                     labels = c("adults \n& subadults", "cubs")) +
    ylab("number of carcasses") +
    xlab("")
)


# ~~~ b. Effect of sex --------------------------------------------------------- 

# ~~~~~ Calculate the average adult and subadult sex ratio ---------------------

clan_size_year_clan <- read_csv("06_processed_data/hyenas/Clan_size_year_clan.csv", 
                                col_types = cols(...1 = col_skip())) %>%
  na.omit() %>%
  group_by(age_class) %>%
  summarise(mean_nb = mean(nb))
proportion_age_classes_raw <- data.frame(age_class = clan_size_year_clan$age_class,
                                         proportion = clan_size_year_clan$mean_nb/sum(clan_size_year_clan$mean_nb))

proportions <- c(proportion_age_classes_raw[1, 2],    # Adult females
                 proportion_age_classes_raw[2, 2] + 
                   proportion_age_classes_raw[3, 2],  # Adult males
                 proportion_age_classes_raw[5, 2],    # Subadult females
                 proportion_age_classes_raw[6, 2],    # Subadult males
                 proportion_age_classes_raw[4, 2])    # Cubs
names(proportions) <- c("Adult females", "Adult males", 
                        "Subadults females", "Subadults males",
                        "Cubs")

ad.sex.ratio <- unname(proportions[1]/(proportions[1] + proportions[2]))
 
subad.sex.ratio <- unname(proportions[3]/(proportions[3] + proportions[4]))



# ~~~~~ Count individuals of each sex ------------------------------------------
hy_carcasses <- read_delim("06_processed_data/carcasses/12_hy.carcasses.certainty.formatted.spatial_updated_2022-04.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE)

# ----------- Adults --
ad.counts <- hy_carcasses %>%
  filter(age == "adult",
         sex %in% c("F", "M")) %>%
  count(sex)
nb.ad.F <- as.numeric(ad.counts[1,2])
N <- nb.ad.F + as.numeric(ad.counts[2,2])

# Adults With individuals of unknwon age considered adults
ad.counts.bis <- hy_carcasses %>%
  filter(age %in% c("adult", "unknown"),
         sex %in% c("F", "M")) %>%
  count(sex)
nb.ad.F.bis <- as.numeric(ad.counts.bis[1,2])
N.bis <- nb.ad.F + as.numeric(ad.counts.bis[2,2])

# ~~~~~  Plot ------------------------------------------------------------------

#Adults sex ratio (with unknowns)
df.sex.ad <- data.frame(sex = c("females", "females", "males", "males"),
                        type = c("observed", "expected", "observed", "expected"),
                        n = c(nb.ad.F.bis, N.bis*ad.sex.ratio, N.bis - nb.ad.F.bis, N.bis*(1-ad.sex.ratio)),
                        age = rep("Adults", times = 4))

(plot.sex.ad <- ggplot(df.sex.ad, aes(x = sex, y = n, fill = type)) +
    geom_bar(stat = "identity",
             position = position_dodge(),
             color = "black",
             size = 0.5) +
    # scale_fill_grey(start = 0.8, end = 0.2) +
    scale_fill_manual(values = c("#999999", "#E69F00")) +
    theme_classic() +
    theme(legend.title = element_blank(),
          legend.position = 'none') +
    scale_x_discrete(limits = c("females", "males")) +
    labs(x = "", 
         y = "number of carcasses")
)



# ~~~ c. Effect of rank --------------------------------------------------------
# All the carcasses of females from the three studied clans have collision 
# certainty scores >= 0.75.

hy_carcasses_clan_members <- read_delim("06_processed_data/carcasses/3_hy.clan.members.csv", 
                                        ";", escape_double = FALSE, trim_ws = TRUE)

# Remove the males and the female cub
hy_carcasses_clan_members <- hy_carcasses_clan_members %>%
  filter(sex == "F",
         age_category %in% c("adult", "subadult")) %>%
  arrange(standardized_rank)


# With line observed median, line expected median
median.df <- data.frame(collision_certainty_score_NEW = c(0.98, 0.98),
                        type = c("expected", "observed"),
                        value = c(0, 0.4))

(plot.rank.dotplot <- ggplot(hy_carcasses_clan_members, 
                             aes(x = collision_certainty_score_NEW, y = standardized_rank)) +
    geom_dotplot(binaxis = 'y', stackdir = 'center', fill = "black",
                 stackratio = 2, dotsize = 1.4) +
    geom_hline(data = median.df, 
               aes(yintercept = value, color = type)) +
    
    # geom_hline(yintercept = median.df$value) +
    # size = 2.75, shape = 24, color = "black") +
    scale_color_manual(values = c("#999999", "#E69F00"),
                       labels = c("expected \nmedian", "observed \nmedian")) +
    # annotate("point", x = 1, y = 0.4,
    #          colour = "black", size = 3) +
    # annotate("point", x = 1, y = 0, 
    #          colour = "black", size = 3) +
    # annotate("point", x = 1, y = 0.4, 
    #          colour = "#E69F00", size = 2.4,) +
    # annotate("point", x = 1, y = 0, 
    #          colour = "#999999", size = 2.4) +
    # geom_hline(yintercept = 0.4, size = 0.5, color = "red") +
    theme_classic() +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = c(0.5, 0.2),
          legend.title = element_blank(),
          # legend.background = element_rect(fill = "white",
          #                                  size = 0.5, # linetype = "solid", 
          #                                  colour = "white")
          legend.background = element_blank()) +
    # legend.position = "none") +
    ylim(-1, 1) +
    labs(y = "social status",
         x = "") 
)

# ~~~ d. Construct the figure --------------------------------------------------

plot.age + plot.sex.ad + plot.rank.dotplot +
  plot_annotation(tag_levels = 'a', tag_prefix = '(',
                tag_suffix = ')') 

ggsave("11_manuscript/V4 Figures/figure 4 (age, sex and rank) raw.svg",
       width = 7, height = 2.75)




# ~ 5. Supplementary Figure 1: sampling effort ---------------------------------

# ~~~ a. By month --------------------------------------------------------------
daily.monitoring <- read_csv("06_processed_data/hyenas/daily_monitoring_1989-2022.csv")

daily.monitoring.month <- daily.monitoring %>%
  mutate(year = year(date),
         month = month(date)) %>%
  group_by(year, month) %>%
  summarise(monitoring = mean(monitoring))   %>% 
  mutate(herds_position_w_transit = ifelse(month %in% c(8:10), "north west", 
                                           ifelse(month %in% c(5:7, 11, 12), "transit",
                                                  "south east")),
         covid = ifelse(year %in% c(2020, 2021), "covid", "normal"))

# (sampling.month <- ggplot(data = daily.monitoring.month, 
#        aes(x = as.factor(month), y = monitoring*100, 
#            fill = herds_position_w_transit)) +
#   geom_boxplot(color = "#222222") +
#   scale_x_discrete(labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
#                               "Aug", "Sep", "Oct", "Nov", "Dec")) +
#   
#   scale_fill_manual(limits = c("south east", "north west", "transit"),
#                     labels = c("south east", "north & MMR", "transit"),
#                     values = c("#336600", "#E69F00", "#333333")) +
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # legend.position = "",
#   labs(x = "month",
#        y = "percentage of days with \nmonitoring session",
#        fill = "wildebeest herd's \nlocation")) 

(sampling.month <- ggplot() +
    geom_boxplot(data = daily.monitoring.month, 
                 aes(x = as.factor(month), y = monitoring*100, 
                     fill = herds_position_w_transit),
                 color = "#222222") +
    # geom_jitter(data = daily.monitoring.month, 
    #             aes(x = as.factor(month), y = monitoring*100, 
    #                 color = covid),
    #             width = 0.20) + 
    scale_x_discrete(labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                                "Aug", "Sep", "Oct", "Nov", "Dec")) +
    # scale_color_manual(values = c("red", "black")) +
    scale_fill_manual(limits = c("south east", "north west", "transit"),
                      labels = c("south east", "north & MMR", "transit"),
                      values = c("#336600", "#E69F00", "#333333")) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # legend.position = "",
    labs(x = "month",
         y = "percentage of days with \nmonitoring session",
         fill = "migratory herds' \nlocation"))
# color = "year"))

# ~~~ b. By season -------------------------------------------------------------
sampling.effort.season <- read_csv("06_processed_data/hyenas/sampling_effort_1989-2022.csv")


# Remove the "transit season"
daily.monitoring.season.no.transit <- sampling.effort.season %>%
  filter(herds_position_w_transit %in% c("north west", "south east")) %>%
  mutate(covid = ifelse(year %in% c(2020, 2021), "covid", "normal"))

# (sampling.season <- ggplot(data = daily.monitoring.season.no.transit, 
#                           aes(x = herds_position_w_transit, y = prop*100, 
#                               fill = herds_position_w_transit)) +
#   geom_boxplot() +
#   geom_jitter(width = 0.20) + 
#   scale_x_discrete(limits = c("south east", "north west"),
#                    # labels = c("wet", "dry")) +
#                    labels = c("late Dec -\nearly May", "Aug - Oct")) +
#   scale_fill_manual(limits = c("south east", "north west"),
#                     labels = c("south east", "north & MMR"),
#                     values = c("#336600", "#E69F00")) +
#   theme_classic() +
#   # theme(legend.position = "") + 
#   labs(x = "period",
#        y = "percentage of days with \nmonitoring session",
#        fill = "migratory herds' \nlocation"))

(sampling.season <- ggplot() +
    geom_boxplot(data = daily.monitoring.season.no.transit, 
                 aes(x = herds_position_w_transit, y = prop*100, 
                     fill = herds_position_w_transit),
                 color = "#222222") +
    geom_jitter(data = daily.monitoring.season.no.transit, 
                aes(x = herds_position_w_transit, y = prop*100, 
                    color = covid),
                width = 0.20) + 
    scale_x_discrete(limits = c("south east", "north west"),
                     labels = c("late Dec -\nearly May", "Aug - Oct")) +
    scale_color_manual(values = c("red", "black")) +
    scale_fill_manual(limits = c("south east", "north west"),
                      labels = c("south east", "north & MMR"),
                      values = c("#336600", "#E69F00")) +
    theme_classic() +
    labs(x = "period",
         y = "percentage of days with \nmonitoring session",
         fill = "migratory herds' \nlocation",
         color = "year"))


# ~~~ c. Construct the figure --------------------------------------------------

library(patchwork)

# layout <- "AAAABB"
# sampling.month + sampling.season +
#   plot_annotation(tag_levels = 'a', tag_prefix = '(',
#                   tag_suffix = ')') +
#   plot_layout(design = layout)
sampling.month / sampling.season +
  plot_annotation(tag_levels = 'a', tag_prefix = '(',
                  tag_suffix = ')')

ggsave("11_manuscript/V4 Figures/figure S1 (sampling effort).svg",
       width = 14, height = 12, unit = "cm")
