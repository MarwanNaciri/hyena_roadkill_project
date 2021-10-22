#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                              #
#                                  Figure GLM                                  #        
#                                                                              #
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(tidyverse)
library(MASS)
library(ggthemes)
library(cowplot)

Sys.setenv(LANG = "en")


# 1. ALL AVAILABLE CARCASSES ---------------------------------------------------

buffer.size <- 500
# buffer.size <- 1000

table.glm <- read_csv(paste0("06_processed_data/glm_data_2021-09/table.glm_9.", 
                             buffer.size, "m.csv"))

glm_nb_all<- MASS::glm.nb(formula = nbr_carcasses ~ road_importance + distance_amenity_km
                          + distance_water_km + woodland,
                          data = table.glm,
                          link = log)
summary(glm_nb_all)


# ~~~ All coefs -----------------

df_glm <- data.frame(parameter = names(coef(glm_nb_all)),
                     mean = summary(glm_nb_all)$coefficients[, 1],
                     se = summary(glm_nb_all)$coefficients[, 2]) %>%
  mutate_at(c("mean", "se"), as.numeric) %>%
  mutate(lCI = as.numeric(mean) - 2*as.numeric(se),
         uCI = as.numeric(mean) + 2*as.numeric(se))


df_glm$parameter     


ggplot(df_glm, aes(x = parameter, y = mean)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lCI,
                    ymax = uCI)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_classic() +
  labs(x = "",
       y = "estimate") +
  
  coord_flip() +
  scale_x_discrete(limits = rev(c("(Intercept)", 
                                  "road_importanceminor_road", "distance_amenity_km", 
                                  "distance_water_km", "woodland")),
                   labels = rev(c("intercept", "road type: track", 
                                  "distance to amenity",
                                  "distance to water",
                                  "woodland cover")))

ggsave(filename = "07_intermediate_results/2021-04/plots/GLM/GLM_coefs_2021-09.png",
       width = 4, height = 2.5)  


# ~~~ Road type -----------------

# Create some data
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


# PLOT

(plt.road.imp <- ggplot(newdata_road_importance, aes(x = road_importance, y = fit)) +
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

ggsave(filename = paste0("07_intermediate_results/2021-04/plots/GLM/GLM_road_importance (",
                         buffer.size, "m buffer).png"),
       width = 2, height = 3)

# ~~~ Amenity ----------------

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


# PLOT
N <- nrow(newdata_distance_amenity)

# To rescale the predictor
mean_sd <- read_csv(paste0("06_processed_data/glm_data_2021-09/predictor_mean_sd_", buffer.size, "m.csv"))
x <- which(mean_sd$variable == "distance_amenity_km")
y <- newdata_distance_amenity$distance_amenity_km * mean_sd$sd[x] + mean_sd$mean[x]
range(y)
(plt.dist.amenity <- ggplot() +
    geom_line(data = newdata_distance_amenity ,
              aes(x = newdata_distance_amenity$distance_amenity_km * mean_sd$sd[x] + mean_sd$mean[x],
                  y = newdata_distance_amenity$fit, #[, 5],
                  linetype = road_importance)) +
    geom_ribbon(aes(x = newdata_distance_amenity$distance_amenity_km[1:(N/2)] * mean_sd$sd[x] + mean_sd$mean[x],
                    ymin = newdata_distance_amenity$right_lwr [1:(N/2)],
                    ymax = newdata_distance_amenity$right_upr[1:(N/2)]),
                alpha = 0.2) +
    geom_ribbon(aes(x = newdata_distance_amenity$distance_amenity_km[(N/2 + 1):N] * mean_sd$sd[x] + mean_sd$mean[x],
                    ymin = newdata_distance_amenity$right_lwr[(N/2 + 1):N],
                    ymax = newdata_distance_amenity$right_upr[(N/2 + 1):N]),
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

ggsave(filename = paste0("07_intermediate_results/2021-04/plots/GLM/GLM_amenity (",
                         buffer.size, "m buffer).png"),
       width = 3, height = 3)


# ~~~ Water --------------------

# Create some data
newdata_distance_water <- expand.grid(distance_water_km = seq(from = range(table.glm$distance_water_km)[1],
                                                              to = range(table.glm$distance_water_km)[2],
                                                              by = 0.025), 
                                      road_importance = "major_road",
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
# To rescale the predictor
mean_sd <- read_csv(paste0("06_processed_data/glm_data_2021-09/predictor_mean_sd_", buffer.size, "m.csv"))
x <- which(mean_sd$variable == "distance_water_km")


(plt.dist.water <- ggplot(newdata_distance_water, aes(x = distance_water_km * mean_sd$sd[x] + mean_sd$mean[x], y = fit)) +
    geom_line() +
    geom_ribbon(data = newdata_distance_water,
                aes(x = distance_water_km * mean_sd$sd[x] + mean_sd$mean[x],
                    ymin = right_lwr, ymax = right_upr),
                alpha = 0.2) +
    theme_classic() +
    labs(x = 'Distance from the \nclosest water source (km)', 
         y = 'Estimated mean number \nof carcasses per segment')
  
)

ggsave(filename = paste0("07_intermediate_results/2021-04/plots/GLM/GLM_water (",
                         buffer.size, "m buffer).png"),
       width = 3, height = 3)


# ~~~ Percentage woodland --------------------

# Create some data
newdata_woodland <- expand.grid(woodland = seq(from = range(table.glm$woodland)[1],
                                               to = range(table.glm$woodland)[2],
                                               by = 0.025), 
                                road_importance = "major_road",
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

# To rescale the predictor
mean_sd <- read_csv(paste0("06_processed_data/glm_data_2021-09/predictor_mean_sd_", buffer.size, "m.csv"))
x <- which(mean_sd$variable == "woodland")

(plt.per.woodland <- ggplot(newdata_woodland, aes(x = woodland * mean_sd$sd[x] + mean_sd$mean[x], y = fit)) +
    geom_line() +
    geom_ribbon(data = newdata_woodland,
                aes(x = woodland* mean_sd$sd[x] + mean_sd$mean[x],
                    ymin = right_lwr, ymax = right_upr),
                alpha = 0.2) +
    theme_classic() +
    labs(x = 'Percentage of \nwoodland cover', 
         y = 'Estimated mean number \nof carcasses per segment')
  
)

ggsave(filename = paste0("07_intermediate_results/2021-04/plots/GLM/GLM_woodland (",
                         buffer.size, "m buffer).png"),
       width = 3, height = 3)



# ~~~ Figure with all 4 plots --------------------------------------------------

library(cowplot)
plot_grid(plt.road.imp, plt.dist.amenity,
          plt.dist.water, plt.per.woodland,
          labels = c("(a)", "(b)", "(c)", "(d)"),
          ncol = 2, nrow = 2)

# (
#   figure.GLM <- ggdraw() +
#     draw_plot(plt.road.imp, x = 0.01,  y = 0.5, width = 0.49, height = 0.5) +
#     draw_plot(plt.dist.amenity, x = 0.51, y = 0.5, width = 0.49, height = 0.5) +
#     
#     draw_plot(plt.dist.water, x = 0.01, y = 0, width = 0.49, height = 0.5) + 
#     draw_plot(plt.per.woodland, x = 0.51, y = 0, width = 0.49, height = 0.5) + 
#     draw_plot_label(label = c("(a)", "(b)", "(c)", "(d)"),
#                     x = c(0, 0.50, 0, 0.50), y = c(1, 1, 0.5, 0.5), size = 14) 
# )


(
  figure.GLM <- ggdraw() +
    draw_plot(plt.road.imp, x = 0.01,  y = 0.45, width = 0.39, height = 0.4) +
    draw_plot(plt.dist.amenity, x = 0.41, y = 0.45, width = 0.39, height = 0.4) +
    
    draw_plot(plt.dist.water, x = 0.01, y = 0, width = 0.39, height = 0.4) + 
    draw_plot(plt.per.woodland, x = 0.41, y = 0, width = 0.39, height = 0.4) + 
    draw_plot_label(label = c("(a)", "(b)", "(c)", "(d)"),
                    x = c(0, 0.40, 0, 0.40), y = c(0.87, 0.87, 0.44, 0.44), size = 14) 
)

save_plot("11_manuscript/V3 Figures/figure 2 raw.png", 
          plot = figure.GLM,
          ncol = 2,
          nrow = 2,
          # base_asp = 1.618,
          base_height = 3.5,
          base_width = 3.5)




# ~~~ Frequency observed VS modeled ------------------------------

glm_nb_all <- MASS::glm.nb(formula = nbr_carcasses ~ road_importance + distance_amenity_km
                           + distance_water_km + woodland,
                           data = table.glm,
                           link = log)

# Calculate the predicted frequencies of the three models

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

ggsave(filename = paste0("07_intermediate_results/2021-04/plots/GLM/GLM_expected_(dots)_VS_observed (",
                         buffer.size, "m buffer).png"),
       width = 3, height = 3)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# 2. CARCASSES WITH CERTAINTY SCORES >= 0.5 ------------------------------------

buffer.size <- 500
buffer.size <- 1000

table.glm.0.5 <- read_csv(paste0("06_processed_data/glm_data_2021-09/table.glm_9.", 
                                 buffer.size, "m.0.5.csv"))

glm_nb_0.5<- MASS::glm.nb(formula = nbr_carcasses ~ road_importance + distance_amenity_km
                          + distance_water_km + woodland,
                          data = table.glm.0.5,
                          link = log)
summary(glm_nb_0.5)

# ~~~ Road type -----------------

# Create some data
newdata_road_importance <- expand.grid(road_importance = c("major_road", "minor_road"),
                                       distance_amenity_km = mean(table.glm.0.5$distance_amenity_km),
                                       distance_water_km = mean(table.glm.0.5$distance_water_km),
                                       woodland = mean(table.glm.0.5$woodland))

## add the fitted values by predicting from the model for the new data
newdata_road_importance <- add_column(newdata_road_importance, fit = predict(glm_nb_0.5,
                                                                             newdata = newdata_road_importance,
                                                                             se = TRUE,
                                                                             type = "response")[["fit"]])

## add fit and se.fit on the **link** scale
newdata_road_importance <- bind_cols(newdata_road_importance, setNames(as_tibble(predict(glm_nb_0.5, 
                                                                                         newdata = newdata_road_importance, 
                                                                                         se.fit = TRUE)[1:2]),
                                                                       c('fit_link','se_link')))
## grab the inverse link function 
ilink <- glm_nb_0.5[["family"]][["linkinv"]]

## create the interval and backtransform
newdata_road_importance <- mutate(newdata_road_importance,
                                  fit_resp  = ilink(fit_link),
                                  right_upr = ilink(fit_link + (2 * se_link)),
                                  right_lwr = ilink(fit_link - (2 * se_link)))


# PLOT

(plt.road.imp <- ggplot(newdata_road_importance, aes(x = road_importance, y = fit)) +
    geom_col(fill = "white",
             color = "black") +
    geom_errorbar(aes(x = newdata_road_importance[,1],
                      ymin = newdata_road_importance[,10], 
                      ymax = newdata_road_importance[,9], 
                      width=.2)) +
    theme_classic() +
    scale_x_discrete(limits = c("major_road", "minor_road"),
                     labels = c("major \nroad", "minor \nroad")) +
    labs(x = 'Road type', 
         y = 'Estimated mean number \nof carcasses per segment')
  
)

ggsave(filename = paste0("07_intermediate_results/2021-04/plots/GLM/GLM_0.5_road_importance (",
                         buffer.size, "m buffer).png"),
       width = 2, height = 3)

# ~~~ Amenity ----------------

# Create some data
newdata_distance_amenity <- expand.grid(distance_amenity_km = seq(from = range(table.glm.0.5$distance_amenity_km)[1],
                                                                  to = range(table.glm.0.5$distance_amenity_km)[2],
                                                                  by = 0.05), 
                                        road_importance = c("major_road", "minor_road"),
                                        woodland = mean(table.glm.0.5$woodland),
                                        distance_water_km = mean(table.glm.0.5$distance_water_km))

## add the fitted values by predicting from the model for the new data
newdata_distance_amenity <- add_column(newdata_distance_amenity, fit = predict(glm_nb_0.5,
                                                                               newdata = newdata_distance_amenity,
                                                                               se = TRUE,
                                                                               type = "response")[["fit"]])
## add fit and se.fit on the **link** scale
newdata_distance_amenity <- bind_cols(newdata_distance_amenity, setNames(as_tibble(predict(glm_nb_0.5, 
                                                                                           newdata = newdata_distance_amenity, 
                                                                                           se.fit = TRUE)[1:2]),
                                                                         c('fit_link','se_link')))
## grab the inverse link function 
ilink <- glm_nb_0.5[["family"]][["linkinv"]]

## create the interval and backtransform
newdata_distance_amenity <- mutate(newdata_distance_amenity,
                                   fit_resp  = ilink(fit_link),
                                   right_upr = ilink(fit_link + (2 * se_link)),
                                   right_lwr = ilink(fit_link - (2 * se_link)))


# PLOT
N <- nrow(newdata_distance_amenity)

# To rescale the predictor
mean_sd <- read_csv(paste0("06_processed_data/glm_data_2021-09/predictor_mean_sd_", buffer.size, "m.csv"))
x <- which(mean_sd$variable == "distance_amenity_km")
y <- newdata_distance_amenity$distance_amenity_km * mean_sd$sd[x] + mean_sd$mean[x]
range(y)
(plt.dist.amenity <- ggplot() +
    geom_line(data = newdata_distance_amenity ,
              aes(x = newdata_distance_amenity$distance_amenity_km * mean_sd$sd[x] + mean_sd$mean[x],
                  y = newdata_distance_amenity$fit, #[, 5],
                  linetype = road_importance)) +
    geom_ribbon(aes(x = newdata_distance_amenity$distance_amenity_km[1:(N/2)] * mean_sd$sd[x] + mean_sd$mean[x],
                    ymin = newdata_distance_amenity$right_lwr [1:(N/2)],
                    ymax = newdata_distance_amenity$right_upr[1:(N/2)]),
                alpha = 0.2) +
    geom_ribbon(aes(x = newdata_distance_amenity$distance_amenity_km[(N/2 + 1):N] * mean_sd$sd[x] + mean_sd$mean[x],
                    ymin = newdata_distance_amenity$right_lwr[(N/2 + 1):N],
                    ymax = newdata_distance_amenity$right_upr[(N/2 + 1):N]),
                alpha = 0.2) +
    theme_classic() +
    theme(legend.position = c(0.70, 0.8),
          legend.title = element_blank(),
          legend.background = element_rect(fill = "white",
                                           size = 0.5, linetype = "solid", 
                                           colour = "black")) +
    scale_linetype_discrete(labels = c("major road", "minor road")) +
    labs(x = 'Distance from the \nclosest amenity (km)', y = 'Estimated mean number \nof carcasses per segment')
) 

ggsave(filename = paste0("07_intermediate_results/2021-04/plots/GLM/GLM_0.5_amenity (",
                         buffer.size, "m buffer).png"),
       width = 3, height = 3)


# ~~~ Water --------------------

# Create some data
newdata_distance_water <- expand.grid(distance_water_km = seq(from = range(table.glm.0.5$distance_water_km)[1],
                                                              to = range(table.glm.0.5$distance_water_km)[2],
                                                              by = 0.025), 
                                      road_importance = "major_road",
                                      distance_amenity_km = mean(table.glm.0.5$distance_amenity_km),
                                      woodland = mean(table.glm.0.5$woodland))

## add the fitted values by predicting from the model for the new data
newdata_distance_water <- add_column(newdata_distance_water, fit = predict(glm_nb_0.5,
                                                                           newdata = newdata_distance_water,
                                                                           se = TRUE,
                                                                           type = "response")[["fit"]])
## add fit and se.fit on the **link** scale
newdata_distance_water <- bind_cols(newdata_distance_water, setNames(as_tibble(predict(glm_nb_0.5, 
                                                                                       newdata = newdata_distance_water, 
                                                                                       se.fit = TRUE)[1:2]),
                                                                     c('fit_link','se_link')))
## grab the inverse link function 
ilink <- glm_nb_0.5[["family"]][["linkinv"]]

## create the interval and backtransform
newdata_distance_water <- mutate(newdata_distance_water,
                                 fit_resp  = ilink(fit_link),
                                 right_upr = ilink(fit_link + (2 * se_link)),
                                 right_lwr = ilink(fit_link - (2 * se_link)))




# Plot
# To rescale the predictor
mean_sd <- read_csv(paste0("06_processed_data/glm_data_2021-09/predictor_mean_sd_", buffer.size, "m.csv"))
x <- which(mean_sd$variable == "distance_water_km")


(plt.dist.water <- ggplot(newdata_distance_water, aes(x = distance_water_km * mean_sd$sd[x] + mean_sd$mean[x], y = fit)) +
    geom_line() +
    geom_ribbon(data = newdata_distance_water,
                aes(x = distance_water_km * mean_sd$sd[x] + mean_sd$mean[x],
                    ymin = right_lwr, ymax = right_upr),
                alpha = 0.2) +
    theme_classic() +
    labs(x = 'Distance from the \nclosest water source (km)', 
         y = 'Estimated mean number \nof carcasses per segment')
  
)

ggsave(filename = paste0("07_intermediate_results/2021-04/plots/GLM/GLM_0.5_water (",
                         buffer.size, "m buffer).png"),
       width = 3, height = 3)


# ~~~ Percentage woodland --------------------

# Create some data
newdata_woodland <- expand.grid(woodland = seq(from = range(table.glm.0.5$woodland)[1],
                                               to = range(table.glm.0.5$woodland)[2],
                                               by = 0.025), 
                                road_importance = "major_road",
                                distance_amenity_km = mean(table.glm.0.5$distance_amenity_km),
                                distance_water_km = mean(table.glm.0.5$distance_water_km))

## add the fitted values by predicting from the model for the new data
newdata_woodland <- add_column(newdata_woodland, fit = predict(glm_nb_0.5,
                                                               newdata = newdata_woodland,
                                                               se = TRUE,
                                                               type = "response")[["fit"]])

## add fit and se.fit on the **link** scale
newdata_woodland <- bind_cols(newdata_woodland, setNames(as_tibble(predict(glm_nb_0.5, 
                                                                           newdata = newdata_woodland, 
                                                                           se.fit = TRUE)[1:2]),
                                                         c('fit_link','se_link')))
## grab the inverse link function 
ilink <- glm_nb_0.5[["family"]][["linkinv"]]

## create the interval and backtransform
newdata_woodland <- mutate(newdata_woodland,
                           fit_resp  = ilink(fit_link),
                           right_upr = ilink(fit_link + (2 * se_link)),
                           right_lwr = ilink(fit_link - (2 * se_link)))


# Plot

# To rescale the predictor
mean_sd <- read_csv(paste0("06_processed_data/glm_data_2021-09/predictor_mean_sd_", buffer.size, "m.csv"))
x <- which(mean_sd$variable == "woodland")

(plt.per.woodland <- ggplot(newdata_woodland, aes(x = woodland * mean_sd$sd[x] + mean_sd$mean[x], y = fit)) +
    geom_line() +
    geom_ribbon(data = newdata_woodland,
                aes(x = woodland* mean_sd$sd[x] + mean_sd$mean[x],
                    ymin = right_lwr, ymax = right_upr),
                alpha = 0.2) +
    theme_classic() +
    labs(x = 'Percentage of \nwoodland cover', 
         y = 'Estimated mean number \nof carcasses per segment')
  
)

ggsave(filename = paste0("07_intermediate_results/2021-04/plots/GLM/GLM_0.5_woodland (",
                         buffer.size, "m buffer).png"),
       width = 3, height = 3)



# ~~~ Figure with all 4 plots --------------------------------------------------

library(cowplot)
plot_grid(plt.road.imp, plt.dist.amenity,
          plt.dist.water, plt.per.woodland,
          labels = c("(a)", "(b)", "(c)", "(d)"),
          ncol = 2, nrow = 2)

(
  figure.GLM <- ggdraw() +
    draw_plot(plt.road.imp, x = 0.01,  y = 0.5, width = 0.49, height = 0.5) +
    draw_plot(plt.dist.amenity, x = 0.51, y = 0.5, width = 0.49, height = 0.5) +
    
    draw_plot(plt.dist.water, x = 0.01, y = 0, width = 0.49, height = 0.5) + 
    draw_plot(plt.per.woodland, x = 0.51, y = 0, width = 0.49, height = 0.5) + 
    draw_plot_label(label = c("(a)", "(b)", "(c)", "(d)"),
                    x = c(0, 0.50, 0, 0.50), y = c(1, 1, 0.5, 0.5), size = 14) 
)

save_plot("07_intermediate_results/2021-04/plots/GLM/GLM_0.5_all.png", 
          plot = figure.GLM,
          ncol = 2,
          nrow = 2,
          # base_asp = 1.618,
          base_height = 3.5,
          base_width = 3.5)



# ~~~ Frequency observed VS modeled ------------------------------

glm_nb_0.5 <- MASS::glm.nb(formula = nbr_carcasses ~ road_importance + distance_amenity_km
                           + distance_water_km + woodland,
                           data = table.glm.0.5,
                           link = log)

# Calculate the predicted frequencies of the three models

estobsk_1 <- function(k){
  sum(dnbinom(k, mu = fitted(glm_nb_0.5),
              size = glm_nb_0.5$theta))
}
# Calculate the absolute frequency of segments with 0:4 carcasses
estobs0to4 <- sapply(X = 0:4,
                     FUN = estobsk_1)
# Calculate the absolute frequency of segments with >4 carcasses
estobs5 <- length(table.glm.0.5$nbr_carcasses) - sum(estobs0to4)

estobs <- as.data.frame(append(estobs0to4, estobs5))

# Extract the observed absolute frequency
observed <- table.glm.0.5 %>%
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


ggsave(filename = paste0("07_intermediate_results/2021-04/plots/GLM/GLM_0.5_expected_(dots)_VS_observed (",
                         buffer.size, "m buffer).png"),
       width = 3, height = 3)






#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# 3. CARCASSES WITH CERTAINTY SCORES >= 0.75 -----------------------------------------------

buffer.size <- 500
buffer.size <- 1000

table.glm.0.75 <- read_csv(paste0("06_processed_data/glm_data_2021-09/table.glm_9.", 
                                  buffer.size, "m.0.75.csv"))

glm_nb_0.75<- MASS::glm.nb(formula = nbr_carcasses ~ road_importance + distance_amenity_km
                           + distance_water_km + woodland,
                           data = table.glm.0.75,
                           link = log)
summary(glm_nb_0.75)

# ~~~ Road type -----------------

# Create some data
newdata_road_importance <- expand.grid(road_importance = c("major_road", "minor_road"),
                                       distance_amenity_km = mean(table.glm.0.75$distance_amenity_km),
                                       distance_water_km = mean(table.glm.0.75$distance_water_km),
                                       woodland = mean(table.glm.0.75$woodland))

## add the fitted values by predicting from the model for the new data
newdata_road_importance <- add_column(newdata_road_importance, fit = predict(glm_nb_0.75,
                                                                             newdata = newdata_road_importance,
                                                                             se = TRUE,
                                                                             type = "response")[["fit"]])

## add fit and se.fit on the **link** scale
newdata_road_importance <- bind_cols(newdata_road_importance, setNames(as_tibble(predict(glm_nb_0.75, 
                                                                                         newdata = newdata_road_importance, 
                                                                                         se.fit = TRUE)[1:2]),
                                                                       c('fit_link','se_link')))
## grab the inverse link function 
ilink <- glm_nb_0.75[["family"]][["linkinv"]]

## create the interval and backtransform
newdata_road_importance <- mutate(newdata_road_importance,
                                  fit_resp  = ilink(fit_link),
                                  right_upr = ilink(fit_link + (2 * se_link)),
                                  right_lwr = ilink(fit_link - (2 * se_link)))


# PLOT

(plt.road.imp <- ggplot(newdata_road_importance, aes(x = road_importance, y = fit)) +
    geom_col(fill = "white",
             color = "black") +
    geom_errorbar(aes(x = newdata_road_importance[,1],
                      ymin = newdata_road_importance[,10], 
                      ymax = newdata_road_importance[,9], 
                      width=.2)) +
    theme_classic() +
    scale_x_discrete(limits = c("major_road", "minor_road"),
                     labels = c("major \nroad", "minor \nroad")) +
    labs(x = 'Road type', 
         y = 'Estimated mean number \nof carcasses per segment')
  
)

ggsave(filename = paste0("07_intermediate_results/2021-04/plots/GLM/GLM_0.75_road_importance (",
                         buffer.size, "m buffer).png"),
       width = 2, height = 3)

# ~~~ Amenity ----------------

# Create some data
newdata_distance_amenity <- expand.grid(distance_amenity_km = seq(from = range(table.glm.0.75$distance_amenity_km)[1],
                                                                  to = range(table.glm.0.75$distance_amenity_km)[2],
                                                                  by = 0.05), 
                                        road_importance = c("major_road", "minor_road"),
                                        woodland = mean(table.glm.0.75$woodland),
                                        distance_water_km = mean(table.glm.0.75$distance_water_km))

## add the fitted values by predicting from the model for the new data
newdata_distance_amenity <- add_column(newdata_distance_amenity, fit = predict(glm_nb_0.75,
                                                                               newdata = newdata_distance_amenity,
                                                                               se = TRUE,
                                                                               type = "response")[["fit"]])
## add fit and se.fit on the **link** scale
newdata_distance_amenity <- bind_cols(newdata_distance_amenity, setNames(as_tibble(predict(glm_nb_0.75, 
                                                                                           newdata = newdata_distance_amenity, 
                                                                                           se.fit = TRUE)[1:2]),
                                                                         c('fit_link','se_link')))
## grab the inverse link function 
ilink <- glm_nb_0.75[["family"]][["linkinv"]]

## create the interval and backtransform
newdata_distance_amenity <- mutate(newdata_distance_amenity,
                                   fit_resp  = ilink(fit_link),
                                   right_upr = ilink(fit_link + (2 * se_link)),
                                   right_lwr = ilink(fit_link - (2 * se_link)))


# PLOT
N <- nrow(newdata_distance_amenity)

# To rescale the predictor
mean_sd <- read_csv(paste0("06_processed_data/glm_data_2021-09/predictor_mean_sd_", buffer.size, "m.csv"))
x <- which(mean_sd$variable == "distance_amenity_km")
y <- newdata_distance_amenity$distance_amenity_km * mean_sd$sd[x] + mean_sd$mean[x]
range(y)
(plt.dist.amenity <- ggplot() +
    geom_line(data = newdata_distance_amenity ,
              aes(x = newdata_distance_amenity$distance_amenity_km * mean_sd$sd[x] + mean_sd$mean[x],
                  y = newdata_distance_amenity$fit, #[, 5],
                  linetype = road_importance)) +
    geom_ribbon(aes(x = newdata_distance_amenity$distance_amenity_km[1:(N/2)] * mean_sd$sd[x] + mean_sd$mean[x],
                    ymin = newdata_distance_amenity$right_lwr [1:(N/2)],
                    ymax = newdata_distance_amenity$right_upr[1:(N/2)]),
                alpha = 0.2) +
    geom_ribbon(aes(x = newdata_distance_amenity$distance_amenity_km[(N/2 + 1):N] * mean_sd$sd[x] + mean_sd$mean[x],
                    ymin = newdata_distance_amenity$right_lwr[(N/2 + 1):N],
                    ymax = newdata_distance_amenity$right_upr[(N/2 + 1):N]),
                alpha = 0.2) +
    theme_classic() +
    theme(legend.position = c(0.70, 0.8),
          legend.title = element_blank(),
          legend.background = element_rect(fill = "white",
                                           size = 0.5, linetype = "solid", 
                                           colour = "black")) +
    scale_linetype_discrete(labels = c("major road", "minor road")) +
    labs(x = 'Distance from the \nclosest amenity (km)', y = 'Estimated mean number \nof carcasses per segment')
) 

ggsave(filename = paste0("07_intermediate_results/2021-04/plots/GLM/GLM_0.75_amenity (",
                         buffer.size, "m buffer).png"),
       width = 3, height = 3)


# ~~~ Water --------------------

# Create some data
newdata_distance_water <- expand.grid(distance_water_km = seq(from = range(table.glm.0.75$distance_water_km)[1],
                                                              to = range(table.glm.0.75$distance_water_km)[2],
                                                              by = 0.025), 
                                      road_importance = "major_road",
                                      distance_amenity_km = mean(table.glm.0.75$distance_amenity_km),
                                      woodland = mean(table.glm.0.75$woodland))

## add the fitted values by predicting from the model for the new data
newdata_distance_water <- add_column(newdata_distance_water, fit = predict(glm_nb_0.75,
                                                                           newdata = newdata_distance_water,
                                                                           se = TRUE,
                                                                           type = "response")[["fit"]])
## add fit and se.fit on the **link** scale
newdata_distance_water <- bind_cols(newdata_distance_water, setNames(as_tibble(predict(glm_nb_0.75, 
                                                                                       newdata = newdata_distance_water, 
                                                                                       se.fit = TRUE)[1:2]),
                                                                     c('fit_link','se_link')))
## grab the inverse link function 
ilink <- glm_nb_0.75[["family"]][["linkinv"]]

## create the interval and backtransform
newdata_distance_water <- mutate(newdata_distance_water,
                                 fit_resp  = ilink(fit_link),
                                 right_upr = ilink(fit_link + (2 * se_link)),
                                 right_lwr = ilink(fit_link - (2 * se_link)))




# Plot
# To rescale the predictor
mean_sd <- read_csv(paste0("06_processed_data/glm_data_2021-09/predictor_mean_sd_", buffer.size, "m.csv"))
x <- which(mean_sd$variable == "distance_water_km")


(plt.dist.water <- ggplot(newdata_distance_water, aes(x = distance_water_km * mean_sd$sd[x] + mean_sd$mean[x], y = fit)) +
    geom_line() +
    geom_ribbon(data = newdata_distance_water,
                aes(x = distance_water_km * mean_sd$sd[x] + mean_sd$mean[x],
                    ymin = right_lwr, ymax = right_upr),
                alpha = 0.2) +
    theme_classic() +
    labs(x = 'Distance from the \nclosest water source (km)', 
         y = 'Estimated mean number \nof carcasses per segment')
  
)

ggsave(filename = paste0("07_intermediate_results/2021-04/plots/GLM/GLM_0.75_water (",
                         buffer.size, "m buffer).png"),
       width = 3, height = 3)


# ~~~ Percentage woodland --------------------

# Create some data
newdata_woodland <- expand.grid(woodland = seq(from = range(table.glm.0.75$woodland)[1],
                                               to = range(table.glm.0.75$woodland)[2],
                                               by = 0.025), 
                                road_importance = "major_road",
                                distance_amenity_km = mean(table.glm.0.75$distance_amenity_km),
                                distance_water_km = mean(table.glm.0.75$distance_water_km))

## add the fitted values by predicting from the model for the new data
newdata_woodland <- add_column(newdata_woodland, fit = predict(glm_nb_0.75,
                                                               newdata = newdata_woodland,
                                                               se = TRUE,
                                                               type = "response")[["fit"]])

## add fit and se.fit on the **link** scale
newdata_woodland <- bind_cols(newdata_woodland, setNames(as_tibble(predict(glm_nb_0.75, 
                                                                           newdata = newdata_woodland, 
                                                                           se.fit = TRUE)[1:2]),
                                                         c('fit_link','se_link')))
## grab the inverse link function 
ilink <- glm_nb_0.75[["family"]][["linkinv"]]

## create the interval and backtransform
newdata_woodland <- mutate(newdata_woodland,
                           fit_resp  = ilink(fit_link),
                           right_upr = ilink(fit_link + (2 * se_link)),
                           right_lwr = ilink(fit_link - (2 * se_link)))


# Plot

# To rescale the predictor
mean_sd <- read_csv(paste0("06_processed_data/glm_data_2021-09/predictor_mean_sd_", buffer.size, "m.csv"))
x <- which(mean_sd$variable == "woodland")

(plt.per.woodland <- ggplot(newdata_woodland, aes(x = woodland * mean_sd$sd[x] + mean_sd$mean[x], y = fit)) +
    geom_line() +
    geom_ribbon(data = newdata_woodland,
                aes(x = woodland* mean_sd$sd[x] + mean_sd$mean[x],
                    ymin = right_lwr, ymax = right_upr),
                alpha = 0.2) +
    theme_classic() +
    labs(x = 'Percentage of \nwoodland cover', 
         y = 'Estimated mean number \nof carcasses per segment')
  
)

ggsave(filename = paste0("07_intermediate_results/2021-04/plots/GLM/GLM_0.75_woodland (",
                         buffer.size, "m buffer).png"),
       width = 3, height = 3)



# ~~~ Figure with all 4 plots --------------------------------------------------

library(cowplot)
plot_grid(plt.road.imp, plt.dist.amenity,
          plt.dist.water, plt.per.woodland,
          labels = c("(a)", "(b)", "(c)", "(d)"),
          ncol = 2, nrow = 2)

(
  figure.GLM <- ggdraw() +
    draw_plot(plt.road.imp, x = 0.01,  y = 0.5, width = 0.49, height = 0.5) +
    draw_plot(plt.dist.amenity, x = 0.51, y = 0.5, width = 0.49, height = 0.5) +
    
    draw_plot(plt.dist.water, x = 0.01, y = 0, width = 0.49, height = 0.5) + 
    draw_plot(plt.per.woodland, x = 0.51, y = 0, width = 0.49, height = 0.5) + 
    draw_plot_label(label = c("(a)", "(b)", "(c)", "(d)"),
                    x = c(0, 0.50, 0, 0.50), y = c(1, 1, 0.5, 0.5), size = 14) 
)

save_plot("07_intermediate_results/2021-04/plots/GLM/GLM_0.75_all.png", 
          plot = figure.GLM,
          ncol = 2,
          nrow = 2,
          # base_asp = 1.618,
          base_height = 3.5,
          base_width = 3.5)




# ~~~ Frequency observed VS modeled ------------------------------

glm_nb_0.75 <- MASS::glm.nb(formula = nbr_carcasses ~ road_importance + distance_amenity_km
                            + distance_water_km + woodland,
                            data = table.glm.0.75,
                            link = log)

# Calculate the predicted frequencies of the three models

estobsk_1 <- function(k){
  sum(dnbinom(k, mu = fitted(glm_nb_0.75),
              size = glm_nb_0.75$theta))
}
# Calculate the absolute frequency of segments with 0:4 carcasses
estobs0to4 <- sapply(X = 0:4,
                     FUN = estobsk_1)
# Calculate the absolute frequency of segments with >4 carcasses
estobs5 <- length(table.glm.0.75$nbr_carcasses) - sum(estobs0to4)

estobs <- as.data.frame(append(estobs0to4, estobs5))

# Extract the observed absolute frequency
observed <- table.glm.0.75 %>%
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


ggsave(filename = paste0("07_intermediate_results/2021-04/plots/GLM/GLM_0.75_expected_(dots)_VS_observed (",
                         buffer.size, "m buffer).png"),
       width = 3, height = 3)