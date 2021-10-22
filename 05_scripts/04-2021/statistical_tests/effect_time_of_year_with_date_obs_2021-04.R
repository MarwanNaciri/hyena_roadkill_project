#==============================================================================#
#                                                                              #
#               Statistical tests - Effect of the time of the year             #        
#                                                                              #
#==============================================================================#


library(tidyverse)
library(lubridate)
library(ggthemes)
library(EMT)
library(RVAideMemoire)
library(ggpubr)
library(cowplot)
Sys.setenv(LANG = "en")

# A. All carcasses -------------------------------------------------------------

# ~ 1. Count the roadkills -----------------------------------------------------
hy_carcasses <- read_delim("06_processed_data/carcasses/12_hy.carcasses.certainty.formatted.spatial_updated_10-2021.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE) %>%
  mutate(death_obs_new = paste0(substr(date_obs, 1, 4), "-", substr(date_obs, 5, 6), "-", substr(date_obs, 7, 8))) %>%
  mutate(date_obs_new = as.Date(death_obs_new, format = "%Y - %m - %d")) %>%
  dplyr::select(-date_obs) %>%
  rename(date_obs = date_obs_new)


# ~~~ a. Marion's seasons ------------------------------------------------------
# start_dry <- as.Date("1989-05-26")
# end_dry <- as.Date("1989-10-31")
# nbr_days <- 365
# 
# 
# hy_carcasses_season <- hy_carcasses %>%
#   mutate(year = year(date_obs), 
#          herds_position = ifelse(date_obs %within% interval(ymd(paste0(year, "-05-26")), 
#                                                               ymd(paste0(year, "-10-31"))), 
#                                  "north west", "south east")) %>%
#   count(herds_position)

# With transit
{
  start_transit_1 <- as.Date("1989-04-26")
  end_transit_1 <- as.Date("1989-06-25")
  start_dry <- as.Date("1989-06-26")
  end_dry <- as.Date("1989-10-25")
  start_transit_2 <- as.Date("1989-10-26")
  end_transit_2 <- as.Date("1989-11-15")
  start_wet <- as.Date("1989-11-16")
  end_wet <- as.Date("1990-04-25")
  nbr_days <- 365
}

hy_carcasses_season_w_transit <- hy_carcasses %>%
  mutate(year = year(date_obs), 
         herds_position_w_transit = ifelse(date_obs %within% interval(ymd(paste0(year, "-06-26")), 
                                                                        ymd(paste0(year, "-10-25"))), 
                                           "north west", 
                                           ifelse(date_obs %within% interval(ymd(paste0(year, "-04-26")), 
                                                                               ymd(paste0(year, "-06-25"))),
                                                  "transit",
                                                  ifelse(date_obs %within% interval(ymd(paste0(year, "-10-26")), 
                                                                                      ymd(paste0(year, "-11-15"))), 
                                                         "transit", "south east")))) %>%
  filter(age %in% c("adult", "unknown", "subadult"),
         year < 2020) %>%
  count(herds_position_w_transit)



# ~~~ b. Season : Serengeti IV -------------------------------------------------

# start_dry <- as.Date("1989-08-01")
# end_dry <- as.Date("1989-10-31")
# start_wet <- as.Date("1989-12-21")
# end_wet <- as.Date("1990-05-10")
# nbr_days <- length(seq(start_dry, end_dry, by = "day")) + length(seq(start_wet, end_wet, by = "day"))
# 
# hy_carcasses_season <- hy_carcasses %>%
#   mutate(year = year(date_obs), 
#          herds_position = ifelse(date_obs %within% interval(ymd(paste0(year, "-08-01")), 
#                                                               ymd(paste0(year, "-10-31"))), 
#                                  "north west", 
#                                  ifelse(date_obs %within% interval(ymd(paste0(year, "-01-01")),
#                                                                      ymd(paste0(year, "-05-10"))),
#                                         "south east", 
#                                         ifelse(date_obs %within% interval(ymd(paste0(year, "-12-21")),
#                                                                             ymd(paste0(year, "-12-31"))),
#                                                "south east", NA)))) %>%
#   filter(age %in% c("adult", "unknown", "subadult"),
#          year < 2020) %>%
#   count(herds_position) %>%
#   filter(!is.na(herds_position))


# With transit
{
  start_transit_1 <- as.Date("1989-05-11")
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


# ~~~ c. During each month -----------------------------------------------------
hy_carcasses_month <- hy_carcasses %>%
  mutate(month = month(date_obs),
         year = year(date_obs)) %>%
  filter(year < 2020) %>% # Remove the year 2020 as it was incomplete
  count(month)

sum(hy_carcasses_month$n)



# ~ 2. Do the tests ------------------------------------------------------------

# Season (no transit)
proba <- c(length(seq(start_dry, end_dry, by = "day"))/nbr_days, 
           (nbr_days - length(seq(start_dry, end_dry, by = "day")))/nbr_days)

dbinom(x = hy_carcasses_season$n[1], 
       size = sum(hy_carcasses_season$n),
       prob = proba[1])

hy_carcasses_season_w_transit

# Season (with transit)
proba2 <- c(length(seq(start_dry, end_dry, by = "day"))/nbr_days, # North west
            # South east
            length(seq(start_wet, end_wet, by = "day"))/nbr_days,
            #Transit
            (nbr_days - length(c(seq(start_dry, end_dry, by = "day"),
                                 seq(start_wet, end_wet, by = "day"))))/nbr_days)


RVAideMemoire::multinomial.theo.multcomp(x = hy_carcasses_season_w_transit$n, 
                                         p = proba2,  
                                         prop = FALSE,
                                         p.method = "fdr")

# Season (with transit but binomial)
proba3 <- c(length(seq(start_dry, end_dry, by = "day"))/(length(seq(start_dry, end_dry, by = "day")) +
                                                           length(seq(start_wet, end_wet, by = "day"))), # North west
            # South east
            length(seq(start_wet, end_wet, by = "day"))/(length(seq(start_dry, end_dry, by = "day")) +
                                                           length(seq(start_wet, end_wet, by = "day"))))


dbinom(x = hy_carcasses_season_w_transit$n[1], 
       size = sum(hy_carcasses_season_w_transit$n[1:2]),
       prob = proba3[1])



# Month
RVAideMemoire::multinomial.theo.multcomp(x = hy_carcasses_month$n, 
                                         p = c(31/365, 28/365, 31/365, 30/365,
                                               31/365, 30/365, 31/365, 31/365,
                                               30/365, 31/365, 30/365, 31/365),  
                                         prop = FALSE,
                                         p.method = "fdr")


# ~ 3. Plot --------------------------------------------------------------------

# ~~~ b. Season -------------------------------------------------


# proba <- c(length(seq(start_dry, end_dry, by = "day"))/nbr_days, 
#            (nbr_days - length(seq(start_dry, end_dry, by = "day")))/nbr_days)
# 
# dbinom(x = hy_carcasses_season$n[1], 
#        size = sum(hy_carcasses_season$n),
#        prob = proba[1])
# 
# df_exp_VS_obs <- data.frame(type = c("observed", "observed", "expected", "expected"),
#                             herds_position = c("north_west", "south_east", "north_west", "south_east"),
#                             value = c(hy_carcasses_season$n[1], 
#                                       hy_carcasses_season$n[2],
#                                       proba[1] * sum(hy_carcasses_season$n),
#                                       proba[2] * sum(hy_carcasses_season$n)))


# Marion's or Serengeti IV's seasons (THIS IS THE ONE I USE IN THE MANUSCRIPT)
days.dry <- length(seq(start_dry, end_dry, by = "day"))
days.wet <- length(seq(start_wet, end_wet, by = "day"))
nbr.days <- days.dry + days.wet
proba <- c(days.dry/nbr.days, days.wet/nbr.days)

hy_carcasses_season_w_transit

dbinom(x = hy_carcasses_season_w_transit$n[1], 
       size = sum(hy_carcasses_season_w_transit$n[1:2]),
       prob = proba[1])

df_exp_VS_obs <- data.frame(type = c("observed", "observed", "expected", "expected"),
                            herds_position = c("north_west", "south_east", "north_west", "south_east"),
                            value = c(hy_carcasses_season_w_transit$n[1], 
                                      hy_carcasses_season_w_transit$n[2],
                                      proba[1] * sum(hy_carcasses_season_w_transit$n[1:2]),
                                      proba[2] * sum(hy_carcasses_season_w_transit$n[1:2])))


plot_seasons <- ggplot(df_exp_VS_obs, aes(x = herds_position, y = value, fill = type)) +
  geom_col(position = position_dodge(),
           color = "black",
           size = 0.5) +
  # scale_fill_grey(start = 0.8, end = 0.2) +
  scale_fill_manual(values = c("#999999", "#E69F00")) +
  theme_classic() +
  theme(legend.title = element_blank()) +
  scale_x_discrete(limits = c("south_east", "north_west"), 
                   labels = c("wet", "dry")) + #"north & \nnorth-west", "south-east")) +
  labs(y = "number of carcasses",
       x = "season",
       fill = "") 

ggsave(filename = "07_intermediate_results/2021-04/plots/exp_obs_seasons_Serengeti_IV_no_transit.svg",
       width = unit(2.5,"cm"), height = unit(3,"cm"))


# ~~~ c. During each month -----------------------------------------------------

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
  # scale_fill_grey(start = 0.8, end = 0.2) +
  scale_fill_manual(limits = c("south_east", "north_west", "transit"),
                    values = c("#336600", "#E69F00", "#333333"),
                    labels = c("south \neast", "north \n& MMR", "transit")) +  # c("north and north-west", "south-east", "in transit")) +
  theme_classic() +
  scale_x_discrete(limits = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  labs(x = "month",
       y = "number of carcasses",
       fill = "wildebeest herds' position") + # position of the wildebeest herds
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        # legend.title = element_blank(),
        axis.text.x = element_text(angle = 60, hjust = 1)) +
   guides(fill = guide_legend(title.position = "top", title.hjust = 0.5))
)
# ggsave(filename = "07_intermediate_results/2021-04/plots/months.svg",
#        width = unit(4,"cm"), height = unit(2.5,"cm"))
# 
# plot_effect_time <- ggarrange(plot_month, plot_seasons, 
#                               ncol = 2, nrow = 1,  align = "hv", 
#                               widths = c(3.5, 2.15),
#                               common.legend = FALSE)


# ~~~ d. Plot the whole figure -------------------------------------------------

hy_carcasses_herds_position <- read_delim("06_processed_data/carcasses/12_hy.carcasses.certainty.formatted.spatial_updated_10-2021.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE) %>%
  mutate(death_obs_new = paste0(substr(date_obs, 1, 4), "-", substr(date_obs, 5, 6), "-", substr(date_obs, 7, 8))) %>%
  mutate(date_obs_new = as.Date(death_obs_new, format = "%Y - %m - %d")) %>%
  dplyr::select(-date_obs) %>%
  rename(date_obs = date_obs_new) %>%
  filter(GPS_certainty_score > 0) %>%
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
  dplyr::select(ID, date_obs, herds_position, long, lat, collision_certainty_score_NEW, GPS_certainty_score, age, sex)

hy_carcasses_herds_position_ad <- hy_carcasses_herds_position %>%
  filter(age %in% c("adult", "unknown", "subadult"))

roads.ggplot.map <- read_csv("06_processed_data/glm_data_2021-09/roads.segmented.cropped.no.short.df.csv") %>%
  filter(long > 34.6)

# With map + violin plots
(long_lat_scatter <- ggplot() +
    geom_path(data = roads.ggplot.map, aes(x = long, y = lat, group = ID_road_seg),
              size = 0.75) +
    geom_point(data = hy_carcasses_herds_position_ad, aes(x = long, y = lat, color = herds_position),
               size = 2) +
    theme_classic() + #theme_minimal() + 
    scale_color_manual(limits = c("south east", "north west"),
                       labels = c("south east", "north & MMR"),
                       values = c("#336600", "#E69F00")) +
    theme(legend.position = c(0.28, 0.88),
          legend.background = element_blank()) + #  element_blank()) +
          # legend.background = element_rect(fill = "white",
          #                                  size = 0.5, linetype = "solid",
          #                                  colour = "black")) +
    labs(y = "latitude", 
         x = "longitude",
         color = "wildebeest herds'\nposition")
  
)

(xlong_violin <- ggplot(hy_carcasses_herds_position_ad, 
                        aes(x = herds_position, y = long, fill = herds_position)) +
    geom_violin(size = 0.5, color = "black") +
    geom_boxplot(width=0.1, size = 0.5, color = "black") +
    scale_fill_manual(limits = c("south east", "north west"),
                      values = c("#336600", "#E69F00")) +
    # scale_x_discrete(limits = c("south east", "north west"),
    #                  labels = c("wet", "dry")) +
    scale_x_discrete(limits = c("south east", "north west"),
                     labels = c("late Dec -\nearly May", "Aug - Oct")) +
    theme_classic() +
    theme(axis.title.x = element_blank(),
          legend.position = "") +
    labs(x = "period", # "Position of the \nmigratory herds",
         fill = "") +
    ylim(34.6, 35.2) +
    coord_flip()
)


(ylat_violin <- ggplot(hy_carcasses_herds_position_ad, 
                       aes(x = herds_position, y = lat, fill = herds_position)) +
    geom_violin(size = 0.5, color = "black") +
    geom_boxplot(width=0.1, size = 0.5, color = "black") +
    scale_fill_manual(limits = c("south east", "north west"),
                      values = c("#336600", "#E69F00")) +
    scale_x_discrete(limits = c("south east", "north west"),
                     labels = c("late Dec -\nearly May", "Aug - Oct")) +
    scale_y_continuous(breaks = c(-3, -2.7, -2.4, -2.1),
                       limits = c(-3.04, -1.9)) +
    theme_classic() + 
    theme(axis.title.y = element_blank()) +
    #       axis.text.x = element_blank(),
    theme(legend.position = "") +
    labs(x = "period", # "Position of the \nmigratory herds",
         fill = "") 
)

(plot_position_herds <- ggarrange(xlong_violin, NULL, long_lat_scatter, ylat_violin,
                                  ncol = 2, nrow = 2,  align = "hv",
                                  widths = c(3.5, 2.15), heights = c(2, 5),
                                  common.legend = FALSE)
)

# _______INTERVENTION_______________________________________________________####
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

(long_lat_scatter <- ggplot() +
    geom_path(data = roads.ggplot.map, aes(x = long, y = lat, group = ID_road_seg),
              size = 0.75) +
    geom_point(data = hy_carcasses_herds_position_ad, aes(x = long, y = lat, color = herds_position),
               size = 2) +
    theme_classic() + #theme_minimal() + 
    scale_color_manual(limits = c("south east", "north west"),
                       labels = c("south east", "north & MMR"),
                       values = c("#336600", "#E69F00")) +
    theme(legend.position = "top",
          legend.direction = "horizontal") +
    labs(y = "latitude", 
         x = "longitude",
         color = "wildebeest herds'position") +
    xlim(34.6, 35.3) 
   #  guides(color = guide_legend(title.position = "top", title.hjust = 0.5))
)

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
                                     ymin = ymin - 0.1, ymax = ymax + 0.1) +
  annotation_custom(grob = xlong_violin_grob, 
                    xmin = xmin - 0.1, xmax = xmax + 0.1,
                    # ymin = ymax - 0.25, ymax = ymax + 0.05)
                    ymin = ymax - 0.4, ymax = ymax + 0.05)
  
(figure <- ggdraw() +
    draw_plot(plot_seasons, x = 0.04, y = 0, width = 0.37, height = 0.3) + 
    draw_plot(plot_month, x = 0.04, y = 0.29, width = 0.34, height = 0.35) + 
    draw_plot(long_lat_scatter_complete, x = 0.39,  y = 0, width = 0.61, height = 0.73) +
    draw_plot_label(label = c("(a)", "(b)", "(c)"),
                    x = c(0, 0, 0.36), y = c(0.67, 0.3, 0.67), size = 14) 
  
)

save_plot("11_manuscript/V3 Figures/figure 3 complete test 2.png", 
          plot = figure,
          ncol = 2,
          nrow = 2,
          base_asp = 1)

# __________________________________________________________________________####


# Temporal pattern above spatiotemporal
# (figure <- ggdraw() +
#     draw_plot(plot_month, x = 0.05, y = 0.6, width = 0.63, height = 0.25) + 
#     draw_plot(plot_seasons, x = 0.70, y = 0.57, width = 0.30, height = 0.28) + 
#     draw_plot(plot_position_herds, x = 0.02,  y = 0.05, width = 1, height = 0.55) +
#     draw_plot_label(label = c("(a)", "(b)", "(c)"),
#                     x = c(0, 0.67, 0), y = c(0.87, 0.87, 0.62), size = 14) 
#   
# )
# save_plot("07_intermediate_results/2021-04/plots/temporal_spatiotemporal.png", 
#           plot = figure,
#           ncol = 2,
#           nrow = 2,
#           base_asp = 1)

# Temporal pattern next to spatiotemporal
(figure <- ggdraw() +
    draw_plot(plot_seasons, x = 0.04, y = 0, width = 0.35, height = 0.3) + 
    draw_plot(plot_month, x = 0.04, y = 0.29, width = 0.35, height = 0.35) + 
    draw_plot(plot_position_herds, x = 0.39,  y = 0, width = 0.61, height = 0.65) +
    draw_plot_label(label = c("(a)", "(b)", "(c)"),
                    x = c(0, 0, 0.36), y = c(0.67, 0.3, 0.67), size = 14) 
  
)

save_plot("11_manuscript/V3 Figures/figure 3 raw.png", 
          plot = figure,
          ncol = 2,
          nrow = 2,
          base_asp = 1)



# ggsave(filename = "07_intermediate_results/2021-04/plots/migratory_herds_adults+subadults_Serengeti IV.svg",
#        width = unit(6.48,"cm"), height = unit(8.1,"cm"))

# B. Carcasses >= 0.5 ----------------------------------------------------------

# ~ 1. Count the roadkills -----------------------------------------------------
hy_carcasses_0.5 <- read_delim("06_processed_data/carcasses/12_hy.carcasses.certainty.formatted.spatial_updated_10-2021.csv", 
                               ";", escape_double = FALSE, trim_ws = TRUE) %>%
  mutate(date_obs = as.Date(date_obs, format = "%d / %m / %Y")) %>%
  filter(collision_certainty_score_NEW >= 0.5)


# ~~~ a. Marion's seasons ------------------------------------------------------

start_dry <- as.Date("1989-05-26")
end_dry <- as.Date("1989-10-31")
nbr_days <- 365

hy_carcasses_season_0.5 <- hy_carcasses_0.5 %>%
  mutate(year = year(date_obs), 
         herds_position = ifelse(date_obs %within% interval(ymd(paste0(year, "-05-26")), 
                                                              ymd(paste0(year, "-10-31"))), 
                                 "north west", "south east")) %>%
  count(herds_position)


# With transit
{
  start_transit_1 <- as.Date("1989-04-26")
  end_transit_1 <- as.Date("1989-06-25")
  start_dry <- as.Date("1989-06-26")
  end_dry <- as.Date("1989-10-31")
  start_transit_2 <- as.Date("1989-10-26")
  end_transit_2 <- as.Date("1989-11-15")
  start_wet <- as.Date("1989-11-16")
  end_wet <- as.Date("1990-04-25")
  nbr_days <- 365
  }
hy_carcasses_season_w_transit_0.5 <- hy_carcasses_0.5 %>%
  mutate(year = year(date_obs), 
         herds_position_w_transit = ifelse(date_obs %within% interval(ymd(paste0(year, "-06-26")), 
                                                                        ymd(paste0(year, "-10-25"))), 
                                           "north west", 
                                           ifelse(date_obs %within% interval(ymd(paste0(year, "-04-26")), 
                                                                               ymd(paste0(year, "-06-25"))),
                                                  "transit",
                                                  ifelse(date_obs %within% interval(ymd(paste0(year, "-10-26")), 
                                                                                      ymd(paste0(year, "-11-15"))), 
                                                         "transit", "south east")))) %>%
  count(herds_position_w_transit)



# ~~~ b. Season : Serengeti IV -------------------------------------------------

start_dry <- as.Date("1989-08-01")
end_dry <- as.Date("1989-10-31")
start_wet <- as.Date("1989-12-21")
end_wet <- as.Date("1990-05-10")
nbr_days <- length(seq(start_dry, end_dry, by = "day")) + length(seq(start_wet, end_wet, by = "day"))

hy_carcasses_season_0.5 <- hy_carcasses_0.5 %>%
  mutate(year = year(date_obs), 
         herds_position = ifelse(date_obs %within% interval(ymd(paste0(year, "-08-01")), 
                                                              ymd(paste0(year, "-10-31"))), 
                                 "north west", 
                                 ifelse(date_obs %within% interval(ymd(paste0(year, "-01-01")),
                                                                     ymd(paste0(year, "-05-10"))),
                                        "south east", 
                                        ifelse(date_obs %within% interval(ymd(paste0(year, "-12-21")),
                                                                            ymd(paste0(year, "-12-31"))),
                                               "south east", NA)))) %>%
  count(herds_position) %>%
  filter(!is.na(herds_position))


# ~~~ c. During each month -----------------------------------------------------
hy_carcasses_month_0.5 <- hy_carcasses_0.5 %>%
  mutate(month = month(date_obs),
         year = year(date_obs)) %>%
  filter(year < 2020) %>%
  count(month)

sum(hy_carcasses_month_0.5$n)



# ~ 2. Do the tests ------------------------------------------------------------

# Season (no transit)
proba <- c(length(seq(start_dry, end_dry, by = "day"))/nbr_days, 
           (nbr_days - length(seq(start_dry, end_dry, by = "day")))/nbr_days)

dbinom(x = hy_carcasses_season_0.5$n[1], 
       size = sum(hy_carcasses_season_0.5$n),
       prob = proba[1])


# Season (with transit)
proba2 <- c(length(seq(start_dry, end_dry, by = "day"))/nbr_days, # North west
            # South east
            length(seq(start_wet, end_wet, by = "day"))/nbr_days,
            #Transit
            (nbr_days - length(c(seq(start_dry, end_dry, by = "day"),
                                 seq(start_wet, end_wet, by = "day"))))/nbr_days)


RVAideMemoire::multinomial.theo.multcomp(x = hy_carcasses_season_w_transit_0.5$n, 
                                         p = proba2,  
                                         prop = FALSE,
                                         p.method = "fdr")




# Month
RVAideMemoire::multinomial.theo.multcomp(x = hy_carcasses_month$n, 
                                         p = c(31/365, 28/365, 31/365, 30/365,
                                               31/365, 30/365, 31/365, 31/365,
                                               30/365, 31/365, 30/365, 31/365),  
                                         prop = FALSE,
                                         p.method = "fdr")



# C. Carcasses >= 0.75 ----------------------------------------------------------

# ~ 1. Count the roadkills -----------------------------------------------------
hy_carcasses_0.75 <- read_delim("06_processed_data/carcasses/12_hy.carcasses.certainty.formatted.spatial_updated_10-2021.csv", 
                                ";", escape_double = FALSE, trim_ws = TRUE) %>%
  mutate(death_obs_new = paste0(substr(date_obs, 1, 4), "-", substr(date_obs, 5, 6), "-", substr(date_obs, 7, 8))) %>%
  mutate(date_obs_new = as.Date(death_obs_new, format = "%Y - %m - %d")) %>%
  dplyr::select(-date_obs) %>%
  rename(date_obs = date_obs_new) %>%
  filter(collision_certainty_score_NEW >= 0.75)


# ~~~ a. Marion's seasons ------------------------------------------------------

start_dry <- as.Date("1989-05-26")
end_dry <- as.Date("1989-10-31")
nbr_days <- 365

hy_carcasses_season_0.75 <- hy_carcasses_0.75 %>%
  mutate(year = year(date_obs), 
         herds_position = ifelse(date_obs %within% interval(ymd(paste0(year, "-05-26")), 
                                                              ymd(paste0(year, "-10-31"))), 
                                 "north west", "south east")) %>%
  count(herds_position)


# With transit
{
  start_transit_1 <- as.Date("1989-04-26")
  end_transit_1 <- as.Date("1989-06-25")
  start_dry <- as.Date("1989-06-26")
  end_dry <- as.Date("1989-10-31")
  start_transit_2 <- as.Date("1989-10-26")
  end_transit_2 <- as.Date("1989-11-15")
  start_wet <- as.Date("1989-11-16")
  end_wet <- as.Date("1990-04-25")
  nbr_days <- 365
  }
hy_carcasses_season_w_transit_0.75 <- hy_carcasses_0.75 %>%
  mutate(year = year(date_obs), 
         herds_position_w_transit = ifelse(date_obs %within% interval(ymd(paste0(year, "-06-26")), 
                                                                        ymd(paste0(year, "-10-25"))), 
                                           "north west", 
                                           ifelse(date_obs %within% interval(ymd(paste0(year, "-04-26")), 
                                                                               ymd(paste0(year, "-06-25"))),
                                                  "transit",
                                                  ifelse(date_obs %within% interval(ymd(paste0(year, "-10-26")), 
                                                                                      ymd(paste0(year, "-11-15"))), 
                                                         "transit", "south east")))) %>%
  count(herds_position_w_transit)



# ~~~ b. Season : Serengeti IV -------------------------------------------------

start_dry <- as.Date("1989-08-01")
end_dry <- as.Date("1989-10-31")
start_wet <- as.Date("1989-12-21")
end_wet <- as.Date("1990-05-10")
nbr_days <- length(seq(start_dry, end_dry, by = "day")) + length(seq(start_wet, end_wet, by = "day"))

hy_carcasses_season_0.75 <- hy_carcasses_0.75 %>%
  mutate(year = year(date_obs), 
         herds_position = ifelse(date_obs %within% interval(ymd(paste0(year, "-08-01")), 
                                                              ymd(paste0(year, "-10-31"))), 
                                 "north west", 
                                 ifelse(date_obs %within% interval(ymd(paste0(year, "-01-01")),
                                                                     ymd(paste0(year, "-05-10"))),
                                        "south east", 
                                        ifelse(date_obs %within% interval(ymd(paste0(year, "-12-21")),
                                                                            ymd(paste0(year, "-12-31"))),
                                               "south east", NA)))) %>%
  filter(age %in% c("adult", "unknown", "subadult")) %>%
  count(herds_position) %>%
  filter(!is.na(herds_position))


# ~~~ c. During each month -----------------------------------------------------
hy_carcasses_month_0.75 <- hy_carcasses_0.75 %>%
  mutate(month = month(date_obs),
         year = year(date_obs)) %>%
  filter(year < 2020) %>%
  count(month)

sum(hy_carcasses_month_0.75$n)



# ~ 2. Do the tests ------------------------------------------------------------

# Season (no transit)
proba <- c(length(seq(start_dry, end_dry, by = "day"))/nbr_days, 
           (nbr_days - length(seq(start_dry, end_dry, by = "day")))/nbr_days)

dbinom(x = hy_carcasses_season_0.75$n[1], 
       size = sum(hy_carcasses_season_0.75$n),
       prob = proba[1])


# Season (with transit)
proba2 <- c(length(seq(start_dry, end_dry, by = "day"))/nbr_days, # North west
            # South east
            length(seq(start_wet, end_wet, by = "day"))/nbr_days,
            #Transit
            (nbr_days - length(c(seq(start_dry, end_dry, by = "day"),
                                 seq(start_wet, end_wet, by = "day"))))/nbr_days)


RVAideMemoire::multinomial.theo.multcomp(x = hy_carcasses_season_w_transit_0.75$n, 
                                         p = proba2,  
                                         prop = FALSE,
                                         p.method = "fdr")




# Month
RVAideMemoire::multinomial.theo.multcomp(x = hy_carcasses_month$n, 
                                         p = c(31/365, 28/365, 31/365, 30/365,
                                               31/365, 30/365, 31/365, 31/365,
                                               30/365, 31/365, 30/365, 31/365),  
                                         prop = FALSE,
                                         p.method = "fdr")








## Number of WVCs over 5-years periods or decades ----------------------

hy_carcasses_formatted <- read_delim("6_processed_data/carcasses/12_hy.carcasses.certainty.formatted.spatial.csv", 
                                     ";", escape_double = FALSE, trim_ws = TRUE)

hy_carcasses_formatted$date_obs <- as.Date(hy_carcasses_formatted$date_obs, 
                                             format = "%d / %m / %Y")

hy.carcasses.decade <- hy_carcasses_formatted %>%
  mutate(year = lubridate::year(date_obs),
         temp = floor_date(date_obs, years(10)),
         decade = lubridate::year(temp)) %>%
  filter(decade %in% c("1990", "2000", "2010")) %>%
  dplyr::select(ID, long, lat, collision_certainty_score_NEW, GPS_certainty_score, age, sex, year, decade) %>%
  mutate(years_by_five = ifelse(year >= 1990 & year <1995, "1990-1994",
                                ifelse(year >=1995 & year <2000, "1995-1999",
                                       ifelse(year >=2000 & year <2005, "2000-2004", 
                                              ifelse(year >=2005 & year <2010, "2005-2009",
                                                     ifelse(year >=2010 & year <2015, "2010-2015", "2015-2019"))))))


counts.decade <- hy.carcasses.decade %>%
  count(decade)

counts.by.five <- hy.carcasses.decade %>%
  count(years_by_five)

ggplot(data = counts.by.five, aes(x = years_by_five, y = n)) +
  geom_point()

model <- lm(formula = n ~ years_by_five,
            data = counts.by.five)
summary(model)

counts.decade <- c(as.numeric(counts.decade[1,2]), as.numeric(counts.decade[2,2]), 
                   as.numeric(counts.decade[3,2]))

proba <- c(1/3, 1/3, 1/3)
EMT::multinomial.test(counts.decade, proba , useChisq = TRUE)

RVAideMemoire::multinomial.theo.multcomp(x = counts.decade, 
                                         p = proba,  
                                         prop = FALSE,
                                         p.method = "fdr")
