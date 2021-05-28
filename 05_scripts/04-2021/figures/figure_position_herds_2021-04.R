#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#                                                                              #
#              Figure effect of the position of the migratory herds            #        
#                                                                              #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

library(tidyverse)
library(ggthemes)
library(ggpubr)
library(raster)
# library(viridis)
Sys.setenv(LANG = "en")

##################################################################################
# A. For all carcasses ------------------------------------------------------------

hy_carcasses_formatted <- read_delim("6_processed_data/carcasses/12_hy.carcasses.certainty.formatted.spatial_updated_04-2021.csv", 
                                     ";", escape_double = FALSE, trim_ws = TRUE)

hy_carcasses_formatted_spatial <- hy_carcasses_formatted %>%
  filter(GPS_certainty_score > 0) %>%
  mutate(date_death = as.Date(date_death, format = "%d / %m / %Y"),
         year = year(date_death))

# Add variable "position of the migratory herds"
hy.carcasses.herds.position <- hy_carcasses_formatted_spatial %>%
  mutate(herds_position = ifelse(date_death %within% interval(ymd(paste0(year, "-05-26")), 
                                                              ymd(paste0(year, "-10-31"))), 
                                 "north west", "south east")) %>%
  dplyr::select(ID, date_death, herds_position, long, lat, collision_certainty_score_NEW, GPS_certainty_score, age, sex)

hy.carcasses.herds.position.ad <- hy.carcasses.herds.position %>%
  filter(age %in% c("adult", "unknown", "subadult"))


roads.ggplot.map <- read_csv("7_intermediate_results/plots/Migratory_herds/roads.map.ggplot2.csv", 
                             col_types = cols(X1 = col_skip()))


# With map + violin plots
(long_lat_scatter <- ggplot() +
    geom_line(data = roads.ggplot.map, aes( x = long, y = lat, group = ID_road_seg),
              size = 0.75) +
    geom_point(data = hy.carcasses.herds.position.ad, aes(x = long, y = lat, color = herds_position),
               size = 3) +
    theme_classic() + #theme_minimal() + 
    scale_color_manual(limits = c("south east", "north west"),
                       values = c("#336600", "#E69F00")) +
    ylab("Latitude") +
    xlab("Longitude")
)

(xlong_violin <- ggplot(hy.carcasses.herds.position.ad, 
                        aes(x = herds_position, y = long, fill = herds_position)) +
    geom_violin(size = 0.5, color = "black") +
    geom_boxplot(width=0.1, size = 0.5, color = "black") +
    scale_fill_manual(limits = c("south east", "north west"),
                       values = c("#336600", "#E69F00")) +
    scale_x_discrete(limits = c("south east", "north west")) +
    theme_classic() +
    theme(axis.title.x = element_blank(),
          axis.text.y = element_blank()) +
    xlab("Position of the \nmigratory herds") +
    ylim(34.65, 35.14) +
    coord_flip()
)



(ylat_violin <- ggplot(hy.carcasses.herds.position.ad, 
                       aes(x = herds_position, y = lat, fill = herds_position)) +
    geom_violin(size = 0.5, color = "black") +
    geom_boxplot(width=0.1, size = 0.5, color = "black") +
    scale_fill_manual(limits = c("south east", "north west"),
                       values = c("#336600", "#E69F00")) +
    scale_x_discrete(limits = c("south east", "north west")) +
    theme_classic() + 
    theme(axis.title.y = element_blank(),
          axis.text.x = element_blank()) +
    xlab("Position of the migratory herds") +
    ylim(-2.995, -2.005)
)

ggarrange(xlong_violin, NULL, long_lat_scatter, ylat_violin, 
          ncol = 2, nrow = 2,  align = "hv", 
          widths = c(3.5, 2.15), heights = c(2,6),
          common.legend = TRUE)

ggsave("10-24_map-violinplot.adults+subadults.svg", path = "7_intermediate_results/plots/Migratory_herds",
       width = unit(6.48,"cm"), height = unit(8.1,"cm"))





#+++++++++++++++++++++--------------
# With colorblind colors
(long_lat_scatter <- ggplot() +
   geom_line(data = roads.ggplot.map, aes( x = long, y = lat, group = ID_road_seg),
             size = 0.75) +
   geom_point(data = hy.carcasses.season.ad, aes(x = long, y = lat, color = season),
              size = 3) +
   theme_classic() + #theme_minimal() + 
   scale_color_manual(limits = c("wet season", "wet to dry", "dry season", "dry to wet"),
                      values = c("#018571", "#80cdc1", "#a6611a", "#dfc27d")) + # values = c("#336600", "#9a9c00", "#E69F00", "#997800")) +
   ylab("Latitude") +
   xlab("Longitude")
)

(xlong_violin <- ggplot(hy.carcasses.season.ad, aes(x = season, y = long, fill = season)) +
    geom_violin(size = 0.5, color = "black") +
    geom_boxplot(width=0.1, size = 0.5, color = "black") +
    scale_fill_manual(limits = c("wet season", "wet to dry", "dry season", "dry to wet"),
                      values = c("#018571", "#80cdc1", "#a6611a", "#dfc27d")) + # values = c("#336600", "#9a9c00", "#E69F00", "#997800")) +
    scale_x_discrete(limits = c("wet season", "wet to dry", "dry season", "dry to wet")) +
    theme_classic() +
    theme(axis.title.x = element_blank(),
          axis.text.y = element_blank()) +
    xlab("Season") +
    ylim(34.65, 35.14) +
    coord_flip()
)

(ylat_violin <- ggplot(hy.carcasses.season.ad, aes(x = season, y = lat, fill = season)) +
    geom_violin(size = 0.5, color = "black") +
    geom_boxplot(width=0.1, size = 0.5, color = "black") +
    scale_fill_manual(limits = c("wet season", "wet to dry", "dry season", "dry to wet"),
                      values = c("#018571", "#80cdc1", "#a6611a", "#dfc27d")) + # values = c("#336600", "#9a9c00", "#E69F00", "#997800")) +
    scale_x_discrete(limits = c("wet season", "wet to dry", "dry season", "dry to wet")) +
    theme_classic() + 
    theme(axis.title.y = element_blank(),
          axis.text.x = element_blank()) +
    xlab("Season") +
    ylim(-2.995, -2.005)
)

ggarrange(xlong_violin, NULL, long_lat_scatter, ylat_violin, 
          ncol = 2, nrow = 2,  align = "hv", 
          widths = c(3.5, 2.15), heights = c(2,6),
          common.legend = TRUE)

ggsave("02-01.test.map-violinplot.adults+subadults.svg", path = "7_intermediate_results/plots/Migratory_herds",
       width = unit(6.48,"cm"), height = unit(8.1,"cm"))
#+++++++++++++++++++++--------------










# B. For carcasses >=0.5 ----------------------------------------------------------

X0_hy_carcasses_formatted <- read_delim("6_processed_data/carcasses/0_hy.carcasses.formatted.csv", 
                                        ";", escape_double = FALSE, trim_ws = TRUE)
hy_carcasses_formatted_spatial_0.5 <- X0_hy_carcasses_formatted %>%
  filter (GPS_certainty_score >= 0.5,
          collision_certainty_score_NEW >= 0.5)

hy_carcasses_formatted_spatial_0.5$date_death <- as.Date(hy_carcasses_formatted_spatial_0.5$date_death, 
                                                         format = "%d / %m / %Y")

# Create a variable, the season, by extracting the month from the date.
# Version with 4 seasons (wet, wet-to-dry, dry, dry-to-wet)
hy.carcasses.season.0.5 <- hy_carcasses_formatted_spatial_0.5 %>%
  mutate(season = ifelse(data.frame(format(as.Date(date_death), "%m")) == as.character("01"), "wet season", # if the number of the month is 1 (i.e. january), 
                         # add wet season to the column season
                         ifelse(data.frame(format(as.Date(date_death), "%m")) == as.character("02"), "wet season",
                                ifelse(data.frame(format(as.Date(date_death), "%m")) == as.character("03"), "wet season",
                                       ifelse(data.frame(format(as.Date(date_death), "%m")) == as.character("04"), "wet season",
                                              ifelse(data.frame(format(as.Date(date_death), "%m")) == as.character("05"), "wet to dry",
                                                     ifelse(data.frame(format(as.Date(date_death), "%m")) == as.character("06"), "wet to dry",
                                                            ifelse(data.frame(format(as.Date(date_death), "%m")) == as.character("11"), "dry to wet",
                                                                   ifelse(data.frame(format(as.Date(date_death), "%m")) == as.character("12"), "dry to wet", "dry season"))))))))) %>%
  # filter(season != "dry_to_wet") %>%
  # filter(season != "wet_to_dry") %>%
  dplyr::select(ID, season, long, lat, collision_certainty_score_NEW, GPS_certainty_score, age, sex) 

hy.carcasses.season.ad.0.5 <- hy.carcasses.season.0.5 %>%
  filter(age %in% c("adult", "Y", "subadult"))


## Figure with map-violin plot
roads.ggplot.map <- read_csv("7_intermediate_results/plots/Migratory_herds/roads.map.ggplot2.csv", 
                             col_types = cols(X1 = col_skip()))

(long_lat_scatter <- ggplot() +
    geom_line(data = roads.ggplot.map, aes( x = long, y = lat, group = ID_road_seg),
              size = 0.75) +
    geom_point(data = hy.carcasses.season.ad.0.5, aes(x = long, y = lat, color = season),
               size = 3) +
    theme_classic() + #theme_minimal() + 
    scale_color_manual(limits = c("wet season", "wet to dry", "dry season", "dry to wet"),
                       values = c("#336600", "#9a9c00", "#E69F00", "#997800")) +
    ylab("Latitude") +
    xlab("Longitude")
)

(xlong_violin <- ggplot(hy.carcasses.season.ad.0.5, aes(x = season, y = long, fill = season)) +
    geom_violin(size = 0.5, color = "black") +
    geom_boxplot(width=0.1, size = 0.5, color = "black") +
    scale_fill_manual(limits = c("wet season", "wet to dry", "dry season", "dry to wet"),
                      values = c("#336600", "#9a9c00", "#E69F00", "#997800")) +
    scale_x_discrete(limits = c("wet season", "wet to dry", "dry season", "dry to wet")) +
    theme_classic() +
    theme(axis.title.x = element_blank(),
          axis.text.y = element_blank()) +
    xlab("Season") +
    ylim(34.65, 35.14) +
    coord_flip()
)

(ylat_violin <- ggplot(hy.carcasses.season.ad.0.5, aes(x = season, y = lat, fill = season)) +
    geom_violin(size = 0.5, color = "black") +
    geom_boxplot(width=0.1, size = 0.5, color = "black") +
    scale_fill_manual(limits = c("wet season", "wet to dry", "dry season", "dry to wet"),
                      values = c("#336600", "#9a9c00", "#E69F00", "#997800")) +
    scale_x_discrete(limits = c("wet season", "wet to dry", "dry season", "dry to wet")) +
    theme_classic() + 
    theme(axis.title.y = element_blank(),
          axis.text.x = element_blank()) +
    xlab("Season") +
    ylim(-2.995, -2.005)
)

ggarrange(xlong_violin, NULL, long_lat_scatter, ylat_violin, 
          ncol = 2, nrow = 2,  align = "hv", 
          widths = c(3.5, 2.15), heights = c(2, 6),
          common.legend = TRUE)

ggsave("08-28_0.5.map-violinplot.adults+subadults.svg", path = "7_intermediate_results/plots/Migratory_herds",
       width = unit(6.48,"cm"), height = unit(8.1,"cm"))





# C.For carcasses >=0.75 ---------------------------------------------------------


hy_carcasses_formatted_spatial_0.75 <- read_delim("6_processed_data/carcasses/0_hy.carcasses.formatted.csv", 
                                                  ";", escape_double = FALSE, trim_ws = TRUE) %>%
  filter (GPS_certainty_score >= 0.75,
          collision_certainty_score_NEW >= 0.75) %>%
  mutate(date_death <- as.Date(date_death, format = "%d / %m / %Y"))


# Create a variable, the season, by extracting the month from the date.
# Version with 4 seasons (wet, wet-to-dry, dry, dry-to-wet)
hy.carcasses.season.0.75 <- hy_carcasses_formatted_spatial_0.75 %>%
  mutate(season = ifelse(data.frame(format(as.Date(date_death), "%m")) == as.character("01"), "wet season", # if the number of the month is 1 (i.e. january), 
                         # add wet season to the column season
                         ifelse(data.frame(format(as.Date(date_death), "%m")) == as.character("02"), "wet season",
                                ifelse(data.frame(format(as.Date(date_death), "%m")) == as.character("03"), "wet season",
                                       ifelse(data.frame(format(as.Date(date_death), "%m")) == as.character("04"), "wet season",
                                              ifelse(data.frame(format(as.Date(date_death), "%m")) == as.character("05"), "wet to dry",
                                                     ifelse(data.frame(format(as.Date(date_death), "%m")) == as.character("06"), "wet to dry",
                                                            ifelse(data.frame(format(as.Date(date_death), "%m")) == as.character("11"), "dry to wet",
                                                                   ifelse(data.frame(format(as.Date(date_death), "%m")) == as.character("12"), "dry to wet", "dry season"))))))))) %>%
  # filter(season != "dry_to_wet") %>%
  # filter(season != "wet_to_dry") %>%
  dplyr::select(ID, season, long, lat, collision_certainty_score_NEW, GPS_certainty_score, age, sex)

hy.carcasses.season.ad.0.75 <- hy.carcasses.season.0.75 %>%
  filter(age %in% c("adult", "Y", "subadult"))


# With violin plots instead of boxplot + density plot

roads.ggplot.map <- read_csv("7_intermediate_results/plots/Migratory_herds/roads.map.ggplot2.csv", 
                             col_types = cols(X1 = col_skip()))

(long_lat_scatter.0.75 <- ggplot() +
    geom_line(data = roads.ggplot.map, aes(x = long, y = lat, group = ID_road_seg),
              size = 0.75) +
    geom_point(data = hy.carcasses.season.ad.0.75, aes(x = long, y = lat, color = season),
               size = 3) +
    theme_classic() + #theme_minimal() + 
    scale_color_manual(limits = c("wet season", "wet to dry", "dry season", "dry to wet"),
                       values = c("#018571", "#80cdc1", "#a6611a", "#dfc27d")) + # values = c("#336600", "#9a9c00", "#E69F00", "#997800")) +
    ylab("Latitude") +
    xlab("Longitude")
)


(xlong_violin.0.75 <- ggplot(hy.carcasses.season.ad.0.75, aes(x = season, y = long, fill = season)) +
    geom_violin(size = 0.5, color = "black") +
    geom_boxplot(width=0.1, size = 0.5, color = "black") +
    scale_fill_manual(limits = c("wet season", "wet to dry", "dry season", "dry to wet"),
                      values = c("#018571", "#80cdc1", "#a6611a", "#dfc27d")) + # values = c("#336600", "#9a9c00", "#E69F00", "#997800")) +
    scale_x_discrete(limits = c("wet season", "wet to dry", "dry season", "dry to wet")) +
    theme_classic() +
    theme(axis.title.x = element_blank(),
          axis.text.y = element_blank()) +
    ylim(34.65, 35.14) +
    xlab("Season") +
    coord_flip()
)

(ylat_violin.0.75 <- ggplot(hy.carcasses.season.ad.0.75, aes(x = season, y = lat, fill = season)) +
    geom_violin(size = 0.5, color = "black") +
    geom_boxplot(width=0.1, size = 0.5, color = "black") +
    scale_fill_manual(limits = c("wet season", "wet to dry", "dry season", "dry to wet"),
                      values = c("#018571", "#80cdc1", "#a6611a", "#dfc27d")) + # values = c("#336600", "#9a9c00", "#E69F00", "#997800")) +
    scale_x_discrete(limits = c("wet season", "wet to dry", "dry season", "dry to wet")) +
    theme_classic() + 
    theme(axis.title.y = element_blank(),
          axis.text.x = element_blank()) +
    ylim(-2.995, -2.005) +
    xlab("Season")
)

ggarrange(xlong_violin.0.75, NULL, long_lat_scatter.0.75, ylat_violin.0.75, 
          ncol = 2, nrow = 2,  align = "hv", 
          widths = c(4, 2.15), heights = c(2, 6),
          common.legend = TRUE)

ggsave("02-01_0.75.map-violinplot.adults+subadults.svg", path = "7_intermediate_results/plots/Migratory_herds",
       width = unit(6.48,"cm"), height = unit(8.1,"cm"))
