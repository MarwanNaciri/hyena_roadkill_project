#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#                                                                              #
#         Analysis of the effect of the position of the migratory herds        #        
#                                                                              #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

library(tidyverse)
library(lubridate)
Sys.setenv(LANG = "en")

# A. Serengeti IV seasons no transit season ====================================

# ~ 1. Count the roadkills during each herds_position --------------------------

# ~~~ a. For all carcasses -----------------------

hy_carcasses <- read_delim("06_processed_data/carcasses/12_hy.carcasses.certainty.formatted.spatial_updated_10-2021.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE) %>%
  filter(GPS_certainty_score > 0) %>%
  mutate(date_death = as.Date(date_death, format = "%d / %m / %Y"),
         year = year(date_death))


# Add variable "position of the migratory herds"
hy_carcasses_herds_position <- hy_carcasses %>%
  mutate(herds_position = ifelse(date_death %within% interval(ymd(paste0(year, "-08-01")), 
                                                              ymd(paste0(year, "-10-31"))), 
                                 "north west", 
                                 ifelse(date_death %within% interval(ymd(paste0(year, "-01-01")),
                                                                     ymd(paste0(year, "-05-10"))),
                                        "south east", 
                                        ifelse(date_death %within% interval(ymd(paste0(year, "-12-21")),
                                                                            ymd(paste0(year, "-12-31"))),
                                               "south east", NA)))) %>%
  filter(!is.na(herds_position)) %>%
  dplyr::select(ID, date_death, herds_position, long, lat, collision_certainty_score_NEW, GPS_certainty_score, age, sex)

hy_carcasses_herds_position_ad <- hy_carcasses_herds_position %>%
  filter(age %in% c("adult", "unknown", "subadult"))


# ~~~ b. For carcasses >=0.5 ---------------------------------------------------

hy_carcasses_0.5 <- read_delim("06_processed_data/carcasses/12_hy.carcasses.certainty.formatted.spatial_updated_10-2021.csv", 
                               ";", escape_double = FALSE, trim_ws = TRUE) %>%
  filter(GPS_certainty_score >= 0.5,
         collision_certainty_score_NEW >= 0.5) %>%
  mutate(date_death = as.Date(date_death, format = "%d / %m / %Y"),
         year = year(date_death))

# Add variable "position of the migratory herds"
hy_carcasses_herds_position_0.5 <- hy_carcasses_0.5 %>%
  mutate(herds_position = ifelse(date_death %within% interval(ymd(paste0(year, "-08-01")), 
                                                              ymd(paste0(year, "-10-31"))), 
                                 "north west", 
                                 ifelse(date_death %within% interval(ymd(paste0(year, "-01-01")),
                                                                     ymd(paste0(year, "-05-10"))),
                                        "south east", 
                                        ifelse(date_death %within% interval(ymd(paste0(year, "-12-21")),
                                                                            ymd(paste0(year, "-12-31"))),
                                               "south east", NA)))) %>%
  filter(!is.na(herds_position)) %>%
  dplyr::select(ID, date_death, herds_position, long, lat, collision_certainty_score_NEW, 
                GPS_certainty_score, age, sex)

hy_carcasses_herds_position_ad_0.5 <- hy_carcasses_herds_position_0.5 %>%
  filter(age %in% c("adult", "unknown", "subadult"))




# ~~~ c. For carcasses >=0.75 --------------------------------------------------

hy_carcasses_0.75 <- read_delim("06_processed_data/carcasses/12_hy.carcasses.certainty.formatted.spatial_updated_10-2021.csv", 
                                ";", escape_double = FALSE, trim_ws = TRUE) %>%
  filter(GPS_certainty_score >= 0.75,
         collision_certainty_score_NEW >= 0.75) %>%
  mutate(date_death = as.Date(date_death, format = "%d / %m / %Y"),
         year = year(date_death))

# Add variable "position of the migratory herds"
hy_carcasses_herds_position_0.75 <- hy_carcasses_0.75 %>%
  mutate(herds_position = ifelse(date_death %within% interval(ymd(paste0(year, "-08-01")), 
                                                              ymd(paste0(year, "-10-31"))), 
                                 "north west", 
                                 ifelse(date_death %within% interval(ymd(paste0(year, "-01-01")),
                                                                     ymd(paste0(year, "-05-10"))),
                                        "south east", 
                                        ifelse(date_death %within% interval(ymd(paste0(year, "-12-21")),
                                                                            ymd(paste0(year, "-12-31"))),
                                               "south east", NA)))) %>%
  filter(!is.na(herds_position)) %>%
  dplyr::select(ID, date_death, herds_position, long, lat, collision_certainty_score_NEW, 
                GPS_certainty_score, age, sex)

hy_carcasses_herds_position_ad_0.75 <- hy_carcasses_herds_position_0.75 %>%
  filter(age %in% c("adult", "unknown", "subadult"))






# ~ 2. Statistical tests ----------------------------------------------------------

# ~~~ a. For all carcasses -----------------------

#lat
wilcox.test(formula = lat ~ herds_position, 
            data= hy_carcasses_herds_position_ad, 
            alternative = "two.sided", exact = TRUE)
#long
wilcox.test(formula = long ~ herds_position, 
            data = hy_carcasses_herds_position_ad, 
            alternative = "two.sided", exact = TRUE)


# ~~~ b. For carcasses >=0.5 ---------------------------------------------------
#lat
wilcox.test(formula = lat ~ herds_position, 
            data = hy_carcasses_herds_position_ad_0.5, 
            alternative = "two.sided", exact = TRUE)
# p = 0.02
#long
wilcox.test(formula = long ~ herds_position, 
            data = hy_carcasses_herds_position_ad_0.5, 
            alternative = "two.sided", exact = TRUE)
# p = 0.009



# ~~~ c. For carcasses >=0.75 --------------------------------------------------
#lat
wilcox.test(formula = lat ~ herds_position, 
            data = hy_carcasses_herds_position_ad_0.75, 
            alternative = "two.sided", exact = TRUE)
# p = 0.053
#long
wilcox.test(formula = long ~ herds_position, 
            data = hy_carcasses_herds_position_ad_0.75, 
            alternative = "two.sided", exact = TRUE)
# p = 0.022




# ~ 3. Median lat and long for each herds position -----------------------------

median <- hy_carcasses_herds_position_ad %>%
  group_by(herds_position) %>%
  summarise(med_long = median(long),
           med_lat = median(lat))

median_0.5 <- hy_carcasses_herds_position_ad_0.5 %>%
  group_by(herds_position) %>%
  summarise(med_long = median(long),
            med_lat = median(lat))

median_0.75 <- hy_carcasses_herds_position_ad_0.75 %>%
  group_by(herds_position) %>%
  summarise(med_long = median(long),
            med_lat = median(lat))

# B. Marion's seasons no transit season ========================================

# ~ 1. Count the roadkills during each herds_position --------------------------

# ~~~ a. For all carcasses -----------------------

hy_carcasses_formatted <- read_delim("06_processed_data/carcasses/12_hy.carcasses.certainty.formatted.spatial_updated_10-2021.csv", 
                                     ";", escape_double = FALSE, trim_ws = TRUE)

hy_carcasses_formatted_spatial <- hy_carcasses_formatted %>%
  filter(GPS_certainty_score > 0) %>%
  mutate(date_death = as.Date(date_death, format = "%d / %m / %Y"),
         year = year(date_death))

# Add variable "position of the migratory herds"
hy_carcasses_herds_position <- hy_carcasses_formatted_spatial %>%
  mutate(herds_position = ifelse(date_death %within% interval(ymd(paste0(year, "-05-26")), 
                                                              ymd(paste0(year, "-10-31"))), 
                                 "north west", "south east")) %>%
  dplyr::select(ID, date_death, herds_position, long, lat, collision_certainty_score_NEW, GPS_certainty_score, age, sex)
  
hy_carcasses_herds_position_ad <- hy_carcasses_herds_position %>%
  filter(age %in% c("adult", "unknown", "subadult"))


roads.ggplot.map <- read_csv("07_intermediate_results/plots/Migratory_herds/roads.map.ggplot2.csv", 
                             col_types = cols(X1 = col_skip()))
(long_lat_scatter <- ggplot() +
    geom_line(data = roads.ggplot.map, aes( x = long, y = lat, group = ID_road_seg),
              size = 0.75) +
    geom_point(data = hy_carcasses_herds_position_ad, aes(x = long, y = lat, color = herds_position),
               size = 3) +
    theme_classic() + #theme_minimal() + 
    scale_color_manual(limits = c("south east", "north west"),
                       values = c("#336600", "#E69F00")) +
    ylab("Latitude") +
    xlab("Longitude")
)

  

# ~~~ b. For carcasses >=0.5 ---------------------------------------------------

hy_carcasses_formatted <- read_delim("06_processed_data/carcasses/12_hy.carcasses.certainty.formatted.spatial_updated_10-2021.csv", 
                                     ";", escape_double = FALSE, trim_ws = TRUE)

hy_carcasses_formatted_spatial_0.5 <- hy_carcasses_formatted %>%
  filter(GPS_certainty_score >= 0.5,
         collision_certainty_score_NEW >= 0.5) %>%
  mutate(date_death = as.Date(date_death, format = "%d / %m / %Y"),
         year = year(date_death))

# Add variable "position of the migratory herds"
hy_carcasses_herds_position_0.5 <- hy_carcasses_formatted_spatial_0.5 %>%
  mutate(herds_position = ifelse(date_death %within% interval(ymd(paste0(year, "-06-01")), 
                                                              ymd(paste0(year, "-10-31"))), 
                                 "north west", "south east")) %>%
  dplyr::select(ID, date_death, herds_position, long, lat, collision_certainty_score_NEW, GPS_certainty_score, age, sex)

hy_carcasses_herds_position_ad.0.5 <- hy_carcasses_herds_position_0.5 %>%
  filter(age %in% c("adult", "unknown", "subadult"))




# ~~~ c. For carcasses >=0.75 --------------------------------------------------

hy_carcasses_formatted <- read_delim("06_processed_data/carcasses/12_hy.carcasses.certainty.formatted.spatial_updated_10-2021.csv", 
                                     ";", escape_double = FALSE, trim_ws = TRUE)

hy_carcasses_formatted_spatial_0.75 <- hy_carcasses_formatted %>%
  filter(GPS_certainty_score >= 0.75,
         collision_certainty_score_NEW >= 0.75) %>%
  mutate(date_death = as.Date(date_death, format = "%d / %m / %Y"),
         year = year(date_death))

# Add variable "position of the migratory herds"
hy_carcasses_herds_position_0.75 <- hy_carcasses_formatted_spatial_0.75 %>%
  mutate(herds_position = ifelse(date_death %within% interval(ymd(paste0(year, "-06-01")), 
                                                              ymd(paste0(year, "-10-31"))), 
                                 "north west", "south east")) %>%
  dplyr::select(ID, date_death, herds_position, long, lat, collision_certainty_score_NEW, GPS_certainty_score, age, sex)

hy_carcasses_herds_position_ad.0.75 <- hy_carcasses_herds_position_0.75 %>%
  filter(age %in% c("adult", "unknown", "subadult"))






# ~ 2. Statistical tests ----------------------------------------------------------

# ~~~ a. For all carcasses -----------------------

#lat
wilcox.test(formula = lat ~ herds_position, data = hy_carcasses_herds_position_ad, alternative = "two.sided", exact = TRUE)
#long
wilcox.test(formula = long ~ herds_position, data = hy_carcasses_herds_position_ad, alternative = "two.sided", exact = TRUE)

#lat
kruskal.test(formula = lat ~ herds_position, data = hy_carcasses_herds_position_ad)
dunn.test::dunn.test(x = hy_carcasses_herds_position_ad$lat, g = hy_carcasses_herds_position_ad$herds_position, 
                     method = "bonferroni",
                     kw = TRUE, # Do the Kruskal Wallis test
                     label = TRUE)
#long
kruskal.test(formula = long ~ herds_position, data = hy_carcasses_herds_position_ad)
dunn.test::dunn.test(x = hy_carcasses_herds_position_ad$long, g = hy_carcasses_herds_position_ad$herds_position, 
                     method = "bonferroni",
                     kw = TRUE, # Do the Kruskal Wallis test
                     label = TRUE)



# ~~~ b. For carcasses >=0.5 ---------------------------------------------------
#lat
wilcox.test(formula = lat ~ herds_position, data = hy_carcasses_herds_position_ad.0.5, alternative = "two.sided", exact = TRUE)
# p = 0.02
#long
wilcox.test(formula = long ~ herds_position, data = hy_carcasses_herds_position_ad.0.5, alternative = "two.sided", exact = TRUE)
# p = 0.009

#lat
kruskal.test(formula = lat ~ herds_position, data = hy_carcasses_herds_position_ad.0.5)
dunn.test::dunn.test(x = hy_carcasses_herds_position_ad.0.5$lat, g = hy_carcasses_herds_position_ad.0.5$herds_position, 
                     method = "bonferroni",
                     kw = TRUE, # Do the Kruskal Wallis test
                     label = TRUE)
#long
kruskal.test(formula = long ~ herds_position, data = hy_carcasses_herds_position_ad.0.5)
dunn.test::dunn.test(x = hy_carcasses_herds_position_ad.0.5$long, g = hy_carcasses_herds_position_ad.0.5$herds_position, 
                     method = "bonferroni",
                     kw = TRUE, # Do the Kruskal Wallis test
                     label = TRUE)


# ~~~ c. For carcasses >=0.75 --------------------------------------------------
#lat
wilcox.test(formula = lat ~ herds_position, data = hy_carcasses_herds_position_ad.0.75, alternative = "two.sided", exact = TRUE)
# p = 0.053
#long
wilcox.test(formula = long ~ herds_position, data = hy_carcasses_herds_position_ad.0.75, alternative = "two.sided", exact = TRUE)
# p = 0.022

#lat
kruskal.test(formula = lat ~ herds_position, data = hy_carcasses_herds_position_ad.0.75)
dunn.test::dunn.test(x = hy_carcasses_herds_position_ad.0.75$lat, g = hy_carcasses_herds_position_ad.0.75$herds_position, 
                     method = "bonferroni",
                     kw = TRUE, # Do the Kruskal Wallis test
                     label = TRUE)
#long
kruskal.test(formula = long ~ herds_position, data = hy_carcasses_herds_position_ad.0.75)
dunn.test::dunn.test(x = hy_carcasses_herds_position_ad.0.75$long, g = hy_carcasses_herds_position_ad.0.75$herds_position, 
                     method = "bonferroni",
                     kw = TRUE, # Do the Kruskal Wallis test
                     label = TRUE)


# ~ 3. Median lat and long for each herds position -----------------------------

(medians <- hy_carcasses_herds_position_ad %>%
  group_by(herds_position) %>%
  summarize(median_long = median(long),
            median_lat = median(lat))
)
(medians.0.5 <- hy_carcasses_herds_position_ad.0.5 %>%
  group_by(herds_position) %>%
  summarize(median_long = median(long),
            median_lat = median(lat))
)
(medians.0.75 <- hy_carcasses_herds_position_ad.0.75 %>%
    group_by(herds_position) %>%
    summarize(median_long = median(long),
              median_lat = median(lat))
)




# C. Marion's seasons with transit season ======================================

# ~ 1. Count the roadkills during each herds_position --------------------------

# ~~~ a. For all carcasses -----------------------------------------------------

hy_carcasses_formatted <- read_delim("06_processed_data/carcasses/12_hy.carcasses.certainty.formatted.spatial_updated_10-2021.csv", 
                                     ";", escape_double = FALSE, trim_ws = TRUE)

hy_carcasses_formatted_spatial <- hy_carcasses_formatted %>%
  filter(GPS_certainty_score > 0) %>%
  mutate(date_death = as.Date(date_death, format = "%d / %m / %Y"),
         year = year(date_death))


# Add variable "position of the migratory herds"
hy_carcasses_herds_position <- hy_carcasses_formatted_spatial %>%
  mutate(herds_position = ifelse(date_death %within% interval(ymd(paste0(year, "-06-26")), 
                                                              ymd(paste0(year, "-10-25"))), 
                                 "north west", 
                                 ifelse(date_death %within% interval(ymd(paste0(year, "-04-26")), 
                                                                     ymd(paste0(year, "-06-25"))),"transit",
                                        ifelse(date_death %within% interval(ymd(paste0(year, "-10-25")), 
                                                                            ymd(paste0(year, "-10-15"))), "transit", "south east")))) %>%
  dplyr::select(ID, date_death, herds_position, long, lat, collision_certainty_score_NEW, GPS_certainty_score, age, sex)

hy_carcasses_herds_position_ad <- hy_carcasses_herds_position %>%
  filter(age %in% c("adult", "unknown", "subadult"))


roads.ggplot.map <- read_csv("07_intermediate_results/plots/Migratory_herds/roads.map.ggplot2.csv", 
                             col_types = cols(X1 = col_skip()))
(long_lat_scatter <- ggplot() +
    geom_line(data = roads.ggplot.map, aes( x = long, y = lat, group = ID_road_seg),
              size = 0.75) +
    geom_point(data = hy_carcasses_herds_position_ad, aes(x = long, y = lat, color = herds_position),
               size = 3) +
    theme_classic() + #theme_minimal() + 
    scale_color_manual(limits = c("south east", "transit", "north west"),
                       values = c("#336600", "#9a9c00", "#E69F00")) +
    ylab("Latitude") +
    xlab("Longitude")
)

(xlong_violin <- ggplot(hy_carcasses_herds_position_ad, aes(x = herds_position, y = long, fill = herds_position)) +
    geom_violin(size = 0.5, color = "black") +
    geom_boxplot(width=0.1, size = 0.5, color = "black") +
    scale_fill_manual(limits = c("south east", "transit", "north west"),
                      values = c("#336600", "#9a9c00", "#E69F00")) +
    scale_x_discrete(limits = c("south east", "transit", "north west")) +
    theme_classic() +
    theme(axis.title.x = element_blank(),
          axis.text.y = element_blank()) +
    xlab("Migratory herds' position") +
    ylim(34.65, 35.14)
)

(ylat_violin <- ggplot(hy_carcasses_herds_position_ad, aes(x = herds_position, y = lat, fill = herds_position)) +
    geom_violin(size = 0.5, color = "black") +
    geom_boxplot(width=0.1, size = 0.5, color = "black") +
    scale_fill_manual(limits = c("south east", "transit", "north west"),
                      values = c("#336600", "#9a9c00", "#E69F00")) +
    scale_x_discrete(limits = c("south east", "transit", "north west")) +
    theme_classic() + 
    theme(axis.title.y = element_blank(),
          axis.text.x = element_blank()) +
    xlab("Migratory herds' position") +
    ylim(-2.995, -2.005)
)



# ~~~ b. For carcasses >=0.5 ---------------------------------------------------

hy_carcasses_formatted <- read_delim("06_processed_data/carcasses/12_hy.carcasses.certainty.formatted.spatial_updated_10-2021.csv", 
                                     ";", escape_double = FALSE, trim_ws = TRUE)

hy_carcasses_formatted_spatial_0.5 <- hy_carcasses_formatted %>%
  filter(GPS_certainty_score >= 0.5,
         collision_certainty_score_NEW >= 0.5) %>%
  mutate(date_death = as.Date(date_death, format = "%d / %m / %Y"),
         year = year(date_death))

# Add variable "position of the migratory herds"
hy_carcasses_herds_position_0.5 <- hy_carcasses_formatted_spatial_0.5 %>%
  mutate(herds_position = ifelse(date_death %within% interval(ymd(paste0(year, "-06-26")), 
                                                              ymd(paste0(year, "-10-25"))), 
                                 "north west", 
                                 ifelse(date_death %within% interval(ymd(paste0(year, "-04-26")), 
                                                                     ymd(paste0(year, "-06-25"))),"transit",
                                        ifelse(date_death %within% interval(ymd(paste0(year, "-10-25")), 
                                                                            ymd(paste0(year, "-10-15"))), "transit", "south east")))) %>%
  dplyr::select(ID, date_death, herds_position, long, lat, collision_certainty_score_NEW, GPS_certainty_score, age, sex)

hy_carcasses_herds_position_ad.0.5 <- hy_carcasses_herds_position_0.5 %>%
  filter(age %in% c("adult", "unknown", "subadult"))




# ~~~ c. For carcasses >=0.75 --------------------------------------------------

hy_carcasses_formatted <- read_delim("06_processed_data/carcasses/12_hy.carcasses.certainty.formatted.spatial_updated_10-2021.csv", 
                                     ";", escape_double = FALSE, trim_ws = TRUE)

hy_carcasses_formatted_spatial_0.75 <- hy_carcasses_formatted %>%
  filter(GPS_certainty_score >= 0.75,
         collision_certainty_score_NEW >= 0.75) %>%
  mutate(date_death = as.Date(date_death, format = "%d / %m / %Y"),
         year = year(date_death))

# Add variable "position of the migratory herds"
hy_carcasses_herds_position_0.75 <- hy_carcasses_formatted_spatial_0.75 %>%
  mutate(herds_position = ifelse(date_death %within% interval(ymd(paste0(year, "-06-26")), 
                                                              ymd(paste0(year, "-10-25"))), 
                                 "north west", 
                                 ifelse(date_death %within% interval(ymd(paste0(year, "-04-26")), 
                                                                     ymd(paste0(year, "-06-25"))),"transit",
                                        ifelse(date_death %within% interval(ymd(paste0(year, "-10-25")), 
                                                                            ymd(paste0(year, "-10-15"))), "transit", "south east")))) %>%
  dplyr::select(ID, date_death, herds_position, long, lat, collision_certainty_score_NEW, GPS_certainty_score, age, sex)

hy_carcasses_herds_position_ad.0.75 <- hy_carcasses_herds_position_0.75 %>%
  filter(age %in% c("adult", "unknown", "subadult"))



# ~ 2. Statistical tests -------------------------------------------------------

# ~~~ a. For all carcasses -----------------------

#lat
wilcox.test(formula = lat ~ herds_position, data = hy_carcasses_herds_position_ad, alternative = "two.sided", exact = TRUE)
#long
wilcox.test(formula = long ~ herds_position, data = hy_carcasses_herds_position_ad, alternative = "two.sided", exact = TRUE)

#lat
kruskal.test(formula = lat ~ herds_position, data = hy_carcasses_herds_position_ad)
dunn.test::dunn.test(x = hy_carcasses_herds_position_ad$lat, g = hy_carcasses_herds_position_ad$herds_position, 
                     method = "bonferroni",
                     kw = TRUE, # Do the Kruskal Wallis test
                     label = TRUE)
#long
kruskal.test(formula = long ~ herds_position, data = hy_carcasses_herds_position_ad)
dunn.test::dunn.test(x = hy_carcasses_herds_position_ad$long, g = hy_carcasses_herds_position_ad$herds_position, 
                     method = "bonferroni",
                     kw = TRUE, # Do the Kruskal Wallis test
                     label = TRUE)



# ~~~ b. For carcasses >=0.5 ---------------------------------------------------
#lat
wilcox.test(formula = lat ~ herds_position, data = hy_carcasses_herds_position_ad.0.5, alternative = "two.sided", exact = TRUE)
# p = 0.02
#long
wilcox.test(formula = long ~ herds_position, data = hy_carcasses_herds_position_ad.0.5, alternative = "two.sided", exact = TRUE)
# p = 0.009

#lat
kruskal.test(formula = lat ~ herds_position, data = hy_carcasses_herds_position_ad.0.5)
dunn.test::dunn.test(x = hy_carcasses_herds_position_ad.0.5$lat, g = hy_carcasses_herds_position_ad.0.5$herds_position, 
                     method = "bonferroni",
                     kw = TRUE, # Do the Kruskal Wallis test
                     label = TRUE)
#long
kruskal.test(formula = long ~ herds_position, data = hy_carcasses_herds_position_ad.0.5)
dunn.test::dunn.test(x = hy_carcasses_herds_position_ad.0.5$long, g = hy_carcasses_herds_position_ad.0.5$herds_position, 
                     method = "bonferroni",
                     kw = TRUE, # Do the Kruskal Wallis test
                     label = TRUE)


# ~~~ c. For carcasses >=0.75 --------------------------------------------------
#lat
wilcox.test(formula = lat ~ herds_position, data = hy_carcasses_herds_position_ad.0.75, alternative = "two.sided", exact = TRUE)
# p = 0.053
#long
wilcox.test(formula = long ~ herds_position, data = hy_carcasses_herds_position_ad.0.75, alternative = "two.sided", exact = TRUE)
# p = 0.022

#lat
kruskal.test(formula = lat ~ herds_position, data = hy_carcasses_herds_position_ad.0.75)
dunn.test::dunn.test(x = hy_carcasses_herds_position_ad.0.75$lat, g = hy_carcasses_herds_position_ad.0.75$herds_position, 
                     method = "bonferroni",
                     kw = TRUE, # Do the Kruskal Wallis test
                     label = TRUE)
#long
kruskal.test(formula = long ~ herds_position, data = hy_carcasses_herds_position_ad.0.75)
dunn.test::dunn.test(x = hy_carcasses_herds_position_ad.0.75$long, g = hy_carcasses_herds_position_ad.0.75$herds_position, 
                     method = "bonferroni",
                     kw = TRUE, # Do the Kruskal Wallis test
                     label = TRUE)


# ~ 3. Median lat and long for each herds position ---------------------------------

(medians <- hy_carcasses_herds_position_ad %>%
   group_by(herds_position) %>%
   summarize(median_long = median(long),
             median_lat = median(lat))
)


(medians.0.5 <- hy_carcasses_herds_position_ad.0.5 %>%
    group_by(herds_position) %>%
    summarize(median_long = median(long),
              median_lat = median(lat))
)

(medians.0.75 <- hy_carcasses_herds_position_ad.0.75 %>%
    group_by(herds_position) %>%
    summarize(median_long = median(long),
              median_lat = median(lat))
)



# C. Serengeti IV seasons no transit season ====================================

# ~ 1. Count the roadkills during each herds_position --------------------------

# ~~~ a. For all carcasses -----------------------------------------------------

hy_carcasses <- read_delim("06_processed_data/carcasses/12_hy.carcasses.certainty.formatted.spatial_updated_10-2021.csv", 
                                     ";", escape_double = FALSE, trim_ws = TRUE) %>%
  filter(GPS_certainty_score > 0) %>%
  mutate(date_death = as.Date(date_death, format = "%d / %m / %Y"),
         year = year(date_death))


# Add variable "position of the migratory herds"
hy_carcasses_herds_position <- hy_carcasses %>%
  mutate(herds_position = ifelse(date_death %within% interval(ymd(paste0(year, "-08-01")), 
                                                              ymd(paste0(year, "-10-31"))), 
                                 "north west", 
                                 ifelse(date_death %within% interval(ymd(paste0(year, "-01-01")),
                                                                     ymd(paste0(year, "-05-10"))),
                                        "south east", 
                                        ifelse(date_death %within% interval(ymd(paste0(year, "-12-21")),
                                                                            ymd(paste0(year, "-12-31"))),
                                               "south east", NA)))) %>%
  filter(!is.na(herds_position)) %>%
  dplyr::select(ID, date_death, herds_position, long, lat, collision_certainty_score_NEW, GPS_certainty_score, age, sex)

hy_carcasses_herds_position_ad <- hy_carcasses_herds_position %>%
  filter(age %in% c("adult", "unknown", "subadult"))


roads.ggplot.map <- read_csv("07_intermediate_results/plots/Migratory_herds/roads.map.ggplot2.csv", 
                             col_types = cols(X1 = col_skip()))
(long_lat_scatter <- ggplot() +
    geom_line(data = roads.ggplot.map, aes( x = long, y = lat, group = ID_road_seg),
              size = 0.75) +
    geom_point(data = hy_carcasses_herds_position_ad, aes(x = long, y = lat, color = herds_position),
               size = 3) +
    theme_classic() + #theme_minimal() + 
    scale_color_manual(limits = c("south east", "north west"),
                       values = c("#336600", "#E69F00")) +
    ylab("Latitude") +
    xlab("Longitude")
)



# ~~~ b. For carcasses >=0.5 ---------------------------------------------------

hy_carcasses_0.5 <- read_delim("06_processed_data/carcasses/12_hy.carcasses.certainty.formatted.spatial_updated_10-2021.csv", 
                                     ";", escape_double = FALSE, trim_ws = TRUE) %>%
  filter(GPS_certainty_score >= 0.5,
         collision_certainty_score_NEW >= 0.5) %>%
  mutate(date_death = as.Date(date_death, format = "%d / %m / %Y"),
         year = year(date_death))

# Add variable "position of the migratory herds"
hy_carcasses_herds_position_0.5 <- hy_carcasses_0.5 %>%
  mutate(herds_position = ifelse(date_death %within% interval(ymd(paste0(year, "-08-01")), 
                                                              ymd(paste0(year, "-10-31"))), 
                                 "north west", 
                                 ifelse(date_death %within% interval(ymd(paste0(year, "-01-01")),
                                                                     ymd(paste0(year, "-05-10"))),
                                        "south east", 
                                        ifelse(date_death %within% interval(ymd(paste0(year, "-12-21")),
                                                                            ymd(paste0(year, "-12-31"))),
                                               "south east", NA)))) %>%
  filter(!is.na(herds_position)) %>%
  dplyr::select(ID, date_death, herds_position, long, lat, collision_certainty_score_NEW, 
                GPS_certainty_score, age, sex)

hy_carcasses_herds_position_ad_0.5 <- hy_carcasses_herds_position_0.5 %>%
  filter(age %in% c("adult", "unknown", "subadult"))




# ~~~ c. For carcasses >=0.75 --------------------------------------------------

hy_carcasses_0.75 <- read_delim("06_processed_data/carcasses/12_hy.carcasses.certainty.formatted.spatial_updated_10-2021.csv", 
                               ";", escape_double = FALSE, trim_ws = TRUE) %>%
  filter(GPS_certainty_score >= 0.75,
         collision_certainty_score_NEW >= 0.75) %>%
  mutate(date_death = as.Date(date_death, format = "%d / %m / %Y"),
         year = year(date_death))

# Add variable "position of the migratory herds"
hy_carcasses_herds_position_0.75 <- hy_carcasses_0.75 %>%
  mutate(herds_position = ifelse(date_death %within% interval(ymd(paste0(year, "-08-01")), 
                                                              ymd(paste0(year, "-10-31"))), 
                                 "north west", 
                                 ifelse(date_death %within% interval(ymd(paste0(year, "-01-01")),
                                                                     ymd(paste0(year, "-05-10"))),
                                        "south east", 
                                        ifelse(date_death %within% interval(ymd(paste0(year, "-12-21")),
                                                                            ymd(paste0(year, "-12-31"))),
                                               "south east", NA)))) %>%
  filter(!is.na(herds_position)) %>%
  dplyr::select(ID, date_death, herds_position, long, lat, collision_certainty_score_NEW, 
                GPS_certainty_score, age, sex)

hy_carcasses_herds_position_ad_0.75 <- hy_carcasses_herds_position_0.75 %>%
  filter(age %in% c("adult", "unknown", "subadult"))






# ~ 2. Statistical tests -------------------------------------------------------

# ~~~ a. For all carcasses -----------------------------------------------------

#lat
wilcox.test(formula = lat ~ herds_position, 
            data= hy_carcasses_herds_position_ad, 
            alternative = "two.sided", exact = TRUE)
#long
wilcox.test(formula = long ~ herds_position, 
            data = hy_carcasses_herds_position_ad, 
            alternative = "two.sided", exact = TRUE)


# ~~~ b. For carcasses >=0.5 ---------------------------------------------------
#lat
wilcox.test(formula = lat ~ herds_position, 
            data = hy_carcasses_herds_position_ad_0.5, 
            alternative = "two.sided", exact = TRUE)
# p = 0.02
#long
wilcox.test(formula = long ~ herds_position, 
            data = hy_carcasses_herds_position_ad_0.5, 
            alternative = "two.sided", exact = TRUE)
# p = 0.009



# ~~~ c. For carcasses >=0.75 --------------------------------------------------
#lat
wilcox.test(formula = lat ~ herds_position, 
            data = hy_carcasses_herds_position_ad_0.75, 
            alternative = "two.sided", exact = TRUE)
# p = 0.053
#long
wilcox.test(formula = long ~ herds_position, 
            data = hy_carcasses_herds_position_ad_0.75, 
            alternative = "two.sided", exact = TRUE)
# p = 0.022


