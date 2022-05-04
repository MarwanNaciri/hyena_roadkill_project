#==============================================================================#
#                                                                              #
#             Investigate potential biases in research effort &                #
#                           carcass detectability                              #        
#                                                                              #
#==============================================================================#

library(tidyverse)
library(lubridate)
library(ggpubr)


# A. Carcasses found during monitoring -----------------------------------------

daily.monitoring <- read_csv("04_raw_data/hyenas/Daily_monitoring.csv", 
                             col_types = cols(X1 = col_skip()))

hy_carcasses.monitoring <- read_delim("06_processed_data/carcasses/12_hy.carcasses.certainty.formatted.spatial_updated_10-2021.csv", 
                                      ";", escape_double = FALSE, trim_ws = TRUE) %>%
  mutate(death_obs_new = paste0(substr(date_obs, 1, 4), "-", substr(date_obs, 5, 6), "-", substr(date_obs, 7, 8))) %>%
  mutate(date_obs_new = as.Date(death_obs_new, format = "%Y - %m - %d")) %>%
  dplyr::select(-date_obs) %>%
  rename(date_obs = date_obs_new) %>%
  mutate(year = year(date_obs)) %>%
  left_join(x = .,
            y = daily.monitoring,
            by = c("date_obs" = "date")) %>%
  count(monitored)



hy_carcasses.monitoring <- read_delim("06_processed_data/carcasses/12_hy.carcasses.certainty.formatted.spatial_updated_10-2021.csv", 
                                      ";", escape_double = FALSE, trim_ws = TRUE) %>%
  mutate(death_obs_new = paste0(substr(date_obs, 1, 4), "-", substr(date_obs, 5, 6), "-", substr(date_obs, 7, 8))) %>%
  mutate(date_obs_new = as.Date(death_obs_new, format = "%Y - %m - %d")) %>%
  dplyr::select(-date_obs) %>%
  rename(date_obs = date_obs_new) %>%
  mutate(year = year(date_obs)) %>%
  left_join(x = .,
            y = daily.monitoring,
            by = c("date_obs" = "date")) %>%
  count(monitored, year)

ggplot(hy_carcasses.monitoring, 
       aes(x = year, y = n, fill = as.factor(monitored))) +
  geom_col()


hy_carcasses.monitoring.season <- read_delim("06_processed_data/carcasses/12_hy.carcasses.certainty.formatted.spatial_updated_10-2021.csv", 
                                             ";", escape_double = FALSE, trim_ws = TRUE) %>%
  mutate(death_obs_new = paste0(substr(date_obs, 1, 4), "-", substr(date_obs, 5, 6), "-", substr(date_obs, 7, 8))) %>%
  mutate(date_obs_new = as.Date(death_obs_new, format = "%Y - %m - %d")) %>%
  dplyr::select(-date_obs) %>%
  rename(date_obs = date_obs_new) %>%
  mutate(year = year(date_obs)) %>%
  left_join(x = .,
            y = daily.monitoring,
            by = c("date_obs" = "date")) %>%
  mutate(herds_position_w_transit = ifelse(date_obs %within% interval(ymd(paste0(year, "-08-01")), 
                                                                      ymd(paste0(year, "-10-31"))), 
                                           "dry season\n(north and MMR)", 
                                           ifelse(date_obs %within% interval(ymd(paste0(year, "-05-11")), 
                                                                             ymd(paste0(year, "-07-31"))),
                                                  "transit",
                                                  ifelse(date_obs %within% interval(ymd(paste0(year, "-11-01")), 
                                                                                    ymd(paste0(year, "-12-20"))), 
                                                         "transit", "wet season\n(south-east)")))) %>%
  count(monitored, herds_position_w_transit) %>%
  mutate(monitored = ifelse(is.na(monitored), 1, monitored)) %>%
  mutate(monitored2 = factor(ifelse(monitored == 0, "no", "yes"),
                             levels = c("yes", "no")))

ggplot(hy_carcasses.monitoring.season, 
       aes(x = herds_position_w_transit, y = n, fill = as.factor(monitored2))) +
  geom_col() +
  scale_x_discrete(limits = c("dry season\n(north and MMR)", "wet season\n(south-east)", "transit")) +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "season (location of the migratory herds)",
       y = "number of carcasses",
       fill = "monitoring session on \nday of observation \nof the carcass")




hy_carcasses.monitoring.map <- read_delim("06_processed_data/carcasses/12_hy.carcasses.certainty.formatted.spatial_updated_10-2021.csv", 
                                          ";", escape_double = FALSE, trim_ws = TRUE) %>%
  filter(location_certainty_score >= 0.75) %>%
  mutate(death_obs_new = paste0(substr(date_obs, 1, 4), "-", substr(date_obs, 5, 6), "-", substr(date_obs, 7, 8))) %>%
  mutate(date_obs_new = as.Date(death_obs_new, format = "%Y - %m - %d")) %>%
  dplyr::select(-date_obs) %>%
  rename(date_obs = date_obs_new) %>%
  mutate(year = year(date_obs)) %>%
  left_join(x = .,
            y = daily.monitoring,
            by = c("date_obs" = "date")) %>%
  mutate(monitored = ifelse(is.na(monitored), 1, monitored)) %>%
  mutate(monitored2 = ifelse(monitored == 0, "no", "yes")) %>%
  mutate(monitored2 = factor(monitored2, levels = c("yes", "no"))) %>% 
  mutate(herds_position_w_transit = ifelse(date_obs %within% interval(ymd(paste0(year, "-08-01")), 
                                                                      ymd(paste0(year, "-10-31"))), 
                                           "north and MMR \n(dry season)", 
                                           ifelse(date_obs %within% interval(ymd(paste0(year, "-05-11")), 
                                                                             ymd(paste0(year, "-07-31"))),
                                                  "transit",
                                                  ifelse(date_obs %within% interval(ymd(paste0(year, "-11-01")), 
                                                                                    ymd(paste0(year, "-12-20"))), 
                                                         "transit", "south east \n(wet season)"))))

ggplot(hy_carcasses.monitoring.map, aes(x = long, y = lat, color = monitored)) +
  geom_point()

wilcox.test(formula = lat ~ monitored, 
            data = hy_carcasses.monitoring.map, 
            alternative = "two.sided", exact = TRUE)

wilcox.test(formula = long ~ monitored, 
            data = hy_carcasses.monitoring.map, 
            alternative = "two.sided", exact = TRUE)

ggplot(hy_carcasses.monitoring.map, aes(x = monitored2, y = lat)) +
  geom_boxplot() +
  labs(x = "monitoring session on day of \nobservation of the carcass",
       y = "south <-     latitude     -> north")

ggplot(hy_carcasses.monitoring.map, aes(x = monitored2, y = long)) +
  geom_boxplot() +
  labs(x = "monitoring session on day of \nobservation of the carcass",
       y = "west <-     longitude     -> east")


ggplot(hy_carcasses.monitoring.map, aes(x = herds_position_w_transit, y = lat, fill = monitored2)) +
  geom_boxplot() +
  geom_dotplot(binaxis ='y', stackdir='center',
               position = position_dodge(0.75),
               binwidth = 1/70) +
  labs(x = "position of the migratory herds (season)",
       y = "south <-     latitude     -> north",
       fill = "monitoring session\non day of observation\nof the carcass")

ggplot(hy_carcasses.monitoring.map, aes(x = herds_position_w_transit, y = long, fill = monitored2)) +
  geom_boxplot() +
  geom_dotplot(binaxis ='y', stackdir='center',
               position = position_dodge(0.75),
               binwidth = 1/110) +
  labs(x = "position of the migratory herds (season)",
       y = "west <-     longitude     -> east",
       fill = "monitoring session\non day of observation\nof the carcass")




# B. Handler of carcass -----------------------------------------------------------

handler <- read_delim("06_processed_data/carcasses/15_finder_of_carcasses_justified.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)[1:85, 1:11]


handler_season <- handler %>%
  mutate(date_obs_new = paste0(substr(date_obs, 1, 4), "-", substr(date_obs, 5, 6), "-", substr(date_obs, 7, 8))) %>%
  mutate(date_obs_new = as.Date(date_obs_new, format = "%Y - %m - %d")) %>%
  dplyr::select(-date_obs) %>%
  rename(date_obs = date_obs_new) %>%
  mutate(year = year(date_obs)) %>%
  mutate(herds_position_w_transit = ifelse(date_obs %within% interval(ymd(paste0(year, "-08-01")), 
                                                                      ymd(paste0(year, "-10-31"))), 
                                           "north and MMR \n(dry season)", 
                                           ifelse(date_obs %within% interval(ymd(paste0(year, "-05-11")), 
                                                                             ymd(paste0(year, "-07-31"))),
                                                  "transit",
                                                  ifelse(date_obs %within% interval(ymd(paste0(year, "-11-01")), 
                                                                                    ymd(paste0(year, "-12-20"))), 
                                                         "transit", "south east \n(wet season)")))) %>%
  # mutate(handled_by_hyena_project = factor(handled_by_hyena_project, levels = c("yes", "no", "unknown")))
  mutate(handled_by_hyena_project = factor(handled_by_hyena_project, levels = c("yes", "no", "unknown")))

ggplot(handler_season, 
       aes(x = herds_position_w_transit, fill = handled_by_hyena_project)) +
  geom_bar() +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "position of the migratory herds (season)",
       y = "number of carcasses", 
       fill = "handled by \nhyena project?")


hy_carcasses <- read_delim("06_processed_data/carcasses/12_hy.carcasses.certainty.formatted.spatial_updated_10-2021.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE) %>%
  filter(!is.na(location_certainty_score)) %>%
  mutate(date_obs_new = paste0(substr(date_obs, 1, 4), "-", substr(date_obs, 5, 6), "-", substr(date_obs, 7, 8))) %>%
  mutate(date_obs_new = as.Date(date_obs_new, format = "%Y - %m - %d")) %>%
  dplyr::select(-date_obs) %>%
  rename(date_obs = date_obs_new)


handler_GPS <- handler_season %>%
  left_join(x = .,
            y = hy_carcasses,
            by = "ID") %>%
  filter(lat < 0) %>%
  mutate(handled_by_hyena_project = factor(handled_by_hyena_project,
                                           levels = c("yes", "no", "unknown")))



ggplot(handler_GPS, aes(x = herds_position_w_transit, y = lat, fill = handled_by_hyena_project)) +
  # geom_violin()
  geom_boxplot() +
  geom_dotplot(binaxis ='y', stackdir='center',
               position = position_dodge(0.75),
               binwidth = 1/50) +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "season",
       y = "south <-     latitude     -> north",
       fill = "handled by\nhyena project")
ggplot(handler_GPS, aes(x = herds_position_w_transit, y = long, fill = handled_by_hyena_project)) +
  # geom_violin()
  geom_boxplot()  +
  geom_dotplot(binaxis ='y', stackdir='center',
               position = position_dodge(0.75),
               binwidth = 1/90) +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "season",
       y = "west <-     longitude     -> east",
       fill = "handled by\nhyena project")


# Latitude by handler
ggplot(handler_GPS, aes(x = handled_by_hyena_project, y = lat)) +
  # geom_violin()
  geom_boxplot() +
  labs(x = "handled by hyena project",
       y = "south <-     latitude     -> north")

# Longitude by handler
ggplot(handler_GPS, aes(x = handled_by_hyena_project, y = long)) +
  # geom_violin()
  geom_boxplot() +
  labs(x = "handled by hyena project",
       y = "west <-     longitude     -> east")

roads <- rgdal::readOGR(dsn  =  "06_processed_data/roads/SNP.roads.QGIS",
                        layer = "snp_roads_according_to_FZS_map_cropped")
roads@data[["osm_id"]] <- seq(from = 1, to = length(roads@data[["osm_id"]]))
source(file = "05_scripts/04-2021/GLM_2021-10/GLM_functions_calculate_predictors_2021-10.R")
roads.ggplot.map <- CreateDataFrameRoads(roads) %>%
  mutate(ID = as.factor(ID))

ggplot() +
  geom_path(data = roads.ggplot.map, aes(x = long, y = lat, group = ID), #ID_road_seg),
            size = 0.75) +
  geom_point(data = handler_GPS, aes(x = long, y = lat, color = handled_by_hyena_project),
             size = 2) +
  theme_classic() + #theme_minimal() + 
  
  labs(y = "latitude", 
       x = "longitude",
       color = "handled by \nhyena project")



# Look simultaneously at monitoring and handler
hy_carcasses.monitoring.season <- read_delim("06_processed_data/carcasses/12_hy.carcasses.certainty.formatted.spatial_updated_10-2021.csv", 
                                             ";", escape_double = FALSE, trim_ws = TRUE) %>%
  mutate(death_obs_new = paste0(substr(date_obs, 1, 4), "-", substr(date_obs, 5, 6), "-", substr(date_obs, 7, 8))) %>%
  mutate(date_obs_new = as.Date(death_obs_new, format = "%Y - %m - %d")) %>%
  dplyr::select(-date_obs) %>%
  rename(date_obs = date_obs_new) %>%
  mutate(year = year(date_obs)) %>%
  left_join(x = .,
            y = daily.monitoring,
            by = c("date_obs" = "date")) %>%
  mutate(herds_position_w_transit = ifelse(date_obs %within% interval(ymd(paste0(year, "-08-01")), 
                                                                      ymd(paste0(year, "-10-31"))), 
                                           "north and MMR", 
                                           ifelse(date_obs %within% interval(ymd(paste0(year, "-05-11")), 
                                                                             ymd(paste0(year, "-07-31"))),
                                                  "transit",
                                                  ifelse(date_obs %within% interval(ymd(paste0(year, "-11-01")), 
                                                                                    ymd(paste0(year, "-12-20"))), 
                                                         "transit", "south east")))) 

test <- hy_carcasses.monitoring.season %>%
  left_join(x = .,
            y = handler,
            by = "ID")

(x <- table(test$handled_by_hyena_project, test$monitored))

chisq.test(x)
fisher.test(x)




ggplot(test, aes(x = handled_by_hyena_project, y = monitored))



# C. Reporter of carcass -------------------------------------------------------

handler <- read_delim("06_processed_data/carcasses/15_finder_of_carcasses_justified.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)[1:85, 1:11]

x <- handler %>% count(finder_reporter)
reporter_season <- handler %>%
  mutate(date_obs_new = paste0(substr(date_obs, 1, 4), "-", substr(date_obs, 5, 6), "-", substr(date_obs, 7, 8))) %>%
  mutate(date_obs_new = as.Date(date_obs_new, format = "%Y - %m - %d")) %>%
  dplyr::select(-date_obs) %>%
  rename(date_obs = date_obs_new) %>%
  mutate(year = year(date_obs)) %>%
  mutate(herds_position_w_transit = ifelse(date_obs %within% interval(ymd(paste0(year, "-08-01")), 
                                                                      ymd(paste0(year, "-10-31"))), 
                                           "north and MMR \n(dry season)", 
                                           ifelse(date_obs %within% interval(ymd(paste0(year, "-05-11")), 
                                                                             ymd(paste0(year, "-07-31"))),
                                                  "transit",
                                                  ifelse(date_obs %within% interval(ymd(paste0(year, "-11-01")), 
                                                                                    ymd(paste0(year, "-12-20"))), 
                                                         "transit", "south east \n(wet season)")))) %>%
  # mutate(handled_by_hyena_project = factor(handled_by_hyena_project, levels = c("yes", "no", "unknown")))
  mutate(from_hyena_project = factor(ifelse(!is.na(from_hyena_project), from_hyena_project,
                                            "unknown"), levels = c("yes", "no", "unknown")))

ggplot(reporter_season, 
       aes(x = herds_position_w_transit, fill = from_hyena_project)) +
  geom_bar() +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "position of the migratory herds (season)",
       y = "number of carcasses", 
       fill = "reported/found by \nhyena project?")



hy_carcasses <- read_delim("06_processed_data/carcasses/12_hy.carcasses.certainty.formatted.spatial_updated_10-2021.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE) %>%
  filter(!is.na(location_certainty_score)) %>%
  mutate(date_obs_new = paste0(substr(date_obs, 1, 4), "-", substr(date_obs, 5, 6), "-", substr(date_obs, 7, 8))) %>%
  mutate(date_obs_new = as.Date(date_obs_new, format = "%Y - %m - %d")) %>%
  dplyr::select(-date_obs) %>%
  rename(date_obs = date_obs_new)


reporter_GPS <- reporter_season %>%
  left_join(x = .,
            y = hy_carcasses,
            by = "ID") %>%
  filter(lat < 0) %>%
  mutate(from_hyena_project = factor(from_hyena_project, levels = c("yes", "no", "unknown", "unsure")))

ggplot(reporter_GPS, aes(x = herds_position_w_transit, y = lat, fill = from_hyena_project)) +
  # geom_violin()
  geom_boxplot() +
  geom_dotplot(binaxis ='y', stackdir='center',
               position = position_dodge(0.75),
               binwidth = 1/50) +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "season",
       y = "south <-     latitude     -> north",
       fill = "handled by\nhyena project")
ggsave("11_manuscript/V3 Corrections/2022-01 Mail to M, H, Aimara & Morgane/latitude by reporter by season (counts).png",
       width = 5, height = 4)
ggplot(reporter_GPS, aes(x = herds_position_w_transit, y = long, fill = from_hyena_project)) +
  # geom_violin()
  geom_boxplot()  +
  geom_dotplot(binaxis ='y', stackdir='center',
               position = position_dodge(0.75),
               binwidth = 1/90) +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "season",
       y = "west <-     longitude     -> east",
       fill = "handled by\nhyena project")
ggsave("11_manuscript/V3 Corrections/2022-01 Mail to M, H, Aimara & Morgane/longitude by reporter by season (counts).png",
       width = 5, height = 4)







# Latitude by reporter
ggplot(reporter_GPS, aes(x = from_hyena_project, y = lat)) +
  # geom_violin()
  geom_boxplot() +
  labs(x = "handled by hyena project",
       y = "south <-     latitude     -> north")

# Longitude by reporter
ggplot(reporter_GPS, aes(x = from_hyena_project, y = long)) +
  # geom_violin()
  geom_boxplot() +
  labs(x = "handled by hyena project",
       y = "west <-     longitude     -> east")






# D. Table for Sarah -----------------------------------------------------------

table_carcasses_for_Sarah <- read_delim("06_processed_data/carcasses/table_carcasses_for_Sarah.csv", 
                                        delim = ";", escape_double = FALSE, trim_ws = TRUE)

daily.monitoring <- read_csv("04_raw_data/hyenas/Daily_monitoring.csv", 
                             col_types = cols(X1 = col_skip()))

data_table <- table_carcasses_for_Sarah %>%
  mutate(date_obs_new = paste0(substr(date_obs, 1, 4), "-", substr(date_obs, 5, 6), "-", substr(date_obs, 7, 8))) %>%
  mutate(date_obs_new = as.Date(date_obs_new, format = "%Y - %m - %d")) %>%
  dplyr::select(-date_obs) %>%
  rename(date_obs = date_obs_new) %>%
  mutate(year = year(date_obs)) %>%
  left_join(x = .,
            y = daily.monitoring,
            by = c("date_obs" = "date")) %>%
  mutate(herds_position_w_transit = ifelse(date_obs %within% interval(ymd(paste0(year, "-08-01")), 
                                                                      ymd(paste0(year, "-10-31"))), 
                                           "north and MMR", 
                                           ifelse(date_obs %within% interval(ymd(paste0(year, "-05-11")), 
                                                                             ymd(paste0(year, "-07-31"))),
                                                  "transit",
                                                  ifelse(date_obs %within% interval(ymd(paste0(year, "-11-01")), 
                                                                                    ymd(paste0(year, "-12-20"))), 
                                                         "transit", "south east")))) %>%
  dplyr::select(-...1)

write_csv(data_table, "06_processed_data/carcasses/table_for_sarah_2021-11-12.csv")





