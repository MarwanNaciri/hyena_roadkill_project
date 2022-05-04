#==============================================================================#
#                                                                              #
#                     Analyses with only carcasses found                       #
#                         on day with monitoring                               #        
#                                                                              #
#==============================================================================#

library(tidyverse)
library(lubridate)
library(ggpubr)


# A. Effect of the time of the year ============================================

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
  filter(monitored == 1)

# ~ 1. By season ---------------------------------------------------------------

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

hy_carcasses_season_w_transit <- hy_carcasses.monitoring %>%
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

# Do the test
proba3 <- c(length(seq(start_dry, end_dry, by = "day"))/(length(seq(start_dry, end_dry, by = "day")) +
                                                           length(seq(start_wet, end_wet, by = "day"))), # North west
            # South east
            length(seq(start_wet, end_wet, by = "day"))/(length(seq(start_dry, end_dry, by = "day")) +
                                                           length(seq(start_wet, end_wet, by = "day"))))


dbinom(x = hy_carcasses_season_w_transit$n[1], 
       size = sum(hy_carcasses_season_w_transit$n[1:2]),
       prob = proba3[1])
# p = 0.10


# ~ 2. By month ----------------------------------------------------------------
hy_carcasses_month <- hy_carcasses.monitoring %>%
  mutate(month = month(date_obs),
         year = year(date_obs)) %>%
  filter(year < 2020) %>% # Remove the year 2020 as it was incomplete
  count(month)

sum(hy_carcasses_month$n)



# B. Effect of the location of the migratory herd ------------------------------

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
  filter(monitored == 1)

hy_carcasses_season_w_transit <- hy_carcasses.monitoring %>%
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
         year < 2020,
         location_certainty_score >= 0.75,
         herds_position_w_transit %in% c("north west", "south east"))
#lat
wilcox.test(formula = lat ~ herds_position_w_transit, 
            data = hy_carcasses_season_w_transit, 
            alternative = "two.sided", exact = TRUE)
# p = 0.037
#long
wilcox.test(formula = long ~ herds_position_w_transit, 
            data = hy_carcasses_season_w_transit, 
            alternative = "two.sided", exact = TRUE)
# p = 0.008



# C. Effect of age, sex and rank ===============================================

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
  filter(monitored == 1)



Clan_size_year_clan <- read_csv("06_processed_data/hyenas/Clan_size_year_clan.csv", 
                                col_types = cols(X1 = col_skip()))

# ~ 1. Effect of age -----------------------------------------------------------

# Calculate the average age distribution

Clan_size_year_clan <- na.omit(Clan_size_year_clan)

Clan_size_year_clan_I <- Clan_size_year_clan %>%
  filter(clan == "Isiaka") %>%
  group_by(age_class) %>%
  summarise(mean_nb = mean(nb))

Clan_size_year_clan_M <- Clan_size_year_clan %>%
  filter(clan == "Mamba") %>%
  group_by(age_class) %>%
  summarise(mean_nb = mean(nb, na.rm = TRUE))

Clan_size_year_clan_P <- Clan_size_year_clan %>%
  filter(clan == "Pool") %>%
  group_by(age_class) %>%
  summarise(mean_nb = mean(nb, na.rm = TRUE),
            sum_nb = sum(mean_nb))

{total <- 0
  for (i in 1:nrow(Clan_size_year_clan_I)) {
    total <- total + Clan_size_year_clan_I$mean_nb[i]
  }
  proportion_I <- c()
  for (j in 1:nrow(Clan_size_year_clan_I)) {
    proportion_I <- rbind(proportion_I, Clan_size_year_clan_I$mean_nb[j]/total)
  }
  
  
  total <- 0
  for (i in 1:nrow(Clan_size_year_clan_M)) {
    total <- total + Clan_size_year_clan_M$mean_nb[i]
  }
  proportion_M <- c()
  for (j in 1:nrow(Clan_size_year_clan_M)) {
    proportion_M <- rbind(proportion_M, Clan_size_year_clan_M$mean_nb[j]/total)
  }
  
  
  total <- 0
  for (i in 1:nrow(Clan_size_year_clan_P)) {
    total <- total + Clan_size_year_clan_P$mean_nb[i]
  }
  proportion_P <- c()
  for (j in 1:nrow(Clan_size_year_clan_P)) {
    proportion_P <- rbind(proportion_P, Clan_size_year_clan_P$mean_nb[j]/total)
  }
  
  
  probabilities <- c()
  for (i in 1:length(proportion_I)) {
    probabilities <- rbind(probabilities, mean(c(proportion_I[i], proportion_M[i], proportion_P[i])))
  }
  }
sum(probabilities)

probabilities <- t(as.data.frame(probabilities))
colnames(probabilities) <- c("Adult females", 
                             "Adult immigrant males",
                             "Adult natal males", 
                             "Cubs",
                             "Subadults females",
                             "Subadults males")


# Fuse natal and immigrant males, and subadult males and females
proba1 <- cbind(probabilities[1,1],                              # Adult females
                probabilities[1,2] + probabilities[1,3], # Adult males
                probabilities[1,5] + probabilities[1,6], # Subadults
                probabilities[1,4])                      # Cubs

colnames(proba1) <- c("Adult females", 
                      "Adult males",
                      "Subadults", 
                      "Cubs")

# Fuse all adults together, and subadult males and females
proba2 <- cbind(probabilities[1,1] + probabilities[1,2] + probabilities[1,3], # Adults 
                probabilities[1,5] + probabilities[1,6], # Subadults
                probabilities[1,4])
colnames(proba2) <- c("Adults",
                      "Subadults", 
                      "Cubs")

# Count individuals of each age class

{counts <- hy_carcasses.monitoring %>%
    count(age,sex)
  
  nb.ad.F <- as.numeric(counts[1,3])
  nb.ad.M <- as.numeric(counts[2,3])
  nb.ad <- as.numeric(counts[1,3] + counts[2,3] + counts[3,3])
  nb.sub <- as.numeric(counts[7,3] + counts[8,3] + counts[9,3])
  nb.cub <- as.numeric(counts[4,3] + counts[5,3] + counts[6,3])
  # nb.unkn <- as.numeric(counts[10,3] + counts[11,3]) 
  
  # There aren't any carcasses with unknwon age
  
  
  obs <- c(nb.ad.F, nb.ad.M, nb.sub, nb.cub) #adult males and adult females in separate categories
  obs1 <- c(nb.ad, nb.sub, nb.cub) 
  # obs2 <- c(nb.ad + nb.unkn, nb.sub, nb.cub) # unknown age considered as adults.
}

# Statistical tests
dbinom(x = nb.ad + nb.sub, 
       size = nb.ad + nb.sub + nb.cub,
       prob = proba2[1,1] + proba2[1,2])
# 0.016




# ~ 2. Effect of sex -----------------------------------------------------------

Clan_size_year_clan <- na.omit(Clan_size_year_clan)

Clan_size_year_clan_I <- Clan_size_year_clan %>%
  filter(clan == "Isiaka") %>%
  group_by(age_class) %>%
  summarise(mean_nb = mean(nb))

Clan_size_year_clan_M <- Clan_size_year_clan %>%
  filter(clan == "Mamba") %>%
  group_by(age_class) %>%
  summarise(mean_nb = mean(nb, na.rm = TRUE))

Clan_size_year_clan_P <- Clan_size_year_clan %>%
  filter(clan == "Pool") %>%
  group_by(age_class) %>%
  summarise(mean_nb = mean(nb, na.rm = TRUE),
            sum_nb = sum(mean_nb))

{total <- 0
  for (i in 1:nrow(Clan_size_year_clan_I)) {
    total <- total + Clan_size_year_clan_I$mean_nb[i]
  }
  proportion_I <- c()
  for (j in 1:nrow(Clan_size_year_clan_I)) {
    proportion_I <- rbind(proportion_I, Clan_size_year_clan_I$mean_nb[j]/total)
  }
  
  total <- 0
  for (i in 1:nrow(Clan_size_year_clan_M)) {
    total <- total + Clan_size_year_clan_M$mean_nb[i]
  }
  proportion_M <- c()
  for (j in 1:nrow(Clan_size_year_clan_M)) {
    proportion_M <- rbind(proportion_M, Clan_size_year_clan_M$mean_nb[j]/total)
  }
  
  total <- 0
  for (i in 1:nrow(Clan_size_year_clan_P)) {
    total <- total + Clan_size_year_clan_P$mean_nb[i]
  }
  proportion_P <- c()
  for (j in 1:nrow(Clan_size_year_clan_P)) {
    proportion_P <- rbind(proportion_P, Clan_size_year_clan_P$mean_nb[j]/total)
  }
  
  
  probabilities <- c()
  for (i in 1:length(proportion_I)) {
    probabilities <- rbind(probabilities, mean(c(proportion_I[i], proportion_M[i], proportion_P[i])))
  }
  }
# sum(probabilities)

probabilities <- t(as.data.frame(probabilities))
colnames(probabilities) <- c("Adult females", 
                             "Adult immigrant males",
                             "Adult natal males", 
                             "Cubs",
                             "Subadults females",
                             "Subadults males")


ad.sex.ratio <- probabilities[1,1]/(probabilities[1,1] + probabilities[1,2] + probabilities[1,3])

subad.sex.ratio <- probabilities[1,5]/(probabilities[1,5] + probabilities[1,6])

# Count individuals of each sex

# ----------- Adults --
ad.counts <- hy_carcasses.monitoring %>%
  filter(age == "adult",
         sex %in% c("F", "M")) %>%
  count(sex)
nb.ad.F <- as.numeric(ad.counts[1,2])
N <- nb.ad.F + as.numeric(ad.counts[2,2])

# Adults with individuals of unknwon age considered adults
ad.counts.bis <- hy_carcasses.monitoring %>%
  filter(age %in% c("adult", "unknown"),
         sex %in% c("F", "M")) %>%
  count(sex)
nb.ad.F.bis <- as.numeric(ad.counts.bis[1,2])
N.bis <- nb.ad.F.bis + as.numeric(ad.counts.bis[2,2])

# ----------- Adults --
dbinom(x = nb.ad.F, size = N, prob = ad.sex.ratio)
# p = 0.059

# With unknown
dbinom(x = nb.ad.F.bis, size = N.bis, prob = ad.sex.ratio)
# p = 0.059, same thing because no unknown


# ~ 3. Effect of rank ----------------------------------------------------------

hy_carcasses_clan_members <- read_delim("06_processed_data/carcasses/3_hy.clan.members.csv", 
                                        ";", escape_double = FALSE, trim_ws = TRUE) %>%
  filter(ID %in% hy_carcasses.monitoring$ID,
         sex == "F",
         age_category %in% c("adult", "subadult"))

hy_carcasses_clan_members_ad <- hy_carcasses_clan_members %>%
  filter(age_category == "adult")

# Statistical tests
# Only three clan members found on a day with a monitoring sessions
wilcox.test(hy_carcasses_clan_members$standardized_rank, mu = 0, exact = TRUE)

wilcox.test(hy_carcasses_clan_members_ad$standardized_rank, mu = 0, exact = TRUE)


# D. GLM =======================================================================

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
  filter(monitored == 1)

# Calculate the number of carcasses per segment
source(file = "05_scripts/04-2021/GLM_2021-10/GLM_functions_calculate_predictors_2021-10.R")

table.glm_4 <- read_csv("06_processed_data/glm_data_2021-10/table.glm_4.csv")
roads.segmented.cropped.no.short.df <- read_csv("06_processed_data/glm_data_2021-10/roads.segmented.cropped.no.short.df.csv")


hy_carcasses_formatted_spatial <- read_delim("06_processed_data/carcasses/12_hy.carcasses.certainty.formatted.spatial_updated_10-2021.csv", 
                                             ";", escape_double = FALSE, trim_ws = TRUE) %>%
  filter(ID_OG %in% hy_carcasses.monitoring$ID_OG,
         location_certainty_score > 0)


start <- Sys.time()
closest.segment <- GetClosestRoadSegment(carcasses.df = hy_carcasses_formatted_spatial, # ~ 5 min
                                         roads.df = roads.segmented.cropped.no.short.df)
end <- Sys.time() ; end-start ; beepr::beep(9)

table.glm.4.temp <- table.glm_4
table.glm_5 <- AssignCarcassesToSegments(table.glm_4, 
                                         closest.segment) # 5-10 seconds


sum(table.glm_5$nbr_carcasses)
write_csv(table.glm_5, "06_processed_data/glm_data_2021-10/table.glm_5_found_on_day_with_monitoring.csv")

# Replace the column "number of carcasses" in the final table:

buffer.size <- 500
table.glm <- read_csv(paste0("06_processed_data/glm_data_2021-10_high_certainty/table.glm_9.", 
                             buffer.size, "m.csv")) %>%
  mutate(road_importance = factor(road_importance, levels = c("minor_road", "major_road")))
table.glm$nbr_carcasses <- table.glm_5$nbr_carcasses

glm_nb_all <- MASS::glm.nb(formula = nbr_carcasses ~ road_importance + distance_amenity_km
                           + distance_water_km + woodland,
                           data = table.glm,
                           link = log)
summary(glm_nb_all)
anova(glm_nb_all)
