#==============================================================================#
#                                                                              #
#                           Figure sampling effort                             #        
#                                                                              #
#==============================================================================#

library(tidyverse)
library(lubridate)
library(ggthemes)
library(EMT)
library(RVAideMemoire)
library(ggpubr)
Sys.setenv(LANG = "en")


# IMPORTANT_____________________________________________________________________
# Check script in the statistical_tests folder, it's more recent, but there's 
# less code in it


# A. Marion's seasons ----------------------------------------------------------

# Load the data
daily.monitoring <- read_csv("04_raw_data/hyenas/Daily_monitoring.csv", 
                             col_types = cols(X1 = col_skip())) %>%
  mutate(year = year(date), 
         herds_position = ifelse(date %within% interval(ymd(paste0(year, "-05-26")), 
                                                        ymd(paste0(year, "-10-31"))), 
                                 "north west", "south east"),
         herds_position_w_transit = ifelse(date %within% interval(ymd(paste0(year, "-06-26")), 
                                                                  ymd(paste0(year, "-10-25"))), 
                                           "north west", 
                                           ifelse(date %within% interval(ymd(paste0(year, "-04-26")), 
                                                                         ymd(paste0(year, "-06-25"))),"transit",
                                                  ifelse(date %within% interval(ymd(paste0(year, "-10-25")), 
                                                                                ymd(paste0(year, "-11-15"))), "transit", "south east"))))
# Remove the year 1987 and 1988, as well as 2019 as it is incomplete 
'%ni%' <- Negate('%in%')
daily.monitoring <- daily.monitoring %>%
  filter(year %ni% c(1987, 1988, 2020))

# ~~~ a. Proportion season monitored (no transit) ------------------------------
total.days <- daily.monitoring %>%
  count(year, herds_position) %>%
  dplyr::select(-herds_position)

days.monitored <- daily.monitoring %>%
  group_by(year, herds_position) %>%
  summarize(days.monitored.each.year = sum(monitored)) %>%
  left_join(x = .,
             y = total.days,
             by = "year") %>%
  mutate(proportion.monitored = 100*days.monitored.each.year/n)

sum(days.monitored$days.monitored.each.year < days.monitored$n)

wilcox.test(formula = proportion.monitored ~ herds_position, data = days.monitored, 
            alternative = "two.sided", exact = TRUE)



ggplot(data = days.monitored, 
       aes(x = herds_position, y = proportion.monitored, fill = herds_position)) +
  geom_boxplot(color = "black") +
  theme_classic() +
  scale_fill_manual(limits = c("north west", "south east"),
                    values = c("#E69F00", "#336600")) +
  labs(x = "Season",
       y = "Proportion of the season monitored (%)") +
  stat_compare_means(method="wilcox.test") +
  theme(legend.position = "")

ggsave("10_meetings/2021-06-16 Meeting with Sarah, Aimara & Morgane/days_monitored.png",
       width = 3.5, height = 3.5)


# ~~~ b. Proportion season monitored (with transit) ----------------------------

total.days <- daily.monitoring %>%
  group_by(year, herds_position_w_transit) %>%
  summarize(total.days = sum(ones))

days.monitored <- daily.monitoring %>%
  group_by(year, herds_position_w_transit) %>%
  summarize(days.monitored.each.year = sum(monitored)) %>%
  left_join(x = .,
            y = total.days,
            by = c("year", "herds_position_w_transit")) %>%
  mutate(proportion.monitored = 100*days.monitored.each.year/total.days)


ggplot(data = days.monitored, 
       aes(x = herds_position_w_transit, y = proportion.monitored, fill = herds_position_w_transit)) +
  geom_boxplot(color = "black") +
  theme_classic() +
  scale_fill_manual(limits = c("north west", "south east", "transit"),
                    values = c("#E69F00", "#336600", "#997800")) +
  labs(x = "Position of the migratory herds",
       y = "Percentage of the period") +
  theme(legend.position = "")



# ~~~ c. Proportion season monitored VS nbr carcasses --------------------------

hy_carcasses <- read_delim("06_processed_data/carcasses/12_hy.carcasses.certainty.formatted.spatial_updated_04-2021.csv", 
                                     ";", escape_double = FALSE, trim_ws = TRUE) %>%
  mutate(date_death = as.Date(date_death, format = "%d/%m/%Y"),
         year = year(date_death),
         herds_position = ifelse(date_death %within% interval(ymd(paste0(year, "-05-26")), 
                                                        ymd(paste0(year, "-10-31"))), 
                                 "north west", "south east"),
         herds_position_w_transit = ifelse(date_death %within% interval(ymd(paste0(year, "-06-26")), 
                                                                  ymd(paste0(year, "-10-25"))), 
                                           "north west", 
                                           ifelse(date_death %within% interval(ymd(paste0(year, "-04-26")), 
                                                                         ymd(paste0(year, "-06-25"))),"transit",
                                                  ifelse(date_death %within% interval(ymd(paste0(year, "-10-25")), 
                                                                                ymd(paste0(year, "-10-15"))), "transit", "south east"))),
         ones = 1) %>%
  group_by(year, herds_position) %>%
  summarize(nbr_carcasses = sum(ones)) %>%
  filter(!year == 2020)



carcasses_monitoring <- days.monitored %>%
  left_join(x = days.monitored,
            y = hy_carcasses,
            by = c("year", "herds_position")) %>%
  mutate(nbr_carcasses = ifelse(is.na(nbr_carcasses), 0, nbr_carcasses),
         proportion.monitored.scaled = scale(proportion.monitored),
         proportion.monitored.scaled.squarred = proportion.monitored.scaled^2)


ggplot(carcasses_monitoring, aes(x = proportion.monitored, y = nbr_carcasses)) +
  geom_point() +
  geom_smooth(method = lm)

x <- lm(formula = nbr_carcasses ~ proportion.monitored,
   data = carcasses_monitoring)
summary(x)



# B. Serengeti IV seasons ----------------------------------------------------------

start_dry <- as.Date("1989-08-01")
end_dry <- as.Date("1989-10-31")
start_wet <- as.Date("1989-12-21")
end_wet <- as.Date("1990-05-10")
nbr_days <- length(seq(start_dry, end_dry, by = "day")) + length(seq(start_wet, end_wet, by = "day"))

# Load the data
daily.monitoring <- read_csv("04_raw_data/hyenas/Daily_monitoring.csv", 
                             col_types = cols(X1 = col_skip())) %>%
  mutate(year = year(date), 
         herds_position = ifelse(date %within% interval(ymd(paste0(year, "-08-01")), 
                                                              ymd(paste0(year, "-10-31"))), 
                                 "north west", 
                                 ifelse(date %within% interval(ymd(paste0(year, "-01-01")),
                                                                     ymd(paste0(year, "-05-10"))),
                                        "south east", 
                                        ifelse(date %within% interval(ymd(paste0(year, "-12-21")),
                                                                            ymd(paste0(year, "-12-31"))),
                                               "south east", NA)))) %>%
  filter(!is.na(herds_position),
         # Remove the year 1987 and 1988, as well as 2020 as it is incomplete 
         year > 1988,
         year < 2020)


# ~~~ a. Proportion season monitored (no transit) ------------------------------
total.days <- daily.monitoring %>%
  count(year, herds_position) 

days.monitored <- daily.monitoring %>%
  group_by(year, herds_position) %>%
  summarize(days.monitored.each.year = sum(monitored)) %>%
  left_join(x = .,
            y = total.days,
            by = c("year", "herds_position")) %>%
  mutate(proportion.monitored = 100*days.monitored.each.year/n)

sum(days.monitored$days.monitored.each.year < days.monitored$n)

wilcox.test(formula = proportion.monitored ~ herds_position, data = days.monitored, 
            alternative = "two.sided", exact = TRUE)

test <- days.monitored %>%
  count(herds_position)


# ggplot(data = days.monitored,
#        aes(x = proportion.monitored, fill = herds_position)) +
#   # geom_histogram(color = "black") +
#   geom_density(color = "black", alpha = 0.5) +
#   scale_fill_manual(limits = c("north west", "south east"),
#                     values = c("#E69F00", "#336600")) +
#   theme_classic()
  
  

ggplot(data = days.monitored, 
       aes(x = herds_position, y = proportion.monitored, fill = herds_position)) +
  geom_boxplot(color = "black") +
  theme_classic() +
  scale_fill_manual(limits = c("north west", "south east"),
                    values = c("#E69F00", "#336600")) +
  labs(x = "Season",
       y = "Proportion of the season monitored (%)") +
  stat_compare_means(method="wilcox.test") +
  theme(legend.position = "")

ggsave(filename = "07_intermediate_results/2021-04/plots/10-24_days_monitored_Serengeti_IV_seasons.png",
       width = 3.5, height = 3.5)




# ~~~ c. Proportion season monitored VS nbr carcasses --------------------------

hy_carcasses <- read_delim("06_processed_data/carcasses/12_hy.carcasses.certainty.formatted.spatial_updated_04-2021.csv", 
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
  count(year, herds_position)


carcasses_monitoring <- days.monitored %>%
  left_join(x = days.monitored,
            y = hy_carcasses_herds_position,
            by = c("year", "herds_position")) %>%
  mutate(nbr_carcasses = ifelse(is.na(n.y), 0, n.y))

ggplot(carcasses_monitoring, aes(x = proportion.monitored, y = nbr_carcasses, color = herds_position)) +
  geom_point() +
  geom_smooth(method = lm)

x <- lm(formula = nbr_carcasses ~ proportion.monitored + herds_position,
        data = carcasses_monitoring)
summary(x)



# C. Year ----------------------------------------------------------------------

# Plot proportion of year monitored and nbr of carcasses -----------------------

hy_carcasses <- read_delim("6_processed_data/carcasses/12_hy.carcasses.certainty.formatted.spatial_updated_04-2021.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE) %>%
  mutate(date_death = as.Date(date_death, format = "%d/%m/%Y"),
         year = year(date_death),
         ones = 1) %>%
  group_by(year) %>%
  summarize(nbr_carcasses = sum(ones)) %>%
  filter(!year == 2020)

total.days <- daily.monitoring %>%
  group_by(year) %>%
  summarize(total.days = sum(ones))

days.monitored <- daily.monitoring %>%
  group_by(year) %>%
  summarize(days.monitored.each.year = sum(monitored)) %>%
  left_join(x = .,
            y = total.days,
            by = c("year")) %>%
  mutate(proportion.monitored = 100*days.monitored.each.year/total.days) 

carcasses_monitoring <- days.monitored %>%
  left_join(x = days.monitored,
            y = hy_carcasses,
            by = c("year")) %>%
  mutate(nbr_carcasses = ifelse(is.na(nbr_carcasses), 0, nbr_carcasses))



ggplot(carcasses_monitoring, aes(x = proportion.monitored, y = nbr_carcasses)) +
  geom_point() +
  geom_smooth(method = lm)

x <- lm(formula = nbr_carcasses ~ proportion.monitored,
        data = carcasses_monitoring)
summary(x)


