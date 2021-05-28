#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#                                                                              #
#                           Figure sampling effort                             #        
#                                                                              #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

library(tidyverse)
library(lubridate)
library(ggthemes)
library(EMT)
library(RVAideMemoire)
Sys.setenv(LANG = "en")

# sampling.effort <- read_delim("4_raw_data/hyenas/Sampling_effort.csv", 
#                               ";", escape_double = FALSE, trim_ws = TRUE)[1:34, 1:3]
# 
# ggplot(data = sampling.effort, aes(x = year, y = proportion_year_presence)) +
#   geom_col(color = "black", fill = "#cccccc") +
#   geom_hline(aes(yintercept = median(proportion_year_presence, na.rm = TRUE))) +
#   geom_hline(aes(yintercept = mean(proportion_year_presence, na.rm = TRUE)), linetype = "dashed") +
#   theme_classic() +
#   labs(x = "Year",
#        y = "Proportion of the year")
# 
# 
# ggsave("11-26 proportion year presence.svg", path = "7_intermediate_results/plots/Sampling_effort",
#        width = unit(6,"cm"), height = unit(2.5,"cm"))


# Load the data ----------------------------------------------------------------
daily.monitoring <- read_csv("4_raw_data/hyenas/Daily_monitoring.csv", 
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
                                                                                ymd(paste0(year, "-10-15"))), "transit", "south east"))),
         ones = 1)

# Remove the year 1987 and 1988, as well as 2019 as it is incomplete 
'%ni%' <- Negate('%in%')
daily.monitoring <- daily.monitoring %>%
  filter(year %ni% c(1987, 1988, 2020))

# Proportion of each season monitored (no transit) -----------------------------
total.days <- daily.monitoring %>%
  group_by(year, herds_position) %>%
  summarize(total.days = sum(ones)) %>%
  dplyr::select(-herds_position)

days.monitored <- daily.monitoring %>%
  group_by(year, herds_position) %>%
  summarize(days.monitored.each.year = sum(monitored)) %>%
  left_join(x = .,
             y = total.days,
             by = "year") %>%
  mutate(proportion.monitored = 100*days.monitored.each.year/total.days)



ggplot(data = days.monitored, 
       aes(x = herds_position, y = proportion.monitored, fill = herds_position)) +
  geom_boxplot(color = "black") +
  theme_classic() +
  scale_fill_manual(limits = c("north west", "south east"),
                    values = c("#E69F00", "#336600")) +
  labs(x = "Season",
       y = "Percentage of the season") +
  theme(legend.position = "")



# Proportion of each season monitored (with transit) -----------------------------
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




# Plot proportion of "herd_position" monitored and nbr of carcasses ------------

hy_carcasses <- read_delim("6_processed_data/carcasses/12_hy.carcasses.certainty.formatted.spatial_updated_04-2021.csv", 
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
  mutate(nbr_carcasses = ifelse(is.na(nbr_carcasses), 0, nbr_carcasses))


ggplot(carcasses_monitoring, aes(x = proportion.monitored, y = nbr_carcasses)) +
  geom_point() +
  geom_smooth(method = lm)

x <- lm(formula = nbr_carcasses ~ proportion.monitored,
   data = carcasses_monitoring)
summary(x)




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


