#==============================================================================#
#                                                                              #
#                    Process post-covid monitoring records                     #
#                                                                              #
#==============================================================================#

# In this script, I calculate the samples sizes, the size effects, 
# the total length of roads, the lengths of the segments, etc.

library(tidyverse)
library(lubridate)


# Process post-covid data ------------------------------------------------------
raw_data_field_sessions_post_covid <- read_csv("04_raw_data/hyenas/Daily_monitoring_post_covid_data.csv") %>%
  mutate(date = as.Date(paste0(substr(date_field_session, 1, 4), 
                               "-", substr(date_field_session, 5, 6),
                               "-", substr(date_field_session, 7, 8)), format = "%Y-%m-%d"),
         monitoring = 1) %>%
  distinct(date, monitoring) %>%
  dplyr::select(date, monitoring)

all_dates <- data.frame(date = seq(from = as.Date("2020-04-01"), 
                                   to = as.Date("2022-03-28"), by = "day"))

monitoring_covid <- all_dates %>%
  left_join(x = .,
            y = raw_data_field_sessions_post_covid,
            by = "date")
monitoring_covid[is.na(monitoring_covid)] <- 0

monitoring_pre_covid <- read_csv("04_raw_data/hyenas/Daily_monitoring.csv", 
         col_types = cols(...1 = col_skip())) %>%
  rename(monitoring = monitored)

monitoring <- rbind(monitoring_pre_covid, monitoring_covid)

write_csv(monitoring, "06_processed_data/hyenas/daily_monitoring_1989-2022.csv")


# Calculate proportion per season per year ------------------------------------------------------

monitoring_per_season <- monitoring %>%
  mutate(year = year(date), 
         herds_position_w_transit = ifelse(date %within% interval(ymd(paste0(year, "-08-01")), 
                                                                      ymd(paste0(year, "-10-31"))), 
                                           "north west", 
                                           ifelse(date %within% interval(ymd(paste0(year, "-05-11")), 
                                                                             ymd(paste0(year, "-07-31"))),
                                                  "transit",
                                                  ifelse(date %within% interval(ymd(paste0(year, "-11-01")), 
                                                                                    ymd(paste0(year, "-12-20"))), 
                                                         "transit", "south east"))),
         ones = 1) %>%
  group_by(year, herds_position_w_transit) %>%
  summarize(days_monitored = sum(monitoring),
            days = sum(ones)) %>%
  mutate(prop = days_monitored/days)

write_csv(monitoring_per_season, "06_processed_data/hyenas/sampling_effort_1989-2022.csv")
