#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#                                                                              #
#               Statistical tests - Effect of the time of the year             #        
#                                                                              #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

library(tidyverse)
library(lubridate)
library(ggthemes)
library(EMT)
library(RVAideMemoire)
library(ggpubr)
Sys.setenv(LANG = "en")


# A. All carcasses -------------------------------------------------------------

# ~ 1. Count the roadkills -----------------------------------------------------
hy_carcasses <- read_delim("06_processed_data/carcasses/12_hy.carcasses.certainty.formatted.spatial_updated_04-2021.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE) %>%
  mutate(date_death = as.Date(date_death, format = "%d / %m / %Y"))

# ~~~ a. Marion's seasons ----------------------------------------------

start_dry <- as.Date("1989-05-26")
end_dry <- as.Date("1989-10-31")
nbr_days <- 365


hy_carcasses_season <- hy_carcasses %>%
  mutate(year = year(date_death), 
         herds_position = ifelse(date_death %within% interval(ymd(paste0(year, "-05-26")), 
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
hy_carcasses_season_w_transit <- hy_carcasses %>%
  mutate(year = year(date_death), 
         herds_position_w_transit = ifelse(date_death %within% interval(ymd(paste0(year, "-06-26")), 
                                                                        ymd(paste0(year, "-10-25"))), 
                                           "north west", 
                                           ifelse(date_death %within% interval(ymd(paste0(year, "-04-26")), 
                                                                               ymd(paste0(year, "-06-25"))),
                                                  "transit",
                                                  ifelse(date_death %within% interval(ymd(paste0(year, "-10-26")), 
                                                                                      ymd(paste0(year, "-11-15"))), 
                                                         "transit", "south east")))) %>%
  count(herds_position_w_transit)



# ~~~ b. Season : Serengeti IV -------------------------------------------------

start_dry <- as.Date("1989-08-01")
end_dry <- as.Date("1989-10-31")
start_wet <- as.Date("1989-12-21")
end_wet <- as.Date("1990-05-10")
nbr_days <- length(seq(start_dry, end_dry, by = "day")) + length(seq(start_wet, end_wet, by = "day"))

hy_carcasses_season <- hy_carcasses %>%
  mutate(year = year(date_death), 
         herds_position = ifelse(date_death %within% interval(ymd(paste0(year, "-08-01")), 
                                                              ymd(paste0(year, "-10-31"))), 
                                 "north west", 
                                 ifelse(date_death %within% interval(ymd(paste0(year, "-01-01")),
                                                                     ymd(paste0(year, "-05-10"))),
                                        "south east", 
                                        ifelse(date_death %within% interval(ymd(paste0(year, "-12-21")),
                                                                            ymd(paste0(year, "-12-31"))),
                                               "south east", NA)))) %>%
  count(herds_position) %>%
  filter(!is.na(herds_position))


# ~~~ c. During each month -----------------------------------------------------
hy_carcasses_month <- hy_carcasses %>%
  mutate(month = month(date_death),
         year = year(date_death)) %>%
  filter(year < 2020) %>%
  count(month)

sum(hy_carcasses_month$n)



# ~ 2. Do the tests ------------------------------------------------------------

# Season (no transit)
proba <- c(length(seq(start_dry, end_dry, by = "day"))/nbr_days, 
           (nbr_days - length(seq(start_dry, end_dry, by = "day")))/nbr_days)

dbinom(x = hy_carcasses_season$n[1], 
       size = sum(hy_carcasses_season$n),
       prob = proba[1])


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


# Month
RVAideMemoire::multinomial.theo.multcomp(x = hy_carcasses_month$n, 
                                         p = c(31/365, 28/365, 31/365, 30/365,
                                               31/365, 30/365, 31/365, 31/365,
                                               30/365, 31/365, 30/365, 31/365),  
                                         prop = FALSE,
                                         p.method = "fdr")





# B. Carcasses >= 0.5 ----------------------------------------------------------

# ~ 1. Count the roadkills -----------------------------------------------------
hy_carcasses_0.5 <- read_delim("06_processed_data/carcasses/12_hy.carcasses.certainty.formatted.spatial_updated_04-2021.csv", 
                               ";", escape_double = FALSE, trim_ws = TRUE) %>%
  mutate(date_death = as.Date(date_death, format = "%d / %m / %Y")) %>%
  filter(collision_certainty_score_NEW >= 0.5)


# ~~~ a. Marion's seasons ----------------------------------------------

start_dry <- as.Date("1989-05-26")
end_dry <- as.Date("1989-10-31")
nbr_days <- 365

hy_carcasses_season_0.5 <- hy_carcasses_0.5 %>%
  mutate(year = year(date_death), 
         herds_position = ifelse(date_death %within% interval(ymd(paste0(year, "-05-26")), 
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
  mutate(year = year(date_death), 
         herds_position_w_transit = ifelse(date_death %within% interval(ymd(paste0(year, "-06-26")), 
                                                                        ymd(paste0(year, "-10-25"))), 
                                           "north west", 
                                           ifelse(date_death %within% interval(ymd(paste0(year, "-04-26")), 
                                                                               ymd(paste0(year, "-06-25"))),
                                                  "transit",
                                                  ifelse(date_death %within% interval(ymd(paste0(year, "-10-26")), 
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
  mutate(year = year(date_death), 
         herds_position = ifelse(date_death %within% interval(ymd(paste0(year, "-08-01")), 
                                                              ymd(paste0(year, "-10-31"))), 
                                 "north west", 
                                 ifelse(date_death %within% interval(ymd(paste0(year, "-01-01")),
                                                                     ymd(paste0(year, "-05-10"))),
                                        "south east", 
                                        ifelse(date_death %within% interval(ymd(paste0(year, "-12-21")),
                                                                            ymd(paste0(year, "-12-31"))),
                                               "south east", NA)))) %>%
  count(herds_position) %>%
  filter(!is.na(herds_position))


# ~~~ c. During each month -----------------------------------------------------
hy_carcasses_month_0.5 <- hy_carcasses_0.5 %>%
  mutate(month = month(date_death),
         year = year(date_death)) %>%
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
hy_carcasses_0.75 <- read_delim("06_processed_data/carcasses/12_hy.carcasses.certainty.formatted.spatial_updated_04-2021.csv", 
                               ";", escape_double = FALSE, trim_ws = TRUE) %>%
  mutate(date_death = as.Date(date_death, format = "%d / %m / %Y")) %>%
  filter(collision_certainty_score_NEW >= 0.75)


# ~~~ a. Marion's seasons ----------------------------------------------

start_dry <- as.Date("1989-05-26")
end_dry <- as.Date("1989-10-31")
nbr_days <- 365

hy_carcasses_season_0.75 <- hy_carcasses_0.75 %>%
  mutate(year = year(date_death), 
         herds_position = ifelse(date_death %within% interval(ymd(paste0(year, "-05-26")), 
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
  mutate(year = year(date_death), 
         herds_position_w_transit = ifelse(date_death %within% interval(ymd(paste0(year, "-06-26")), 
                                                                        ymd(paste0(year, "-10-25"))), 
                                           "north west", 
                                           ifelse(date_death %within% interval(ymd(paste0(year, "-04-26")), 
                                                                               ymd(paste0(year, "-06-25"))),
                                                  "transit",
                                                  ifelse(date_death %within% interval(ymd(paste0(year, "-10-26")), 
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
  mutate(year = year(date_death), 
         herds_position = ifelse(date_death %within% interval(ymd(paste0(year, "-08-01")), 
                                                              ymd(paste0(year, "-10-31"))), 
                                 "north west", 
                                 ifelse(date_death %within% interval(ymd(paste0(year, "-01-01")),
                                                                     ymd(paste0(year, "-05-10"))),
                                        "south east", 
                                        ifelse(date_death %within% interval(ymd(paste0(year, "-12-21")),
                                                                            ymd(paste0(year, "-12-31"))),
                                               "south east", NA)))) %>%
  count(herds_position) %>%
  filter(!is.na(herds_position))


# ~~~ c. During each month -----------------------------------------------------
hy_carcasses_month_0.75 <- hy_carcasses_0.75 %>%
  mutate(month = month(date_death),
         year = year(date_death)) %>%
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

hy_carcasses_formatted <- read_delim("06_processed_data/carcasses/12_hy.carcasses.certainty.formatted.spatial.csv", 
                                     ";", escape_double = FALSE, trim_ws = TRUE)

hy_carcasses_formatted$date_death <- as.Date(hy_carcasses_formatted$date_death, 
                                             format = "%d / %m / %Y")

hy.carcasses.decade <- hy_carcasses_formatted %>%
  mutate(year = lubridate::year(date_death),
         temp = floor_date(date_death, years(10)),
         decade = lubridate::year(temp)) %>%
  filter(decade %in% c("1990", "2000", "2010")) %>%
  dplyr::select(ID, long, lat, collision_certainty_score_NEW, GPS_certainty_score, age, sex, year, decade) %>%
  mutate(years_by_five = ifelse(year >= 1990 & year <1995, "1990-1994",
                                ifelse(year >=1995 & year <2000, "1995-1999",
                                       ifelse(year >=2000 & year <2005, "2000-2004", 
                                              ifelse(year >=2005 & year <2010, "2005-2009",
                                                     ifelse(year >=2010 & year <2015, "2010-2015", "2016-2020"))))))


count.year <- hy_carcasses_formatted %>%
  mutate(year = year(date_death)) %>%
  count(year)

ggplot(data = count.year, aes(x = year, y = n)) +
  geom_col()

counts.decade <- hy.carcasses.decade %>%
  count(decade)

ggplot(data = counts.decade, aes(x = decade, y = n)) +
  geom_col()


counts.by.five <- hy.carcasses.decade %>%
  count(years_by_five)

ggplot(data = counts.by.five, aes(x = years_by_five, y = n)) +
  geom_col()

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
