#==============================================================================#
#                                                                              #
#               Statistical tests - Effect of the time of the year             #        
#                                                                              #
#==============================================================================#


library(tidyverse)
library(lubridate)
# library(ggthemes)
library(EMT)
library(RVAideMemoire)
library(ggpubr)
library(cowplot)
Sys.setenv(LANG = "en")

# ~ 1. Count the roadkills -----------------------------------------------------
hy_carcasses <- read_delim("06_processed_data/carcasses/12_hy.carcasses.certainty.formatted.spatial_updated_2022-04.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE) %>%
  mutate(date_obs_new = paste0(substr(date_obs, 1, 4), "-", substr(date_obs, 5, 6), "-", substr(date_obs, 7, 8))) %>%
  mutate(date_obs_new = as.Date(date_obs_new, format = "%Y - %m - %d")) %>%
  dplyr::select(-date_obs) %>%
  rename(date_obs = date_obs_new)


# ~~~ a. During each season -----------------------------------------------------

{ # Seasons from Serengeti IV
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
  filter(year < 2020) %>% # age %in% c("adult", "unknown", "subadult")) %>%
  count(herds_position_w_transit)
sum(hy_carcasses_season_w_transit$n)

# ~~~ b. During each month -----------------------------------------------------
hy_carcasses_month <- hy_carcasses %>%
  mutate(month = month(date_obs),
         year = year(date_obs)) %>%
  filter(year < 2020) %>% # Remove the year 2020 as it was incomplete
  count(month)

sum(hy_carcasses_month$n)

# ~~~ c. Spatiotemporal --------------------------------------------------------

{ # Seasons from Serengeti IV
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

hy_carcasses_season_w_transit_spatial <- hy_carcasses %>%
  filter(location_certainty_score >= 0.75) %>%
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
         herds_position_w_transit %in% c("south east", "north west"))


# ~ 2. Do the tests ------------------------------------------------------------

# ~~~ a. Temporal pattern ------------------------------------------------------

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

dbinom(x = hy_carcasses_season_w_transit$n[hy_carcasses_season_w_transit$herds_position_w_transit == "north west"], 
       size = sum(hy_carcasses_season_w_transit$n[1:2]),
       prob = proba3[1])



# Month
dmultinom(x = hy_carcasses_month$n, 
          prob = c(31/365, 28/365, 31/365, 30/365,
                31/365, 30/365, 31/365, 31/365,
                30/365, 31/365, 30/365, 31/365))

RVAideMemoire::multinomial.theo.multcomp(x = hy_carcasses_month$n, 
                                         p = c(31/365, 28/365, 31/365, 30/365,
                                               31/365, 30/365, 31/365, 31/365,
                                               30/365, 31/365, 30/365, 31/365),  
                                         prop = FALSE,
                                         p.method = "fdr")



# ~~~ b. Spatiotemporal pattern ------------------------------------------------

#lat
wilcox.test(formula = lat ~ herds_position_w_transit, 
            data = hy_carcasses_season_w_transit_spatial, 
            alternative = "two.sided", exact = TRUE)
#long
wilcox.test(formula = long ~ herds_position_w_transit, 
            data = hy_carcasses_season_w_transit_spatial, 
            alternative = "two.sided", exact = TRUE)


