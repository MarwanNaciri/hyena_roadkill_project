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


# ~~~ a. By month --------------------------------------------------------------
daily.monitoring <- read_csv("06_processed_data/hyenas/daily_monitoring_1989-2022.csv")

daily.monitoring.month <- daily.monitoring %>%
  mutate(year = year(date),
         month = month(date)) %>%
  group_by(year, month) %>%
  summarise(monitoring = mean(monitoring))   %>% 
  mutate(herds_position_w_transit = ifelse(month %in% c(8:10), "north west", 
                                           ifelse(month %in% c(5:7, 11, 12), "transit",
                                                  "south east")),
         covid = ifelse(year %in% c(2020, 2021), "covid", "normal"))


kruskal.test(formula = monitoring ~ month, data = daily.monitoring.month)

res <- dunn.test::dunn.test(x = daily.monitoring.month$monitoring, g = daily.monitoring.month$month, 
                     method = "bh",
                     kw = TRUE, # Do the Kruskal Wallis test
                     label = TRUE)


# ~~~ b. By season -------------------------------------------------------------
sampling.effort.season <- read_csv("06_processed_data/hyenas/sampling_effort_1989-2022.csv")

# Remove the "transit season"
daily.monitoring.season.no.transit <- sampling.effort.season %>%
  filter(herds_position_w_transit %in% c("north west", "south east")) %>%
  mutate(covid = ifelse(year %in% c(2020, 2021), "covid", "normal"))

wilcox.test(formula = prop ~ herds_position_w_transit, 
            data = daily.monitoring.season.no.transit, 
            alternative = "two.sided", exact = TRUE)



