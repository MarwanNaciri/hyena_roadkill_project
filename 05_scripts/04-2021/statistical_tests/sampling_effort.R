#==============================================================================#
#                                                                              #
#                    Statistical tests for sampling effort                     #        
#                                                                              #
#==============================================================================#

library(tidyverse)
library(lubridate)
library(ggpubr)

# IMPORTANT_____________________________________________________________________
# Check script in the figures folder, it's older, but there's more code in it


# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!





# A. Per year ---------------------------------------------

sampling.effort <- read_delim("04_raw_data/hyenas/Sampling_effort.csv", 
                              ";", escape_double = FALSE, trim_ws = TRUE)[1:34, 1:3]

ggplot(data = sampling.effort, aes(x = year, y = proportion_year_presence)) +
  geom_col(color = "black", fill = "#cccccc") +
  geom_hline(aes(yintercept = median(proportion_year_presence, na.rm = TRUE))) +
  geom_hline(aes(yintercept = mean(proportion_year_presence, na.rm = TRUE)), linetype = "dashed") +
  theme_classic() +
  labs(x = "Year",
       y = "Proportion of the year")


ggsave("07_intermediate_results/2021-04/plots/sampling_effort/2021-10-08 proportion year presence.png",
       width = 4, height = 3)



# B. Within years --------------------------------------------------------------

# ~ 1. By "position of migratory herds" ----------------------------------------

daily.monitoring <- read_csv("04_raw_data/hyenas/Daily_monitoring.csv", 
                             col_types = cols(X1 = col_skip()))

  
daily.monitoring.season <- daily.monitoring %>%
  mutate(year = year(date),
         month = month(date)) %>%
  mutate(herds_position_w_transit = ifelse(date %within% interval(ymd(paste0(year, "-08-01")), 
                                                                  ymd(paste0(year, "-10-31"))), 
                                           "north west", 
                                           ifelse(date %within% interval(ymd(paste0(year, "-05-11")), 
                                                                         ymd(paste0(year, "-07-31"))),
                                                  "transit",
                                                  ifelse(date %within% interval(ymd(paste0(year, "-11-01")), 
                                                                                ymd(paste0(year, "-12-20"))), 
                                                         "transit", "south east")))) %>%
  group_by(year, herds_position_w_transit) %>%
  summarise(monitoring = mean(monitored))

# Remove the "transit season"
daily.monitoring.season.no.transit <- daily.monitoring.season %>%
  filter(herds_position_w_transit %in% c("north west", "south east"))

(sampling.season <- ggplot(data = daily.monitoring.season.no.transit, 
                          aes(x = herds_position_w_transit, y = monitoring*100, 
                              fill = herds_position_w_transit)) + # , color = year)) +
  geom_boxplot() +
  geom_jitter(width = 0.20) + 
  scale_x_discrete(limits = c("south east", "north west"),
                   # labels = c("wet", "dry")) +
                   labels = c("late Dec -\nearly May", "Aug - Oct")) +
  scale_fill_manual(limits = c("south east", "north west"),
                    labels = c("south east", "north & MMR"),
                    values = c("#336600", "#E69F00")) +
  theme_classic() +
  # theme(legend.position = "") + 
  labs(x = "period",
       y = "percentage of days with \nmonitoring session",
       fill = "wildebeest herds' \nlocation"))


ggsave("07_intermediate_results/2021-04/plots/sampling_effort/2021-10-08 boxplot_monitoring_season.png",
       width = 5, height = 3)




# t-test is impossible since the distribution of the percentage of monitoring in the NW is not normal
shapiro.test(daily.monitoring.season.no.transit$monitoring[daily.monitoring.season.no.transit$herds_position_w_transit == "south east"])
shapiro.test(daily.monitoring.season.no.transit$monitoring[daily.monitoring.season.no.transit$herds_position_w_transit == "north west"])

# Wilcoxon test

# Before the wilcoxon test, let's add an infinitesimal number to each row to avoid
# ties
daily.monitoring.season.no.transit$monitoring <- daily.monitoring.season.no.transit$monitoring +
  seq(0.0000000001, 0.0000000001*nrow(daily.monitoring.season.no.transit), by = 0.0000000001)

wilcox.test(monitoring ~ herds_position_w_transit, data = daily.monitoring.season.no.transit)




# ~ 2. By month ----------------------------------------------------------------

daily.monitoring <- read_csv("04_raw_data/hyenas/Daily_monitoring.csv", 
                             col_types = cols(X1 = col_skip()))


daily.monitoring.month <- daily.monitoring %>%
  mutate(year = year(date),
         month = month(date)) %>%
  group_by(year, month) %>%
  summarise(monitoring = mean(monitored))   %>% 
  mutate(herds_position_w_transit = ifelse(month %in% c(8:10), "north west", 
                                           ifelse(month %in% c(5:7, 11, 12), "transit",
                                                  "south east")))
  
(sampling.month <- ggplot(data = daily.monitoring.month, 
       aes(x = as.factor(month), y = monitoring*100, 
           fill = herds_position_w_transit)) +
  geom_boxplot(color = "#222222") +
  scale_x_discrete(labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                              "Aug", "Sep", "Oct", "Nov", "Dec")) +
  
  scale_fill_manual(limits = c("south east", "north west", "transit"),
                    labels = c("south east", "north & MMR", "transit"),
                    values = c("#336600", "#E69F00", "#333333")) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # legend.position = "",
  labs(x = "month",
       y = "percentage of days with \nmonitoring session",
       fill = "wildebeest herd's \nlocation")) 
  
ggsave("07_intermediate_results/2021-04/plots/sampling_effort/2021-10-08 boxplot_monitoring_month.png",
       width = 7, height = 3)


# ~ 3. Build the figure --------------------------------------------------------

library(cowplot)
(
  figure.S4 <- ggdraw() +
    draw_plot(sampling.month, x = 0.04,  y = 0, width = 0.55, height = 1) +
    draw_plot(sampling.season, x = 0.61, y = 0, width = 0.39, height = 1) +
  
    draw_plot_label(label = c("(a)", "(b)"),
                    x = c(0, 0.56), y = c(1, 1), size = 14) 
  
)

save_plot("11_manuscript/V3 Figures/figure S4 raw.svg", 
          plot = figure.S4,
          ncol = 2,
          nrow = 1,
          base_height = 2.5,
          base_asp =  2) #1.618)

# C. By "position of migratory herds" over years -------------------------------

daily.monitoring <- read_csv("04_raw_data/hyenas/Daily_monitoring.csv", 
                             col_types = cols(X1 = col_skip()))


daily.monitoring.season <- daily.monitoring %>%
  mutate(year = year(date),
         month = month(date)) %>%
  mutate(herds_position_w_transit = ifelse(date %within% interval(ymd(paste0(year, "-08-01")), 
                                                                  ymd(paste0(year, "-10-31"))), 
                                           "north & MMR", 
                                           ifelse(date %within% interval(ymd(paste0(year, "-05-11")), 
                                                                         ymd(paste0(year, "-07-31"))),
                                                  "transit",
                                                  ifelse(date %within% interval(ymd(paste0(year, "-11-01")), 
                                                                                ymd(paste0(year, "-12-20"))), 
                                                         "transit", "south-east")))) %>%
  group_by(year, herds_position_w_transit) %>%
  summarise(monitoring = mean(monitored))

# Remove the "transit season"
daily.monitoring.season.no.transit <- daily.monitoring.season %>%
  filter(herds_position_w_transit %in% c("north & MMR", "south-east")) %>%
  mutate(herds_position_w_transit = factor(herds_position_w_transit, 
                                           levels = c("south-east", "north & MMR")))

ggplot(daily.monitoring.season.no.transit, aes(x = year, y = monitoring*100)) +
  geom_col() +
  theme_classic() +
  facet_wrap(. ~ herds_position_w_transit) +
  labs(x = "year",
       y = "percentage of days with \nmonitoring session")



