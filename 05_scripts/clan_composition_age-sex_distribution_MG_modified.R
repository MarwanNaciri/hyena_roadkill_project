#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#                                                                              #
#                               COMPUTING CLAN SIZE                            #
#                                                                              #                      
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

# PACKAGES
library(tidyverse)
library(lubridate)
library(ggthemes)



# DATA
Clan_members <- read_delim("4_raw_data/hyenas/Clan_members.csv", 
                           ";", escape_double = FALSE, 
                           col_types = cols(X1 = col_skip()), 
                           trim_ws = TRUE)

# FORMATTING
Clan_members$start <- as.Date(Clan_members$start, format = "%d / %m / %Y")
Clan_members$end <- as.Date(Clan_members$end, format = "%d / %m / %Y")


Clan_members <- Clan_members %>%
  mutate(age_class = ifelse(age_class == "Adult females (> 2 years)", "Adult females (> 2 years)",
                            ifelse(age_class == "Adult immigrant males (> 2 years)", "Adult immigrant males (> 2 years)",
                                   ifelse(age_class == "Adult natal males (> 2 years)", "Adult natal males (> 2 years)", 
                                          ifelse(age_class == "Cubs (0-1 year)", "Cubs (0-1 year)",
                                                 ifelse(age_class == "Subadults (1-2 years)" & sex == "1", "Subadult males (1-2 years)",
                                                        "Subadult females (1-2 years)"))))))



all_presence <- Clan_members %>%
  dplyr::mutate(interval_presence_clan = lubridate::interval(start, end)) %>%
  dplyr::mutate(days = lubridate::time_length(interval_presence_clan, "days")) %>%
  dplyr::select(ID_natal, clan, age_class, sex, start, end, days)


all_presence_expanded <- all_presence[rep(seq.int(1, nrow(all_presence)), all_presence$days),]
all_presence_expanded <- data.frame(all_presence_expanded, 
                                    date = as.Date(all_presence_expanded$start) + (sequence(all_presence$days))-1)
all_presence_expanded$date <- as.Date(all_presence_expanded$date)


Clan_size_year_clan <- all_presence_expanded %>%
  dplyr::mutate(year = lubridate::year(date)) %>%
  dplyr::group_by(clan, age_class, date, year) %>%
  dplyr::summarise(nb = n()) %>%
  dplyr::group_by(clan, age_class, year) %>%
  dplyr::summarise(nb = round(mean(nb),0)) %>%
  dplyr::filter(year > 1987, year < 2020) %>%
  dplyr::mutate(nb = ifelse(clan %in% c("M","P") & year < 1991, NA, nb))

Clan_size_year_clan$clan <- factor(Clan_size_year_clan$clan, labels = c("Isiaka","Mamba","Pool"))
Clan_size_year_clan$age_class <- factor(Clan_size_year_clan$age_class, labels = c("Adult females (> 2 years)", 
                                                                                  "Adult immigrant males (> 2 years)",
                                                                                  "Adult natal males (> 2 years)", 
                                                                                  "Cubs (0-1 year)",
                                                                                  "Subadults females (1-2 years)",
                                                                                  "Subadults males (1-2 years)"))


write.csv(Clan_size_year_clan, "6_processed_data/hyenas/Clan_size_year_clan.csv")

# GRAPHS

plot <- ggplot(data = Clan_size_year_clan) +
  scale_y_continuous(breaks = c(0,10,20,30,40,50)) +
  scale_x_continuous(breaks = seq(1988,2020,4)) +
  geom_line(aes(x = year, y = nb, color = age_class)) +
  geom_point(aes(x = year, y = nb, color = age_class)) +
  facet_wrap(~ clan) +
  labs(x = "Year", y = "Mean clan size", color = "Age classes: ") +
  theme_few() +
  theme(legend.position = "bottom")

plot



## Added by Marwan -------------------------------------------------------------

# Calculate the total  number of males/females ----------------------------------

Clan_members <- read_delim("4_raw_data/hyenas/Clan_members.csv", 
                           ";", escape_double = FALSE, 
                           col_types = cols(X1 = col_skip()), 
                           trim_ws = TRUE)

# FORMATTING
Clan_members$start <- as.Date(Clan_members$start, format = "%d / %m / %Y")
Clan_members$end <- as.Date(Clan_members$end, format = "%d / %m / %Y")

adults.sex <- Clan_members %>%
  filter(age_class %in% c("Adult females (> 2 years)", 
                          "Adult immigrant males (> 2 years)", 
                          "Adult natal males (> 2 years)")) %>%
  mutate(ID_mix = ifelse(!is.na(ID_natal), ID_natal, ID_new)) %>%
  distinct(ID_mix, sex) %>%
  count(sex)

nbr_hyenas <- Clan_members %>%
  mutate(ID_mix = ifelse(!is.na(ID_natal), ID_natal, ID_new)) %>%
  distinct(ID_mix)


nrow(nbr_hyenas_adult <- Clan_members %>%
  filter(age_class %in% c("Adult females (> 2 years)", 
                          "Adult immigrant males (> 2 years)", 
                          "Adult natal males (> 2 years)")) %>%
  mutate(ID_mix = ifelse(!is.na(ID_natal), ID_natal, ID_new)) %>%
  distinct(ID_mix))

nrow(nbr_hyenas_subadult <- Clan_members %>%
  filter(age_class %in% c("Subadults (1-2 years)")) %>%
  mutate(ID_mix = ifelse(!is.na(ID_natal), ID_natal, ID_new)) %>%
  distinct(ID_mix))

nrow(nbr_hyenas_cub <- Clan_members %>%
  filter(age_class %in% c("Cubs (0-1 year)")) %>%
  mutate(ID_mix = ifelse(!is.na(ID_natal), ID_natal, ID_new)) %>%
  distinct(ID_mix))
  


# Calculate proportions of females/adults/etc.  --------------------------------
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

total <- 0
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
sum(probabilities)
probabilities <- as.data.frame(probabilities)

# Adult females
# 0.36205526

# Adult immigrant male 
# 0.22126760

# Adult natal male
# 0.07750200

# Cubs
# 0.20712074

# Subadult female 
# 0.06790307

# Subadult male 
# 0.06415134





