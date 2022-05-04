#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#                                                                              #
#            Statistical test of the effect of age, sex and rank               #        
#                                                                              #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

library(tidyverse)
Sys.setenv(LANG = "en")


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# A. EFFECT OF AGE ---------------------------------------------------------------

# Import data --------------*
Clan_size_year_clan <- read_csv("06_processed_data/hyenas/Clan_size_year_clan.csv", 
                                col_types = cols(X1 = col_skip()))

hy_carcasses <- read_delim("06_processed_data/carcasses/12_hy.carcasses.certainty.formatted.spatial_updated_10-2021.csv", 
                                     ";", escape_double = FALSE, trim_ws = TRUE)

# Calculate the average age distribution ----------------------

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

# Count individuals of each age class ------------

{counts <- hy_carcasses %>%
  count(age,sex)

nb.ad.F <- as.numeric(counts[1,3])
nb.ad.M <- as.numeric(counts[2,3])
nb.ad <- as.numeric(counts[1,3] + counts[2,3] + counts[3,3])
nb.sub <- as.numeric(counts[7,3] + counts[8,3] + counts[9,3])
nb.cub <- as.numeric(counts[4,3] + counts[5,3] + counts[6,3])
nb.unkn <- as.numeric(counts[10,3] + counts[11,3])


obs <- c(nb.ad.F, nb.ad.M, nb.sub, nb.cub) #adult males and adult females in separate categories
obs1 <- c(nb.ad, nb.sub, nb.cub) 
obs2 <- c(nb.ad + nb.unkn, nb.sub, nb.cub) # unknown age considered as adults.
}

# Statistical tests ------------------------------------------------------------

# Adults and subadults VS cubs (without unknowns as adults)
dbinom(x =  nb.ad + nb.sub, 
       size = nb.ad + nb.sub + nb.cub,
       prob = proba2[1,1] + proba2[1,2])
# p < 0.01

# (with unknwons as adults)
dbinom(x = nb.ad + nb.sub + nb.unkn, 
       size = nb.ad + nb.sub + nb.cub + nb.unkn,
       prob = proba2[1,1] + proba2[1,2])
# p < 0.001



# Adults VS subadults VS cubs

RVAideMemoire::multinomial.theo.multcomp(x = obs, 
                                         p = as.vector(proba1),  
                                         prop = FALSE,
                                         p.method = "fdr")
# observed expected P-value  
# 34        26.792  0.12045  adult females
# 18        22.109  0.37373  adult males
# 15        9.772   0.12045  
# 7         15.327  0.05716 .


RVAideMemoire::multinomial.theo.multcomp(x = obs1, 
                                         p = as.vector(proba2), 
                                         prop = FALSE,
                                         p.method = "fdr")
#  observed expected P-value  
#     58    52.87   0.23991  
#     15    10.56   0.20814  
#     7     16.57   0.01682 *

RVAideMemoire::multinomial.theo.multcomp(x = obs2, 
                                         p = as.vector(proba2), 
                                         prop = FALSE,
                                         p.method = "fdr")
# observed expected  P-value   
# 69        60.14   0.088678  .
# 15        12.02   0.352049   
# 7         18.85   0.003286 **





#________________________________________________________________________#######
### B. EFFECT OF SEX ===========================================================

# Import data --------------*
Clan_size_year_clan <- read_csv("06_processed_data/hyenas/Clan_size_year_clan.csv", 
                                col_types = cols(X1 = col_skip()))
hy_carcasses <- read_delim("06_processed_data/carcasses/12_hy.carcasses.certainty.formatted.spatial_updated_10-2021.csv", 
                                     ";", escape_double = FALSE, trim_ws = TRUE)

# Calculate the average adult and subadult sex ratio -------------

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

# Count individuals of each sex ------------

# ----------- Adults --
ad.counts <- hy_carcasses %>%
  filter(age == "adult",
         sex %in% c("F", "M")) %>%
  count(sex)
nb.ad.F <- as.numeric(ad.counts[1,2])
N <- nb.ad.F + as.numeric(ad.counts[2,2])

# Adults with individuals of unknwon age considered adults
ad.counts.bis <- hy_carcasses %>%
  filter(age %in% c("adult", "unknown"),
         sex %in% c("F", "M")) %>%
  count(sex)
nb.ad.F.bis <- as.numeric(ad.counts.bis[1,2])
N.bis <- nb.ad.F.bis + as.numeric(ad.counts.bis[2,2])


# ---------- Subadults --
subad.counts <- hy_carcasses %>%
  filter(age == "subadult",
         sex %in% c("F", "M")) %>%
  count(sex)
nb.subad.F <- as.numeric(subad.counts[1,2])
N.subad <- nb.subad.F + as.numeric(subad.counts[2,2])


# Statistical tests -------------

# ----------- Adults --
dbinom(x = nb.ad.F, size = N, prob = ad.sex.ratio)
# p = 0.011

# With unknown
dbinom(x = nb.ad.F.bis, size = N.bis, prob = ad.sex.ratio)
# p = 0.016

# ---------- Subadults --
dbinom(x = nb.subad.F, size = N.subad, prob = subad.sex.ratio)
# p = 0.028






#________________________________________________________________________#######
### C. EFFECT OF RANK ==========================================================

# All the carcasses of females from the three studied clans have collision 
# certainty scores = 1.

hy_carcasses_clan_members <- read_delim("06_processed_data/carcasses/3_hy.clan.members.csv", 
                                        ";", escape_double = FALSE, trim_ws = TRUE)

hy_carcasses_clan_members <- hy_carcasses_clan_members %>%
  filter(sex == "F",
         age_category %in% c("adult", "subadult"))

# Order in ascending ranks
hy_carcasses_clan_members <- hy_carcasses_clan_members[order(hy_carcasses_clan_members$standardized_rank),]


hy_carcasses_clan_members_ad <- hy_carcasses_clan_members %>%
  filter(age_category == "adult")

# Statistical tests ----------------------------

wilcox.test(hy_carcasses_clan_members$standardized_rank, mu = 0, exact = TRUE)
# p = 0.023

wilcox.test(hy_carcasses_clan_members_ad$standardized_rank, mu = 0, exact = TRUE)
# p = 0.015

?wilcox.test
# Multinomial test (nothing significant)

observed <- c(0, 4, 5)
observed.ad <- c(0, 3, 5) # Here I removed the subadult.
prob <- c(1/3, 1/3, 1/3)

EMT::multinomial.test(observed, prob, useChisq = TRUE)
# Exact Multinomial Test, distance measure: chisquare
# 
# Events    chi2Obs    p.value
#   55     4.6667     0.1657

EMT::multinomial.test(observed.ad, prob, useChisq = TRUE)
# Exact Multinomial Test, distance measure: chisquare
# 
# Events    chi2Obs    p.value
#  45        4.75     0.1102
