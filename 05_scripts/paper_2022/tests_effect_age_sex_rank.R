#==============================================================================#
#                                                                              #
#            Statistical test of the effect of age, sex and rank               #        
#                                                                              #
#==============================================================================#

library(tidyverse)
Sys.setenv(LANG = "en")

# A. Effect of age =============================================================

# ~ 1. Calculate the average age distribution -----------------------------------

clan_size_year_clan <- read_csv("06_processed_data/hyenas/Clan_size_year_clan.csv", 
                                col_types = cols(...1 = col_skip())) %>%
  na.omit() %>%
  group_by(age_class) %>%
  summarise(mean_nb = mean(nb))
proportion_age_classes_raw <- data.frame(age_class = clan_size_year_clan$age_class,
                                         proportion = clan_size_year_clan$mean_nb/sum(clan_size_year_clan$mean_nb))

proportions <- c(proportion_age_classes_raw[1, 2],    # Adult females
                 proportion_age_classes_raw[2, 2] + 
                   proportion_age_classes_raw[3, 2],  # Adult males
                 proportion_age_classes_raw[5, 2] + 
                   proportion_age_classes_raw[6, 2],  # Subadults
                 proportion_age_classes_raw[4, 2])    # Cubs
names(proportions) <- c("Adult females", "Adult males",
                        "Subadults", "Cubs")

# Fuse all adults together, and subadult males and females
proportions_1 <- c(proportion_age_classes_raw[1, 2] +
                     proportion_age_classes_raw[2, 2] + 
                     proportion_age_classes_raw[3, 2],  # Adults
                   proportion_age_classes_raw[5, 2] + 
                     proportion_age_classes_raw[6, 2],  # Subadults
                   proportion_age_classes_raw[4, 2])    # Cubs
names(proportions_1) <- c("Adults", "Subadults", "Cubs")


# ~ 2. Count roadkills of each age class --------------------------------------

hy_carcasses <- read_delim("06_processed_data/carcasses/12_hy.carcasses.certainty.formatted.spatial_updated_2022-04.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE)
{counts <- hy_carcasses %>%
    count(age, sex)
  nb.ad.F <- as.numeric(counts[1,3])
  nb.ad.M <- as.numeric(counts[2,3])
  nb.ad <- as.numeric(counts[1,3] + counts[2,3] + counts[3,3])
  nb.sub <- as.numeric(counts[7,3] + counts[8,3] + counts[9,3])
  nb.cub <- as.numeric(counts[4,3] + counts[5,3] + counts[6,3])
  nb.unkn <- as.numeric(counts[10,3] + counts[11,3])
  
  obs <- c(nb.ad.F, nb.ad.M, nb.sub, nb.cub) # adult males and adult females in separate categories
  obs1 <- c(nb.ad, nb.sub, nb.cub) 
  obs2 <- c(nb.ad + nb.unkn, nb.sub, nb.cub) # unknown age considered as adults.
}


# ~ 3. Run statistical tests ---------------------------------------------------

# Adults and subadults VS cubs (without unknowns as adults)
dbinom(x =  nb.ad + nb.sub, 
       size = nb.ad + nb.sub + nb.cub,
       prob = proportions_1[1] + proportions_1[2])
# p < 0.01

# (with unknwons as adults)
dbinom(x = nb.ad + nb.sub + nb.unkn, 
       size = nb.ad + nb.sub + nb.cub + nb.unkn,
       prob = proportions_1[1] + proportions_1[2])
# p < 0.001



# Adults VS subadults VS cubs
RVAideMemoire::multinomial.theo.multcomp(x = obs, 
                                         p = as.vector(proportions),  
                                         prop = FALSE,
                                         p.method = "fdr")
# observed expected P-value  
# 38        28.24  0.0485  * adult females
# 16        23.31  0.08266 . adult males
# 16        10.30  0.08266 .   
# 78        16.15  0.0485  *


RVAideMemoire::multinomial.theo.multcomp(x = obs1, 
                                         p = as.vector(proportions_1), 
                                         prop = FALSE,
                                         p.method = "fdr")
# observed expected P-value  
# 64         58.16  0.21572  
# 16         11.62  0.21572  
# 8          18.22  0.01608 *

RVAideMemoire::multinomial.theo.multcomp(x = obs2, 
                                         p = as.vector(proportions_1), 
                                         prop = FALSE,
                                         p.method = "fdr")
# observed expected  P-value   
# 71        62.78    0.124220   
# 16        12.54    0.288525   
# 8         19.67    0.006346 **


# B. Effect of sex =============================================================

# ~ 1. Calculate the average adult and subadult sex ratio ---------------------

clan_size_year_clan <- read_csv("06_processed_data/hyenas/Clan_size_year_clan.csv", 
                                col_types = cols(...1 = col_skip())) %>%
  na.omit() %>%
  group_by(age_class) %>%
  summarise(mean_nb = mean(nb))
proportion_age_classes_raw <- data.frame(age_class = clan_size_year_clan$age_class,
                                         proportion = clan_size_year_clan$mean_nb/sum(clan_size_year_clan$mean_nb))

proportions <- c(proportion_age_classes_raw[1, 2],    # Adult females
                 proportion_age_classes_raw[2, 2] + 
                   proportion_age_classes_raw[3, 2],  # Adult males
                 proportion_age_classes_raw[5, 2],    # Subadult females
                 proportion_age_classes_raw[6, 2],    # Subadult males
                 proportion_age_classes_raw[4, 2])    # Cubs
names(proportions) <- c("Adult females", "Adult males", 
                        "Subadults females", "Subadults males",
                        "Cubs")

ad.sex.ratio <- unname(proportions[1]/(proportions[1] + proportions[2]))

subad.sex.ratio <- unname(proportions[3]/(proportions[3] + proportions[4]))



# ~ 2. Count individuals of each sex -------------------------------------------
hy_carcasses <- read_delim("06_processed_data/carcasses/12_hy.carcasses.certainty.formatted.spatial_updated_2022-04.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE)

# ----------- Adults --
ad.counts <- hy_carcasses %>%
  filter(age == "adult",
         sex %in% c("F", "M")) %>%
  count(sex)
nb.ad.F <- as.numeric(ad.counts[1, 2])
N <- nb.ad.F + as.numeric(ad.counts[2, 2])

# Adults With individuals of unknwon age considered adults --
ad.counts.bis <- hy_carcasses %>%
  filter(age %in% c("adult", "unknown"),
         sex %in% c("F", "M")) %>%
  count(sex)
nb.ad.F.bis <- as.numeric(ad.counts.bis[1, 2])
N.bis <- nb.ad.F + as.numeric(ad.counts.bis[2, 2])

# ---------- Subadults --
subad.counts <- hy_carcasses %>%
  filter(age == "subadult",
         sex %in% c("F", "M")) %>%
  count(sex)
nb.subad.F <- as.numeric(subad.counts[1, 2])
N.subad <- nb.subad.F + as.numeric(subad.counts[2, 2])

# ~ 3. Statistical tests -------------------------------------------------------

# ----------- Adults --
dbinom(x = nb.ad.F, size = N, prob = ad.sex.ratio)
# p = 0.0075

# With unknown
dbinom(x = nb.ad.F.bis, size = N.bis, prob = ad.sex.ratio)
# p = 0.011

# ---------- Subadults --
dbinom(x = nb.subad.F, size = N.subad, prob = subad.sex.ratio)
# p = 0.028

# C. Effect of rank ============================================================

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
