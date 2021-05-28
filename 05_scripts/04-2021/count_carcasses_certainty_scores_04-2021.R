#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                              +
#             Count carcasses of different categories for manuscript           +
#                                                                              +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(tidyverse)
Sys.setenv(LANG = "en")

# This code is to count the carcasses with given certainty scores (e.g. those with
# only the cause-of-death certainty score >= 0.5). This is useful in the manuscript 
# when I need to include the n.

## Certainty/confidence scores --------------------------------------------------

# Cd = Cause of Death certainty score
# Cl = Location certainty score

all.carcasses <- read_delim("6_processed_data/carcasses/12_hy.carcasses.certainty.formatted.spatial.csv", 
                            ";", escape_double = FALSE, trim_ws = TRUE)


# Number of carcasses with Cd >= 0.75
carcasses_0.75_Cd <- all.carcasses %>%
  filter(!collision_certainty_score_NEW < 0.75)

# Number of carcasses with Cd >= 0.75 AND Cl >= 0.75
carcasses_0.75 <- all.carcasses %>%
  filter(!collision_certainty_score_NEW < 0.75,
         !GPS_certainty_score < 0.75)


# Number of carcasses with Cd >= 0.75 of each age class 
carcasses_0.75_Cd_age <- all.carcasses %>%
  filter(!collision_certainty_score_NEW < 0.75) %>%
  count(age)



# Number of carcasses with each Cl 
carcasses_Cl <- all.carcasses %>%
  count(GPS_certainty_score)

# Number of carcasses with each Cd 
carcasses_Cd <- all.carcasses %>%
  count(collision_certainty_score_NEW)


# Carcass found by ? -----------------------------------------------------------

table_01 <- read_delim("6_processed_data/carcasses/table_01.csv", 
                       ";", escape_double = FALSE, trim_ws = TRUE)[1:90, 1:33]

x <- which(table_01$reported_to_hyena_project_by %in% c("cheetah_project_Denis_Minja",
      "CDP"))
table_01$reported_to_hyena_project_by[x] <- "scientists"



x <- which(table_01$reported_to_hyena_project_by %in% c("scientists", "staff", "vet", 
                                                        "scientists/staff/vet", "staff/vet"))
table_01$reported_to_hyena_project_by[x] <- "scientists/staff/vet"


found_by <- table_01 %>%
  count(reported_to_hyena_project_by) %>%
  arrange(-n)
found_by



