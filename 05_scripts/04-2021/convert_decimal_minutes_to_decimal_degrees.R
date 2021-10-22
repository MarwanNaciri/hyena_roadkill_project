#==============================================================================#
#                                                                              #
#              Convert degrees decimal minutes to decimal degrees              #
#                                                                              #
#==============================================================================#

# Same as what this website does: https://www.pgc.umn.edu/apps/convert/
library(tidyverse)
library(measurements)

coords <- read_delim("06_processed_data/carcasses/degree_minute_second_coordinates.csv", 
                     delim = ";", escape_double = FALSE, trim_ws = TRUE)[1:22,] %>%
  mutate(lat = substr(Comment, 1, 8),
         long = substr(Comment, 10, 17)) %>%
  dplyr::select(-Comment) %>%
  mutate(lat = ifelse(substr(lat, 1, 1) == "0", substr(lat, 2, 8), lat)) %>%
  mutate(lat = paste0(substr(lat, 1, 4), ".", substr(lat, 6, 7)),
         long = paste0(substr(long, 1, 5), ".", substr(long, 7, 8)))

decimal_coords <- coords %>%
  mutate(lat_decimal = measurements::conv_unit(lat, from = 'deg_dec_min', to = 'dec_deg'),
         long_decimal = measurements::conv_unit(long, from = 'deg_dec_min', to = 'dec_deg')) %>%
  mutate_at(vars("lat_decimal", "long_decimal"), as.numeric) %>%
  mutate_at(vars("lat_decimal", "long_decimal"), round, 6)

write_csv(decimal_coords, "06_processed_data/carcasses/13_carcasses_decimal_minutes.csv")

