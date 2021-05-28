#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#                                                                              #
#                 Check how palette looks for colorblind people                #        
#                                                                              #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#


library(colorblindcheck)
library(tidyverse)


pal <-  c("#336600", "#E69F00")

palette_check(pal, plot = TRUE)
