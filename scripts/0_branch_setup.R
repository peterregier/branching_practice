## This is a setup script to load packages and any other environmental factors
## we want to stay consistent across multiple scripts
##
## Peter Regier
## 2024-07-23
##
# ########## #
# ########## #


# 1. Setup ---------------------------------------------------------------------

require(pacman)
p_load(tidyverse,
       cowplot,
       googledrive, 
       readxl,
       lubridate,
       parsedate,
       plotly,
       corrplot,
       tictoc,
       PNWColors,
       seacarb,
       janitor)

theme_set(theme_bw())

common_tz = "Etc/GMT+7"
