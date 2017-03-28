# Initial script

# 27/7/2015


# Eligible if: 
# age 22 to 25 in wave A or wave B

# Subgroups: do you have a full driving licence? (latest response)
# wDRIVER in wINDRESP for w in a, b

# wCARUSE in wINDRESP for w from c onwards
# 1 : yes
# 2: no
# 3: does not drive




# And whether this affects health, ghq, etc

#wHLGHQ1-2 in wINDRESP : GHQ score

rm(list = ls())

pacman::p_load(
  tidyverse,
  forcats,
  ggplot2,   cowplot,
  RColorBrewer, 
  lattice, latticeExtra
)

############################################################################################################
############################################################################################################
source("Scripts/smoother_function.R")
source("Scripts/lexis_helper_functions.R")

source("scripts/manage_data_bhps.R")
source("scripts/manage_data_ukhls.R")


source("scripts/create_graphs.R")


# Modelling/descriptive stats ---------------------------------------------


