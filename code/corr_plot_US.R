###########################################################
# Title: Correlation Matrix plot with US ACS and NATA data
# Authors: Kara McCormack (kara.mccormack@duke.edu)
# Date created: 10/22/2019
# Input files: 
#   /Users/karamccormack/Box/SES-environment/Spatial LCM Paper/Data/NC_NATA_wide_perc.csv
#   
# Output file: 
#   /Users/karamccormack/Box/SES-environment/Spatial LCM Paper/Plots_Figures/corrplot_full_US.png
###########################################################

# Goals:
# 1. Merge ACS and NATA datasets for NC
# 2. Create a correlation plot matrix with pollution and SES data in NC

# install.packages("ggcorrplot")

# load libraries
library(dplyr)
library(ggcorrplot)

# set up paths
data.dir = "/Users/karamccormack/Box/SES-environment/Spatial LCM Paper/Data"
output.dir = "/Users/karamccormack/Box/SES-environment/Spatial LCM Paper/Plots_Figures"

# read in data
acs.prop.data = read.csv(file.path(data.dir,"ACS_1yr_2014_prop_NC_MPlus.csv"))
nata.perc.data = read.csv(file.path(data.dir,"NC_NATA_wide_perc.csv"))

