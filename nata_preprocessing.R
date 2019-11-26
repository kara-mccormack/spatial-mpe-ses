###########################################################
# Title: NATA 2014 Data Preprocessing
# Authors: Kara McCormack (kara.mccormack@duke.edu)
# Date created: 10/22/2019
# Input files: 
#   /Users/karamccormack/Box/SES-environment/Code/v1/Output/NATA_2014_US.csv
#   
# Output files:
#     "/Users/karamccormack/Box/SES-environment/Spatial LCM Paper/Data/NC_NATA_wide_total_conc.csv"
#     "/Users/karamccormack/Box/SES-environment/Spatial LCM Paper/Data/NC_NATA_wide_perc.csv"
###########################################################

# Goal:
# 1. Subset the NATA US data to NC
# 2. Create a wide dataset with 'Tract', all pollutants, 
#     and their percentages of exposure levels, and total exposure
# 3. Order the variables in order of Table 1 (Alex's paper)
#     - this is for the corrplot 
#     - will help interpretation to have variables grouped together
#       (e.g. PMs, Heavy Metals, VOCs)
# 3. Export .csv files of NC datasets
#     - one with overall exposure levels
#     - one with percentages of exposure levels (relative to NC)
# 4. Create wide dataset of US data with 'Tract', all pollutants,
#     and their percentages of exposure levels, and total exposure. 
# load libraries
library(tidyr)

# set up paths
data.dir = "/Users/karamccormack/Box/SES-environment/Spatial LCM Paper/Data"
output.dir = "/Users/karamccormack/Box/SES-environment/Spatial LCM Paper/Output"

# upload NATA Data
US_df <- read_csv(file = "/Users/karamccormack/Box/SES-environment/Code/v1/Output/NATA_2014_US.csv")

# create NC subset data
# subset to NC
NC_df <- US_df %>%
  filter(State == "NC")

# convert to data frame
NC_df <- as.data.frame(NC_df)

# add a column for the percentile of each pollutant
NC_df$perc <- rep(0, nrow(NC_df))
for (i in 1:nrow(nata_pollutants)) {
  w <- which(NC_df$`Pollutant Name`== nata_pollutants$nms[i])
  NC_df$perc[w] <- 100*ecdf(NC_df[w, "Total Conc"])(NC_df[w, "Total Conc"])
}

# convert Tract to character
NC_df$Tract <- with(NC_df, as.character(as.numeric(Tract)))

# convert to wide: total concentrations of pollutants
NC_dfw <- NC_df %>%
  select(Tract, `Pollutant Name`, `Total Conc`) %>%
  spread(`Pollutant Name`,`Total Conc`)

# correct order of variables based on Table 1 categories
ord = c(1:5,7:9,13:14, 16:17,6,10:12,15)

# re-order variables
NC_dfw = NC_dfw[,ord]
# write to csv
write.csv(NC_dfw, 
          "/Users/karamccormack/Box/SES-environment/Spatial LCM Paper/Data/NC_NATA_wide_total_conc.csv")

# convert to wide: percentages of concentrations of pollutants
NC_dfw_perc <- NC_df %>%
  select(Tract, `Pollutant Name`, perc) %>%
  spread(`Pollutant Name`, perc)

# re-order variable names
NC_dfw_perc = NC_dfw_perc[,ord]

# write to csv 
write.csv(NC_dfw_perc, 
          "/Users/karamccormack/Box/SES-environment/Spatial LCM Paper/Data/NC_NATA_wide_perc.csv")

# US Data

# convert to data frame
US_df <- as.data.frame(US_df)
