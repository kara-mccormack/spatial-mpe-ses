###########################################################
# Title: Preprocessing for Bayesian Profile Regression of 
#        NATA 2014 for NC
# Authors: Kara McCormack (kara.mccormack@duke.edu)
# Date created: 10/19/2019
# Input files: 
#   /Users/karamccormack/Box/SES-environment/Spatial LCM Paper/DataNC_NATA_wide_total_conc.csv
#    
# Output file: 
############################################################

# Goals:
# 1. log transform NATA data and save to csv
# 2. z-transformations to NATA data and save to csv

# source pollutant names file
source("/Users/karamccormack/Box/SES-environment/Code/v1/utils/pollutant_names.R")

# load libraries
library(stringr)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)

# set up paths
data.dir = "/Users/karamccormack/Box/SES-environment/Spatial LCM Paper/v2/spatial-mpe-ses/data/"
output.dir = "/Users/karamccormack/Box/SES-environment/Spatial LCM Paper/v2/spatial-mpe-ses/data/"
output.dir.rawboxplots = "/Users/karamccormack/Box/SES-environment/Spatial LCM Paper/v2/spatial-mpe-ses/plots_figures/raw_boxplots/"

# upload NATA Data
NC_df <- read_csv(file.path(data.dir, "NC_NATA_wide_total_conc.csv"))[,-1]

# convert to data frame
NC_df <- as.data.frame(NC_df)

# prepare data for profile regression
# z transformation
NC_df_mean_sd = NC_df %>%
  gather(Pollutant, Level, `1,3-BUTADIENE`:`NICKEL COMPOUNDS`, -Tract) %>%
  group_by(Pollutant) %>%
  summarise(mean_level = mean(Level),
            sd_level = sd(Level))

NC_df_z = NC_df %>%
  gather(Pollutant, Level, `1,3-BUTADIENE`:`NICKEL COMPOUNDS`, -Tract) %>%
  group_by(Pollutant) %>%
  mutate(mean_level = mean(Level),
         sd_level = sd(Level), 
         z_level = (Level - mean(Level, na.rm = T))/sd(Level, na.rm = T)) %>%
  select(Tract, Pollutant, z_level) %>%
  spread(key = Pollutant, value = z_level) %>%
  as.data.frame

# write to csv
write.csv(NC_df_z, file = file.path(output.dir, "NC_NATA_wide_z_transform.csv"))

# log transformations (just in case)
NC_df_log_transform = NC_df %>%
  gather(Pollutant, Level, `1,3-BUTADIENE`:`NICKEL COMPOUNDS`, -Tract) %>%
  mutate(Level = log(Level)) %>%
  spread(key = Pollutant, value = Level)

# write to csv
write.csv(NC_df_log_transform, 
          file = file.path(output.dir, "NC_NATA_wide_log_transform.csv"))

# Single long dataset
# containing raw, log, z(mean), and z(median) transformations
NC_df_long = NC_df %>%
  gather(Pollutant, Level, `1,3-BUTADIENE`:`NICKEL COMPOUNDS`, -Tract) %>%
  group_by(Pollutant) %>%
  mutate(mean_level = mean(Level),
         sd_level = sd(Level), 
         z_level = (Level - mean(Level, na.rm = T))/sd(Level, na.rm = T),
         z_level_median = (Level - median(Level, na.rm = T))/sd(Level, na.rm = T),
         log_level = log(Level))

# boxplots of original levels. 
p <- ggplot(NC_df_long, aes(x=Pollutant, y=Level)) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1, 
                                   size = 8)) +
  ggtitle("Boxplot of Raw Pollutants for NATA in NC")
p
# save boxplot of raw pollutant levels to .png
ggsave(filename = file.path(output.dir, "nata_raw.png"),
       plot = last_plot(),
       width = 10.5,
       height = 6.5)

# boxplots of log levels
# create long version of NC df with log transformation
NC_df_log_long = NC_df %>%
  gather(Pollutant, Level, `1,3-BUTADIENE`:`NICKEL COMPOUNDS`, -Tract) %>%
  mutate(loglevel = log(Level))

p_log = ggplot(NC_df_log_long, aes(x=Pollutant, y=loglevel)) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1, 
                                   size = 8)) +
  ggtitle("Boxplot of Log-Level of Pollutants for NATA in NC")
p_log
# save boxplot of raw pollutant levels to .png
ggsave(filename = file.path(output.dir.rawboxplots, "nata_log.png"),
       plot = last_plot(),
       width = 10.5,
       height = 6.5)

# boxplots of z-transformed data (mean)
# create long dataset with z-transformation
NC_df_z_long = NC_df %>%
  gather(Pollutant, Level, `1,3-BUTADIENE`:`NICKEL COMPOUNDS`, -Tract) %>%
  group_by(Pollutant) %>%
  mutate(mean_level = mean(Level),
         sd_level = sd(Level), 
         z_level = (Level - mean(Level, na.rm = T))/sd(Level, na.rm = T)) %>%
  select(Tract, Pollutant, z_level)

# create boxplot of z-transformed data
p_z = ggplot(NC_df_z_long, aes(x=Pollutant, y=z_level)) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1, 
                                   size = 8)) +
  ggtitle("Boxplot of Z-Transformed Levels of Pollutants for NATA in NC")
p_z

# save boxplot of z-transformed pollutant data to .png
ggsave(filename = file.path(output.dir.rawboxplots, "nata_z.png"),
       plot = last_plot(),
       width = 10.5,
       height = 6.5)

# boxplot of z-transformed data (using median)
# create long dataset of z-transformed data (using median)
NC_df_z_median_long = NC_df %>%
  gather(Pollutant, Level, `1,3-BUTADIENE`:`NICKEL COMPOUNDS`, -Tract) %>%
  group_by(Pollutant) %>%
  mutate(mean_level = mean(Level),
         sd_level = sd(Level), 
         z_median_level = (Level - median(Level, na.rm = T))/sd(Level, na.rm = T)) %>%
  select(Tract, Pollutant, z_median_level)

# create boxplots
p_z_median = ggplot(NC_df_z_median_long, aes(x=Pollutant, y=z_median_level)) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1, 
                                   size = 8)) +
  ggtitle("Z-Transform (Median) Levels of Pollutants: NATA NC")
p_z_median

# save boxplot of z-transformed pollutant data to .png
ggsave(filename = file.path(output.dir.rawboxplots, "nata_z_median.png"),
       plot = last_plot(),
       width = 10.5,
       height = 6.5)