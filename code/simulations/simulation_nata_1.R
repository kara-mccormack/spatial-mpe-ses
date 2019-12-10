###########################################################
# Title: Simulation 1 - Bayesian Profile Regression of NATA 2014 for NC
# Authors: Kara McCormack (kara.mccormack@duke.edu)
# Date created: 12/10/2019
# Input files: 
#   /Users/karamccormack/Box/SES-environment/Spatial LCM Paper/Data/NC_NATA_wide_z_transform.csv
#   
# Output file: 
#   
###########################################################

# load libraries
library(PReMiuM)
library(stringr)

# set up paths
data.dir = "/Users/karamccormack/Box/SES-environment/Spatial LCM Paper/Data/"
# output.dir is where temporary output (.txt) files will go from 'profRegr' command
output.dir = "/Users/karamccormack/OneDrive - Duke University/Spatial LCM Paper/Output/Scratch/scratch_sim/"
# setwd to where the output (.txt) files should go
setwd(output.dir)

# load log-transformed NC NATA data
# log-transformed data comes from data.dir
NC_df_log = read.csv(file.path(data.dir, "NC_NATA_wide_log_transform.csv"))[,-1]

# define covariate names
# do not include the first column since this is 'Tract'
covariate_names <- colnames(NC_df_log)[-1]

# simulation 1


