###########################################################
# Title: Simulation 2 - BPR of NATA 2014 for NC
# Authors: Kara McCormack (kara.mccormack@duke.edu)
# Date created: 12/12/2019
# Input files: 
#   /Users/karamccormack/Box/SES-environment/Spatial LCM Paper/Data/NC_NATA_wide_z_transform.csv
#   
# Output file: 
#   /Users/karamccormack/Box/SES-environment/Spatial LCM Paper/v2/spatial-mpe-ses/output/simulation-2/
###########################################################

# Goals:
# 1. Run a simulation of bayesian profile regression using the 
#     PReMiuM package on the NATA data. Use 20,000 sweeps. 
#     Plot several parameters by sweep to get an idea of model
#     convergence.

# load libraries
library(PReMiuM)
library(stringr)
library(ggplot2)

# set up paths
data.dir = "/Users/karamccormack/Box/SES-environment/Spatial LCM Paper/Data/"
# output.dir is where temporary output (.txt) files will go from 'profRegr' command
output.dir = "/Users/karamccormack/Box/SES-environment/Spatial LCM Paper/v2/spatial-mpe-ses/output/scratch/scratch1/"
# outputPlots.dir is where final plots will go
outputPlots.dir = "/Users/karamccormack/Box/SES-environment/Spatial LCM Paper/v2/spatial-mpe-ses/output/simulation-2/"
# setwd to where the output (.txt) files should go
setwd(output.dir)

# load log-transformed NC NATA data
NC_df_log = read.csv(file.path(data.dir, "NC_NATA_wide_log_transform.csv"))[,-1]

# define covariate names
# do not include the first column since this is 'Tract'
covariate_names <- colnames(NC_df_log)[-1]

# define inputs for function
seed = 898 # set random seed
nBurn = 10000 # number of burn iterations
nSweeps = 20000 # number of sweeps of MCMC algorithm
data = NC_df_log # assign log transformed dataset
output = "output_scratch" # assign prefix for temporary output .txt files
covNames = covariate_names # assign covariate names from data frame
nClusInit = 35 # number of initial clusters
plotBySweep = 20 # in final plots, how often (in terms of sweeps) to plot a point
# note: a ratio of about 1/10 seems to get good resolution if plotting
# a single plot. 

runInfoObj_2 = profRegr(yModel = "Normal", 
                      xModel = "Normal",
                      nSweeps = nSweeps,
                      nBurn = nBurn,
                      data = data,
                      output = output,
                      covNames = covNames,
                      nClusInit = nClusInit,
                      whichLabelSwitch="123",
                      run = TRUE,
                      excludeY = TRUE, 
                      seed = seed)

