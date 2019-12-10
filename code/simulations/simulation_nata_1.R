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
library(ggplot2)

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
n = 10000 # set number of simulations desired
# set prior values
set.seed(678)
# create reproducible vector of seeds for simulation
# seed vector will be of length n 
seed = sample(1:999999, size = n, replace = F)
nBurn = 10000 # number of burn iterations
nSweeps = 80000 # number of sweeps of MCMC algorithm
data = NC_df_log # assign log transformed dataset
output = "output" # assign prefix for temporary output .txt files
covNames = covariate_names # assign covariate names from data frame
num_clusters = rep(0,n) # create empty vector for final # of clusters found

# simulation
  for(i in 1:n){
    runInfoObj = profRegr(yModel = "Normal", 
                          xModel = "Normal",
                          nSweeps = nSweeps,
                          nBurn = nBurn,
                          data = NC_df_log,
                          output = "output",
                          covNames = covariate_names,
                          nClusInit = 35,
                          whichLabelSwitch="123",
                          run = TRUE,
                          excludeY = TRUE, 
                          seed = seed[i])
    dissimObj = calcDissimilarityMatrix(runInfoObj)
    clusObj = calcOptimalClustering(dissimObj)
    num_clusters[i] <- max(unique(clusObj$clustering))
  }

# create boxplot dataframe for ggplot
sweeps = rep(nSweeps, n) # create a vector of the number of sweeps, repeated n times
dat = as.data.frame(cbind(num_clusters, sweeps))

p <- ggplot(dat, aes(x=as.factor(sweeps), y=num_clusters)) + 
  geom_boxplot()
p

