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

# Goals:
# 1. Run a simulation of bayesian profile regression using the 
#     PReMiuM package on the NATA data. Use 50000, 75000, and 100000
#     sweeps and create a final boxplot of the results
# load libraries
library(PReMiuM)
library(stringr)
library(ggplot2)

# set up paths
data.dir = "/Users/karamccormack/Box/SES-environment/Spatial LCM Paper/Data/"
# output.dir is where temporary output (.txt) files will go from 'profRegr' command
output.dir = "/Users/karamccormack/Box/SES-environment/Spatial LCM Paper/v2/spatial-mpe-ses/output/scratch/scratch1/"
# setwd to where the output (.txt) files should go
setwd(output.dir)

# load log-transformed NC NATA data
# log-transformed data comes from data.dir
NC_df_log = read.csv(file.path(data.dir, "NC_NATA_wide_log_transform.csv"))[,-1]

# define covariate names
# do not include the first column since this is 'Tract'
covariate_names <- colnames(NC_df_log)[-1]

# simulation 1
n = 1 # set number of simulations desired
# set prior values
set.seed(678)
# create reproducible vector of seeds for simulation
# seed vector will be of length n 
seed = sample(1:999999, size = n, replace = F)
nBurn = 10000 # number of burn iterations
nSweeps = 20000 # number of sweeps of MCMC algorithm
data = NC_df_log # assign log transformed dataset
output = "output" # assign prefix for temporary output .txt files
covNames = covariate_names # assign covariate names from data frame
num_clusters = rep(0,n) # create empty vector for final # of clusters found

# run with 80000 sweeps
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
    num_clusters_1[i] <- max(unique(clusObj$clustering))
  }

# plot the number of clusters by number of sweeps
nClustersSweep<-read.table("output_nClusters.txt")
head(nClustersSweep)
nClusters_dat = as.data.frame(cbind(sweeps = seq(1:20000), nClusters = nClustersSweep))
# try subsetting every 20th sweep
nClusters_dat2 = nClusters_dat[seq(1,20000, by = 20),]
p <- ggplot(data = nClusters_dat2, 
            aes(x = sweeps, y = V1)) + 
  geom_line(color = "#00AFBB", size = .5) + 
  stat_smooth(color = "#FC4E07", 
              fill = "#FC4E07",
              method = "loess") +
  labs(y="Number of Clusters", x = "Number of Sweeps")
p

# run a second time with 100,000 sweeps
nSweeps_2 = 100000
num_clusters_2 = rep(0,n)

# run with 100000 sweeps 
for(i in 1:n){
  runInfoObj = profRegr(yModel = "Normal", 
                        xModel = "Normal",
                        nSweeps = nSweeps_2,
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
  num_clusters_2[i] <- max(unique(clusObj$clustering))
}

# create boxplot dataframe for ggplot
sweeps = c(rep(nSweeps_1, n), rep(nSweeps_2, n)) # create a vector of the number of sweeps, repeated n times
num_clusters = c(num_clusters_1, num_clusters_2)
dat = as.data.frame(cbind(num_clusters, sweeps)) # bring them together in data frame

p <- ggplot(dat, aes(x=as.factor(sweeps), y=num_clusters)) + 
  geom_boxplot()
p

# scratch

myfunction <- function(seed_1) {
  log <- capture.output({
    runInfoObj = profRegr(yModel = "Normal", 
                          xModel = "Normal",
                          nSweeps = nSweeps_1,
                          nBurn = nBurn,
                          data = NC_df_log,
                          output = "output",
                          covNames = covariate_names,
                          nClusInit = 35,
                          whichLabelSwitch="123",
                          run = TRUE,
                          excludeY = TRUE, 
                          seed = seed_1,
                          nProgress = nSweeps_1+1);
  })
  
  dissimObj = calcDissimilarityMatrix(runInfoObj)
  log2 <- capture.output({
    clusObj = calcOptimalClustering(dissimObj);
  })
  num_clusters_1 <- max(unique(clusObj$clustering))
  return(num_clusters_1)
}

myfunction(1)
library(parallel)

seed_vec = rep(2,2)
num_cores = detectCores()
results = mclapply(seed_vec, myfunction, mc.cores = num_cores)

profRegr

