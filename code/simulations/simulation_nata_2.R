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

# put together pertinent information about this run
alpha = read.table("output_scratch_alpha.txt")
kappa1 = read.table("output_scratch_kappa1.txt")
logpost = read.table("output_scratch_logPost.txt")

dat_20ksweeps_nClusInit35 = cbind(sweep = 1:nSweeps, 
            alpha = alpha, 
            kappa = kappa1, 
            logPost = logpost[,1],
            logLike = logpost[,2],
            logPrior = logpost[,3],
            nClusInit = rep(nClusInit, nSweeps))
colnames(dat_20ksweeps_nClusInit35)[2] <- "alpha"
colnames(dat_20ksweeps_nClusInit35)[3] <- "kappa"

# subset by every 20 sweeps
dat_20ksweeps_nClusInit35_subset = dat[seq(1, nSweeps, by = 20),]

# plot posterior distribution of alpha
p = ggplot(dat_subset, aes(x = sweep, y = alpha)) + 
  geom_line(color = "darkorange1") +
  theme_minimal() +
  labs(y = "Posterior Alpha Distribution",
       x = "Sweeps")
p
# alpha doesn't seem to stabilize within 20,000 sweeps.

p_kappa = ggplot(dat_20ksweeps_nClusInit35_subset, aes(x = sweep, y = kappa)) + 
  geom_line(color = "darkseagreen3") +
  theme_minimal() +
  labs(y = "Posterior Kappa Distribution",
       x = "Sweeps")
p_kappa


# see if alpha stabilizes for 50,000 sweeps.
nSweeps = 50000
output = "output_50ksweeps"
runInfoObj_50ksweeps = profRegr(yModel = "Normal", 
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

# put together pertinent information about this run
alpha_50k = read.table("output_50ksweeps_alpha.txt")
kappa1_50k = read.table("output_50ksweeps_kappa1.txt")
logpost_50k = read.table("output_50ksweeps_logPost.txt")

dat_50ksweeps_nClusInit35 = cbind(sweep = 1:nSweeps, 
                                  alpha = alpha_50k, 
                                  kappa = kappa1_50k, 
                                  logPost = logpost_50k[,1],
                                  logLike = logpost_50k[,2],
                                  logPrior = logpost_50k[,3],
                                  nClusInit = rep(nClusInit, nSweeps))
colnames(dat_50ksweeps_nClusInit35)[2] <- "alpha"
colnames(dat_50ksweeps_nClusInit35)[3] <- "kappa"
# subset by every 50 sweeps
dat_50ksweeps_nClusInit35_subset = dat_50ksweeps_nClusInit35[seq(1, nSweeps, by = 50),]

#plot posterior distribution of alpha
p_50_alpha = ggplot(dat_50ksweeps_nClusInit35_subset, 
           aes(x = sweep, y = alpha)) + 
  geom_line(color = "darkorange1") +
  theme_minimal() +
  labs(y = "Posterior Alpha Distribution",
       x = "Sweeps")
p_50_alpha

# we see an increase but not really any convergence 
# of alpha with 50k sweeps