###########################################################
# Title: Simulation 1 - Bayesian Profile Regression of NATA 2014 for NC
# Authors: Kara McCormack (kara.mccormack@duke.edu)
# Date created: 12/10/2019
# Input files: 
#   /Users/karamccormack/Box/SES-environment/Spatial LCM Paper/Data/NC_NATA_wide_z_transform.csv
#   
# Output file: 
#   /Users/karamccormack/Box/SES-environment/Spatial LCM Paper/v2/spatial-mpe-ses/output/simulation-1/sweepsbyclusters.png
###########################################################

# Goals:
# 1. Run a simulation of bayesian profile regression using the 
#     PReMiuM package on the NATA data. Examine plot of number
#     of clusters versus number of sweeps by different number of
#     initial clusters to get an idea if the model converged.

# load libraries
library(PReMiuM)
library(stringr)
library(ggplot2)

# set up paths
data.dir = "/Users/karamccormack/Box/SES-environment/Spatial LCM Paper/Data/"
# output.dir is where temporary output (.txt) files will go from 'profRegr' command
output.dir = "/Users/karamccormack/Box/SES-environment/Spatial LCM Paper/v2/spatial-mpe-ses/output/scratch/scratch1/"
# outputPlots.dir is where final plot will go
outputPlots.dir = "/Users/karamccormack/Box/SES-environment/Spatial LCM Paper/v2/spatial-mpe-ses/output/simulation-1/"
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

# create function that inputs a number of initial clusters
# and outputs a data frame with two columns:
#   1. Sweep number
#   2. Number of clusters during that sweep
myFunction = function(nClusInit){
  runInfoObj = profRegr(yModel = "Normal", 
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
  # plot the number of clusters by number of sweeps
  nClustersSweep<-read.table(paste0(output, "_nClusters.txt"))
  nClusters_dat = as.data.frame(cbind(sweeps = seq(1:nSweeps), 
                                      nClusters = nClustersSweep), 
                                      nClusInit = rep(nClusInit, nSweeps))
  # subset every 20th sweep
  nClusters_dat2 = nClusters_dat[seq(1,nSweeps, by = plotBySweep),]
  return(nClusters_dat2)
}

# get results for different number of initial clusters
nClusInitVec = c(10, 35, 50, 70)
b = rbind(lapply(nClusInitVec, myFunction)) # note: this returns a list

# bring together each dataframe of results
c = list()  # initialize empty list, need to add on nclusinit column for plotting
for(i in 1:length(nClusInitVec)){
  c[[i]] <- cbind(b[[i]], nclusinit = rep(nClusInitVec[i], dim(b[[i]])[1]))
}
b_full = rbind(c[[1]],c[[2]], c[[3]], c[[4]])

# create overall plot of results
p2 <- ggplot(data = b_full, 
            aes(x = sweeps, y = V1)) + 
  facet_wrap(~nclusinit) +
  geom_line(color = "#00AFBB", size = .5) + 
  stat_smooth(color = "#FC4E07", 
              fill = "#FC4E07",
              method = "loess") +
  labs(y="Number of Clusters", x = "Number of Sweeps (After Burn-In)") +
  ggtitle("Number of Clusters by Number of Sweeps for \n Different Number of Initial Clusters")

# output plot
ggsave("sweepsbyclusters.png",
       plot = last_plot(),
       path = outputPlots.dir)


# End of Simulation 1
########################################

