###########################################################
# Title: Bayesian Profile Regression of NATA 2014 for NC
# Authors: Kara McCormack (kara.mccormack@duke.edu)
# Date created: 10/19/2019
# Input files: 
#   /Users/karamccormack/Box/SES-environment/Spatial LCM Paper/Data/NC_NATA_wide_z_transform.csv
#   
# Output file: 
#   /Users/karamccormack/Box/SES-environment/Spatial LCM Paper/Output/NC_poll_clusters.csv
###########################################################

# install.packages("PReMiuM")
setwd("/Users/karamccormack/Box/SES-environment/Spatial LCM Paper/Code")
# source pollutant names file
source("/Users/karamccormack/Box/SES-environment/Code/v1/utils/pollutant_names.R")

# load libraries
library(PReMiuM)
library(stringr)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)

# set up paths
data.dir = "/Users/karamccormack/Box/SES-environment/Spatial LCM Paper/Data/"
output.dir = "/Users/karamccormack/Box/SES-environment/Spatial LCM Paper/Output/"

# load z-transformed NC NATA data
NC_df_z = read.csv(file.path(data.dir, "NC_NATA_wide_z_transform.csv"))[,-1]
# grab variable names from original NC NATA data
NC_df <- read_csv(file.path(data.dir, "NC_NATA_wide_total_conc.csv"))[,-1]
colnames(NC_df_z) <- colnames(NC_df)
# need no spaces in pollutant names to plug into profile regression
colnames(NC_df_z) <- str_replace_all(colnames(NC_df_z)," " , "_")

# define covariate names
covariate_names <- colnames(NC_df_z)[-1]

# check for NA in pollutants
for (name in covariate_names){
  print(paste("There are", sum(is.na(NC_df_z$name)), "NAs in", name, ".", sep = " "))
}

# bayesian profile regression
# simulation
setwd("/Users/karamccormack/Box/SES-environment/Spatial LCM Paper/Output/Scratch/scratch_bpr3")



# run bayesian profile regression
# nBurn = 10000
# nSweeps = 20000
# will keep the output files of each run in scratch directory
setwd("/Users/karamccormack/Box/SES-environment/Spatial LCM Paper/Output/Scratch")
runInfoObj_z_burn10k_sweep20k = profRegr(yModel = "Normal",
                      xModel = "Normal",
                      nSweeps = 20000,
                      nBurn = 10000,
                      data = NC_df_z,
                      output = "output.dir",
                      covNames = covariate_names,
                      nClusInit = 10,
                      run = TRUE,
                      excludeY = TRUE, 
                      seed = 1234)

# calculate dissimilarity matrix
dissimObj_burn10k_sweep20k = calcDissimilarityMatrix(runInfoObj_z_burn10k_sweep20k)
clusObj_burn10k_sweep20k_max10 = calcOptimalClustering(dissimObj_burn10k_sweep20k, maxNClusters = 10)
table(clusObj_burn10k_sweep20k_max10$clustering)
clusObj_burn10k_sweep20k_max15 = calcOptimalClustering(dissimObj_burn10k_sweep20k, maxNClusters = 15)
table(clusObj_burn10k_sweep20k_max15$clustering)

# note: deleted all .txt output files from previous run prior to 
#       running the next profile regression. 
# try same as above, but with 30 initial clusters. 
runInfoObj_z_burn10k_sweep20k_init30 = profRegr(yModel = "Normal",
                                         xModel = "Normal",
                                         nSweeps = 20000,
                                         nBurn = 10000,
                                         data = NC_df_z,
                                         output = "output.dir",
                                         covNames = covariate_names,
                                         nClusInit = 30,
                                         run = TRUE,
                                         excludeY = TRUE, 
                                         seed = 1234)
# calculate dissimilarity matrix
dissimObj_burn10k_sweep20k_init30 = calcDissimilarityMatrix(runInfoObj_z_burn10k_sweep20k_init30)
# calculate optimal clustering
# vary the max number of clusters allowed
clusObj_burn10k_sweep20k_init30_nomax = calcOptimalClustering(dissimObj_burn10k_sweep20k_init30)
table(clusObj_burn10k_sweep20k_init30_nomax$clustering)
clusObj_burn10k_sweep20k_init30_max10 = calcOptimalClustering(dissimObj_burn10k_sweep20k_init30, maxNClusters = 10)
table(clusObj_burn10k_sweep20k_init30_max10$clustering)
clusObj_burn10k_sweep20k_init30_max20 = calcOptimalClustering(dissimObj_burn10k_sweep20k_init30, maxNClusters = 20)
table(clusObj_burn10k_sweep20k_init30_max20$clustering)


setwd("/Users/karamccormack/OneDrive - Duke University/Spatial LCM Paper/Output/Scratch/scratch_bpr/")
runInfoObj_z_burn10k_sweep30k_init30 = profRegr(yModel = "Normal",
                                         xModel = "Normal",
                                         nSweeps = 30000,
                                         nBurn = 10000,
                                         data = NC_df_z,
                                         output = "output.sweeps100k.init30",
                                         covNames = covariate_names,
                                         nClusInit = 30,
                                         run = TRUE,
                                         excludeY = TRUE, 
                                         seed = 1234)

# calculate dissimilarity matrix
dissimObj_burn10k_sweep30k_init30 = calcDissimilarityMatrix(runInfoObj_z_burn10k_sweep30k_init30)
# calculate optimal clustering
# vary the max number of clusters allowed
clusObj_burn10k_sweep30k_init30_nomax = calcOptimalClustering(dissimObj_burn10k_sweep30k_init30)
table(clusObj_burn10k_sweep30k_init30_nomax$clustering)
clusObj_burn10k_sweep30k_init30_max10 = calcOptimalClustering(dissimObj_burn10k_sweep30k_init30, maxNClusters = 10)
table(clusObj_burn10k_sweep30k_init30_max10$clustering)
clusObj_burn10k_sweep30k_init30_max20 = calcOptimalClustering(dissimObj_burn10k_sweep30k_init30, maxNClusters = 20)
table(clusObj_burn10k_sweep30k_init30_max20$clustering)



# nBurn = 10000
# nSweeps = 40000
# nClusInit = 30
runInfoObj_z_burn10k_sweep40k_init30 = profRegr(yModel = "Normal",
                                         xModel = "Normal",
                                         nSweeps = 40000,
                                         nBurn = 10000,
                                         data = NC_df_z,
                                         output = "output",
                                         covNames = covariate_names,
                                         nClusInit = 30,
                                         run = TRUE,
                                         excludeY = TRUE, 
                                         seed = 1234)

# calculate dissimilarity matrix
dissimObj_burn10k_sweep40k_init30 = calcDissimilarityMatrix(runInfoObj_z_burn10k_sweep40k_init30)
# calculate optimal clustering
# vary the max number of clusters allowed
clusObj_burn10k_sweep40k_init30_nomax = calcOptimalClustering(dissimObj_burn10k_sweep40k_init30)
table(clusObj_burn10k_sweep40k_init30_nomax$clustering)
clusObj_burn10k_sweep40k_init30_max10 = calcOptimalClustering(dissimObj_burn10k_sweep30k_init30, maxNClusters = 10)
table(clusObj_burn10k_sweep40k_init30_max10$clustering)
clusObj_burn10k_sweep40k_init30_max20 = calcOptimalClustering(dissimObj_burn10k_sweep30k_init30, maxNClusters = 20)
table(clusObj_burn10k_sweep40k_init30_max20$clustering)

# nBurn = 10000
# nSweeps = 20000
# nClusInit = 10
runInfoObj_z_burn10k_sweep20k_init10 = profRegr(yModel = "Normal",
                                                xModel = "Normal",
                                                nSweeps = 20000,
                                                nBurn = 10000,
                                                data = NC_df_z,
                                                output = "output.dir",
                                                covNames = covariate_names,
                                                nClusInit = 10,
                                                run = TRUE,
                                                excludeY = TRUE, 
                                                seed = 1234)

# calculate dissimilarity matrix
dissimObj_burn10k_sweep20k_init10 = calcDissimilarityMatrix(runInfoObj_z_burn10k_sweep20k_init10)
# calculate optimal clustering
# vary the max number of clusters allowed
clusObj_burn10k_sweep20k_init10_nomax = calcOptimalClustering(dissimObj_burn10k_sweep20k_init10)
table(clusObj_burn10k_sweep20k_init10_nomax$clustering)
clusObj_burn10k_sweep20k_init10_max10 = calcOptimalClustering(dissimObj_burn10k_sweep20k_init10, maxNClusters = 10)
table(clusObj_burn10k_sweep20k_init10_max10$clustering)
clusObj_burn10k_sweep20k_init10_max20 = calcOptimalClustering(dissimObj_burn10k_sweep20k_init10, maxNClusters = 20)
table(clusObj_burn10k_sweep20k_init10_max20$clustering)


# nBurn = 10000
# nSweeps = 100000
# nClusInit = 30
runInfoObj_z_burn10k_sweep100k_init30 = profRegr(yModel = "Normal",
                                         xModel = "Normal",
                                         nSweeps = 100000,
                                         nBurn = 10000,
                                         data = NC_df_z,
                                         output = "output",
                                         covNames = covariate_names,
                                         nClusInit = 30,
                                         run = TRUE,
                                         excludeY = TRUE, 
                                         seed = 1234)

# calculate dissimilarity matrix
dissimObj_burn10k_sweep100k_init30 = calcDissimilarityMatrix(runInfoObj_z_burn10k_sweep100k_init30)
# calculate optimal clustering
# vary the max number of clusters allowed
clusObj_burn10k_sweep100k_init30_nomax = calcOptimalClustering(dissimObj_burn10k_sweep100k_init30)
table(clusObj_burn10k_sweep100k_init30_nomax$clustering)
# ran this above on 11/21/19
clusObj_burn10k_sweep100k_init30_max10 = calcOptimalClustering(dissimObj_burn10k_sweep100k_init30, maxNClusters = 10)
table(clusObj_burn10k_sweep100k_init30_max10$clustering)
clusObj_burn10k_sweep100k_init30_max20 = calcOptimalClustering(dissimObj_burn10k_sweep100k_init30, maxNClusters = 20)
table(clusObj_burn10k_sweep100k_init30_max20$clustering)

globalParsTrace(runInfoObj_z_burn10k_sweep100k_init30, 
                parameters = "beta", 
                plotBurnIn = FALSE,
                whichBeta = 1)
betaChain <- mcmc(read.table("output_beta.txt")[, 1])
autocorr.plot(betaChain)

# nBurn = 10000
# nSweeps = 100000
# nClusInit = 5
runInfoObj_z_burn10k_sweep100k_init5 = profRegr(yModel = "Normal",
                                                 xModel = "Normal",
                                                 nSweeps = 100000,
                                                 nBurn = 10000,
                                                 data = NC_df_z,
                                                 output = "output",
                                                 covNames = covariate_names,
                                                 nClusInit = 5,
                                                 run = TRUE,
                                                 excludeY = TRUE, 
                                                 seed = 1234)

# calculate dissimilarity matrix
dissimObj_burn10k_sweep100k_init5 = calcDissimilarityMatrix(runInfoObj_z_burn10k_sweep100k_init5)
# calculate optimal clustering
# vary the max number of clusters allowed
clusObj_burn10k_sweep100k_init5_nomax = calcOptimalClustering(dissimObj_burn10k_sweep100k_init5)
table(clusObj_burn10k_sweep100k_init5_nomax$clustering)

# incorporate label-switching moves

# nBurn = 10000
# nSweeps = 50000
# nClusInit = 20
runInfoObj_z_burn10k_sweep100k_init5_label123 = profRegr(yModel = "Normal",
                                                xModel = "Normal",
                                                nSweeps = 20000,
                                                nBurn = 10000,
                                                data = NC_df_z,
                                                output = "output",
                                                covNames = covariate_names,
                                                nClusInit = 20,
                                                whichLabelSwitch="123",
                                                run = TRUE,
                                                excludeY = TRUE, 
                                                seed = 1234)
# calculate dissimilarity matrix
dissimObj_z_burn10k_sweep20k_init20_label123 <- calcDissimilarityMatrix(runInfoObj_z_burn10k_sweep100k_init5_label123)
# calculate optimal clustering
# vary the max number of clusters allowed
clusObj_z_burn10k_sweep20k_init20_label123_nomax = calcOptimalClustering(dissimObj_z_burn10k_sweep20k_init20_label123)
table(clusObj_z_burn10k_sweep20k_init20_label123_nomax$clustering)

# WAY BETTER: found 11 clusters!!!
beta_diagnostic_z_burn10k_sweep20k_init20_label123_nomax <- globalParsTrace(runInfoObj_z_burn10k_sweep100k_init5_label123,
                                                                             parameters = "beta", plotBurnIn = FALSE,
                                                                             whichBeta = 1)
beta_diagnostic_z_burn10k_sweep20k_init20_label123_nomax
library(coda)
betaChain <- mcmc(read.table("output_beta.txt")[, 1])
autocorr.plot(betaChain)
logpost <- read_delim("output_logPost.txt", delim = '\t')


# try more sweeps
setwd("/Users/karamccormack/OneDrive - Duke University/Spatial LCM Paper/Output/Scratch/scratch_bpr2/")
# nBurn = 10000
# nSweeps = 70000
# nClusInit = 20
runInfoObj_z_burn10k_sweep50k_init20_label123 = profRegr(yModel = "Normal",
                                                         xModel = "Normal",
                                                         nSweeps = 50000,
                                                         nBurn = 10000,
                                                         data = NC_df_z,
                                                         output = "output_50k",
                                                         covNames = covariate_names,
                                                         nClusInit = 20,
                                                         whichLabelSwitch="123",
                                                         run = TRUE,
                                                         excludeY = TRUE, 
                                                         seed = 1234)
# calculate dissimilarity matrix
dissimObj_z_burn10k_sweep50k_init20_label123 <- calcDissimilarityMatrix(runInfoObj_z_burn10k_sweep50k_init20_label123)
# calculate optimal clustering
# vary the max number of clusters allowed
clusObj_z_burn10k_sweep50k_init20_label123_nomax = calcOptimalClustering(dissimObj_z_burn10k_sweep50k_init20_label123)
table(clusObj_z_burn10k_sweep50k_init20_label123_nomax$clustering)
# 23 clusters
beta_diagnostic_z_burn10k_sweep50k_init20_label123_nomax <- globalParsTrace(runInfoObj_z_burn10k_sweep50k_init20_label123,
                                                                            parameters = "beta", plotBurnIn = FALSE,
                                                                            whichBeta = 1)
#########################################
# try with log transformed data

# load log-transformed NC NATA data
NC_df_log = read.csv(file.path(data.dir, "NC_NATA_wide_log_transform.csv"))[,-1]
# grab variable names from original NC NATA data
colnames(NC_df_log) <- colnames(NC_df)
# need no spaces in pollutant names to plug into profile regression
colnames(NC_df_log) <- str_replace_all(colnames(NC_df_log)," " , "_")

setwd("/Users/karamccormack/OneDrive - Duke University/Spatial LCM Paper/Output/Scratch/scratch_log_sweep20k_label123/")
# nBurn = 10000
# nSweeps = 20000
# nClusInit = 15

runInfoObj_log_burn10k_sweep20k_init15 = profRegr(yModel = "Normal",
                                                 xModel = "Normal",
                                                 nSweeps = 20000,
                                                 nBurn = 10000,
                                                 data = NC_df_log,
                                                 output = "output",
                                                 covNames = covariate_names,
                                                 nClusInit = 20,
                                                 whichLabelSwitch="123",
                                                 run = TRUE,
                                                 excludeY = TRUE, 
                                                 seed = 1234)

# calculate dissimilarity matrix
dissimObj_log_burn10k_sweep20k_init15 = calcDissimilarityMatrix(runInfoObj_log_burn10k_sweep20k_init15)
# calculate optimal clustering
clusObj_log_burn10k_sweep20k_init15_nomax = calcOptimalClustering(dissimObj_log_burn10k_sweep20k_init15)
table(clusObj_log_burn10k_sweep20k_init15_nomax$clustering)

library(coda)
gelman.plot(runInfoObj_log_burn10k_sweep20k_init15)

logpost = read.table("/Users/karamccormack/OneDrive - Duke University/Spatial LCM Paper/Output/Scratch/scratch_log_sweep20k_label123/output_logpost.txt")
lp_mcmc = mcmc(logpost[,1])
gelman.plot(lp_mcmc[,1], bin.width = 10, max.bins = 50)
nchain(lp_mcmc)
# note: "~/OneDrive - Duke University/Spatial LCM Paper/Code/updated_rdata/nata_bpr_11.27.19.RData"
# contains rdata for above two runs

# running after meeting 11/27/28
# use log data, set alpha = 1
# nBurn = 10000
# nSweeps = 20000
# nClusInit = 15
setwd("/Users/karamccormack/OneDrive - Duke University/Spatial LCM Paper/Output/Scratch/scratch-alpha1-11.28.19/")
runInfoObj_log_burn10k_sweep20k_init15_alpha1 = profRegr(yModel = "Normal",
                                                         xModel = "Normal",
                                                         nSweeps = 20000,
                                                         nBurn = 10000,
                                                         data = NC_df_log,
                                                         output = "output",
                                                         covNames = covariate_names,
                                                         nClusInit = 15,
                                                         alpha = 1,
                                                         run = TRUE,
                                                         excludeY = TRUE, 
                                                         seed = 1234)
dissimObj_log_burn10k_sweep20k_init15_alpha1 = calcDissimilarityMatrix(runInfoObj_log_burn10k_sweep20k_init15_alpha1)
clusObj_log_burn10k_sweep20k_init15_alpha1 = calcOptimalClustering(dissimObj_log_burn10k_sweep20k_init15_alpha1)
table(clusObj_log_burn10k_sweep20k_init15_alpha1$clustering)
# got 17 clusters... interesting. 
# I'll try again below with nClusInit = 30.
# I will overwrite the output files in the working directory. 
# output files for the run below are again in: "/Users/karamccormack/OneDrive - Duke University/Spatial LCM Paper/Output/Scratch/scratch-alpha1-11.28.19/"
runInfoObj_log_burn10k_sweep20k_init30_alpha1 = profRegr(yModel = "Normal",
                                                         xModel = "Normal",
                                                         nSweeps = 20000,
                                                         nBurn = 10000,
                                                         data = NC_df_log,
                                                         output = "output",
                                                         covNames = covariate_names,
                                                         nClusInit = 30,
                                                         alpha = 1,
                                                         run = TRUE,
                                                         excludeY = TRUE, 
                                                         seed = 1234)
dissimObj_log_burn10k_sweep20k_init30_alpha1 = calcDissimilarityMatrix(runInfoObj_log_burn10k_sweep20k_init30_alpha1)
clusObj_log_burn10k_sweep20k_init30_alpha1 = calcOptimalClustering(dissimObj_log_burn10k_sweep20k_init30_alpha1)
table(clusObj_log_burn10k_sweep20k_init30_alpha1$clustering)
# got 9 clusters, yay!
# try again with 30k sweeps.

# set this up later!
setwd("/Users/karamccormack/OneDrive - Duke University/Spatial LCM Paper/Output/Scratch/scratch_bpr/")
runInfoObj_log_burn10k_sweep20k_init30_alpha1_switch123 = profRegr(yModel = "Normal",
                                                  xModel = "Normal",
                                                  nSweeps = 20000,
                                                  nBurn = 10000,
                                                  data = NC_df_log,
                                                  output = "output",
                                                  covNames = covariate_names,
                                                  nClusInit = 30,
                                                  run = TRUE,
                                                  excludeY = TRUE, 
                                                  seed = 1234,
                                                  alpha = 1,
                                                  whichLabelSwitch = "123")
dissimObj_log_burn10k_sweep20k_init30_alpha1_switch123 = calcDissimilarityMatrix(runInfoObj_log_burn10k_sweep20k_init30_alpha1_switch123)
clusObj_log_burn10k_sweep20k_init30_alpha1_switch123 = calcOptimalClustering(dissimObj_log_burn10k_sweep20k_init30_alpha1_switch123)
table(clusObj_log_burn10k_sweep20k_init30_alpha1_switch123$clustering)
# got 9 clusters!
# try again with exact same inputs.
runInfoObj_log_burn10k_sweep20k_init30_alpha1_switch123_v2 = profRegr(yModel = "Normal",
                                                                   xModel = "Normal",
                                                                   nSweeps = 20000,
                                                                   nBurn = 10000,
                                                                   data = NC_df_log,
                                                                   output = "output",
                                                                   covNames = covariate_names,
                                                                   nClusInit = 30,
                                                                   run = TRUE,
                                                                   excludeY = TRUE, 
                                                                   seed = 1234,
                                                                   alpha = 1,
                                                                   whichLabelSwitch = "123")
dissimObj_log_burn10k_sweep20k_init30_alpha1_switch123_v2 = calcDissimilarityMatrix(runInfoObj_log_burn10k_sweep20k_init30_alpha1_switch123_v2)
clusObj_log_burn10k_sweep20k_init30_alpha1_switch123_v2 = calcOptimalClustering(dissimObj_log_burn10k_sweep20k_init30_alpha1_switch123_v2)
table(clusObj_log_burn10k_sweep20k_init30_alpha1_switch123_v2$clustering)
# yesss got 9 clusters again, exact same table. 

# now try again with init 35 clusters
runInfoObj_log_burn10k_sweep20k_init35_alpha1_switch123 = profRegr(yModel = "Normal",
                                                                      xModel = "Normal",
                                                                      nSweeps = 20000,
                                                                      nBurn = 10000,
                                                                      data = NC_df_log,
                                                                      output = "output",
                                                                      covNames = covariate_names,
                                                                      nClusInit = 35,
                                                                      run = TRUE,
                                                                      excludeY = TRUE, 
                                                                      seed = 1234,
                                                                      alpha = 1,
                                                                      whichLabelSwitch = "123")
dissimObj_log_burn10k_sweep20k_init35_alpha1_switch123 = calcDissimilarityMatrix(runInfoObj_log_burn10k_sweep20k_init35_alpha1_switch123)
clusObj_log_burn10k_sweep20k_init35_alpha1_switch123 = calcOptimalClustering(dissimObj_log_burn10k_sweep20k_init35_alpha1_switch123)
table(clusObj_log_burn10k_sweep20k_init35_alpha1_switch123$clustering)
# got 8 clusters!

# add clusters to dataset, rename column to "cluster"
NC_df = cbind(NC_df, clusObj$clustering)
names(NC_df)[ncol(NC_df)] = "Cluster"
unique(NC_df$Cluster)

# output to csv
write.csv(NC_df, file = file.path(output.dir, "NC_poll_clusters.csv"))

