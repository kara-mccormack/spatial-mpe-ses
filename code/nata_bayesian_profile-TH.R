###########################################################
# Title: Bayesian Profile Regression of NATA 2014 for NC
# Authors: Kara McCormack (kara.mccormack@duke.edu)
# Date created: 10/19/2019
# Input files: 
#   /Users/karamccormack/Box/SES-environment/Code/v1/Output/NATA_2014_US.csv
#   
# Output file: 
###########################################################

# install.packages("PReMiuM")

# source pollutant names file
#source("/Users/karamccormack/Box/SES-environment/Code/v1/utils/pollutant_names.R")

# load libraries
library(PReMiuM)
library(stringr)
library(readr)

# set up paths

data.dir = "/Users/tmh45/Box Sync/SENSITIVE Folder tmh45/zz-projects/seewaldt/food deserts/SES-Environment/Spatial LCM Paper/Data"
output.dir = "/Users/tmh45/Box Sync/SENSITIVE Folder tmh45/zz-projects/seewaldt/food deserts/SES-Environment/Spatial LCM Paper/Output"

# upload NATA Data
NC_df <- read_csv(file.path(data.dir, "NC_NATA_wide_total_conc.csv"))[,-1]

# convert to data frame
NC_df <- as.data.frame(NC_df)
is.data.frame(NC_df)
# Prepare data for profile regression

# need no spaces in pollutant names to plug into profile regression
colnames(NC_df) <- str_replace_all(colnames(NC_df)," " , "_")


# add fake outcome for now (simply to implement Premium package)
# we will be ignoring y outcome but need it for initial profRegr step
Outcome = rnorm(nrow(NC_df), mean = 0, sd = 1)
NC_df = cbind(Outcome, NC_df)
NC_df = as.data.frame(NC_df)

# check if still a data frame
is.data.frame(NC_df)

# define covariate names
covariate_names <- colnames(NC_df)[-c(1, 2)]

# check for NA in pollutants
for (name in covariate_names){
  print(paste("There are", sum(is.na(NC_df$name)), "NAs in", name, ".", sep = " "))
}

# run bayesian profile regression
runInfoObj = profRegr(yModel = "Normal",
                      xModel = "Normal",
                      nSweeps = 10000,
                      nBurn = 20000,
                      data = NC_df,
                      output = "output_test",
                      covNames = covariate_names,
                      nClusInit = 20,
                      run = TRUE,
                      seed=1234,
                      excludeY = TRUE)

dissimObj = calcDissimilarityMatrix(runInfoObj)
clusObj = calcOptimalClustering(dissimObj)
riskProfileObj = calcAvgRiskAndProfile(clusObj)
clusterOrderObj = plotRiskProfile(riskProfileObj, file.path(output.dir, "summary-sim-TH.png"))
heatDissMat(dissimObj)
cluster.yes<-clusObj$clustering
table(cluster.yes)
names(profile_nata)
profile_nata$nCovariates
profile_nata$nSweeps
NC_clus = cbind (NC_df,)