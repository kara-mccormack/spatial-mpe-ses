###########################################################
# Title: Bayesian Profile Regression of ACS data for NC
# Authors: Kara McCormack (kara.mccormack@duke.edu)
# Date created: 10/19/2019
# Input files: 
#   
#   
# Output file: 
###########################################################

# load libraries
library(dplyr)
library(PReMiuM)

# set up paths
data.dir = "/Users/karamccormack/Box/SES-environment/Spatial LCM Paper/Data"
output.dir = "/Users/karamccormack/Box/SES-environment/Spatial LCM Paper/Output"

ses_data = read.csv(file.path(data.dir,"ACS_1yr_2014_prop_NC_MPlus.csv"))
raw.data = read.csv(file.path(data.dir,"ACS_1yr_2014_raw_NC_MPlus.csv"))

# add square root transformation
# keep in the management variables for now, but keep an eye on their distribution
ses_data2 <- ses_data %>%
  mutate(
    pop_non_hispanic_black = sqrt(pop_non_hispanic_black),
    vehicle_none = sqrt(vehicle_none),
    family_type_single = sqrt(family_type_single),
    family_type_female_householder = sqrt(family_type_female_householder),
    over_25_less_than_hs_graduate = sqrt(over_25_less_than_hs_graduate),
    below_poverty_line = sqrt(below_poverty_line),
    public_assistance = sqrt(public_assistance),
    over_16_unemployed = sqrt(over_16_unemployed),
    housing_rental = sqrt(housing_rental),
    fem_occ_professional = sqrt(fem_occ_professional),
    males_occ_professional = sqrt(males_occ_professional),
    num_occupants_crowded = sqrt(num_occupants_crowded),
    phone_service = sqrt(phone_service))

ses_data = ses_data2

ses_data = ses_data[,-c(12,14)]
names(ses_data)
# bayesian profile regression

# define covariate names
covariate_names <- colnames(ses_data)[-c(1)]

# add fake outcome for now (simply to implement Premium package)
# we will be ignoring y outcome but need it for initial profRegr step
# Outcome = rnorm(nrow(ses_data), mean = 0, sd = 1)
# ses_data = cbind(Outcome, ses_data)
ses_data = as.data.frame(ses_data)

# run bayesian profile regression
runInfoObj_ses = profRegr(yModel = "Normal",
                      xModel = "Normal",
                      nSweeps = 10000,
                      nBurn = 20000,
                      data = ses_data,
                      output = "output.dir2",
                      covNames = covariate_names,
                      nClusInit = 5,
                      run = TRUE,
                      excludeY = TRUE, 
                      seed = 123)

# calculate dissimilarity matrix
dissimObj = calcDissimilarityMatrix(runInfoObj_ses)
clusObj = calcOptimalClustering(dissimObj)

# add clusters to dataset, rename column to "cluster"
ses_data = cbind(ses_data, clusObj$clustering)
names(ses_data)[ncol(ses_data)] = "Cluster"

# output to csv
write.csv(ses_data, file = file.path(output.dir, "NC_SES_clusters.csv"))
