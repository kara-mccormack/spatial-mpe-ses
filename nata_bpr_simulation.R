###########################################################
# Title: Bayesian Profile Regression Simulations/Model Diagnostics of NATA 2014 for NC
# Authors: Kara McCormack (kara.mccormack@duke.edu)
# Date created: 11/22/2019
# Input files: 
#   /Users/karamccormack/Box/SES-environment/Spatial LCM Paper/Data/NC_NATA_wide_z_transform.csv
#   
# Output file: 
#   /Users/karamccormack/Box/SES-environment/Spatial LCM Paper/Output/NC_poll_clusters.csv
###########################################################

# set up paths
data.dir = "/Users/karamccormack/Box/SES-environment/Spatial LCM Paper/Data/"
output.dir = "/Users/karamccormack/Box/SES-environment/Spatial LCM Paper/Output/"

# load z-transformed NC NATA data
NC_df_z = read.csv(file.path(data.dir, "NC_NATA_wide_z_transform.csv"))[,-1]

##### beta plots
runInfoObj_z_burn10k_sweep10k_init5 = profRegr(yModel = "Normal",
                                                xModel = "Normal",
                                                nSweeps = 10000,
                                                nBurn = 10000,
                                                data = NC_df_z,
                                                output = "output",
                                                covNames = covariate_names,
                                                run = TRUE,
                                                excludeY = TRUE, 
                                                seed = 1234)
globalParsTrace(runInfoObj, parameters = "beta", plotBurnIn = FALSE,
                whichBeta = 1)
betaChain <- mcmc(read.table("output_beta.txt")[, 1])
autocorr.plot(betaChain)