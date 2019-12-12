# SCRATCH

# barplot example
library(ggplot2)
df <- data.frame(dose=c("D0.5", "D1", "D2"),
                 len=c(4.2, 10, 29.5))
head(df)

# Basic barplot
p<-ggplot(data=df, aes(x=dose, y=len)) +
  geom_bar(stat="identity")
p

# Horizontal bar plot
p + coord_flip()

# Change the width of bars
ggplot(data=df, aes(x=dose, y=len)) +
  geom_bar(stat="identity", width=0.5)
# Change colors
ggplot(data=df, aes(x=dose, y=len)) +
  geom_bar(stat="identity", color="blue", fill="white")
# Minimal theme + blue fill color
p<-ggplot(data=df, aes(x=dose, y=len)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal()
p

# choose which items to display
p + scale_x_discrete(limits=c("D0.5", "D2"))

# Outside bars
ggplot(data=df, aes(x=dose, y=len)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=len), vjust=-0.3, size=3.5)+
  theme_minimal()
# Inside bars
ggplot(data=df, aes(x=dose, y=len)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=len), vjust=1.6, color="white", size=3.5)+
  theme_minimal()

head(mtcars)
# Don't map a variable to y
ggplot(mtcars, aes(x=factor(cyl)))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_minimal()

# Change barplot line colors by groups
p<-ggplot(df, aes(x=dose, y=len, color=dose)) +
  geom_bar(stat="identity", fill="white")
p


#################################
# summarise

# Usually, you'll want to group first
mtcars %>%
  group_by(cyl) %>%
  summarise(mean = mean(disp), n = n())

#######################
# spdep

data(used.cars, package="spData")
data(state)
cont_st <- match(attr(usa48.nb, "region.id"), state.abb)
cents <- as.matrix(as.data.frame(state.center))[cont_st,]
opar <- par(mfrow=c(2,1))
plot(usa48.nb, cents, xlim=c(-125, -65), ylim=c(25, 50))
IDs <- as.character(state.division[cont_st])
agg_cents <- aggregate(cents, list(IDs), mean)
agg_nb <- aggregate(usa48.nb, IDs)
plot(agg_nb, agg_cents[, 2:3], xlim=c(-125, -65), ylim=c(25, 50))
text(agg_cents[, 2:3], agg_cents[, 1], cex=0.6)
par(opar)

####
library(PReMiuM)
inputs <- generateSampleDataFile(clusSummaryBernoulliDiscrete())
# prediction profiles
preds<-data.frame(matrix(c(0, 0, 1, 0, 0,
                           0, 0, 1, NA, 0),ncol=5,byrow=TRUE))
colnames(preds)<-names(inputs$inputData)[2:(inputs$nCovariates+1)]
# run profile regression
setwd("/Users/karamccormack/Box/SES-environment/Spatial LCM Paper/Output/Scratch/scratch_bpr/")
runInfoObj<-profRegr(yModel=inputs$yModel, xModel=inputs$xModel,
                     nSweeps=100, nBurn=1000, data=inputs$inputData, output="output",
                     covNames=inputs$covNames,predict=preds)
# postprocessing
dissimObj <- calcDissimilarityMatrix(runInfoObj)
clusObj <- calcOptimalClustering(dissimObj)

head(clusObj$clustering, 20)
table(clusObj$clustering)

#### scratch
setwd("/Users/karamccormack/Box/SES-environment/Spatial LCM Paper/Output/Scratch")
load("runInfoObj_z_b10k_s20k")
dissimObj_b10k_s20k <- calcDissimilarityMatrix(runInfoObj_z_burn10k_sweep20k)
clusObj <- calcOptimalClustering(dissimObj_b10k_s20k, maxNClusters = 12)
table(clusObj$clustering)


###### scratch bpr simulation
library(PReMiuM)

working.dir = "/Users/karamccormack/Box/SES-environment/Spatial LCM Paper/v2/spatial-mpe-ses/output/scratch/example_logmarginalpost/"
# generate sample data file
inputs <- generateSampleDataFile(clusSummaryBernoulliDiscrete())
# set working directory - temporary output files will live here
setwd(working.dir)
# number of initial clusters
nClusInit <- c(10, 20, 50, 75)
# run simulation for each number of initial clusters
for (i in 1:length(nClusInit)) { 
  runInfoObj <- profRegr(yModel = inputs$yModel, 
                         xModel = inputs$xModel, 
                         nSweeps = 10000, 
                         nBurn = 10000, 
                         data = inputs$inputData, 
                         output = paste("init", nClusInit[i], sep = ""), 
                         covNames = inputs$covNames, 
                         alpha = 1, 
                         fixedEffectsNames = inputs$fixedEffectNames, 
                         nClusInit = nClusInit[i]) 
  margModelPosterior(runInfoObj)
}

mmp <- list()
for(i in 1:length(nClusInit)){
  mmp[[i]] <- read.table(paste("init", nClusInit[i], "_margModPost.txt",
                               sep = ""))[,1]
}
# plotting
plot(c(head(nClusInit, n = 1) -0.5, tail(nClusInit, n = 1) + 0.5),
     c(min(unlist(mmp)), max(unlist(mmp))), type = "n",
     ylab = "Log Marginal Model Posterior",
     xlab = "Initial number of clusters", cex.lab = 1.3, xaxt = "n")
axis(1, at = nClusInit, labels = nClusInit)
for(i in 1:length(nClusInit)) {
  boxplot(mmp[[i]], add = TRUE, at = nClusInit[i], pch = ".",
          boxwex = 5, col = "coral2")
}

# try alternate plotting
logPost = list()
for(i in 1:length(nClusInit)){
  logPost[[i]] <- read.table(paste("init", nClusInit[i], "_logPost.txt",
                               sep = ""))[,1]
}
# plotting
plot(c(head(nClusInit, n = 1) -0.5, tail(nClusInit, n = 1) + 0.5),
     c(min(unlist(logPost)), max(unlist(logPost))), type = "n",
     ylab = "Log Posterior",
     xlab = "Initial number of clusters", cex.lab = 1.3, xaxt = "n")
axis(1, at = nClusInit, labels = nClusInit)
for(i in 1:length(nClusInit)) {
  boxplot(logPost[[i]], add = TRUE, at = nClusInit[i], pch = ".",
          boxwex = 5, col = "lightblue1")
}

### convergence of beta plots
# load coda library
library(coda)
library(PReMiuM)
# set working director - place where output files will live
setwd("/Users/karamccormack/OneDrive - Duke University/Spatial LCM Paper/Output/Scratch/scratch_bpr/")
output.dir = "/Users/karamccormack/OneDrive - Duke University/Spatial LCM Paper/Output/Scratch/scratch_plots/"

# generate random inputs
inputs <- generateSampleDataFile(clusSummaryBernoulliDiscrete())
# define initial number of clusters to be explored
initial_clusters <- c(5, 10, 20)
# define number of sweeps (to do after burn-in)
nsweeps = 1000
# define number of burn-in sweeps
nburn = 100

for (i in 1:length(initial_clusters)){
  runInfoObj <- profRegr(yModel = inputs$yModel, 
                         xModel = inputs$xModel,
                         nSweeps = nsweeps, 
                         nBurn = nburn, 
                         data = inputs$inputData,
                         output = "output", 
                         nClusInit = initial_clusters[i], 
                         seed = 456, 
                         covNames = inputs$covNames,
                         fixedEffectsNames = inputs$fixedEffectNames)
  globalParsTrace(runInfoObj, parameters = "beta", plotBurnIn = FALSE,
                  whichBeta = 1)
  betaChain <- mcmc(read.table("output_beta.txt")[, 1])
  autocorr.plot(betaChain, main = paste("Initial Clusters = ", 
                                        initial_clusters[i], 
                                        sep = ""))
}


runInfoObj <- profRegr(yModel = inputs$yModel, 
                       xModel = inputs$xModel,
                       nSweeps = nsweeps, 
                       nBurn = nburn, 
                       data = inputs$inputData,
                       output = "output", 
                       nClusInit = initial_clusters[1], 
                       covNames = inputs$covNames,
                       seed = 456, 
                       fixedEffectsNames = inputs$fixedEffectNames)


# 1. Open jpeg file
png("rplot_5init.png", width = 350, height = "350")
# 2. Create the plot
globalParsTrace(runInfoObj, parameters = "beta", plotBurnIn = FALSE,
                whichBeta = 1)
# 3. Close the file
dev.off()

globalParsTrace(runInfoObj, parameters = "beta", plotBurnIn = FALSE,
                whichBeta = 1)
betaChain <- mcmc(read.table("output_beta.txt")[, 1])
autocorr.plot(betaChain, main = paste(nSweeps, 
                                      "Sweeps, ", 
                                      nBurn, 
                                      "Burn, ", 
                                      initial_clusters[i], 
                                      "Initial Clusters", 
                                      sep = ""))

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

