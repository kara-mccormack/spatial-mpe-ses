###########################################################
# Title: Plotting NC Pollutants: Bayesian Profile Regression Clusters
# Authors: Kara McCormack (kara.mccormack@duke.edu)
# Date created: 10/30/2019
# Input files: 
#   
# Output file: 
###########################################################

# Goals: plot average pollutant levels based on cluster. 

options(scipen=999) # turn off scientific notation

# load libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(stringr)

# set up paths
data.dir = "/Users/karamccormack/Box/SES-environment/Spatial LCM Paper/Output"
output.dir = "/Users/karamccormack/Box/SES-environment/Spatial LCM Paper/Plots_Figures/NATA_results/"
scratch.dir = "/Users/karamccormack/Box/SES-environment/Spatial LCM Paper/Output/Scratch/scratch_plots/nata_barplots_raw/"

# source files
source("/Users/karamccormack/Box/SES-environment/Code/v1/utils/plot_functions.R")
source("/Users/karamccormack/Box/SES-environment/Code/v2/utils/variable_names.R")

# read in nc pollutant data (with clusters!)
# note these measurements are in log scale
NC_df = read.csv(file.path(data.dir, "NC_poll_clusters.csv"))[,-1]

# how many tracts in each cluster?
for(i in 1:length(unique(NC_df$Cluster))) {
  df = NC_df[NC_df$Cluster==i,]
  print(paste(dim(df)[1], "tracts in cluster", df$Cluster[1]))
}

# Cluster membership Barplots

# wide to long
# Make sure the subject column is a factor
# undo log transformation
NC_df$Tract <- factor(NC_df$Tract)
NC_df_long <- NC_df %>%
  gather(Pollutant,
         Level,
         X1.3.BUTADIENE:XYLENES_.MIXED_ISOMERS., 
         -Cluster) %>% # keep the cluster column - need this for plotting
  mutate(est_raw = exp(Level), 
         Pollutant = tolower(str_replace_all(Pollutant, "_", " "))) 

# add on type variable
# need to make "Pollutant" names match those of poln_vrbs$nms
# rename three pollutants who still don't match
NC_df_long[NC_df_long$Pollutant == "x1.3.butadiene",]$Pollutant <- "1,3-butadiene"
NC_df_long[NC_df_long$Pollutant == "methyl chloride .chloromethane.",]$Pollutant <- "methyl chloride"
NC_df_long[NC_df_long$Pollutant == "xylenes .mixed isomers.",]$Pollutant <- "xylenes"

# make sure names match
unique(NC_df_long$Pollutant) == poln_vrbs$nms

# prep for left join
poln = poln_vrbs[,c("nms", "type")]
colnames(poln)[1] <- "Pollutant"
poln$Pollutant <- as.character(poln$Pollutant)

# left join with type
NC_df_long = NC_df_long %>%
  left_join(poln) 

# calculate percentiles
est_perc = NC_df_long$est_raw
for(pol in unique(NC_df_long$Pollutant)){
  w <- which(NC_df_long$Pollutant == pol)
  est_perc[w] <- 100*ecdf(NC_df_long[NC_df_long$Pollutant == pol,]$est_raw)(NC_df_long$est_raw[w])
}

NC_df_long$est_perc = est_perc

# individual barplots
# initiate empty plots variable to store plots
plots = NULL

# do barplots for all clusters, save in plots variable.
for(i in unique(NC_df_long$Cluster)){
  NC_cluster = NC_df_long %>%
    filter(Cluster == i) 
  plots[[i]] = NC_cluster %>%
    group_by(Pollutant) %>%
    summarise(mean = mean(est_perc)) %>%
    left_join(poln) %>%
    ggplot(aes(x = Pollutant, 
               y = mean, 
               fill = type)) +
    geom_bar(stat = "identity") + 
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, 
                                     hjust = 1, 
                                     size = 14),
          axis.title.y = element_text(size = 12),
          axis.title.x = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 14),
          plot.title = element_text(hjust = 0.5),
          axis.text.y = element_text(size = 18)) +
    ylab("Exposure Level Percentile") +
    ylim(c(0, 100)) +
    ggtitle(paste("Cluster", i, sep = " "))
  
#  ggsave(filename = paste0("/Users/karamccormack/Box/SES-environment/Spatial LCM Paper/Plots_Figures/NATA_results/nata_perc_barplot_c", i, ".png"),
#         width=10.5, 
#         height=6.5)
  ggsave(filename = paste0("/Users/karamccormack/Box/SES-environment/Spatial LCM Paper/Plots_Figures/NATA_results/five_clusters/nata_perc_barplot_c", i, ".png"),
         width = 10.5,
         height = 6.5)
}

plots[[1]]
plots[[2]]
plots[[3]]
plots[[4]]
plots[[5]]

par(mfrow = c(3,2))


# facet plots based on cluster
NC_df_long %>%
  group_by(Pollutant) %>%
  summarise(mean = mean(est_perc)) %>%
  ggplot(aes(x = Pollutant, y = mean)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Cluster) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1, 
                                   size = 14),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(size = 18)) +
  ylab("Exposure Level Percentile") +
  ylim(c(0, 100)) +
  ggtitle("Cluster 1")
# scratch

NC_df_long %>%
  filter(Cluster == 1) %>%
  group_by(Pollutant) %>%
  summarise(mean = mean(est_perc)) %>%
  left_join(poln) %>%
  head
  ggplot(aes(x = Pollutant, y = mean)) +
  geom_bar(stat = "identity") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1, 
                                   size = 14),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(size = 18)) +
  ylab("Exposure Level Percentile") +
  ylim(c(0, 100)) +
  ggtitle("Cluster 1")

# scratch #2
df2 = NC_df_long %>%
  group_by(Cluster) %>%
  group_by(Pollutant) %>%
  summarise(mean = mean(est_perc))

ggplot(data = NC_df_long) +
  geom_bar(aes(x = Pollutant, y = est_perc))

# notes
# starting seeds and variation
# map the clusters on map of NC
#   - how to do units? some will be on different scales
# how much are they touching
# my task: create a new kind of adjacency matrix!
# arcgis
# fix file paths in this file
# ask profs to be on committee (jane, william pan)
# r-inla gentle tutorial
