###########################################################
# Title: Plotting NC ACS Data: Bayesian Profile Regression Clusters
# Authors: Kara McCormack (kara.mccormack@duke.edu)
# Date created: 11/5/2019
# Input files: "/Users/karamccormack/Box/SES-environment/Spatial LCM Paper/Output/NC_SES_Clusters.csv"
#   
# Output file: "/Users/karamccormack/Box/SES-environment/Spatial LCM Paper/Plots_Figures/ACS_results/acs_barplot_*.png"
###########################################################

# Goals: plot average SES proportions based on cluster. 

# load libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)

# set up paths
data.dir = "/Users/karamccormack/Box/SES-environment/Spatial LCM Paper/Output"
output.dir = "/Users/karamccormack/Box/SES-environment/Spatial LCM Paper/Plots_Figures/ACS_results/"

# read in nc pollutant data (with clusters!)
NC_df = read.csv(file.path(data.dir, "NC_SES_clusters.csv"))[-1]

# wide to long
# Make sure the subject column is a factor
# undo transformation
NC_df$Tract <- factor(NC_df$Tract)
NC_df_long <- NC_df %>%
  gather(ses_factor,
         proportion,
         pop_non_hispanic_black:phone_service, 
         -Cluster) %>%
  mutate(proportion = (proportion)^2) 

plots = NULL
# do barplots for all clusters, save in plots variable.
for(i in unique(NC_df_long$Cluster)){
  NC_cluster = NC_df_long %>%
    filter(Cluster == i) 
  plots[[i]] = NC_cluster %>%
    group_by(ses_factor) %>%
    filter(!is.na(proportion)== T) %>%
    summarise(mean = mean(proportion)) %>%
    ggplot(aes(x = ses_factor, y = mean, fill = "CCCC00")) +
    geom_bar(stat = "identity") + 
    theme_minimal() +
    ylab("Percent") +
    ylim(c(0, 1)) +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45, 
                                     hjust = 1, 
                                     size = 12),
          plot.title = element_text(hjust = 0.5),
          axis.text.y = element_text(size = 14), 
          axis.title.x = element_blank()) +
    ggtitle(paste("Cluster", i, sep = " "))
  
  ggsave(filename = paste0("/Users/karamccormack/Box/SES-environment/Spatial LCM Paper/Plots_Figures/ACS_results/acs_barplot_", i, ".png"),
        width=10.5, 
         height=6.5)
}
plots[[1]]

# facet wrap on same figure
NC_df_long %>%
  group_by(Cluster) %>%
  group_by(ses_factor) %>%
  filter(!is.na(proportion)== T) %>%
  summarise(mean = mean(proportion)) %>%
  ggplot(aes(x = ses_factor, 
             y = mean,
             fill = ses_factor)) +
  geom_bar(stat = "identity") +
  facet_wrap ( ~ Cluster) +
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1, 
                                   size = 8),
        axis.text.y = element_text(size = 14), 
        axis.title.x = element_blank()) +
  theme_minimal() +
  ylab("Percent") +
  ylim(c(0, 1))
p







ggplot(data = NC_df_long) + 
  geom_bar(aes(x=ses_factor, y=proportion, fill=ses_factor)) +
  facet_wrap( ~ Cluster) 


           
           
           
par(mfrow = c(2,2))
plots[[1]]
plots[[2]]
plots[[3]]
plots[[4]]

# scratch barplot 
prop1 = NC_df_long %>%
  filter(Cluster == 1) %>%
  group_by(ses_factor) %>%
  filter(!is.na(proportion)== T) %>%
  summarise(mean = mean(proportion)) %>%
  ggplot(aes(x = ses_factor, y = mean, fill = ses_factor)) +
  geom_bar(stat = "identity") + 
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, 
                                   hjust = 1, 
                                   size = 12),
        axis.text.y = element_text(size = 14), 
        axis.title.x = element_blank()) +
  ylab("Percent") +
  ylim(c(0, 1)) +
  ggtitle("Cluster 1")

prop1
