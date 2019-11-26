###########################################################
# Title: Correlation Matrix plot with NC ACS and NATA data
# Authors: Kara McCormack (kara.mccormack@duke.edu)
# Date created: 10/19/2019
# Input files: 
#   /Users/karamccormack/Box/SES-environment/Spatial LCM Paper/Data/NC_NATA_wide_perc.csv
#   
# Output file: 
#   /Users/karamccormack/Box/SES-environment/Spatial LCM Paper/Plots_Figures/corrplot_full_NC.png
###########################################################

# Goals:
# 1. Merge ACS and NATA datasets for NC
# 2. Create a correlation plot matrix with pollution and SES data in NC

# install.packages("ggcorrplot")

# load libraries
library(dplyr)
library(ggcorrplot)


# set up paths
data.dir = "/Users/karamccormack/Box/SES-environment/Spatial LCM Paper/Data"
output.dir = "/Users/karamccormack/Box/SES-environment/Spatial LCM Paper/Plots_Figures"

# read in data
acs.prop.data = read.csv(file.path(data.dir,"ACS_1yr_2014_prop_NC_MPlus.csv"))
nata.perc.data = read.csv(file.path(data.dir,"NC_NATA_wide_total_conc.csv"))

# order each dataset by the census tract
acs.prop.data = acs.prop.data[order(acs.prop.data$Tract),]
nata.perc.data = nata.perc.data[order(nata.perc.data$Tract),]

# reorder acs data based on correct categories (based on Table 1)
ord = c(1, 4, 5, 15, 10, 16, 7:9, 3, 2, 11, 13, 6)
acs.prop.data = acs.prop.data[,ord]

# merge the datasets
full.data = acs.prop.data %>% left_join(nata.perc.data[,2:18], by = "Tract")

# compute correlation matrix
corr.full = round(cor(full.data[,-1], use="complete.obs"),1)

# plot correlation matrix
corrplot = ggcorrplot(corr.full, 
                      type = "lower", 
                      outline.col = "white",
                      ggtheme = ggplot2::theme_gray,
                      colors = c("#0a4670", "white", "#E46726"), 
                      tl.cex = 9) +
  scale_fill_gradient2(low = "#0a4670", high = "#E46726", 
                       limit = c(-1, 1),
                       breaks = seq(from = -1, to = 1, by=.2), 
                       label = seq(from = -1, to = 1, by=.2)) + 
  theme(legend.title = element_blank(), 
        legend.key.height = unit(1.3, "in"))

# view plot
# corrplot

# save plot
ggsave("corrplot_NC.png", 
       plot = last_plot(),
       path = output.dir, 
       width = 10,
       height = 10, 
       units = "in")
