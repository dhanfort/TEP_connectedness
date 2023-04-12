################################################################################
#
# This R-code is for the paper by D.L.B. Fortela and A.P. Mikolajczyk
# "Adopting Econometrics Models and Techniques for Detection of Volatility Spillover 
# Effects of Disturbances in A Chemical Process Plant" (2023)
#
#
# This analysis uses the codes and models developed and implemented by 
# Dr. David Gabauer and collaborators (for the R-package "ConnectednessApproach")
# URL: https://gabauerdavid.github.io/ConnectednessApproach/#Dynamic_Connectedness_Measures
#
# The Tenessee-Eastman Chemical Process (TEP) dataset used is downloaded from: 
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/6C3JR1
# The database has a set for: 
#     (1) TEP No Fault - Training Data Set
#     (2) TEP No Fault - Testing Data Set
#     (3) TEP With Faults - Training Data Set
#     (4) TEP With Faults - Testing Data Set
#
# The TEP varibales are: 
#     (1) measured/controlled var. x1 to x41, and 
#     (2) manipulated var. m1 to m11
#
# It is suggested to install R using R-Studio: https://support--rstudio-com.netlify.app/products/rstudio/download/
#
###############################################################################

# import libraries/packages needed for analysis
library(ConnectednessApproach)


# set the working directory

setwd("...(set the path to the local directory of your data and project)....")



# import data using load() function since the TEP datasets are ".RData" files
# make sure that the TEP ".RData" files have been doanloaded to your local computer already
# 
# analyze the 4 datasets separately to not mix the workflow
# make sure to use the data file path for your own setup


load("TEP_Faulty_Testing.RData")
data2 <- faulty_testing



#load("TEP_FaultFree_Testing.RData")
#data0 <- fault_free_testing



# slice datta based on Fault Number "faultNumber":

slice_df <- function(df, col, values){
  df_small = subset(df, df[[col]] %in% values)
  return(df_small)
}


# subset the TEP main dataset
data3 <- slice_df(data2, "faultNumber", 1) # set the number based on the target fault being analyzed
data3 <- slice_df(data3, "simulationRun", 1) # set the number based on the target size o of simulations being analyzed (max of 500 simulations in the TEP dataset)


# subset the TEP main dataset
#data01 <- slice_df(data0, "faultNumber", 0) # set the number based on the target fault being analyzed
#data01 <- slice_df(data01, "simulationRun", 1) # set the number based on the target size o of simulations being analyzed (max of 500 simulations in the TEP dataset)



# slice only the process variables
data3 <- data3[, 4:55]

#data01 <- data01[, 4:55]
#data31 <- data3 - data01


# rename to the columns names using shorter name-codes
colnames(data3) <- c(paste0("x", rep(1:41, each = 1)), paste0("m", rep(1:11, each = 1))) # x1-to-x41 measure vars; m1-to-m11 manipulated vars


#
# arbitray date-stamp but must be in sequence for the model analysis to work
# Since this ConnectednessApproach R-package is intended for finance, the time domain is in days
# Nonetheless, adding an arbitrary time series column to the data does not change the trends in the
# dynamics because the TEP dataset used a regular every 3-minute data sampling: "The TEP variables (columns 4 to 55) were sampled every 3 minutes for a 
# total duration of 25 hours and 48 hours respectively. Note that the faults were introduced 1 and 8 hours into 
# the Faulty Training and Faulty Testing datasets, respectively." (Rieth, Cory A.; Amsel, Ben D.; Tran, Randy; Cook, 
# Maia B., 2017, "Additional Tennessee Eastman Process Simulation Data for Anomaly Detection Evaluation",
# https://doi.org/10.7910/DVN/6C3JR1, Harvard Dataverse, V1)
#

date_vals <- rev(seq(Sys.Date(), by="-1 days", length.out=nrow(data3))) 
data4 <- data.frame(dates=date_vals, data3)


# Plotting of the raw data (a sample/snippet graph)
# Do not use the working data for "zoo" to eliminate extra steps

library(ggplot2)
library(reshape2)

# Plot the time-series data
date_vals2 <- seq(0, length.out=nrow(data3), by = 3)
data42 <- data.frame(dates=date_vals2, data3)


meltdf <- melt(data42,id="dates")
p1 <- ggplot(meltdf,aes(x=dates,y=value,colour=variable,group=variable)) + 
  geom_line() + 
  xlab("Time (minutes)") +
  ylab(NULL) +
  xlim(0,2875) +
  facet_wrap(~ variable, scales="free_y", ncol = 4) +
  theme(legend.position="none") +
  theme(strip.text.x = element_text(size = 20)) +
  theme(axis.text.x = element_text(size=14)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(plot.margin = unit(c(0.2,1,0.5,0.2), "cm"))



# Save figure for presentation/publishing
library(svglite)
#ggsave(p1, filename = "test_1_fault_0.svg", width = 16.5, height = 20 )



# Now performing the computations for connectedness
# Converto to "zoo" format
## Convert date to date
library("zoo")

data4$dates <- as.Date(data4$dates)  
data5 <- read.zoo(data4, index.column = 1)

# use "frequency" analysis to model the short-term, mid-term and long-term effects
# Literature for "frequency": Barunik, J., & Krehlik, T. (2018). Measuring the frequency dynamics of financial 
# connectedness and systemic risk. Journal of Financial Econometrics, 16(2), 271-296.




# Using Time connctedness:
#dca = ConnectednessApproach(data5,model="QVAR",connectedness = "Time", VAR_config = list(ElasticNet = list(nfolds = 10, alpha =0.1, loss = "mae", n_alpha = 10)))
dca = ConnectednessApproach(data5,model="VAR",connectedness = "Time")
# arguments possible:
#model = c("VAR", "QVAR", "LASSO", "Ridge", "Elastic", "TVP-VAR", "DCC-GARCH")



# Graph Network of spill-over effect

PlotNetwork(dca, method="NPDC", threshold=0.25) # NPDC means "Dynamic net pairwise connectedness"






##################################################################################################
# 
# # Using Frequency connectedness:
# partition = c(pi,pi/2,0)
# 
# dca1 = ConnectednessApproach(data5,model="VAR",connectedness = "Frequency", 
#                             VAR_config = list(ElasticNet = list(nfolds = 10, alpha =0.1, loss = "mae", n_alpha = 10)),
#                             #Connectedness_config=list(FrequencyConnectedness=list(partition=partition))
#                             Connectedness_config=list(FrequencyConnectedness=list(partition=partition, generalized=TRUE, scenario="ABS"))
#                             )
# # arguments possible:
# # model = c("VAR", "QVAR", "LASSO", "Ridge", "Elastic", "TVP-VAR", "DCC-GARCH")
#
# # Graph Network of spill-over effect
#
#PlotNetwork(dca1, method="NPDC", threshold=0.25) # NPDC means "Dynamic net pairwise connectedness"

