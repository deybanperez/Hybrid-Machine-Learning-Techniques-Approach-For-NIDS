#Preparing environment
rm(list = ls())
setwd("/home/dperez/Documents/Repos/Tesis/source")
source("functions/functions.R")
require("e1071")

#Loading the results
results  = readRDS("feature_selection/SVM/results_PCA.rds")
###########################################################
#Creating new variables
sd.results = apply(results, 1, sd)
mean.results = apply(results, 1, mean)

##########################################################
#Splitting the screen in 2
par(mfrow = c(1,2))

#Plotting standard deviation vs number of components
plot(sd.results, col = "blue", type = "b",
     main = "Standard Deviation vs Number of Components",
     xlab = "Number of Components", ylab = "Standard Deviation")

#Plotting mean accuracy vs number of components
plot(mean.results, col = "blue", type = "b",
     main = "Mean Accuracy vs Number of Components",
     xlab = "Number of Components", ylab = "Mean Accuracy")
