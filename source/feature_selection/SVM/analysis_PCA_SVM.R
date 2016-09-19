#Preparing environment
rm(list = ls())
setwd("/home/dperez/Documents/Repos/Tesis/source")
source("functions/functions.R")
require("e1071")

#Loading the results
results  = readRDS("feature_selection/SVM/results_PCA.rds")
###########################################################
#Creating new variables
sd.results = vector(mode = "numeric", length = nrow(results))
mean.results = vector(mode = "numeric", length = nrow(results))

#Calculating standard deviation
for (i in 1:length(sd.results))
  sd.results[i] = sd(results[i,])

#Calculating mean
for (i in 1:length(mean.results))
  mean.results[i] = mean(results[i,])

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