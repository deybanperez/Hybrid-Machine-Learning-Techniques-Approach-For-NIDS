rm(list = ls())
setwd("/home/dperez/Documents/Repos/Tesis/source")
source("functions/functions.R")
require("e1071")
results = readRDS("feature_selection/SVM/results_PCA.rds")

#############################################################
#Calculating mean an standard deviation of results
sd.results = vector(mode = "numeric", length = nrow(results))
mean.results = vector(mode = "numeric", length = nrow(results))

#Standard deviation
for(i in 1:length(sd.results))
  sd.results[i] = sd(results[i,])

#Mean
for(i in 1:length(mean.results))
  mean.results[i] = mean(results[i,])

#Splitting the screen for plot into two sections
par(mfrow = c(1,2))

#Plotting the SD vs #Comps
plot(sd.results, col = "blue", type = "b",
     main = "Standard Deviation per Component",
     xlab = "Number of Components", ylab = "Standard Deviation")

#Plotting the Mean vs #Comps
plot(mean.results, col = "blue", type = "b",
     main = "Mean Accuracy per Component",
     xlab = "Number of Components", ylab = "Mean Accuracy")
