rm(list = ls())
setwd("~/Documents/Repos/Tesis/")
svm.gfr = readRDS("source/feature_selection/SVM/results_GFR.rds")

# I need to extract mean, std and variance from the results and then plot

mean.values = apply(svm.gfr, 1, mean)
sdeviation.values = apply(svm.gfr, 1, sd)

par(mfrow = c(1,2))
plot(sdeviation.values[2:length(mean.values)],
     type = "b", col = "blue",
     main = "Standard Deviation vs Number of Components",
     xlab = "Number of Components", ylab = "Standard Deviation")
plot(mean.values[2:length(mean.values)],
     type = "b", col = "blue",
     main = "Standard Deviation vs Number of Components",
     xlab = "Number of Components", ylab = "Standard Deviation")
