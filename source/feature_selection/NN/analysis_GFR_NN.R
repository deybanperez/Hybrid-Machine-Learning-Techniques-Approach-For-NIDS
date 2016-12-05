rm(list = ls())
source("source/functions/functions.R")
nn.gfr = readRDS("source/feature_selection/NN/results_GFR.rds")

# I need to extract mean, std and variance from the results and then plot
rownames(nn.gfr)[-nrow(nn.gfr)]
mean.values = apply(nn.gfr, 1, mean)
sdeviation.values = apply(nn.gfr, 1, sd)

par(mfrow = c(1,2))
plot(sdeviation.values[2:length(mean.values)],
     type = "b", col = "blue",
     main = "Standard Deviation vs Number of Components",
     xlab = "Number of Components", ylab = "Standard Deviation")
plot(mean.values[2:length(mean.values)],
     type = "b", col = "blue",
     main = "Standard Deviation vs Number of Components",
     xlab = "Number of Components", ylab = "Standard Deviation")

rownames(nn.gfr)[1:9]

#############################################################################
#############################################################################

dataset.training = read.csv("dataset/NSLKDD_Training_New.csv",
                            sep = ",", header = TRUE)

#Removing unnecessary labels
dataset = dataset.training
dataset$Label_Normal_TypeAttack = NULL
dataset$Label_Num_Classifiers = NULL
dataset$Label_Normal_or_Attack = NULL

#Transforming features into numeric class
for (i in 1:(ncol(dataset)-1))
  dataset[,i] = as.numeric(dataset[,i])

#Transforming remaining labes into numeric class
dataset[,ncol(dataset)] = as.factor(dataset[,ncol(dataset)])

#################################################################################
colors = as.character(dataset[,ncol(dataset)])
colors[colors == "normal"] = "black"
colors[colors == "DoS"] = "red"
colors[colors == "Probing"] = "green"
colors[colors == "R2L"] = "blue"
colors[colors == "U2R"] = "cyan"

par(mfrow = c(1,1))

plot(dataset[, rownames(nn.gfr)[1]], dataset[, rownames(nn.gfr)[2]],
     col = dataset$Label, pch = 19,
     xlab = "Count", ylab = "Protocol Type",
     main = "Características Principales GFR - NN")