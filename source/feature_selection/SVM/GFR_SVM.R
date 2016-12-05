#Setting work directory
rm(list = ls())
require("e1071")
source("source/functions/functions.R")
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

#Transforming remaining labels into numeric class
dataset[,ncol(dataset)] = as.factor(dataset[,ncol(dataset)])

#Scaling
dataset = ScaleSet(dataset)

#Applying GFR method
results = GFR(dataset, "SVM")

#Saving the model
saveRDS(results, "source/feature_selection/SVM/results_GFR.rds")