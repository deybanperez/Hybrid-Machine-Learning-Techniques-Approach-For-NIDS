rm(list = ls())

#Loading packages
library("e1071")

#Loading functions
source("source/functions/functions.R")

#Loading datasets
dataset.training = read.csv("dataset/NSLKDD_Training_New.csv",
                            sep = ",", header = TRUE)

#removing unecesary labels in training set
dataset.training$Label_Normal_TypeAttack = NULL
dataset.training$Label_Num_Classifiers = NULL
dataset.training$Label_Normal_or_Attack = NULL

#Assigning classes to the data
for (i in 1 : (ncol(dataset.training) -1) )
  dataset.training[,i] = as.numeric(dataset.training[,i])

dataset.training[,ncol(dataset.training)] = as.factor(dataset.training[,ncol(dataset.training)])

#Scaling dataset
dataset.training = ScaleSet(dataset.training)

#Selecting GFR features
svm.gfr = readRDS("source/feature_selection/SVM/results_GFR.rds")
svm.gfr = rownames(svm.gfr)[1:19]

#Extracting info
Label = dataset.training$Label

#Creating new DF
dataset.training = dataset.training[, svm.gfr]
dataset.training = cbind(dataset.training, Label = Label)

#Loading tuned parameters
tuned.parameters = readRDS("source/parameter_selection/SVM/GFR/tuned_model_19_features.rds")
tuned.cost = tuned.parameters$best.parameters$cost
tuned.gamma = tuned.parameters$best.parameters$gamma
tuned.cost
tuned.gamma

#Taking time start
start.time = Sys.time()

set.seed(22)
model = svm(Label~.,
            data = dataset.training,
            kernel = "radial",
            cost = tuned.cost,
            gamma = tuned.gamma,
            scale = FALSE,
            probability = TRUE)

#Calculating time of training
total.time = Sys.time() - start.time

#Storing information
list.results = list(total.time, model)

saveRDS(list.results, file = "source/tuned_model/GFR/SVM/testing_set/list_results_19_features.rds")