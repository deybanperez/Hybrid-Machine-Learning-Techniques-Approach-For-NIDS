rm(list = ls())

#Loading packages
library("nnet")

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
nn.gfr = readRDS("source/feature_selection/NN/results_GFR.rds")
nn.gfr = rownames(nn.gfr)[1:9]

#Extracting info
Label = dataset.training$Label

#Creating new DF
dataset.training = dataset.training[, nn.gfr]
dataset.training = cbind(dataset.training, Label = Label)

#Loading best number of hidden neurons
hidden.neurons = readRDS("source/parameter_selection/NN/GFR/tuned_model.rds")
hidden.neurons = hidden.neurons$best.parameters$size
hidden.neurons

#Taking time start
start.time = Sys.time()

#Training model
set.seed(22)
model = nnet(Label ~ .,
             data = dataset.training,
             size = hidden.neurons,
             maxit = 100)

#Calculating time of training
total.time = Sys.time() - start.time

#Storing information
list.results = list(total.time, model)

saveRDS(list.results, file = "source/tuned_model/GFR/NN/testing_set/list_results.rds")