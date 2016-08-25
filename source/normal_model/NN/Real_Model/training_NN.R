rm(list = ls())
#setwd("/home/dperez/Documents/Repos/Tesis/source")
#setwd("/home/dperez/Tesis/source")
setwd("C:/Users/deyban.perez/Documents/Repos/source") #Windows
#Loading packages
library("e1071")
library("nnet")

#Loading functions
source("functions/functions.R")

#Loading datasets
dataset.training = read.csv("../dataset/NSLKDD_Training_New.csv",
                            sep = ",", header = TRUE)

#removing unecesary labels in training set
dataset.training$Label_Normal_TypeAttack = NULL
dataset.training$Label_Num_Classifiers = NULL
dataset.training$Label_Normal_or_Attack = NULL

#Assigning classes to the data
for (i in 1 : (ncol(dataset.training) -1) )
{
  dataset.training[,i] = as.numeric(dataset.training[,i])
  dataset.testing[,i] = as.numeric(dataset.testing[,i])
}

dataset.training[,ncol(dataset.training)] = as.factor(dataset.training[,ncol(dataset.training)])

#Scaling dataset
dataset.training = ScaleSet(dataset.training)

#Training model
nn.model = nnet(Label ~ .,
                   data = dataset.training,
                   size = 20,
                   maxit = 100)

saveRDS(svm.model, file = "normal_model/NN/Real_Model/nn_model.rds")