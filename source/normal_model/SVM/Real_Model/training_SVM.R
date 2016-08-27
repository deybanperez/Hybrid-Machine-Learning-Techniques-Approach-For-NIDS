rm(list = ls())
#setwd("/home/dperez/Documents/Repos/Tesis/source") #virtual Machine
#setwd("/home/dperez/Tesis/source") #Server
#setwd("C:/Users/deyban.perez/Documents/Repos/source") #Windows
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
  dataset.training[,i] = as.numeric(dataset.training[,i])

dataset.training[,ncol(dataset.training)] = as.factor(dataset.training[,ncol(dataset.training)])

#Scaling dataset
dataset.training = ScaleSet(dataset.training)

#Taking time start
start.time = Sys.time()

set.seed(22)
model = svm(Label~.,
                data = dataset.training,
                kernel = "radial",
                scale = FALSE,
                probability = TRUE)

#Calculating time of training
total.time = Sys.time() - start.time

#Storing information
list.results = list(total.time, model)

saveRDS(list.results, file = "normal_model/SVM/Real_Model/list_results.rds")