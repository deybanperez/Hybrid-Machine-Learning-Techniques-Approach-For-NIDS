#Setting work directory
rm(list = ls())
setwd("/home/dperez/Documents/Repos/Tesis/source")

#Loading packages
require("e1071")
require("nnet")

#Loading functions
source("functions/functions.R")

#Loading dataset
dataset.training = read.csv("../dataset/NSLKDD_Training_New.csv",
                            sep = ",", header = TRUE)

#Removing unnecesary labels
dataset = dataset.training
dataset$Label_Normal_TypeAttack = NULL
dataset$Label_Num_Classifiers = NULL
dataset$Label_Normal_or_Attack = NULL

#Assigning classes to the data
for (i in 1 : (ncol(dataset) -1) )
  dataset[,i] = as.numeric(dataset[,i])

dataset[,ncol(dataset)] = as.factor(dataset[,ncol(dataset)])

#splitting the dataset into training and testing
#Making a strarified sampling
################################################
#Start calculating the number of each class
vector.ocurrences = SumLabels(dataset, ncol(dataset))

#Create probability vector
vector.probabilities = ProbVector(dataset, vector.ocurrences)

#Making stratified sample
indexes.training = IndexesTrainingSample(dataset, vector.probabilities, 0.8, 22)
trainingset = dataset[indexes.training, ]
testingset = dataset[-indexes.training, ]

################################################
#           Training the models                #
################################################
#Scaling testingset
testingset = ScaleSet(testingset)

#Scaling trainingset
trainingset = ScaleSet(trainingset)

#SVM Radial Model
svm.radial.defaults = svm(Label ~ .,
                          data = trainingset,
                          kernel = "radial",
                          scale = FALSE)

svm.radial.defaults.predictions = predict(svm.radial.defaults,
                                          testingset[, 1:(ncol(testingset)-1)], type = "class")

svm.radial.defaults.accuracy = mean(testingset[, ncol(testingset)] == svm.radial.defaults.predictions)
svm.radial.defaults.accuracy
save(svm.radial.defaults, file = "normal_model/svm_radial_defaults.rda")