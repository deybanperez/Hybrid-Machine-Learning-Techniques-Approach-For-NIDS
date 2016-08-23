#Setting work directory
rm(list = ls())
setwd("/home/dperez/Documents/Repos/Tesis/source")
#setwd("/home/dperez/Tesis/source")

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
results.svm = vector(mode = "numeric", length = 10)

for (k in 1:length(results.svm))
{

  #Making stratified sample
  indexes.training = IndexesTrainingSample(dataset, vector.probabilities, 0.8, k)
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
  
  #Making predictions
  svm.radial.defaults.predictions = predict(svm.radial.defaults,
                                            testingset[, 1:(ncol(testingset)-1)], type = "class")
  #Calculating accuracy
  svm.radial.defaults.accuracy = mean(testingset[, ncol(testingset)] == svm.radial.defaults.predictions)
  #Storing result
  results.svm[k] = svm.radial.defaults.accuracy
}
#Showing all results
results.svm
#Calculating the mean of the results
mean(results.svm)
#Calculating the accuracy of the last model created
svm.radial.defaults.accuracy
#Calculating the confusion matrix with the last model created
confusion.matrix.svm = table(Real = testingset[,ncol(testingset)],
                             Prediction = svm.radial.defaults.predictions)
#Showing confusion matrix
confusion.matrix.svm
#Showing accuracy per label
AccuracyPerLabel(confusion.matrix.svm, testingset)
#Saving last model
save(svm.radial.defaults, file = "normal_model/svm_radial_defaults.rda")