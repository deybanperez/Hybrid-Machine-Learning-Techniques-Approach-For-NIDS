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
best.accuracy = 0

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
                            scale = FALSE,
                            probability = TRUE)
  
  #Making predictions
  svm.radial.defaults.predictions = predict(svm.radial.defaults,
                                            testingset[, 1:(ncol(testingset)-1)], type = "class")
  #Calculating accuracy
  svm.radial.defaults.accuracy = mean(testingset[, ncol(testingset)] == svm.radial.defaults.predictions)
  #Storing result
  results.svm[k] = svm.radial.defaults.accuracy
  
  if(best.accuracy < svm.radial.defaults.accuracy)
  {
    best.model = svm.radial.defaults
    best.testingset = testingset
    best.predictions = svm.radial.defaults.predictions
    best.accuracy = svm.radial.defaults.accuracy
  }
}
#Showing all results
results.svm
#Calculating the mean of the results
mean(results.svm)

#Saving models
saveRDS(best.model, file = "normal_model/SVM/svm_best_model.rds")
saveRDS(best.testingset, file = "normal_model/SVM/svm_best_testing_set.rds")
saveRDS(best.predictions, file = "normal_model/SVM/svm_best_predictions.rds")
saveRDS(best.accuracy, file = "normal_model/SVM/svm_best_accuracy.rds")