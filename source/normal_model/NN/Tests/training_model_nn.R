#Setting work directory
rm(list = ls())
setwd("/home/dperez/Documents/Repos/Tesis/source")
#setwd("/home/dperez/Tesis/source")

#Loading packages
require("e1071")
library("nnet")

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
#Making a stratified sampling
################################################
#Start calculating the number of each class
vector.ocurrences = SumLabels(dataset, ncol(dataset))

#Create probability vector
vector.probabilities = ProbVector(dataset, vector.ocurrences)
results.nn = vector(mode = "numeric", length = 10)
best.accuracy = 0
  
for (k in 1:length(results.nn))
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

  #NN Model
  nn.defaults = nnet(Label ~ .,
                     data = trainingset,
                     size = 20,
                     maxit = 100)
      
  #Making predictions
  nn.defaults.predictions = predict(nn.defaults,
                                            testingset[, 1:(ncol(testingset)-1)], type = "class")
  
  
  #Calculating accuracy
  nn.defaults.accuracy = mean(testingset[, ncol(testingset)] == nn.defaults.predictions)
  #Storing result
  results.nn[k] = nn.defaults.accuracy
  
  if(best.accuracy < nn.defaults.accuracy)
  {
    best.model = nn.defaults
    best.testingset = testingset
    best.predictions = nn.defaults.predictions
    best.accuracy = nn.defaults.accuracy
  }
}
#Saving models
saveRDS(results.nn, file = "normal_model/NN/Tests/nn_results.rds")
saveRDS(best.model, file = "normal_model/NN/Tests/nn_best_model.rds")
saveRDS(best.testingset, file = "normal_model/NN/Tests/nn_best_testing_set.rds")
saveRDS(best.predictions, file = "normal_model/NN/Tests/nn_best_predictions.rds")
saveRDS(best.accuracy, file = "normal_model/NN/Tests/nn_best_accuracy.rds")