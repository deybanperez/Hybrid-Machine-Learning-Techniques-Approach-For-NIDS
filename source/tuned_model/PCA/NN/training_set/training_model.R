#Setting work directory
rm(list = ls())

#Loading packages
library("nnet")

#Loading functions
source("source/functions/functions.R")

#Loading dataset
dataset.training = read.csv("dataset/NSLKDD_Training_New.csv",
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

#Scaling set
dataset = ScaleSet(dataset)

#Aplying PCA
pca = prcomp(dataset[, -41], scale. = TRUE)
dataset = cbind(as.data.frame(pca$x[,1:7]), Label = dataset$Label)

#Starting 10-fold cross validation
cv.sets = CVSet(dataset, k = 10, seed = 22)
length(cv.sets)

#Initializing some variables
results = vector(mode = "numeric", length = 10)
list.results = list(0, 0, 0, 0)
names(list.results) = c("results", "best_model", "best_testing_set", "best_predictions")
best.accuracy = 0

#Loading best number of hidden neurons
hidden.neurons = readRDS("source/parameter_selection/NN/PCA/tuned_model.rds")
hidden.neurons = hidden.neurons$best.parameters$size
hidden.neurons
for (i in 1:10)
{
  #Extracting sets
  testingset = as.data.frame(cv.sets[[i]])
  trainingset = cv.sets
  trainingset[[i]] = NULL
  trainingset = do.call(rbind, trainingset)
  
  #NN Model
  model = nnet(Label ~ .,
               data = trainingset,
               size = hidden.neurons,
               maxit = 100)
  
  #Making predictions
  predictions = predict(model, testingset[, 1:(ncol(testingset)-1)], type = "class")
  
  
  #Calculating accuracy
  accuracy = mean(testingset[, ncol(testingset)] == predictions)
  #Storing results
  results[i] = accuracy
  
  #Storing best results
  if(best.accuracy < accuracy)
  {
    list.results$best_model = model
    list.results$best_testing_set = testingset
    list.results$best_predictions = predictions
    best.accuracy = accuracy
  }
}

#Storing results
list.results$results = results

#Saving list of objects
saveRDS(list.results, "source/tuned_model/PCA/NN/training_set/list_results.rds")