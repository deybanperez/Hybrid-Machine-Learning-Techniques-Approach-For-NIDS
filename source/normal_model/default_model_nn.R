  #Setting work directory
  rm(list = ls())
  setwd("/home/dperez/Documents/Repos/Tesis/source")
  #setwd("/home/dperez/Tesis/source")
  
  #Loading packages
  require("e1071")
  library("nnet")
  library("neuralnet")
  
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
}
#Showing all results
results.nn
#Calculating the mean of the results
mean(results.nn)
#Calculating the accuracy of the last model created
nn.defaults.accuracy
#Calculating the confusion matrix with the last model created
confusion.matrix.nn = table(Real = testingset[,ncol(testingset)],
                             Prediction = nn.defaults.predictions)
#Showing confusion matrix
confusion.matrix.nn
#Showing accuracy per label
AccuracyPerLabel(confusion.matrix.nn, testingset)
#Saving last model
save(svm.radial.defaults, file = "normal_model/svm_radial_defaults.rda")