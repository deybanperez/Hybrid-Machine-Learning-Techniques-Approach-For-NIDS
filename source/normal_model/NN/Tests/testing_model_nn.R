#Setting work directory
rm(list = ls())
setwd("/home/dperez/Documents/Repos/Tesis/source")
#setwd("/home/dperez/Tesis/source")
#setwd("C:/Users/deyban.perez/Documents/Repos/source") #Windows

#Loading packages
library("e1071")
library("nnet")

#Loading functions
source("functions/functions.R")

#Loading best objects
results.nn = readRDS("normal_model/NN/Tests/nn_results.rds")
best.model = readRDS("normal_model/NN/Tests/nn_best_model.rds")
best.testingset = readRDS("normal_model/Tests/NN/nn_best_testing_set.rds")
best.predictions = readRDS("normal_model/Tests/NN/nn_best_predictions.rds")
best.accuracy = readRDS("normal_model/Tests/NN/nn_best_accuracy.rds")

#Showing all results
results.nn
#Calculating the mean of the results
mean(results.nn) * 100
#Calculating the confusion matrix with the last model created
confusion.matrix.nn = table(Real = best.testingset[,ncol(best.testingset)],
                            Prediction = best.predictions)
#Showing confusion matrix
confusion.matrix.nn
best.accuracy * 100
ErrorRate(best.accuracy) * 100
#Showing accuracy per label
AccuracyPerLabel(confusion.matrix.nn, best.testingset)
#Confusion matrix Attack vs Normal
attack.normal.confusion.matrix = AttackNormalConfusionMatrix(best.testingset,
                                                             best.predictions)
attack.normal.confusion.matrix
#Binary measures
Sensitivity(attack.normal.confusion.matrix) * 100
Especificity(attack.normal.confusion.matrix) * 100
Precision(attack.normal.confusion.matrix) * 100

#ROC Curve
probabilities = predict(best.model,
                        best.testingset[, 1:(ncol(best.testingset)-1)])

#Generating Curve ROC
prob.vector = ExtractProbabilities(probabilities)
prob.vector.ordered = order(prob.vector, decreasing = TRUE)
prob.vector = prob.vector[prob.vector.ordered]
labels.roc = as.character(best.testingset[,ncol(best.testingset)])
labels.roc[labels.roc != "normal"] = "Attack"
labels.roc = labels.roc[prob.vector.ordered]
generate_ROC(prob.vector, labels.roc, "Attack")

#Adding the second level with K-Means
kmeans.set = best.testingset[best.predictions == "normal",]
dim(kmeans.set)
kmeans.set[,ncol(kmeans.set)] = as.character(kmeans.set[,ncol(kmeans.set)])
kmeans.set[kmeans.set[,ncol(kmeans.set)] != "normal",ncol(kmeans.set)] = "Attack"
SumLabels(kmeans.set, ncol(kmeans.set))
#Finding best centers
matrix.centers = FindCentersKmeans(set = kmeans.set, clusters = 2,
                                  iterations = 100, iter.max = 100)

#Training the absolute model
matrix.centers = matrix.centers/100
kmeans.model = kmeans(kmeans.set[,1:(ncol(kmeans.set)-1)], centers = matrix.centers,
                      iter.max = 100)


#Ordering prediction
prediction = OrderKmeans(kmeans.model)

#Creating confusion matrix
confusion.matrix.kmeans.model = table(Real = kmeans.set[,ncol(kmeans.set)],
                                      Prediction = prediction)
#Printing confusiopn matrix
confusion.matrix.kmeans.model
#Calculating accuracy
accuracy.kmeans.model = mean(prediction == kmeans.set[,ncol(kmeans.set)])
#Printing accuracy
accuracy.kmeans.model*100
#Printing error rate
ErrorRate(accuracy.kmeans.model)
#Printing accuracy per labbel
AccuracyPerLabel(confusion.matrix.kmeans.model, kmeans.set)
#Binary measures
Sensitivity(confusion.matrix.kmeans.model) * 100
Especificity(confusion.matrix.kmeans.model) * 100
Precision(confusion.matrix.kmeans.model) * 100