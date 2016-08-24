#Setting work directory
rm(list = ls())
setwd("/home/dperez/Documents/Repos/Tesis/source")
#setwd("/home/dperez/Tesis/source")

#Loading packages
require("e1071")
library("nnet")

#Loading functions
source("functions/functions.R")

#Loading best things
best.model = readRDS("normal_model/NN/nn_best_model.rds")
best.testingset = readRDS("normal_model/NN/nn_best_testing_set.rds")
best.predictions = readRDS("normal_model/NN/nn_best_predictions.rds")
best.accuracy = readRDS("normal_model/NN/nn_best_accuracy.rds")

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