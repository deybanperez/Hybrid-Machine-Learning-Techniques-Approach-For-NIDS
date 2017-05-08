#Setting work directory
rm(list = ls())

#Loading packages
library("nnet")

#Loading functions
source("source/functions/functions.R")

#Loading best objects
list.results = readRDS("source/tuned_model/GFR/NN/training_set/list_results.rds")

#Showing all results
list.results$results * 100
#Calculating the mean of the results
mean(list.results$results) * 100
#Calculating the confusion matrix with the last model created
confusion.matrix = table(Real = list.results$best_testing_set[,ncol(list.results$best_testing_set)],
                         Prediction = list.results$best_predictions)
#Showing confusion matrix
confusion.matrix

#Calculating Accuraccy
accuracy = mean(list.results$best_testing_set[,ncol(list.results$best_testing_set)] == 
                  list.results$best_predictions)

accuracy * 100
ErrorRate(accuracy) * 100
#Showing accuracy per label
AccuracyPerLabel(confusion.matrix, list.results$best_testing_set)
#Confusion matrix Attack vs Normal
attack.normal.confusion.matrix = AttackNormalConfusionMatrix(list.results$best_testing_set,
                                                             list.results$best_predictions)
attack.normal.confusion.matrix
#Binary measures
Accuracy(attack.normal.confusion.matrix) * 100
Sensitivity(attack.normal.confusion.matrix) * 100
Especificity(attack.normal.confusion.matrix) * 100
Precision(attack.normal.confusion.matrix) * 100

#ROC Curve
probabilities = predict(list.results$best_model,
                        list.results$best_testing_set[, 1:(ncol(list.results$best_testing_set)-1)])

#Generating curve ROC
roc.data = DataROC(list.results$best_testing_set, probabilities,
                   list.results$best_predictions)
generate_ROC(scores = roc.data$Prob, real = roc.data$Label,
             pred = roc.data$Prediction, tittle = "(5) GFR - NN - K-Medias")

#Adding the second level with K-Means
kmeans.set = list.results$best_testing_set[list.results$best_predictions == "normal",]
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
predictions = OrderKmeans(kmeans.model)

#Creating confusion matrix
confusion.matrix.kmeans.model = table(Real = kmeans.set[,ncol(kmeans.set)],
                                      Prediction = predictions)
#Printing confusiopn matrix
confusion.matrix.kmeans.model
#Calculating accuracy
accuracy.kmeans.model = mean(predictions == kmeans.set[,ncol(kmeans.set)])
#Printing accuracy
accuracy.kmeans.model*100
#Printing error rate
ErrorRate(accuracy.kmeans.model)*100
#Printing accuracy per labbel
AccuracyPerLabel(confusion.matrix.kmeans.model, kmeans.set)
#Binary measures
Sensitivity(confusion.matrix.kmeans.model) * 100
Especificity(confusion.matrix.kmeans.model) * 100
Precision(confusion.matrix.kmeans.model) * 100

#Total statistics
confusion.matrix.two.labels = TwoLevelsCM(attack.normal.confusion.matrix,
                                          confusion.matrix.kmeans.model)

confusion.matrix.two.labels
accuracy.total = Accuracy(confusion.matrix.two.labels)
accuracy.total * 100
ErrorRate(accuracy.total) * 100
Sensitivity(confusion.matrix.two.labels) * 100
Especificity(confusion.matrix.two.labels) * 100
Precision(confusion.matrix.two.labels) * 100