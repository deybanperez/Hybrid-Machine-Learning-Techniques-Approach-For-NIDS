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

#Loading Testing set
testing.set = read.csv("../dataset/NSLKDD_Testing_New.csv",
                       sep = ",", header = TRUE)

#Removing unncessary features from testing set
testing.set$Label_Normal_TypeAttack = NULL
testing.set$Label_Num_Classifiers = NULL
testing.set$Label_Normal_or_Attack = NULL

#Scaling the set
testing.set = ScaleSet(testing.set)

#loading results from training
results = readRDS("normal_model/NN/Real_Model/list_results.rds")

#Extracting results
training.time = results[[1]]
model = results[[2]]

#Initializing the time
start.time.model = Sys.time()

#Making predictions
predictions = predict(model, testing.set[, 1:(ncol(testing.set)-1)], type = "class")

#Capturing the total time
total.time.model = Sys.time() - start.time.model
total.time.model

#Confusion Matrix
confusion.matrix = table(Real = testing.set[,ncol(testing.set)],
                         Prediction = predictions)

confusion.matrix

#Accuracy
accuracy = mean(testing.set[,ncol(testing.set)] == predictions)
accuracy * 100
ErrorRate(accuracy) * 100

#Printing Accuracy per label
AccuracyPerLabel(confusion.matrix, testing.set)

# COnfusion matrix Attack vs normal
attack.normal.confusion.matrix = AttackNormalConfusionMatrix(testing.set, predictions)
attack.normal.confusion.matrix

#Binary measures
Sensitivity(attack.normal.confusion.matrix) * 100
Especificity(attack.normal.confusion.matrix) * 100
Precision(attack.normal.confusion.matrix) * 100

#Calculating probabilities
probabilities = predict(model, testing.set[, 1:(ncol(testing.set)-1)])

#Generating ROC Curve
roc.data = DataROC(testing.set, probabilities, predictions)
generate_ROC(roc.data$Prob, roc.data$Label, roc.data$Prediction)

#Adding the second level with k-means
kmeans.set = testing.set[predictions == "normal", ]
dim(kmeans.set)
kmeans.set[,ncol(kmeans.set)] = as.character(kmeans.set[,ncol(kmeans.set)])
kmeans.set[kmeans.set[,ncol(kmeans.set)] != "normal",ncol(kmeans.set)] = "Attack"
SumLabels(kmeans.set, ncol(kmeans.set))

#Finding k-Means Centers

start.time.kmeans = Sys.time()
matrix.centers = FindCentersKmeans(set = kmeans.set, clusters = 2,
                                   iterations = 100, iter.max = 100)

#training the final model
matrix.centers = matrix.centers/100
kmeans.model = kmeans(kmeans.set[,1:(ncol(kmeans.set)-1)], centers = matrix.centers,
                      iter.max = 100)

total.time.kmeans = Sys.time() - start.time.kmeans

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

#Total statistics
confusion.matrix.two.labels = TwoLevelsCM(attack.normal.confusion.matrix, confusion.matrix.kmeans.model)
confusion.matrix.two.labels
accuracy.total = Accuracy(confusion.matrix.two.labels)
accuracy.total * 100
ErrorRate(accuracy.total) * 100
Sensitivity(confusion.matrix.two.labels) * 100
Especificity(confusion.matrix.two.labels) * 100
Precision(confusion.matrix.two.labels) * 100
total.time.model + total.time.kmeans
training.time + total.time.kmeans
