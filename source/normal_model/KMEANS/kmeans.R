#Setting work directory
rm(list = ls())
setwd("/home/dperez/Documents/Repos/Tesis/source") #Virtual Machine
#setwd("/home/dperez/Tesis/source") #Server
#setwd("C:/Users/deyban.perez/Documents/Repos/source") #Windows


#Loading functions
source("functions/functions.R")

#Loading dataset
dataset.training = read.csv("../dataset/NSLKDD_Training_New.csv",
                            sep = ",", header = TRUE)

#Removing unnecesary labels
dataset = dataset.training
dataset$Label_Normal_TypeAttack = NULL
dataset$Label_Num_Classifiers = NULL

#Assigning classes to the data
for (i in 1 : (ncol(dataset) -2) )
  dataset[,i] = as.numeric(dataset[,i])

for (i in (ncol(dataset) -1):ncol(dataset) )
  dataset[,i] = as.factor(dataset[,i])

#Splitting set
dataset.two = dataset[-(ncol(dataset)-1)]
dataset.two[, ncol(dataset.two)] = as.character(dataset.two[, ncol(dataset.two)])
dataset.two[dataset.two[,ncol(dataset.two)] == "attack", ncol(dataset.two)] = "Attack"
dataset.five = dataset[-ncol(dataset)]

#scaling sets
dataset$Label_Normal_or_Attack = NULL
dataset = ScaleSet(dataset)
dataset.two = ScaleSet(dataset.two)
dataset.five = ScaleSet(dataset.five)

#Codo de Jambu
IIC.Hartigan = vector(mode = "numeric", length = 30)
IIC.Lloyd = vector(mode = "numeric", length = 30)
IIC.Forgy = vector(mode = "numeric", length = 30)
IIC.MacQueen = vector(mode = "numeric", length = 30)
for (k in 1:30)
{
  groups = kmeans(dataset[,ncol(dataset)-2], k, iter.max = 100, algorithm = "Hartigan-Wong")
  IIC.Hartigan[k] = groups$tot.withinss
  groups = kmeans(dataset[,ncol(dataset)-2], k, iter.max = 100, algorithm = "Lloyd")
  IIC.Lloyd[k] = groups$tot.withinss
  groups = kmeans(dataset[,ncol(dataset)-2], k, iter.max = 100, algorithm = "Forgy")
  IIC.Forgy[k] = groups$tot.withinss
  groups = kmeans(dataset[,ncol(dataset)-2], k, iter.max = 100, algorithm = "MacQueen")
  IIC.MacQueen[k] = groups$tot.withinss
}
plot(IIC.Hartigan, col = "blue", type = "b", pch = 19, main = "Jambu Elbow",
     xlab = "Variance", ylab = "Centers")
points(IIC.Lloyd, col = "red", type = "b", pch = 19)
points(IIC.Forgy, col = "green", type = "b", pch = 19)
points(IIC.MacQueen, col = "magenta", type = "b", pch= 19)
legend("topright", legend = c("Hartigan", "Lloyd", "Forgy", "MacQueen"),
       col = c("blue","red", "green", "magenta"), pch = 19)

#Testing the models
#Five class model
results.five = vector(mode = "numeric", length = 10)
best.accuracy.five = 0
for (i in 1:length(results.five))
{
  set.seed(i)
  model.kmeans.five = kmeans(dataset.five[,1:(ncol(dataset.five)-1)],
                             5, iter.max = 100)
  
  prediction.five = OrderKmeans(model.kmeans.five)
  accuracy.five = mean(prediction.five == dataset.five$Label)
  
  results.five[i] = accuracy.five
  
  if(best.accuracy.five < accuracy.five)
  {
    best.prediction.five = prediction.five
    best.accuracy.five = accuracy.five
  }
}
#Printing results
results.five
#Calculating mean of results
mean(results.five) * 100
#Creating confusion matrix
confusion.matrix.five = table(Real = dataset.five$Label,
                              Prediction = best.prediction.five)
#Printing confusion matrix
confusion.matrix.five
#Printig accuracy rate and error rate
best.accuracy.five*100
ErrorRate(best.accuracy.five)*100
#Showing accuracy per label
AccuracyPerLabel(confusion.matrix.five, dataset.five)
#Confusion matrix Attack vs Normal
attack.normal.confusion.matrix.five = AttackNormalConfusionMatrix(dataset.five,
                                                             best.prediction.five)
attack.normal.confusion.matrix.five
#printing accuracy per label
AccuracyPerLabel(attack.normal.confusion.matrix.five, dataset.two)

#Binary measures
Sensitivity(attack.normal.confusion.matrix.five) * 100
Especificity(attack.normal.confusion.matrix.five) * 100
Precision(attack.normal.confusion.matrix.five) * 100
#################################################################################
#Two class model
results.two = vector(mode = "numeric", length = 10)
best.accuracy.two = 0

for (i in 1:length(results.two))
{
  set.seed(i)
  model.kmeans.two = kmeans(dataset.two[,1:(ncol(dataset.two)-1)],
                             2, iter.max = 100)
  
  prediction.two = OrderKmeans(model.kmeans.two)
  accuracy.two = mean(prediction.two == dataset.two$Label)
  
  results.two[i] = accuracy.two
  
  if(best.accuracy.two < accuracy.two)
  {
    best.prediction.two = prediction.two
    best.accuracy.two = accuracy.two
  }
}
#Printing results
results.two
#Calculating mean of results
mean(results.two) * 100
#Creating confusion matrix
confusion.matrix.two = table(Real = dataset.two$Label,
                              Prediction = best.prediction.two)
#Printing confusion matrix
confusion.matrix.two
#Printig accuracy rate and error rate
best.accuracy.two*100
ErrorRate(best.accuracy.two)*100
#Showing accuracy per label
AccuracyPerLabel(confusion.matrix.two, dataset.two)
#Binary measures
Sensitivity(confusion.matrix.two) * 100
Especificity(confusion.matrix.two) * 100
Precision(confusion.matrix.two) * 100