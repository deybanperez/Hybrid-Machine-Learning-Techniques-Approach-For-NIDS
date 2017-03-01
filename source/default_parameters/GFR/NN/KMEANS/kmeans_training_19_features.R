#Getting ready enviroment
rm(list = ls())
require(e1071)
require(nnet)
source("source/functions/functions.R")

#Loading Dataset
dataset = read.csv("dataset/NSLKDD_Training_New.csv", sep = ",", header = TRUE)

#Removing unnecesary labels
dataset$Label_Normal_TypeAttack = NULL
dataset$Label_Num_Classifiers = NULL

#Loading features
nn.gfr = readRDS("source/feature_selection/NN/results_GFR.rds")
nn.gfr = rownames(nn.gfr)[1:19]

#Extracting information
Labels = dataset[, (ncol(dataset)-1):ncol(dataset)]
dataset = dataset[, nn.gfr]

#Transforming predictors into numeric
dataset = as.data.frame(apply(dataset, 2, as.numeric))
dataset.five = cbind(dataset, Label = Labels[,1])
dataset.two = cbind(dataset, Label = Labels[,2])
dataset = cbind(dataset, Label = Labels[,1])

#Removing parcial variables
remove(list = c("Labels"))

#Scaling sets
dataset = ScaleSet(dataset)
dataset.two = ScaleSet(dataset.two)
dataset.five = ScaleSet(dataset.five)

#Jambu's Elbow
IIC.Hartigan = vector(mode = "numeric", length = 30)
IIC.Lloyd = vector(mode = "numeric", length = 30)
IIC.Forgy = vector(mode = "numeric", length = 30)
IIC.MacQueen = vector(mode = "numeric", length = 30)

for (k in 1:30)
{
  set.seed(k)
  groups = kmeans(dataset[,-ncol(dataset)], k, iter.max = 100, algorithm = "Hartigan-Wong")
  IIC.Hartigan[k] = groups$tot.withinss
  set.seed(k)
  groups = kmeans(dataset[,-ncol(dataset)], k, iter.max = 100, algorithm = "Lloyd")
  IIC.Lloyd[k] = groups$tot.withinss
  set.seed(k)
  groups = kmeans(dataset[,-ncol(dataset)], k, iter.max = 100, algorithm = "Forgy")
  IIC.Forgy[k] = groups$tot.withinss
  set.seed(k)
  groups = kmeans(dataset[,-ncol(dataset)], k, iter.max = 100, algorithm = "MacQueen")
  IIC.MacQueen[k] = groups$tot.withinss
}

#Creating a list to store results
jambu.results = list(IIC.Hartigan = IIC.Hartigan, IIC.Lloyd = IIC.Lloyd,
                     IIC.Forgy = IIC.Forgy, IIC.MacQueen = IIC.MacQueen)
#Saving jambu's elbow results
saveRDS(object = jambu.results, file = "source/default_parameters/GFR/NN/KMEANS/jambu_results_19_features.rds")

#Selecting best distance measure
measure.two = lapply(MeasuareKMeans(dataset, 2), max)
measure.five = lapply(MeasuareKMeans(dataset, 5), max)
#Creating a list to store results
measures.results = list(measure.two = measure.two, measure.five = measure.five)
#Saving best measures results
saveRDS(object = measures.results, file = "source/default_parameters/GFR/NN/KMEANS/measures_results_19_features.rds")