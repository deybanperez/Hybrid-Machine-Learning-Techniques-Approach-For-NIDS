rm(list = ls())
source("source/functions/functions.R")
###############################################################
dataset = read.csv("dataset/NSLKDD_Training_New.csv")

#Removing unnecesary labels
dataset$Label_Normal_TypeAttack = NULL
dataset$Label_Num_Classifiers = NULL

#Extracting information
Labels = dataset[, (ncol(dataset)-1):ncol(dataset)]

#Transforming predictors into numeric
dataset = as.data.frame(apply(dataset[, c(-41, -42)], 2, as.numeric))
dataset = cbind(dataset, Label = Labels[,1])

#Scaling set
dataset = ScaleSet(dataset)

#Aplying PCA
pca = prcomp(dataset[, -41], scale. = TRUE)
dataset = cbind(as.data.frame(pca$x[,1:7]), Label = Labels[,1])
dataset.five = cbind(as.data.frame(pca$x[,1:7]), Label = Labels[,1])
dataset.two = cbind(as.data.frame(pca$x[,1:7]), Label = Labels[,2])

#removing parcial variables
remove(list = c("pca", "Labels"))

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
saveRDS(object = jambu.results, file = "source/tuned_model/PCA/KMEANS/jambu_results_7_features.rds")

#Selecting best distance measure
measure.two = lapply(MeasuareKMeans(dataset, 2), max)
measure.five = lapply(MeasuareKMeans(dataset, 5), max)
#Creating a list to store results
measures.results = list(measure.two = measure.two, measure.five = measure.five)
#Saving best measures results
saveRDS(object = measures.results, file = "source/tuned_model/PCA/KMEANS/measures_results_7_features.rds")