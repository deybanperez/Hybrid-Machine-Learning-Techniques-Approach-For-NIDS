rm(list = ls())
source("source/functions/functions.R")
require(e1071)
require(nnet)
###############################################################
dataset = read.csv("dataset/NSLKDD_Training_New.csv")

#Removing unnecessary labels
dataset$Label_Normal_TypeAttack = NULL
dataset$Label_Num_Classifiers = NULL
dataset$Label_Normal_or_Attack = NULL

#Extracting inforomation
Label = dataset$Label_Normal_ClassAttack
names = colnames(dataset)

#Transforming predictors into numeric
dataset = as.data.frame(apply(dataset[,-ncol(dataset)], 2, as.numeric))
dataset[,ncol(dataset)+1] = Label
colnames(dataset) = names

#removing parcial variables
remove(list = c("names", "Label"))

#Scaling set
dataset = ScaleSet(dataset)

#Aplying PCA
pca = prcomp(dataset[, -41], scale. = FALSE)
dataset = cbind(as.data.frame(pca$x[,1:24]), Label = dataset$Label)

set.seed(22)
tuned.model = tune.nnet(Label ~.,
                        data = dataset,
                        size = 17:30,
                        maxit = 100)

saveRDS(tuned.model, "source/parameter_selection/NN/PCA/tuned_model_24_features.rds")