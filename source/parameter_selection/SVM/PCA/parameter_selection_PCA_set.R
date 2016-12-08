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
pca = prcomp(dataset[, -41], scale. = TRUE)
dataset = cbind(as.data.frame(pca$x[,1:7]), Label = dataset$Label)

set.seed(22)
tuned.model = tune(svm,
                   Label ~.,
                   data = dataset,
                   scale = F,
                   kernel = "radial",
                   ranges = list(cost = c(1, 2, 3, 4, 5, 6),
                                 gamma = c(0.06, 0.07, 0.08, 0.14, 0.2, 0.3, 0.4)
                                 )
                   )

saveRDS(tuned.model, "source/parameter_selection/SVM/PCA/tuned_model.rds")