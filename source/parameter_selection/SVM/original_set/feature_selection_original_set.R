rm(list = ls())
source("source/functions/functions.R")
require(e1071)
###############################################################
dataset = read.csv("dataset/NSLKDD_Training_New.csv")

#Removing unnecessary labels
dataset$Label_Normal_TypeAttack = NULL
dataset$Label_Num_Classifiers = NULL
dataset$Label_Normal_or_Attack = NULL

#Extracting info
names = colnames(dataset)
Label = dataset$Label_Normal_ClassAttack

#Transforming predictors into numeric
dataset = as.data.frame(apply(dataset[,-ncol(dataset)], 2, as.numeric))
dataset[,ncol(dataset)+1] = Label
colnames(dataset) = names

#removing parcial variables
remove(list = c("names", "Label"))

#Scaling set
dataset = ScaleSet(dataset)

set.seed(22)
tuned.model = tune(svm,
                   Label ~.,
                   data = dataset,
                   scale = F,
                   kernel = "radial",
                   ranges = list(cost = c(1, 2, 3, 4, 5, 6),
                                 gamma = c(0.01, 0.025, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08)
                                 )
                   )

saveRDS(tuned.model, "source/parameter_selection/SVM/original_set/tuned_model.rds")