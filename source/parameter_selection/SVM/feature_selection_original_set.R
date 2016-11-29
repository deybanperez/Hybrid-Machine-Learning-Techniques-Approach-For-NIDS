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

#initializing time
time = Sys.time()

tuned.model = tune.svm(Label ~.,
                       data = dataset,
                       kernel = "radial",
                       scale = FALSE,
                       gamma = c(0.001, 0.025),
                       tunecontrol = tune.control(cross = 10))

#Stopping time
time = Sys.time() - time
