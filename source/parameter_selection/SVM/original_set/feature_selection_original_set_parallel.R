rm(list = ls())
source("source/functions/functions.R")
require(e1071)
require(caret)
require(kernlab)
require(doMC)
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

registerDoMC(cores = 4)
set.seed(222)
grid = expand.grid(sigma = c(0.07, 0.025),
                   C = c(1,2))
ctrl = trainControl(method = "repeatedcv", repeats = 5)
#initializing time
time = Sys.time()

tuned.model = train(x = dataset[,-ncol(dataset)],
                    y = dataset$Label,
                    method = "svmRadial",
                    metric = "Accuracy",
                    tuneGrid = grid,
                    trControl = ctrl)

#Stopping time
time = Sys.time() - time
