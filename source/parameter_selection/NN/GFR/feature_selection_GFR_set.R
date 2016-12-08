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

#Loading features
nn.gfr = readRDS("source/feature_selection/NN/results_GFR.rds")
nn.gfr = rownames(nn.gfr)[1:9]

#Extracting information
Label = dataset$Label_Normal_ClassAttack
dataset = dataset[, nn.gfr]
names = colnames(dataset)

#Transforming predictors into numeric
dataset = as.data.frame(apply(dataset, 2, as.numeric))
dataset[,ncol(dataset)+1] = Label
colnames(dataset) = names

#removing parcial variables
remove(list = c("names", "Label"))

#Scaling set
dataset = ScaleSet(dataset)

set.seed(22)
tuned.model = tune.nnet(Label ~.,
                        data = dataset,
                        size = 17:30,
                        maxit = 100)

saveRDS(tuned.model, "source/parameter_selection/NN/GFR/tuned_model.rds")