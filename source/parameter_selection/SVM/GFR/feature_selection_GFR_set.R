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

#Extracting information
Label = dataset$Label_Normal_ClassAttack
dataset = dataset[, c("Count", "Protocol_type", "Dst_host_srv_count", "Dst_host_same_src_port_rate",
                      "Dst_host_rerror_rate", "Dst_host_count", "Hot", "Dst_host_serror_rate",
                      "Dst_host_serror_rate")]
names = colnames(dataset)


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

tuned.model = tune(svm,
                   Label ~.,
                   data = dataset,
                   scale = F,
                   kernel = "radial",
                   ranges = list(cost = c(2, 3, 4),
                                 gamma = c(0.07, 0.08, 0.06))
                   )

#Stopping time
time = Sys.time() - time

saveRDS(tuned.model, "source/parameter_selection/SVM/GFR/tuned_model.rds")
saveRDS(time, "source/parameter_selection/SVM/GFR/time.rds")