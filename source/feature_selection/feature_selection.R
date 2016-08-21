#Setting work directory
rm(list = ls())
setwd("/home/dperez/Documents/Repos/Tesis/source")

#Loading functions
source("functions.R")

#Installing packages
#install.packages("e1071")
#install.packages("factoextra")
#Loading packages
require("e1071")
require("factoextra")

#Loading datasets
dataset.training = read.csv("../dataset/NSLKDD_Training_New.csv",
                            sep = ",", header = TRUE)
dataset.testing = read.csv("../dataset/NSLKDD_Testing_New.csv",
                            sep = ",", header = TRUE)

#Transforming features into numeric class
for (i in 1:41)
  dataset.training[,i] = as.numeric(dataset.training[,i])

for (i in 1:41)
  dataset.testing[,i] = as.numeric(dataset.testing[,i])

#Tansforming features into factor class
for (i in 42:45)
  dataset.training[,i] = as.factor(dataset.training[,i])
for (i in 42:45)
  dataset.testing[,i] = as.factor(dataset.testing[,i])

#Transforming remaining labes into numeric class
dataset.training$Label_Num_Classifiers = as.numeric(dataset.training$Label_Num_Classifiers)
dataset.testing$Label_Num_Classifiers = as.numeric(dataset.testing$Label_Num_Classifiers)

#Removing unnecesary labels
dataset.training.model = dataset.training
dataset.training.model$Label_Normal_TypeAttack = NULL
dataset.training.model$Label_Num_Classifiers = NULL
dataset.training.model$Label_Normal_or_Attack = NULL
dataset.testing.model = dataset.testing
dataset.testing.model$Label_Normal_TypeAttack = NULL
dataset.testing.model$Label_Num_Classifiers = NULL
dataset.testing.model$Label_Normal_or_Attack = NULL

################################################################################
pca = prcomp(dataset.training.model[,-41], scale. = TRUE)
summary(pca)
std.deviation = pca$sdev
PC.variance = std.deviation^2
PR.variance = PC.variance/sum(PC.variance)
cum.variance = cumsum(PR.variance) * 100
summary.pca = data.frame(std_deviation = std.deviation,
                         PC_variance = PC.variance,
                         PR_variance = PR.variance,
                         cum_variance = cum.variance)
summary.pca
plot(summary.pca$cum_variance,
     ylab = "Cumulative Proportion",
     xlab = "Number of Principal Components",
     type = "b")

fviz_pca_var(pca, col.var= "contrib")

data.training.pca = data.frame(pca$x[,1:24],
                               Target = dataset.training.model$Label_Normal_ClassAttack)

#We need to make a stratifies sample
#Start calculating the number of each class of label
vector.ocurrences = vector(mode = "numeric", length = 5)
#Ordered in alphabetical mode
vector.ocurrences[1] = sum(data.training.pca$Target == "normal")
vector.ocurrences[2] = sum(data.training.pca$Target == "DoS")
vector.ocurrences[3] = sum(data.training.pca$Target == "Probing")
vector.ocurrences[4] = sum(data.training.pca$Target == "R2L")
vector.ocurrences[5] = sum(data.training.pca$Target == "U2R")

#Now,create a vector of probabilities
vector.probabilities = vector(mode = "numeric", length = 5)

for (i in 1:length(vector.probabilities))
  vector.probabilities[i] = 1 - (vector.ocurrences[i]/nrow(data.training.pca))

#Assigning probability to each position in dataset.training.model
vector.probabilities.data = vector(mode = "numeric", length = nrow(data.training.pca))

vector.probabilities.data[data.training.pca$Target == "normal"] = vector.probabilities[1]
vector.probabilities.data[data.training.pca$Target == "DoS"] = vector.probabilities[2]
vector.probabilities.data[data.training.pca$Target == "Probing"] = vector.probabilities[3]
vector.probabilities.data[data.training.pca$Target == "R2L"] = vector.probabilities[4]
vector.probabilities.data[data.training.pca$Target == "U2R"] = vector.probabilities[5]

#Making startified sampling
set.seed(22)
sub.index = sample(nrow(data.training.pca), floor(nrow(data.training.pca) * 0.009),
                   prob = vector.probabilities.data, replace = FALSE)
training.data = data.training.pca[sub.index,]
summary(training.data$Target)
#
logic = vector(mode = "logical", length = ncol(training.data))
logic = !logic
logic[ncol(training.data)] = FALSE
logic
tc = tune.control(cross = 10)

parameters_polynomial = tune.svm(Target ~ .,
                                 data = training.data,
                                 kernel = "polynomial",
                                 scale = logic,
                                 gamma = 10^(-6:-1),
                                 cost = 10^(-2:-1),
                                 degree = 3,
                                 coef0 = (-1:1),
                                 tunecontrol = tc)
parameters_polynomial

model_polynomial = svm(Target ~ .,
                       data = training.data,
                       kernel = "polynomial",
                       scale = logic,
                       cost = parameters_polynomial$best.model$cost,
                       degree = parameters_polynomial$best.model$degree,
                       gamma = parameters_polynomial$best.model$gamma,
                       coef0 = parameters_polynomial$best.model$coef0)

dataset.testing.model$Label_Normal_ClassAttack = as.character(dataset.testing.model$Label_Normal_ClassAttack)
dataset.testing.model$Label_Normal_ClassAttack[dataset.testing.model$Label_Normal_ClassAttack == "normal"] = 1
dataset.testing.model$Label_Normal_ClassAttack[dataset.testing.model$Label_Normal_ClassAttack == "DoS"] = 2
dataset.testing.model$Label_Normal_ClassAttack[dataset.testing.model$Label_Normal_ClassAttack == "Probing"] = 3
dataset.testing.model$Label_Normal_ClassAttack[dataset.testing.model$Label_Normal_ClassAttack == "R2L"] = 4
dataset.testing.model$Label_Normal_ClassAttack[dataset.testing.model$Label_Normal_ClassAttack == "U2R"] = 5
dataset.testing.model$Label_Normal_ClassAttack = as.factor(dataset.testing.model$Label_Normal_ClassAttack)
unique(dataset.testing.model$Label_Normal_ClassAttack)
dim(dataset.testing.model)
pca.testing = prcomp(dataset.testing.model[,-41], scale. = TRUE)
pca.testing.predict = pca.testing$x[,1:24]
prediction = predict(model_polynomial, pca.testing.predict, type = "class")

mean(prediction == dataset.testing.model$Label_Normal_ClassAttack) * 100





#########################################################################################

logic.o = vector(mode = "logical", length = ncol(dataset.training.model))
logic.o = !logic.o
logic.o[ncol(dataset.training.model)] = FALSE
logic.o
model_polynomial = svm(Label_Normal_ClassAttack ~ .,
                       data = dataset.training.model,
                       kernel = "sigmoid",
                       scale = logic)


p = predict(model_polynomial, dataset.testing.model[,-41] , type = "class")


mean(dataset.testing.model$Label_Normal_ClassAttack == p) * 100
##################################################################################







parameters_radial = tune.svm(Target ~ .,
                             data = training.data,
                             kernel = "radial",
                             scale = logic,
                             gamma = 10^(-6:-1),
                             tunecontrol = tc)
parameters_radial



parameters_sigmoid = tune.svm(Target ~ .,
                              data = training.data,
                              kernel = "sigmoid",
                              scale = logic,
                              gamma = 10^(-6:-1),
                              coef0 = (-1:1),
                              tunecontrol = tc)
parameters_sigmoid


dataset.testing.model
























#We need to make a stratifies sample
#Start calculating the number of each class of label
vector.ocurrences = vector(mode = "numeric", length = 5)
#Ordered in alphabetical mode
vector.ocurrences[1] = sum(dataset.training.model$Label_Normal_ClassAttack == "normal")
vector.ocurrences[2] = sum(dataset.training.model$Label_Normal_ClassAttack == "DoS")
vector.ocurrences[3] = sum(dataset.training.model$Label_Normal_ClassAttack == "Probing")
vector.ocurrences[4] = sum(dataset.training.model$Label_Normal_ClassAttack == "R2L")
vector.ocurrences[5] = sum(dataset.training.model$Label_Normal_ClassAttack == "U2R")

#Now,create a vector of probabilities
vector.probabilities = vector(mode = "numeric", length = 5)

for (i in 1:length(vector.probabilities))
  vector.probabilities[i] = 1 - (vector.ocurrences[i]/nrow(dataset.training.model))

#Assigning probability to each position in dataset.training.model
vector.probabilities.data = vector(mode = "numeric", length = nrow(dataset.training.model))

vector.probabilities.data[dataset.training.model$Label_Normal_ClassAttack == "normal"] = vector.probabilities[1]
vector.probabilities.data[dataset.training.model$Label_Normal_ClassAttack == "DoS"] = vector.probabilities[2]
vector.probabilities.data[dataset.training.model$Label_Normal_ClassAttack == "Probing"] = vector.probabilities[3]
vector.probabilities.data[dataset.training.model$Label_Normal_ClassAttack == "R2L"] = vector.probabilities[4]
vector.probabilities.data[dataset.training.model$Label_Normal_ClassAttack == "U2R"] = vector.probabilities[5]

#Making startified sampling
set.seed(22)
sub.index = sample(nrow(dataset.training.model), floor(nrow(dataset.training.model) * 0.009),
                   prob = vector.probabilities.data, replace = FALSE)
training.data = dataset.training.model[sub.index,]
summary(training.data$Label_Normal_ClassAttack)
#Tranforming target labels

dataset.training.model$Label_Normal_ClassAttack = as.numeric(dataset.training.model$Label_Normal_ClassAttack)
dataset.training.model$Label_Normal_ClassAttack[dataset.training.model$Label_Normal_ClassAttack == "normal"] = 1
dataset.training.model$Label_Normal_ClassAttack[dataset.training.model$Label_Normal_ClassAttack == "DoS"] = 2
dataset.training.model$Label_Normal_ClassAttack[dataset.training.model$Label_Normal_ClassAttack == "Probing"] = 3
dataset.training.model$Label_Normal_ClassAttack[dataset.training.model$Label_Normal_ClassAttack == "R2L"] = 4
dataset.training.model$Label_Normal_ClassAttack[dataset.training.model$Label_Normal_ClassAttack == "U2R"] = 5
dataset.training.model$Label_Normal_ClassAttack = as.factor(dataset.training.model$Label_Normal_ClassAttack)

#
logic = vector(mode = "logical", length = ncol(dataset.training.model))
logic = !logic
logic[ncol(dataset.training.model)] = FALSE
tc = tune.control(cross = 10)
parameters_polynomial = tune.svm(Label_Normal_ClassAttack ~ .,
                                 data = training.data,
                                 kernel = "polynomial",
                                 scale = logic,
                                 gamma = 10^(-6:-1),
                                 cost = 10^(-2:-1),
                                 degree = 3,
                                 coef0 = (-1:1),
                                 tunecontrol = tc)


parameters_radial = tune.svm(Label_Normal_ClassAttack ~ .,
                                 data = training.data,
                                 kernel = "radial",
                                 scale = logic,
                                 gamma = 10^(-6:-1),
                                 tunecontrol = tc)

parameters_sigmoid = tune.svm(Label_Normal_ClassAttack ~ .,
                             data = training.data,
                             kernel = "sigmoid",
                             scale = logic,
                             gamma = 10^(-6:-1),
                             coef0 = (-1:1),
                             tunecontrol = tc)
parameters_sigmoid