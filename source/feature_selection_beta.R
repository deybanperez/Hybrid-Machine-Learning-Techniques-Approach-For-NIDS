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

#Transforming features into numeric class
for (i in 1:41)
  dataset.training[,i] = as.numeric(dataset.training[,i])

#Tansforming features into factor class
for (i in 42:45)
  dataset.training[,i] = as.factor(dataset.training[,i])

#Transforming remaining labes into numeric class
dataset.training$Label_Num_Classifiers = as.numeric(dataset.training$Label_Num_Classifiers)

#Removing unnecesary labels
dataset.training.model = dataset.training
dataset.training.model$Label_Normal_TypeAttack = NULL
dataset.training.model$Label_Num_Classifiers = NULL
dataset.training.model$Label_Normal_or_Attack = NULL
################################################################################
#Beggining PCA
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

dataset.training.model.pca = as.data.frame(pca$x)
dataset.training.model.pca = data.frame(dataset.training.model.pca,
                                        Label_Normal_ClassAttack = dataset.training.model$Label_Normal_ClassAttack)

################################################################################
#Stratified sampling
#Start calculating the number of each class of label
vector.ocurrences = vector(mode = "numeric", length = 5)
#Ordered in alphabetical mode
vector.ocurrences[1] = sum(dataset.training.model.pca$Label_Normal_ClassAttack == "normal")
vector.ocurrences[2] = sum(dataset.training.model.pca$Label_Normal_ClassAttack == "DoS")
vector.ocurrences[3] = sum(dataset.training.model.pca$Label_Normal_ClassAttack == "Probing")
vector.ocurrences[4] = sum(dataset.training.model.pca$Label_Normal_ClassAttack == "R2L")
vector.ocurrences[5] = sum(dataset.training.model.pca$Label_Normal_ClassAttack == "U2R")

#Now,create a vector of probabilities
vector.probabilities = vector(mode = "numeric", length = 5)

for (i in 1:length(vector.probabilities))
  vector.probabilities[i] = 1 - (vector.ocurrences[i]/nrow(dataset.training.model.pca))

#Assigning probability to each position in dataset.training.model
vector.probabilities.data = vector(mode = "numeric", length = nrow(dataset.training.model.pca))

vector.probabilities.data[dataset.training.model.pca$Label_Normal_ClassAttack == "normal"] = vector.probabilities[1]
vector.probabilities.data[dataset.training.model.pca$Label_Normal_ClassAttack == "DoS"] = vector.probabilities[2]
vector.probabilities.data[dataset.training.model.pca$Label_Normal_ClassAttack == "Probing"] = vector.probabilities[3]
vector.probabilities.data[dataset.training.model.pca$Label_Normal_ClassAttack == "R2L"] = vector.probabilities[4]
vector.probabilities.data[dataset.training.model.pca$Label_Normal_ClassAttack == "U2R"] = vector.probabilities[5]

#Making stratified sampling for training
set.seed(22)
sub.index = sample(nrow(dataset.training.model.pca), floor(nrow(dataset.training.model.pca) * 0.1),
                   prob = vector.probabilities.data, replace = FALSE)
dataset.training.model.pca = dataset.training.model.pca[sub.index,]
summary(dataset.training.model.pca$Label_Normal_ClassAttack)
dim(dataset.training.model.pca)

#Splitting set in 10 subsets
set.seed(22)
cv.data = dataset.training.model.pca

for (i in 1:10)
{
  index.cv = sample(nrow(cv.data), nrow(dataset.training.model.pca) %/% 10, replace = FALSE)
  
  if(i == 1)
    cv.sets = list(cv.data[index.cv,])
  else
    cv.sets[[length(cv.sets)+1]] = cv.data[index.cv,]
  
  cv.data = cv.data[-index.cv,]
}

#PCA with 10-Fold Cross Validation
results = vector(mode = "numeric", length = 40)
for (i in 1:40)
{
  results.cv = vector(mode = "numeric", length = 10)
  
  for (j in 1:10)
  {
    data.cv.testing = cv.sets[[j]]
    data.cv.training = cv.sets
    data.cv.training[[j]] = NULL
    data.cv.testing = as.data.frame(data.cv.testing)
    data.cv.training = do.call(rbind, data.cv.training)
    
    data.training.pca = as.data.frame(scale(data.cv.training[,1:i]))
    data.training.pca = data.frame(data.training.pca,
                                   Target = data.cv.training$Label_Normal_ClassAttack)
    
    logic = vector(mode = "logical", length = ncol(data.cv.training))
    logic = !logic
    logic[ncol(data.training.pca)] = FALSE
    
    model_polynomial = svm(Target ~ .,
                           data = data.training.pca,
                           kernel = "radial",
                           scale = FALSE)
    
    dataset.testing.pca.predict = as.data.frame(scale(data.cv.testing[,1:i]))
    dataset.testing.pca.predict = data.frame(dataset.testing.pca.predict,
                                             Target = data.cv.testing$Label_Normal_ClassAttack)
    
    prediction = predict(model_polynomial, dataset.testing.pca.predict[,1:i], type = "class")
    results.cv[j] = mean(prediction == dataset.testing.pca.predict[,ncol(dataset.testing.pca.predict)])
  }
  
  results[i] = mean(results.cv)
}