#Setting work directory
rm(list = ls())
setwd("/home/dperez/Documents/Repos/Tesis/source")
#setwd("/home/dperez/Tesis/source")

#Loading functions
source("functions/functions.R")

#Installing packages
#install.packages("e1071")
#install.packages("factoextra")
#Loading packages
require("nnet")
require("factoextra")

#Loading datasets
dataset.training = read.csv("../dataset/NSLKDD_Training_New.csv",
                            sep = ",", header = TRUE)

#Removing unnecessary labels
dataset = dataset.training
dataset$Label_Normal_TypeAttack = NULL
dataset$Label_Num_Classifiers = NULL
dataset$Label_Normal_or_Attack = NULL

#Transforming features into numeric class
for (i in 1:(ncol(dataset)-1))
  dataset[,i] = as.numeric(dataset[,i])

#Transforming remaining labes into numeric class
dataset[,ncol(dataset)] = as.factor(dataset[,ncol(dataset)])

#Scaling
dataset = ScaleSet(dataset)
################################################################################
#Beggining PCA
################################################################################
pca = prcomp(dataset[,-41], scale. = TRUE)
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
     type = "b", col = "blue")

fviz_pca_var(pca, col.var= "contrib")

dataset.pca = as.data.frame(pca$x)
dataset.pca = data.frame(dataset.pca,
                         Label = dataset$Label)

cv.sets = CVSet(dataset.pca, k = 10, seed = 22)
length(cv.sets)

#PCA with 10-Fold Cross Validation
results = matrix(nrow = 40, ncol = 10)

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
    
    data.training.pca = as.data.frame(data.cv.training[,1:i])
    data.training.pca = data.frame(data.training.pca,
                                   Label = data.cv.training$Label)
    
    model = nnet(Label ~ .,
                data = data.training.pca,
                size = 20,
                maxit = 100)
    
    dataset.testing.pca.predict = as.data.frame(data.cv.testing[,1:i])
    dataset.testing.pca.predict = data.frame(dataset.testing.pca.predict,
                                             Label = data.cv.testing$Label)
    
    prediction = predict(model, dataset.testing.pca.predict[,1:i], type = "class")
    results.cv[j] = mean(prediction == dataset.testing.pca.predict[,ncol(dataset.testing.pca.predict)])
  }
  
  results[i,] = results.cv
}

#Exporting results
saveRDS(results, file = "feature_selection/NN/results_PCA.rds")