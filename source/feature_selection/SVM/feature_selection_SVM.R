#Setting work directory
rm(list = ls())
#setwd("/home/dperez/Documents/Repos/Tesis/source")
#setwd("/home/dperez/Tesis/source")

#Loading functions
source("functions/functions.R")

#Installing packages
#install.packages("e1071")
#install.packages("factoextra")
#Loading packages
require("e1071")
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

#Plotting the two first main components
colors = as.character(dataset.pca[,ncol(dataset.pca)])
colors[colors == "normal"] = "black"
colors[colors == "DoS"] = "red"
colors[colors == "Probing"] = "green"
colors[colors == "R2L"] = "blue"
colors[colors == "U2R"] = "cyan"

plot(x = dataset.pca[,1],  y = dataset.pca[,2], col = colors,
     main = "Gráfico de las Dos Componenets Principales",
     xlab = "Componente 1", ylab = "Componente 2", pch = 19)

legend("bottomleft", legend = c("Normal", "DoS", "Probing", "R2L", "U2R"),
       col = c("black","red", "green", "blue", "cyan"), pch = 19)

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
    colnames(data.training.pca) = names(data.cv.training)[1:i]
    data.training.pca = data.frame(data.training.pca,
                                   Label = data.cv.training$Label)
    
    data.testing.pca = as.data.frame(data.cv.testing[,1:i])
    colnames(data.testing.pca) = names(data.cv.testing)[1:i]
    data.testing.pca = data.frame(data.testing.pca,
                                     Label = data.cv.testing$Label)
    
    model = svm(Label ~ .,
                           data = data.training.pca,
                           kernel = "radial",
                           scale = FALSE)
    
    if(i==1)
      prediction = predict(model, data.frame(PC1 = data.testing.pca[,1]), type = "class")
    else
      prediction = predict(model, data.testing.pca[,1:i], type = "class")
    
    results.cv[j] = mean(prediction == data.testing.pca[,ncol(data.testing.pca)])
  }
  
  results[i,] = results.cv
  cat(i)
}

#Exporting results
saveRDS(results, file = "feature_selection/SVM/results_PCA.rds")