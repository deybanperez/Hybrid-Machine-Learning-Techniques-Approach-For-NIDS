#Setting work directory
rm(list = ls())

#Loading functions
source("source/functions/functions.R")

#Loading packages
require("nnet")
require("factoextra")

#Loading datasets
dataset.training = read.csv("dataset/NSLKDD_Training_New.csv",
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
pca = prcomp(dataset[,-41], scale. = FALSE)
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
     ylab = "Proporción Acumulada",
     xlab = "Número de Componentes Principales",
     type = "b", col = "blue")

#Plotting the two first main components
colors = as.character(dataset.pca[,ncol(dataset.pca)])
colors[colors == "normal"] = "black"
colors[colors == "DoS"] = "red"
colors[colors == "Probing"] = "green"
colors[colors == "R2L"] = "blue"
colors[colors == "U2R"] = "cyan"

plot(x = dataset.pca[,1],  y = dataset.pca[,2], col = colors,
     main = "Gráfico de las Dos Componentes Principales",
     xlab = "Componente 1", ylab = "Componente 2", pch = 19)

legend("bottomleft", legend = c("Normal", "DoS", "Probing", "R2L", "U2R"),
       col = c("black","red", "green", "blue", "cyan"), pch = 19)

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
    colnames(data.training.pca) = names(data.cv.training)[1:i]
    data.training.pca = data.frame(data.training.pca,
                                   Label = data.cv.training$Label)
    
    data.testing.pca = as.data.frame(data.cv.testing[,1:i])
    colnames(data.testing.pca) = names(data.cv.testing)[1:i]
    data.testing.pca = data.frame(data.testing.pca,
                                  Label = data.cv.testing$Label)
    
    model = nnet(Label ~ .,
                 data = data.training.pca,
                 size = 20,
                 maxit = 100)
    
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
saveRDS(results, file = "source/feature_selection/NN/results_PCA.rds")