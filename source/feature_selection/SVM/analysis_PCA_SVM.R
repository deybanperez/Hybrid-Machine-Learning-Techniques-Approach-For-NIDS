#Preparing environment
rm(list = ls())
source("source/functions/functions.R")
require("e1071")

#Loading the results
results  = readRDS("source/feature_selection/SVM/results_PCA.rds")
###########################################################
#Creating new variables
sd.results = apply(results, 1, sd)
mean.results = apply(results, 1, mean)

##########################################################
#Splitting the screen in 2
par(mfrow = c(1,2))

#Plotting standard deviation vs number of components
plot(sd.results, col = "blue", type = "b",
     main = "Desviación Estándar vs # Componentes",
     xlab = "Número Componentes", ylab = "Desviación Estándar")

#Plotting mean accuracy vs number of components
plot(mean.results, col = "blue", type = "b",
     main = "Media Acierto vs # Componentes",
     xlab = "Número de Componentes", ylab = "Media Acierto")