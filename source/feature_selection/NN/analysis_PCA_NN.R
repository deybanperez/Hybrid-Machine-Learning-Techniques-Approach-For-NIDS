#Preparing environment
rm(list = ls())
source("source/functions/functions.R")
require("nnet")

#Loading the results
results  = readRDS("source/feature_selection/NN/results_PCA.rds")
###########################################################
#Calculating standard deviation
sd.results = apply(results, 1, sd)
#Calculating mean
mean.results = apply(results, 1, mean)

##########################################################
#Splitting the screen in 2
par(mfrow = c(1,2))

#Plotting standard deviation vs number of components
plot(sd.results, col = "blue", type = "b",
     main = "Desviación Estándar vs Número de Componentes",
     xlab = "Número de Componentes", ylab = "Desviación Estándar")

#Plotting mean accuracy vs number of components
plot(mean.results, col = "blue", type = "b",
     main = "MEdia Acierto vs # Componentes",
     xlab = "Número de Componentes", ylab = "Media Acierto")