GFR = function(dataset, algorithm)
{
  #Definiendo el vector para los nombres a ser removidos
  names.features = vector(mode = "character", length = ncol(dataset))
  #Matriz de resultados
  features = matrix(nrow = ncol(dataset), ncol = 10)
  #respaldando el dataset original
  auxiliar.data = dataset

  #Para cada una de las columnas presentes (40-1)
  for (i in ncol(dataset):1)
  {
    #Si el i == 41 o a 1 entonces solo entreno y almaceno el resultado obtenido
    if(i == ncol(dataset) | i == 1)
    {
      #Creo el conjunto de validación cruzada
      cv.set =  CVSet(set = auxiliar.data, k = 10, seed = 22)
      #Vector para el almacenamiento de los resultados en cada iteración
      results.cv = vector(mode = "numeric", length = 10)
      
      #Inicio de la validación cruzada de 10 conjuntos
      for (k in 1:length(cv.set))
      {
        #Separo el conjutno de validación
        testing.set = cv.set[[k]]
        #Separo el conjunto de prueba
        training.set = cv.set
        training.set[[k]] = NULL
        #Creo los dataframes
        training.set = do.call(rbind, training.set)
        testing.set = as.data.frame(testing.set)
        
        #Si el algoritmo es una red neuronal
        if(algorithm == "NN")
        {
          model = nnet(Label~.,
                       data = training.set,
                       size = 20,
                       maxit = 100)
          
        }else if(algorithm == "SVM") ##Si el algoritmo es SVM
        {
          model = svm(Label~.,
                      data = training.set,
                      kernel = "radial",
                      scale = FALSE)
        }
        
        #Creo la predicciones
        prediction = predict(model, subset(testing.set, subset = TRUE, select = names(testing.set)[-ncol(testing.set)]),
                             type = "class")
        #Almaceno la media en al posición correspondiente
        results.cv[k] = mean(prediction == testing.set[, ncol(testing.set)])
      }
      
      #Almaceno el vector resultado de validación cruzada en la posición correspondiente
      features[i,] = results.cv
      
      #Si i == 1 agrego el de dicha columna al vector de nombres
      if(i == 1)
        names.features[i] = names(training.set)[1]
      else #Sino agrego el vector que dice que usé todas la columnas
        names.features[i] = "All"
      
    }else #Si i [2,40]
    {
      #El mejor resultado virtual es 0
      best.result = 0 
    
      #Itero sobre cada característica y excluye la etiqueta del conjunto de datos
      for(j in 1:(ncol(auxiliar.data) -1))
      {
        #Creo un conjunto temporal sin la columna j
        temporal.set = auxiliar.data[,-j]
        #Creo el conjunto de validación cruzada
        cv.set =  CVSet(set = temporal.set, k = 10, seed = 22)
        #Creo vector de relsultados de validaicón cruzada
        results.cv = vector(mode = "numeric", length = 10)
        
        #Inicia la validación cruzada
        for (k in 1:length(cv.set))
        {
          #Separo el conjunto de entrenamiento
          testing.set = cv.set[[k]]
          #Separo el conjunto de entrenamiento
          training.set = cv.set
          training.set[[k]] = NULL
          #Creo los dataframes
          training.set = do.call(rbind, training.set)
          testing.set = as.data.frame(testing.set)
          
          #Si el algoritmo es red neuronal
          if(algorithm == "NN")
          {
            model = nnet(Label~.,
                         data = training.set,
                         size = 20,
                         maxit = 100)
          }else if(algorithm == "SVM") #Si es SVM
          {
            model = svm(Label~.,
                        data = training.set,
                        kernel = "radial",
                        scale = FALSE)
          }
          
          #Creo las predicciones
          prediction = predict(model, subset(testing.set, subset = TRUE, select = names(testing.set)[-ncol(testing.set)]),
                               type = "class")
          #Almaceno la media de las predicciones en la posición concerniente
          results.cv[k] = mean(prediction == testing.set[, ncol(testing.set)])
        }
        
        #Si la media es mayor que el mejor resultado
        if(mean(results.cv) > best.result)
        {
          best.result = mean(results.cv) #Actualizo el mejor resultado
          best.result.vector = results.cv #Guardo el vector de mejor resultado
          temporal.j = j #Guardo la posición de la columna eliminada
        }
      }
      #Agrego a los nombres el nombre de la columna eliminada
      names.features[i] = names(auxiliar.data)[temporal.j] 
      #Agrego el vector de validación cruzada a la matriz en la posición correspondiente
      features[i,] = best.result.vector
      #Actualizo el vector auxiliar eliminando la columna no presente en el mejor resultado
      auxiliar.data = auxiliar.data[,-temporal.j]
    }
    
    cat(i, " ")
  }
  #Agrego los nombres a las filas
  rownames(features) = names.features
  #Retorno la matriz
  return(features)
}