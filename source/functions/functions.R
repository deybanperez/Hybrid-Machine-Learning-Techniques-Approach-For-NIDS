#Function that assign a new label for type of attack
ClassLabelAttack = function(dataframe)
{
  newColumn = as.character(dataframe$V42)
  
  newColumn[newColumn == "apache2" | newColumn == "back" | newColumn == "land" | newColumn == "mailbomb"
            | newColumn == "neptune" | newColumn == "pod" | newColumn == "processtable" | newColumn == "smurf"
            | newColumn == "teardrop" | newColumn == "udpstorm"] = "DoS"
  
  newColumn[newColumn == "buffer_overflow" | newColumn == "httptunnel" | newColumn == "loadmodule" | newColumn == "perl"
           | newColumn == "ps" | newColumn == "rootkit" | newColumn == "sqlattack" | newColumn == "xterm"] = "U2R"
  
  newColumn[newColumn == "ftp_write" | newColumn == "guess_passwd" | newColumn == "imap" | newColumn == "multihop"
            | newColumn == "named" | newColumn == "phf" | newColumn == "sendmail" | newColumn == "snmpgetattack"
            | newColumn == "snmpguess" | newColumn == "spy" | newColumn == "warezclient" | newColumn == "warezmaster"
            | newColumn == "worm" | newColumn == "xlock" | newColumn == "xsnoop"] = "R2L"
  
  newColumn[newColumn == "ipsweep" | newColumn == "mscan" | newColumn == "nmap" | newColumn == "portsweep"
            | newColumn == "saint" | newColumn == "satan"] = "Probing"
  
  return(newColumn)
}

#Function that assign a label as attack or normal
NormalAttackLabel = function(dataframe)
{
  temporal = as.character(dataframe$V44)
  temporal[temporal != "normal"] = "Attack"
  return(temporal)
}

#Function that give names to columns
ColumnNames = function(dataframe)
{
  colnames(dataframe) = c("Duration", "Protocol_type", "Service", "Flag", "Src_bytes",
                                 "Dst_bytes", "Land", "Wrong_fragment", "Urgent", "Hot",
                                 "Num_failed_logins", "Logged_in", "Num_compromised", "Root_shell",
                                 "Su_attempted", "Num_root", "Num_file_creations", "Num_shells",
                                 "Num_ccess_files", "Num_outbound_cmds", "Is_host_login", "Is_guest_login",
                                 "Count", "Srv_count", "Serror_rate", "Srv_error_rate", "Rerror_rate",
                                 "Srv_rerror_rate", "Same_srv_rate", "Diff_srv_rate", "Srv_diff_host_rate",
                                 "Dst_host_count", "Dst_host_srv_count", "Dst_host_same_srv_rate",
                                 "Dst_host_diff_srv_rate", "Dst_host_same_src_port_rate", "Dst_host_srv_diff_host_rate",
                                 "Dst_host_serror_rate", "Dst_host_srv_serror_rate", "Dst_host_rerror_rate",
                                 "Dst_host_srv_rerror_rate", "Label_Normal_TypeAttack", "Label_Num_Classifiers","Label_Normal_ClassAttack",
                                 "Label_Normal_or_Attack")
  
  return(dataframe)
}

#Transformation of Protocol_type feature into numeric type
ProtocolTransformation = function(dataframe)
{
  dataframe$Protocol_type = as.character(dataframe$Protocol_type)
  dataframe$Protocol_type[dataframe$Protocol_type == "icmp"] = 1
  dataframe$Protocol_type[dataframe$Protocol_type == "tcp"] = 2
  dataframe$Protocol_type[dataframe$Protocol_type == "udp"] = 3

  return(dataframe)
}

#Transformation of Service feature into numeric type
ServiceTransformation = function(dataframe)
{
  dataframe$Service = as.character(dataframe$Service)
  dataframe$Service[dataframe$Service == "aol"] = 1
  dataframe$Service[dataframe$Service == "auth"] = 2
  dataframe$Service[dataframe$Service == "bgp"] = 3
  dataframe$Service[dataframe$Service == "courier"] = 4
  dataframe$Service[dataframe$Service == "csnet_ns"] = 5
  dataframe$Service[dataframe$Service == "ctf"] = 6
  dataframe$Service[dataframe$Service == "daytime"] = 7
  dataframe$Service[dataframe$Service == "discard"] = 8
  dataframe$Service[dataframe$Service == "domain"] = 9
  dataframe$Service[dataframe$Service == "domain_u"] = 10
  dataframe$Service[dataframe$Service == "echo"] = 11
  dataframe$Service[dataframe$Service == "eco_i"] = 12
  dataframe$Service[dataframe$Service == "ecr_i"] = 13
  dataframe$Service[dataframe$Service == "efs"] = 14
  dataframe$Service[dataframe$Service == "exec"] = 15
  dataframe$Service[dataframe$Service == "finger"] = 16
  dataframe$Service[dataframe$Service == "ftp"] = 17
  dataframe$Service[dataframe$Service == "ftp_data"] = 18
  dataframe$Service[dataframe$Service == "gopher"] = 19
  dataframe$Service[dataframe$Service == "harvest"] = 20
  dataframe$Service[dataframe$Service == "hostnames"] = 21
  dataframe$Service[dataframe$Service == "http"] = 22
  dataframe$Service[dataframe$Service == "http_2784"] = 23
  dataframe$Service[dataframe$Service == "http_443"] = 24
  dataframe$Service[dataframe$Service == "http_8001"] = 25
  dataframe$Service[dataframe$Service == "imap4"] = 26
  dataframe$Service[dataframe$Service == "IRC"] = 27
  dataframe$Service[dataframe$Service == "iso_tsap"] = 28
  dataframe$Service[dataframe$Service == "klogin"] = 29
  dataframe$Service[dataframe$Service == "kshell"] = 30
  dataframe$Service[dataframe$Service == "ldap"] = 31
  dataframe$Service[dataframe$Service == "link"] = 32
  dataframe$Service[dataframe$Service == "login"] = 33
  dataframe$Service[dataframe$Service == "mtp"] = 34
  dataframe$Service[dataframe$Service == "name"] = 35
  dataframe$Service[dataframe$Service == "netbios_dgm"] = 36
  dataframe$Service[dataframe$Service == "netbios_ns"] = 37
  dataframe$Service[dataframe$Service == "netbios_ssn"] = 38
  dataframe$Service[dataframe$Service == "netstat"] = 39
  dataframe$Service[dataframe$Service == "nnsp"] = 40
  dataframe$Service[dataframe$Service == "nntp"] = 41
  dataframe$Service[dataframe$Service == "ntp_u"] = 42
  dataframe$Service[dataframe$Service == "other"] = 43
  dataframe$Service[dataframe$Service == "pm_dump"] = 44
  dataframe$Service[dataframe$Service == "pop_2"] = 45
  dataframe$Service[dataframe$Service == "pop_3"] = 46
  dataframe$Service[dataframe$Service == "printer"] = 47
  dataframe$Service[dataframe$Service == "private"] = 48
  dataframe$Service[dataframe$Service == "red_i"] = 49
  dataframe$Service[dataframe$Service == "remote_job"] = 50
  dataframe$Service[dataframe$Service == "rje"] = 51
  dataframe$Service[dataframe$Service == "shell"] = 52
  dataframe$Service[dataframe$Service == "smtp"] = 53
  dataframe$Service[dataframe$Service == "sql_net"] = 54
  dataframe$Service[dataframe$Service == "ssh"] = 55
  dataframe$Service[dataframe$Service == "sunrpc"] = 56
  dataframe$Service[dataframe$Service == "supdup"] = 57
  dataframe$Service[dataframe$Service == "systat"] = 58
  dataframe$Service[dataframe$Service == "telnet"] = 59
  dataframe$Service[dataframe$Service == "tftp_u"] = 60
  dataframe$Service[dataframe$Service == "time"] = 61
  dataframe$Service[dataframe$Service == "tim_i"] = 62
  dataframe$Service[dataframe$Service == "urh_i"] = 63
  dataframe$Service[dataframe$Service == "urp_i"] = 64
  dataframe$Service[dataframe$Service == "uucp"] = 65
  dataframe$Service[dataframe$Service == "uucp_path"] = 66
  dataframe$Service[dataframe$Service == "vmnet"] = 67
  dataframe$Service[dataframe$Service == "whois"] = 68
  dataframe$Service[dataframe$Service == "X11"] = 69
  dataframe$Service[dataframe$Service == "Z39_50"] = 70
  
  return(dataframe)
}

#Transformation of Flag feature into numeric type
FlagTransformation = function(dataframe)
{
  dataframe$Flag = as.character(dataframe$Flag)
  dataframe$Flag[dataframe$Flag == "OTH"] = 1
  dataframe$Flag[dataframe$Flag == "REJ"] = 2
  dataframe$Flag[dataframe$Flag == "RSTO"] = 3
  dataframe$Flag[dataframe$Flag == "RSTOS0"] = 4
  dataframe$Flag[dataframe$Flag == "RSTR"] = 5
  dataframe$Flag[dataframe$Flag == "S0"] = 6
  dataframe$Flag[dataframe$Flag == "S1"] = 7
  dataframe$Flag[dataframe$Flag == "S2"] = 8
  dataframe$Flag[dataframe$Flag == "S3"] = 9
  dataframe$Flag[dataframe$Flag == "SF"] = 10
  dataframe$Flag[dataframe$Flag == "SH"] = 11
  
  return(dataframe)
}

#Check if levels of features are greather than 1
CheckFeaturesLevels = function(dataframe)
{
  returnValue = vector(mode = "numeric")
  
  for (i in 1:ncol(dataframe))
  {
    if(length(unique(dataframe[,i])) == 1)
      returnValue[length(returnValue)+1] = i
  }
  
  return(returnValue)
}

#Calculating sum of labels
SumLabels = function(dataframe, columnLabel)
{
  targets = sort(unique(dataframe[, columnLabel]))
  returnValue = vector(mode = "numeric", length = length(targets))
  
  for (i in 1:length(targets))
    returnValue[i] = sum(dataframe[,columnLabel] == targets[i])
  
  return(returnValue)
}

SumLabelsVector = function(vector)
{
  targets = sort(unique(vector))
  returnValue = vector(mode = "numeric", length = length(targets))
  
  for (i in 1:length(targets))
    returnValue[i] = sum(vector == targets[i])
  
  return(returnValue)
}

#Probabilities vector
ProbVector = function(dataframe, vectorOcurrences)
{
  vectorProbabilities = vector(mode = "numeric", length = length(vectorOcurrences))
  returnValue = vector(mode = "numeric", length = length(nrow(dataframe)))
  labels = sort(unique(dataframe[, ncol(dataframe)]))
  
  #Inversely proportional probabilities
  for (i in 1:length(vectorProbabilities))
    vectorProbabilities = 1 - (vectorOcurrences/nrow(dataframe))
  
  
  for (i in 1:length(labels))
    returnValue[dataframe[, ncol(dataframe)] == labels[i]] = vectorProbabilities[i]
  
  return(returnValue)
}

#Indexes for training sample
IndexesTrainingSample = function(dataframe, vectorProbabilities = NULL, proportion, seed = NULL)
{
  if(!is.null(seed))
    set.seed(seed)
  
  if(!is.null(vectorProbabilities))
    returnValue = sample(nrow(dataframe), floor(nrow(dataframe) * proportion),
                         prob = vectorProbabilities, replace = FALSE)
  else
    returnValue = sample(nrow(dataframe), floor(nrow(dataframe) * proportion), replace = FALSE)
  
  return(returnValue)
}

#Scale set for set
ScaleSet = function(set)
{
  auxLabels = set[, ncol(set)]
  set = scale(set[, 1: (ncol(set) -1)])
  
  for (i in 1:ncol(set))
  {
    if(sum(is.nan(set[,i])) > 0)
      set[,i] = 0
  }
  
  set = data.frame(set, Label = auxLabels)
  
  return(set)
}

#Accuracy for each type of label
AccuracyPerLabel = function(confusionMatrix, set)
{
  labels = sort(unique(set[, ncol(set)]))
  returnValue = vector(mode = "numeric", length = length(labels))
  
  for (i in 1:ncol(confusionMatrix))
    returnValue[i] = confusionMatrix[i,i] / sum(set[,ncol(set)] == labels[i])
  
  return(100*returnValue)
}

#Attack vs normal Confusion matrix
AttackNormalConfusionMatrix = function(set, predictions)
{
  predictions = as.character(predictions)
  predictions[predictions != "normal"] = "Attack"
  predictions = as.factor(predictions)
  labels = as.character(set[,ncol(set)])
  labels[labels != "normal"] = "Attack"
  labels = as.factor(labels)
  return(table(Real = labels,
        Prediction = predictions))
}

# (1 - Accuracy)
ErrorRate = function(accuracy)
{
  return(1-accuracy)
}

# (TP / (TP + FN))
Sensitivity = function(confusionMatrix)
{
  return(confusionMatrix[1,1] / (confusionMatrix[1,1] + confusionMatrix[1,2]))
}
# (TN / (FP + TN))
Especificity = function(confusionMatrix)
{
  return(confusionMatrix[2,2] / (confusionMatrix[2,1] + confusionMatrix[2,2]))
}

# (TP / (TP + FP))
Precision = function(confusionMatrix)
{
  return(confusionMatrix[1,1] / (confusionMatrix[1,1] + confusionMatrix[2,1]))
}

#Extract Probabilities in Prediction
ExtractProbabilities = function(matrix)
{
  returnValue = vector(mode = "numeric", length = nrow(matrix))
  
  for (i in 1:length(returnValue))
  {
    if(which.max(matrix[i,]) == 2)
      returnValue[i] = max(matrix[i,])
    else
      returnValue[i] = 1 - matrix[i,2]
  }
  return(returnValue)
}

#Generate ROC
generate_ROC = function(scores, real, pred)
{
  scores = as.numeric(scores)
  newOrder = order(scores, real, decreasing = TRUE)
  scores = scores[newOrder]
  real = real[newOrder]
  pred = pred[newOrder]
  returnTP = vector(mode = "numeric")
  returnFP = vector(mode = "numeric")
  scorePrev = Inf
  FP = 0
  TP = 0
  P = sum(real == pred)
  N = length(real) - P
  index = 1
  
  for(i in 1:length(scores))
  {
    if(scores[i] != scorePrev)
    {
      returnTP[index] = TP/P
      returnFP[index] = FP/N
      scorePrev = scores[i]
      index = index +1
    }
    
    if(real[i] == pred[i])
      TP = TP + 1
    else
      FP = FP +1
  }
  
  returnTP[length(returnTP)+1] = TP/P
  returnFP[length(returnFP)+1] = FP/N
  
  plot(returnFP, returnTP, type = "l",
       ylab = "Porcentaje Aciertos", xlab = "Porcentaje Errores",
       xlim = c(0,1), ylim = c(0,1))
  abline(0,1, col = "blue")
}

#Order predictions in Kmeans
OrderKmeans = function(model)
{
  prediction = as.numeric(model$cluster)
  
  if(length(unique(prediction)) == 2)
    label.classes = c("normal", "Attack")
  else
    label.classes = c("normal", "DoS", "Probing", "R2L", "U2R")
  
  ordered.labels = order(SumLabelsVector(prediction), decreasing = TRUE)
  
  for (i in 1:length(ordered.labels))
    prediction[prediction == ordered.labels[i]] = label.classes[i]
  
  return(as.character(prediction))
}

#Find Center K-means
FindCentersKmeans = function(set, clusters, iterations,iter.max)
{
  for (i in 1:iterations)
  {
    set.seed(i)
    kmeans.model = kmeans(set[,1:(ncol(set)-1)], centers = clusters,
                          iter.max = iter.max)
    
    if(i == 1)
      matrix.centers = kmeans.model$centers
    else
      matrix.centers = matrix.centers + kmeans.model$centers
  }
  
  return(matrix.centers)
}

#Cross Validation set
CVSet = function(set, k, seed)
{
  cv.data = set
  total.rows = nrow(cv.data)
  
  for (i in 1:k)
  {
    set.seed(22)
    index.cv = sample(nrow(cv.data), total.rows %/% k, replace = FALSE)
    
    if(i == 1)
      cv.return = list(cv.data[index.cv,])
    else
      cv.return[[length(cv.return)+1]] = cv.data[index.cv,]
    
    cv.data = cv.data[-index.cv,]
  }
  
  return(cv.return)
}


#Data for ROC Curve
DataROC = function(set, probabilities, predictions)
{
  set[,ncol(set)] = as.character(set[,ncol(set)])
  set[set[,ncol(set)] != "normal", ncol(set)] = "attack"
  set$Prediction = predictions
  set$Prediction = as.character(set$Prediction)
  set$Prediction[set$Prediction != "normal"] = "attack"
  set$Prob = ExtractProbabilities(probabilities)
  set = set[order(set$Prob, set$Label, decreasing = TRUE), ]
  return(set[,(ncol(set)-2):ncol(set)])
}

#Confusion matrix two levels

TwoLevelsCM = function(CM1, CM2)
{
  TP = CM1[1,1] + CM2[1,1]
  FN = CM2[1,2]
  FP = CM1[2,1] + CM2[2,1]
  TN = CM2[2,2]
  
  matrix(c(TP, FN, FP, TN), nrow = 2, ncol = 2, byrow = TRUE)
}

#Calculate accuracy from confusion matrix
Accuracy = function(confusionMatrix)
{
  total = 0
  hits = 0
  
  for (i in 1:ncol(confusionMatrix))
  {
    total = total + sum(confusionMatrix[i,])
    hits = hits + confusionMatrix[i,i]
  }
  
  return(hits/total)
}

##############################################
#GFR method

GFR = function(dataset, algorithm)
{
  #Defining the vector for the names to be removed
  names.features = vector(mode = "character", length = ncol(dataset))
  #Matrix for results
  features = matrix(nrow = ncol(dataset), ncol = 10)
  #Backup for the original dataset
  auxiliar.data = dataset
  
  #For every row present in the range (40,1)
  for (i in ncol(dataset):1)
  {
    #If i == (41 or 1) then just train and storage the result 
    if(i == ncol(dataset) | i == 1)
    {
      #Create Cross validation set
      cv.set =  CVSet(set = auxiliar.data, k = 10, seed = 22)
      #Vector that storage result per iteration
      results.cv = vector(mode = "numeric", length = 10)
      
      #Starting 10-Fold-CrossValidation
      for (k in 1:length(cv.set))
      {
        #Splitting validation set
        testing.set = cv.set[[k]]
        #Splitting test set
        training.set = cv.set
        training.set[[k]] = NULL
        #Creating dataframes
        training.set = do.call(rbind, training.set)
        testing.set = as.data.frame(testing.set)
        
        #If algortihm selected is NN
        if(algorithm == "NN")
        {
          model = nnet(Label~.,
                       data = training.set,
                       size = 20,
                       maxit = 100)
          
        }else if(algorithm == "SVM") ##If algorithm if SVM
        {
          model = svm(Label~.,
                      data = training.set,
                      kernel = "radial",
                      scale = FALSE)
        }
        
        #Create predictions
        prediction = predict(model, subset(testing.set, subset = TRUE, select = names(testing.set)[-ncol(testing.set)]),
                             type = "class")
        #Storage mean in the right position
        results.cv[k] = mean(prediction == testing.set[, ncol(testing.set)])
      }
      
      #Storage the vector of results from cross validation process in the right position
      features[i,] = results.cv
      
      #If i == 1 then add the name to the name's vector 
      if(i == 1)
        names.features[i] = names(training.set)[1]
      else #Else add "All"
        names.features[i] = "All"
      
    }else #If is in the range [2, 40]
    {
      #Best virtual result is 0
      best.result = 0 
      
      #Itero sobre cada característica y excluye la etiqueta del conjunto de datos
      #Iter over every characteristic and exclude the label from data set
      for(j in 1:(ncol(auxiliar.data) -1))
      {
        #Create a temporal set without column j
        temporal.set = auxiliar.data[,-j]
        #Create cross validation set
        cv.set =  CVSet(set = temporal.set, k = 10, seed = 22)
        #Create result vector for cross validation
        results.cv = vector(mode = "numeric", length = 10)
        
        #Start cross validation
        for (k in 1:length(cv.set))
        {
          #Splitting cross validation set
          testing.set = cv.set[[k]]
          #spliting training set
          training.set = cv.set
          training.set[[k]] = NULL
          #Create dataframes
          training.set = do.call(rbind, training.set)
          testing.set = as.data.frame(testing.set)
          
          #If algorithm is NN
          if(algorithm == "NN")
          {
            model = nnet(Label~.,
                         data = training.set,
                         size = 20,
                         maxit = 100)
          }else if(algorithm == "SVM") #If algorithm is SVM
          {
            model = svm(Label~.,
                        data = training.set,
                        kernel = "radial",
                        scale = FALSE)
          }
          
          #Creating predictions
          prediction = predict(model, subset(testing.set, subset = TRUE, select = names(testing.set)[-ncol(testing.set)]),
                               type = "class")
          #Storing mean from predictions in the right place
          results.cv[k] = mean(prediction == testing.set[, ncol(testing.set)])
        }
        
        #If mean is greater than best result
        if(mean(results.cv) > best.result)
        {
          best.result = mean(results.cv) #Update best result
          best.result.vector = results.cv #Storage best results vector
          temporal.j = j #Storage the position of the dropped column
        }
      }
      #Add the names of the dropped column
      names.features[i] = names(auxiliar.data)[temporal.j] 
      #Add cross validation vector to the matrix in the right place
      features[i,] = best.result.vector
      #Update auxiliar vector removing the column doesn't present in the best result
      auxiliar.data = auxiliar.data[,-temporal.j]
    }
    
    cat(i, " ")
  }
  #Add the names to the row
  rownames(features) = names.features
  #return the matrix
  return(features)
}

#Selec Distance in KMeans

MeasuareKMeans = function(dataset, k)
{
  Hartigan = 0
  Lloyd = 0
  Forgy = 0
  Macqueen = 0
  
  for(i in 1:50)
  {
    set.seed(i)
    groups = kmeans(dataset[,-ncol(dataset)], k, iter.max = 100, algorithm = "Hartigan-Wong")
    Hartigan = Hartigan + groups$betweenss
    set.seed(i)
    groups = kmeans(dataset[,-ncol(dataset)], k, iter.max = 100, algorithm = "Lloyd")
    Lloyd = Lloyd + groups$betweenss
    set.seed(i)
    groups = kmeans(dataset[,-ncol(dataset)], k, iter.max = 100, algorithm = "Forgy")
    Forgy = Forgy + groups$betweenss
    set.seed(i)
    groups = kmeans(dataset[,-ncol(dataset)], k, iter.max = 100, algorithm = "MacQueen")
    Macqueen = Macqueen + groups$betweenss
  }
  
  return(list(Hartigan = Hartigan/50, Lloyd = Lloyd/50, Forgy = Forgy/50, Macqueen = Macqueen/50))
}