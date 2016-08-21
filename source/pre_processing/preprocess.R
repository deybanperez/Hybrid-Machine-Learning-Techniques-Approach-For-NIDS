#Setting work directory
rm(list = ls())
setwd("/home/dperez/Documents/Repos/Tesis/source")

#Loading functions
source("functions_preprocess.R")

#Loading tarining and testing datasets
dataset.training = read.csv(file = "../dataset/KDDTrain+.txt", sep = ",", header = FALSE)
dataset.testing = read.csv(file = "../dataset/KDDTest+.txt", sep = ",", header = FALSE)

#Inspecting datasets
dim(dataset.training)
dim(dataset.testing)

#Watching complete cases
sum(complete.cases(dataset.training)) == nrow(dataset.training)
sum(complete.cases(dataset.testing)) == nrow(dataset.testing)

#Watching different types of attacks in training set
attacks.training = unique(dataset.training$V42)
attacks.training = sort(as.character(attacks.training))
length(attacks.training)
attacks.training

#Watching different types of attacks in testing set
attacks.testing = unique(dataset.testing$V42)
attacks.testing = sort(as.character(attacks.testing))
length(attacks.testing)
attacks.testing

#Total atacks 
total.attacks = sort(unique(c(attacks.training, attacks.testing)))
length(total.attacks)
total.attacks

#Watching difference between types of attacks
#Same labels
index.attacks = which(attacks.testing %in% attacks.training)
length(attacks.testing[index.attacks])
attacks.testing[index.attacks]

#Different attacks in testing
length(attacks.testing[-index.attacks])
attacks.testing[-index.attacks]

#Different attacks in training
index.attacks.training = which(attacks.training %in% attacks.testing)
length(attacks.training[-index.attacks.training])
attacks.training[-index.attacks.training]

#Adding a new label that refers to the class of atack
dataset.training$V44 = ClassLabelAttack(dataset.training)
dataset.testing$V44 = ClassLabelAttack(dataset.testing)

#Adding attack-normal column
dataset.training$V45 = NormalAttackLabel(dataset.training) 
dataset.testing$V45 = NormalAttackLabel(dataset.testing)

#Splitting dataframes
training.split = split(dataset.training, dataset.training$V44)
testing.split = split(dataset.testing, dataset.testing$V44)
summary(training.split)
summary(testing.split)

#Assigning training to a variable
training.DOS = training.split$DoS
training.normal = training.split$normal
training.Probing = training.split$Probing
training.R2L = training.split$R2L
training.U2R = training.split$U2R

#Displaying number of registers
nrow(training.DOS)
nrow(training.normal)
nrow(training.Probing)
nrow(training.R2L)
nrow(training.U2R)
barplot(table(dataset.training$V44), main = "Frecuencia De Las Clases En El Conjunto
        De Entrenamiento")

#Assigning testing to a variable
testing.DOS = testing.split$DoS
testing.normal = testing.split$normal
testing.Probing = testing.split$Probing
testing.R2L = testing.split$R2L
testing.U2R = testing.split$U2R

#Displaying number of registers
nrow(testing.DOS)
nrow(testing.normal)
nrow(testing.Probing)
nrow(testing.R2L)
nrow(testing.U2R)
barplot(table(dataset.testing$V44), main = "Frecuencia De Las Clases En El Conjunto
        De Prueba")

#Naming columns
dataset.training = ColumnNames(dataset.training)
dataset.testing = ColumnNames(dataset.testing)

#Removing fools features
index.dummy.variables.training = CheckFeaturesLevels(dataset.training)
index.dummy.variables.testing = CheckFeaturesLevels(dataset.testing)
names(dataset.training)[index.dummy.variables.training]
names(dataset.testing)[index.dummy.variables.testing]
dataset.training[,index.dummy.variables.training] = NULL
dataset.testing[, index.dummy.variables.testing] = NULL

#Transformation of data in Protocol_Type
sort(unique(dataset.testing$Protocol_type))
sort(unique(dataset.training$Protocol_type))
dataset.training = ProtocolTransformation(dataset.training)
dataset.testing = ProtocolTransformation(dataset.testing)

#Transformation of data in Service
sort(unique(dataset.testing$Service))
sort(unique(dataset.training$Service))
sort(unique(c(as.character(unique(dataset.testing$Service)),
              as.character(unique(dataset.training$Service)))))
length(sort(unique(c(as.character(unique(dataset.testing$Service)),
                     as.character(unique(dataset.training$Service))))))
dataset.training = ServiceTransformation(dataset.training)
dataset.testing = ServiceTransformation(dataset.testing)

#Transformation of data in Flag
sort(unique(dataset.testing$Flag))
sort(unique(dataset.training$Flag))
length(sort(unique(c(as.character(unique(dataset.testing$Flag)),
                     as.character(unique(dataset.training$Flag))))))
sort(unique(c(as.character(unique(dataset.testing$Flag)),
              as.character(unique(dataset.training$Flag)))))
dataset.training = FlagTransformation(dataset.training)
dataset.testing = FlagTransformation(dataset.testing)

#Writing datasets into .csv file
write.csv(dataset.training, file = "../dataset/NSLKDD_Training_New.csv", row.names = FALSE)
write.csv(dataset.testing, file = "../dataset/NSLKDD_Testing_New.csv", row.names = FALSE)