#Este script extrae los nombres de los ataques del conjunto de prueba
##NO presentes en el conjunto de prueba

rm(list = ls())
training = read.csv("dataset/NSLKDD_Training_New.csv")
testing = read.csv("dataset/NSLKDD_Testing_New.csv")

types.attacks.training = sort(unique(as.character(training$Label_Normal_TypeAttack)))
types.attacks.testing = sort(unique(as.character(testing$Label_Normal_TypeAttack)))
diff.attacks = setdiff(types.attacks.testing, types.attacks.training)
saveRDS(object = diff.attacks, file = "source/final_model/testing_set/diff_attacks.rds")