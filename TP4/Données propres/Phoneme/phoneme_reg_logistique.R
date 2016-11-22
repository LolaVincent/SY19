source("phoneme_data.R")
source("phoneme_ACP.R")
source("phoneme_AFD.R")

#Régression logistique : 

library(glmnet)
library(pROC)
library(MASS)


glm.g <- cv.glmnet(as.matrix(phoneme.trainquant), phoneme.trainclass, family="multinomial") #cross-validation
pred.g <- predict(glm.g, newx = as.matrix(phoneme.testquant), type = "class")
perf.g <- table(phoneme.testclass, pred.g)
#Taux d'erreur:
(sum(perf.g)-sum(diag(perf.g)))/ntest # 7,5% d'erreur

#Courbe ROC
roc_curve_reg<-roc(as.numeric(phoneme.testclass), as.numeric(as.factor(pred.g)))
auc(roc_curve_reg) #Aire sous la courbe --> Plus elle est gde, meilleure méthode, ici 0,79


#regression logistique après AFD
glm.g.afd <- cv.glmnet(Z, phoneme.trainclass, family="multinomial") #cross-validation
pred.g.afd <- predict(glm.g.afd, newx = Ztest, type = "class")
perf.g.afd <- table(phoneme.testclass, pred.g.afd)
#Taux d'erreur :
(sum(perf.g.afd)-sum(diag(perf.g.afd)))/ntest # 4.39% d'erreur

#Courbe ROC
roc_curve_reg_afd<-roc(as.numeric(phoneme.testclass), as.numeric(as.factor(pred.g.afd)))
auc(roc_curve_reg_afd) #Aire sous la courbe --> Plus elle est gde, meilleure méthode, ici 0.88