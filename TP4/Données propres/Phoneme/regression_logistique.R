source("phoneme_data.R")
source("phomene_ACP_AFD.R")
#Régression logistique : 

library(glmnet)
library(pROC)
library(MASS)

app = as.data.frame(phoneme.train[,2:258])
test = phoneme.test[,2:258]


glm.g <- cv.glmnet(as.matrix(phoneme.trainquant), app$g, family="multinomial") #cross-validation
pred.g <- predict(glm.g2, newx = as.matrix(phoneme.testquant), type = "class")
perf.g <- table(test$g, pred.g2)
(sum(perf.g)-sum(diag(perf.g)))/ntest # 7,5% d'erreur

roc_curve_reg<-roc(as.numeric(test$g), as.numeric(as.factor(pred.g2)))
plot(roc_curve_reg, col='red') # fonctionne mais courbe bizarre
auc(roc_curve_reg) #Aire sous la courbe --> Plus elle est gde, meilleure méthode  


#regression logistique après ACP

glm.g.acp <- cv.glmnet(acp.train$scores, app$g, family="multinomial") #cross-validation
pred.g.acp <- predict(glm.g.acp, newx = as.matrix(acp.test$scores), type = "class")
perf.g.acp <- table(phoneme.test[,258], pred.g.acp)
(sum(perf.g.acp)-sum(diag(perf.g.acp)))/ntest # 17% d'erreur

#regression logistique après AFD
glm.g.afd <- cv.glmnet(Z, phoneme.train[,258], family="multinomial") #cross-validation
pred.g.afd <- predict(glm.g.afd, newx = Ztest, type = "class")
perf.g.afd <- table(phoneme.test[,258], pred.g.afd)
(sum(perf.g.afd)-sum(diag(perf.g.afd)))/ntest # 5.19% d'erreur

#regression logistique après ACP et AFD
glm.g.acf <- cv.glmnet(Z2, phoneme.train[,258], family="multinomial") #cross-validation
pred.g.acf <- predict(glm.g.acf, newx = Ztest2, type = "class")
perf.g.acf <- table(phoneme.test[,258], pred.g.acf)
(sum(perf.g.acf)-sum(diag(perf.g.acf)))/ntest # 10% d'erreur