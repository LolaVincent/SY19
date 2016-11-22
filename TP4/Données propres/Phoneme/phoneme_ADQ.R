source("phoneme_data.R")
source("phoneme_AFD.R")

phoneme.qda = qda(as.matrix(as.numeric(app$g))~ ., data = app)
pred.qda=predict(phoneme.qda,newdata=test)
perf.qda <- table(pred.qda$class, as.numeric(test$g))
adqErrorRate[i] <- mean(pred.qda$class!=as.numeric(test$g)) 
# 17% d'erreur

#Après AFD : 
phoneme.qda2 = qda(as.matrix(as.numeric(app$g))~ ., data = as.data.frame(Z))
pred.qda2=predict(phoneme.qda2,newdata=as.data.frame(Ztest))
perf.qda2 <- table(pred.qda2$class, as.numeric(test$g))
adqAFDErrorRate[i] <- mean(pred.qda2$class!=as.numeric(test$g)) 
# 4% d'erreur

#Représentation ensemble de test après AFD/ADQ
plot(as.data.frame(Ztest), pch=16, col=palette()[pred.qda2$class]) 