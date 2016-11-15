source("phomene_data.R")
#Régression logistique : 

glm.g <- glm(as.numeric(app$g)~ ., data = app)
pred.glm<-predict(glm.g,newdata=test,type='link')
perf.glm <- table(as.numeric(test$g), pred.glm>2.5)
sum(perf.glm[1:2,2], perf.glm[3:5,1])/nrow(test) # 1.3 % d'erreur 

#Ici family=gaussian par défaut, utiliser glmnet du CRAN

library(pROC)
roc_curve_reg<-roc(as.numeric(test$g), as.vector(pred.glm))
plot(roc_curve_reg, col='red')
auc(roc_curve_reg) #Aire sous la courbe --> Plus elle est gde, meilleure méthode

# Comparaison avec la lda : 
lda.g <- lda(as.matrix(as.numeric(app$g))~ ., data = app)
pred.lda <- predict(lda.g,newdata=test)
perf.lda <- table(as.numeric(test$g), pred.lda$class)
(sum(perf.lda)-sum(diag(perf.lda)))/nrow(test)
# Est ce qu'on peut le faire avec plus de 2 classes ?? ici pred.lda$x contient 4 colonnes donc vecteur trop grand par rapport à test$g ) 
roc_curve_lda<-roc(as.numeric(test$g), as.vector(as.numeric(pred.lda$x))) #marche pas :(
plot(roc_curve_lda, add=TRUE) 


