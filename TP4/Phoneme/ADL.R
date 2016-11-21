adlErrorRate <- vector(length = 5)
for (i in 1:5) {
  source("phoneme_data.R")
  lda.g <- lda(as.matrix(as.numeric(app$g))~ ., data = app)
  pred.lda <- predict(lda.g,newdata=test)
  perf.lda <- table(as.numeric(test$g), pred.lda$class)
  adlErrorRate[i] <- (sum(perf.lda)-sum(diag(perf.lda)))/nrow(test) #7,7 %
}

mean(adlErrorRate)


# Est ce qu'on peut le faire avec plus de 2 classes ?? ici pred.lda$x contient 4 colonnes donc vecteur trop grand par rapport à test$g ) 
roc_curve_lda<-roc(as.numeric(test$g), as.vector(as.numeric(pred.lda$x))) #marche pas :(
plot(roc_curve_lda, add=TRUE) 

lda.g.acp <- lda(as.matrix(as.numeric(phoneme.train[,258]))~ ., data = as.data.frame(acp.train$scores))
pred.lda.acp <- predict(lda.g.acp, newdata=as.data.frame(acp.test$scores))
perf.lda.acp <- table(as.numeric(phoneme.test[,258]), pred.lda.acp$class)
(sum(perf.lda.acp)-sum(diag(perf.lda.acp)))/nrow(test) #19%