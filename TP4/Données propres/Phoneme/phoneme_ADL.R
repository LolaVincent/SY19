#ADL

source("phoneme_data.R")
source("phoneme_ACP.R")
source("phoneme_AFD.R")

adlErrorRate <- vector(length = 5)
for (i in 1:5) {
  source("phoneme_data.R")
  lda.g <- lda(as.matrix(as.numeric(app$g))~ ., data = app)
  pred.lda <- predict(lda.g,newdata=test)
  perf.lda <- table(as.numeric(test$g), pred.lda$class)
  adlErrorRate[i] <- (sum(perf.lda)-sum(diag(perf.lda)))/nrow(test) #7,7 %
}

mean(adlErrorRate)

#ADL sur ACP
lda.g.acp <- lda(as.matrix(as.numeric(phoneme.trainclass))~ ., data = as.data.frame(acp.train$scores))
pred.lda.acp <- predict(lda.g.acp, newdata=as.data.frame(acp.test$scores))
perf.lda.acp <- table(as.numeric(phoneme.testclass), pred.lda.acp$class)
#Taux d'erreur : 
(sum(perf.lda.acp)-sum(diag(perf.lda.acp)))/nrow(test) #19%, taux d'erreur élevé