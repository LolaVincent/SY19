source("phoneme_data.R")
source("phomene_ACP_AFD.R")

#Naive Bayes Classifier :

install.packages('e1071', dependencies=TRUE)
library(e1071)
bayesErrorRate <- vector(length = 10)
for (i in 1:10) {
  source("phoneme_data.R")
  naive.phoneme<- naiveBayes(g~., data=app)
  pred.phoneme.naive<-predict(naive.phoneme,newdata=test)
  perf.naive <-table(test$g,pred.phoneme.naive)
  bayesErrorRate[i] <- 1-sum(diag(perf.naive))/ntest 
  #12% d'erreur
}

mean(bayesErrorRate)

#Après AFD
bayesAFDErrorRate <- vector(length = 5)
for (i in 1:5) {
  source("phoneme_data.R")
  source("phomene_ACP_AFD.R")
  naive.phoneme2<- naiveBayes(app$g~ ., data = as.data.frame(Z))
  pred.phoneme.naive2<-predict(naive.phoneme2,newdata=as.data.frame(Ztest))
  perf.naive2 <-table(test$g,pred.phoneme.naive2)
  bayesAFDErrorRate[i] <-1-sum(diag(perf.naive2))/ntest 
  #7% d'erreur
}

mean(bayesAFDErrorRate[-5])

plot(as.data.frame(Ztest), pch=16, col=palette()[ pred.phoneme.naive2$class]) 

boxplot(list(bayesErrorRate,bayesAFDErrorRate[-5], adqErrorRate, adqAFDErrorRate, adlErrorRate), method="jitter", vertical=T, col=c("red", "blue", "red", "blue", "red"), ylab="Error rate",xlab="Method", las = 2, names=c("Bayésien Naïf", "Bayésien Naïf", "ADQ", "ADQ", "ADL"))
legend("topright", inset=.05, title="Colors",c("Sans AFD","Avec AFD"), fill=c("red" ,"blue"), horiz=FALSE)

