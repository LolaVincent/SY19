source("afd.R")


#Naive Bayes Classifier :
install.packages('e1071', dependencies=TRUE)
library(e1071)
dataNaiveACP  = data.frame(data.acp.train[,1:20], data.acp.trainclass)
dataNaiveACP$data.acp.trainclass = as.factor(dataNaiveACP$data.acp.trainclass)
naive.acp <- naiveBayes(data.acp.trainclass~., data=dataNaiveACP)
pred.naive.acp <-predict(naive.acp, newdata=data.acp.test[,1:20])
perf.naive.acp <-table(data.acp.testclass, pred.naive.acp)
(sum(perf.naive.acp)-sum(diag(perf.naive.acp)))/nrow(data.acp.test) 
#27% d'erreur


#Après AFD
dataNaiveAFD  = data.frame(Z, data.acp.trainclass)
dataNaiveAFD$data.acp.trainclass = as.factor(dataNaiveAFD$data.acp.trainclass)
naive.afd <- naiveBayes(data.acp.trainclass~., data=dataNaiveAFD)
pred.naive.afd <-predict(naive.afd, newdata=Ztest)
perf.naive.afd <-table(data.acp.testclass, pred.naive.afd)
(sum(perf.naive.afd)-sum(diag(perf.naive.afd)))/nrow(data.acp.test) 
#36% d'erreur, plus qu'avec l'ACP

#Après ACP/AFD - cross-validation
K <- 5
folds <- sample(1:K, nrow(Z), replace=TRUE)
error_rate <- vector(length = K)
for(i in 1:K) {
  df.afd.train <- data.frame(Z[(folds!=i),], y= y[(folds!=i),])
  df.afd.train$y = as.factor(y[(folds!=i),])
  afd.test <- data.frame(Z[(folds==i),])
  afd.testclass <- y[(folds==i),]
  
  naive.afd <- naiveBayes(df.afd.train$y~., data=df.afd.train)
  pred.naive.afd <-predict(naive.afd, newdata=afd.test)
  perf.naive.afd <-table(afd.testclass, pred.naive.afd)
  error_rate[i] <- (sum(perf.naive.afd)-sum(diag(perf.naive.afd)))/nrow(afd.test) 
}
error_rate
mean(error_rate) #10.0%
median(error_rate) #9.4%
var(error_rate)
boxplot(error_rate)

library(pROC)
roc_curve<-roc(testclass, as.numeric(pred.naive.afd))
plot(roc_curve)
