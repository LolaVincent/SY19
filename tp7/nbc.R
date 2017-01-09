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

