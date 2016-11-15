source("phomene_ACP_AFD.R")
#Naive Bayes Classifier :

library(e1071)
naive.phoneme<- naiveBayes(as.matrix(as.numeric(app$g)), data=app)
pred.phoneme.naive<-predict(naive.phoneme,newdata=test)
perf.naive <-table(test$g,pred.phoneme.naive)
1-sum(diag(perf.naive))/ntst