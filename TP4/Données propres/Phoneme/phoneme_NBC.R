source("phoneme_data.R")
source("phomene_AFD.R")

#Naive Bayes Classifier :

install.packages('e1071', dependencies=TRUE)
library(e1071)
naive.phoneme<- naiveBayes(g~., data=app)
pred.phoneme.naive<-predict(naive.phoneme,newdata=test)
perf.naive <-table(test$g,pred.phoneme.naive)
bayesErrorRate[i] <- 1-sum(diag(perf.naive))/ntest 
#12% d'erreur


#Après AFD
naive.phoneme2<- naiveBayes(app$g~ ., data = as.data.frame(Z))
pred.phoneme.naive2<-predict(naive.phoneme2,newdata=as.data.frame(Ztest))
perf.naive2 <-table(test$g,pred.phoneme.naive2)
bayesAFDErrorRate[i] <-1-sum(diag(perf.naive2))/ntest 
#7% d'erreur

