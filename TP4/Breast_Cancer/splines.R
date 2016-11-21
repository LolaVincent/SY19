library('splines')
fit<-lm(as.matrix(train[33])~.,data=traindata)
ypred<-predict(fit,newdata=testdata,se=T)

plot(as.data.frame(ypred)$fit, as.data.frame(test[33])$Time)