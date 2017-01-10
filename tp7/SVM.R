#SVM

nb_comp = 20

acp_traindata <- data.frame(data.acp.train[,1:nb_comp], y=as.factor(data.acp.trainclass))
acp_testdata <- data.frame(data.acp.test[,1:nb_comp])
acp_testclass <- as.factor(data.acp.testclass)

traindata <- df.afd.train
testdata <- afd.test
testclass <- afd.testclass

####GENERAL TUNING : COMPARISON ACP VS ACP+AFD###
afd.tune = tune(svm, train.y=traindata$y , train.x=traindata[1:5], ranges=list(
              kernel=c("linear", "polynomial", "radial", "sigmoid"),
              cost=c(1:10),
              gamma=c(0.00001,0.0001,0.005, 0.001,0.01,.5),
              degree=c(1:5)
  )
)
afd.tune
#Best perf : 9% avec sigmoid kernel, cost 8 

acp.tune = tune(svm, train.y=acp_traindata$y , train.x=acp_traindata[1:nb_comp], ranges=list(
  kernel=c("linear", "polynomial", "radial", "sigmoid"),
  cost=c(1:10),
  gamma=c(0.00001,0.0001,0.005, 0.001,0.01,.5),
  degree=c(1:5)
)
)
acp.tune
#Best performance : 20% avec linear kernel cost=2 

####RADIAL###
library(e1071)
default_svmfit = svm(traindata$y~., data=traindata)
summary(default_svmfit) 
#Kernel : radial, cost = 1, gamma = 0.05
default_svm.pred <- predict(default_svmfit, testdata)
default_svm.perf <- table(default_svm.pred, testclass)
(sum(default_svm.perf)-sum(diag(default_svm.perf)))/nrow(testdata) 
#5% d'erreur

svm_tune <- tune(svm, train.y=traindata$y , train.x=traindata[1:5], 
                 kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(0.0001, 0.001, 0.002, 0.005,0.01, 0.05,.5,1,2)))
svm_tune
#Cost : 10, gamma : 0.002

default_svm_after_tune <- svm(traindata$y~., data=traindata, kernel="radial", cost=100, gamma=1e-04)
summary(default_svm_after_tune)
default_svm.pred_tune <- predict(default_svm_after_tune, testdata)
default_svm.perf_tune <- table(default_svm.pred_tune, testclass)
(sum(default_svm.perf_tune)-sum(diag(default_svm.perf_tune)))/nrow(testdata) 
#12.5% d'erreur


library(pROC)
roc_curve<-roc(testclass, as.numeric(default_svm.pred))
plot(roc_curve)

####LINEAR###
linear_svmfit = svm(traindata$y~., data=traindata, kernel="linear")
summary(linear_svmfit) 
#Kernel : linear, cost = 1, gamma = 0.05
linear_svm.pred <- predict(linear_svmfit, testdata)
linear_svm.perf <- table(linear_svm.pred, testclass)
(sum(linear_svm.perf)-sum(diag(linear_svm.perf)))/nrow(testdata) 
#12.5% d'erreur

svm_tune <- tune(svm, train.y=traindata$y , train.x=traindata[1:5], 
                 kernel="linear", ranges=list(cost=10^(-1:2), gamma=c(0.00001, 0.00005,0.0001, 0.001)))
svm_tune
#Cost : 0.1, gamma : 1e-04

linear_svm_after_tune <- svm(traindata$y~., data=traindata, kernel="linear", cost=1, gamma=0.00001)
summary(linear_svm_after_tune)
linear_svm.pred_tune <- predict(linear_svm_after_tune, testdata)
linear_svm.perf_tune <- table(linear_svm.pred_tune, testclass)
(sum(linear_svm.perf_tune)-sum(diag(linear_svm.perf_tune)))/nrow(testdata) 
#12.5% d'erreur

####POLYNOMIAL###
svmfit = svm(traindata$y~., data=traindata, kernel="polynomial")
summary(svmfit) 
#Kernel : linear, cost = 1, degree=3 gamma = 0.05, coef=0
svm.pred <- predict(svmfit, testdata)
svm.perf <- table(svm.pred,testclass)
(sum(svm.perf)-sum(diag(svm.perf)))/nrow(testdata) 
#28% d'erreur

svm_tune <- tune(svm, train.y=traindata$y , train.x=traindata[1:5], 
                 kernel="polynomial", ranges=list(degre=c(1,2,3,4,5),cost=10^(-1:2), gamma=c(0.00001, 0.00005,0.0001, 0.001)))
svm_tune
#Cost : 0.1, gamma : 1e-05

svm_after_tune <- svm(traindata$y~., data=traindata, kernel="polynomial", cost=100, gamma=0.001, degree=1)
summary(svm_after_tune)
svm.pred <- predict(svm_after_tune, testdata)
svm.perf <- table(svm.pred, testclass)
(sum(svm.perf)-sum(diag(svm.perf)))/nrow(testdata) 
#13.8%

####SIGMOID###
svmfit = svm(traindata$y~., data=traindata, kernel="sigmoid")
summary(svmfit) 
#Kernel : sigmoid, cost = 1, gamma = 0.05, coef=0
svm.pred <- predict(svmfit,testdata)
svm.perf <- table(svm.pred, testclass)
(sum(svm.perf)-sum(diag(svm.perf)))/nrow(testdata) 
#9.7% d'erreur

svm_tune <- tune(svm, train.y=traindata$y , train.x=traindata[1:5], 
                 kernel="sigmoid", ranges=list(cost=10^(-1:4), gamma=c(0.00001,0.0001,0.005, 0.001,0.01,.5), coef=c(0.5, 1, 2, 3)))
svm_tune
#Cost : 10, gamma : 0.005, coef:0.5

svm_after_tune <- svm(traindata$y~., data=traindata, kernel="sigmoid", cost=10, gamma=0.005, coef.0=0.5)
summary(svm_after_tune)
svm.pred <- predict(svm_after_tune, testdata)
svm.perf <- table(svm.pred, testclass)
(sum(svm.perf)-sum(diag(svm.perf)))/nrow(testdata) 
#11%
