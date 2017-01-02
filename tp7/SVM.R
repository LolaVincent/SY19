#SVM

df.train = as.data.frame(x=data.acp.train[,1:20], y=data.acp.trainclass)
df.test = as.data.frame(x=data.acp.test[,1:20], y=data.acp.testclass)

####RADIAL###
library(e1071)
svmfit = svm(as.factor(data.acp.trainclass)~., data=df.train)
summary(svmfit) 
#Kernel : radial, cost = 1, gamma = 0.05
svm.pred <- predict(svmfit, data.acp.test[,1:20])
svm.perf <- table(svm.pred, data.acp.testclass)
(sum(svm.perf)-sum(diag(svm.perf)))/nrow(data.acp.test) 
#33% d'erreur

svm_tune <- tune(svm, train.x=data.acp.train[,1:20], train.y=data.acp.trainclass, 
                 kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(0.0001, 0.001, 0.002, 0.005,0.01, 0.05,.5,1,2)))
svm_tune
#Cost : 10, gamma : 0.5

svm_after_tune <- svm(as.factor(data.acp.trainclass) ~ ., data=df.train, kernel="radial", cost=10, gamma=0.001)
summary(svm_after_tune)
svm.pred <- predict(svm_after_tune, data.acp.test[,1:20])
svm.perf <- table(svm.pred, data.acp.testclass)
(sum(svm.perf)-sum(diag(svm.perf)))/nrow(data.acp.test) 
#19% d'erreur

####LINAER###
svmfit = svm(as.factor(data.acp.trainclass)~., data=df.train, kernel="linear")
summary(svmfit) 
#Kernel : linear, cost = 1, gamma = 0.05
svm.pred <- predict(svmfit, data.acp.test[,1:20])
svm.perf <- table(svm.pred, data.acp.testclass)
(sum(svm.perf)-sum(diag(svm.perf)))/nrow(data.acp.test) 
#25% d'erreur

svm_tune <- tune(svm, train.x=data.acp.train[,1:20], train.y=data.acp.trainclass, 
                 kernel="linear", ranges=list(cost=10^(-1:2), gamma=c(0.00001, 0.00005,0.0001, 0.001)))
svm_tune
#Cost : 0.1, gamma : 1e-04

svm_after_tune <- svm(as.factor(data.acp.trainclass) ~ ., data=df.train, kernel="linear", cost=0.1, gamma=0.00001)
summary(svm_after_tune)
svm.pred <- predict(svm_after_tune, data.acp.test[,1:20])
svm.perf <- table(svm.pred, data.acp.testclass)
(sum(svm.perf)-sum(diag(svm.perf)))/nrow(data.acp.test) 
#18% d'erreur

####POLYNOMIAL###
svmfit = svm(as.factor(data.acp.trainclass)~., data=df.train, kernel="polynomial")
summary(svmfit) 
#Kernel : linear, cost = 1, degree=3 gamma = 0.05, coef=0
svm.pred <- predict(svmfit, data.acp.test[,1:20])
svm.perf <- table(svm.pred, data.acp.testclass)
(sum(svm.perf)-sum(diag(svm.perf)))/nrow(data.acp.test) 
#51% d'erreur

####SIGMOID###
svmfit = svm(as.factor(data.acp.trainclass)~., data=df.train, kernel="sigmoid")
summary(svmfit) 
#Kernel : sigmoid, cost = 1, gamma = 0.05, coef=0
svm.pred <- predict(svmfit, data.acp.test[,1:20])
svm.perf <- table(svm.pred, data.acp.testclass)
(sum(svm.perf)-sum(diag(svm.perf)))/nrow(data.acp.test) 
#15% d'erreur

svm_tune <- tune(svm, train.x=data.acp.train[,1:20], train.y=data.acp.trainclass, 
                 kernel="sigmoid", ranges=list(cost=10^(-1:4), gamma=c(0.00001,0.0001,0.005, 0.001,0.01,.5), coef=c(0.5, 1, 2, 3)))
svm_tune
#Cost : 1000, gamma : 0.0001, coef=1

svm_after_tune <- svm(as.factor(data.acp.trainclass) ~ ., data=df.train, kernel="sigmoid", cost=10000, gamma=0.0001, coef.0=3)
summary(svm_after_tune)
svm.pred <- predict(svm_after_tune, data.acp.test[,1:20])
svm.perf <- table(svm.pred, data.acp.testclass)
(sum(svm.perf)-sum(diag(svm.perf)))/nrow(data.acp.test) 
#18% ??

#Tester avec liste de kernels
svm_tune <- tune(svm, train.x=data.acp.train[,1:20], train.y=data.acp.trainclass, 
                 kernel="sigmoid", ranges=list(cost=10^(-1:2), gamma=c(0.00001, 0.00005,0.0001, 0.001)))

