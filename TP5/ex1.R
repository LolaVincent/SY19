library(MASS)

#Question 1
data <- mcycle
summary(data)
pred <- matrix(ncol=133)
for(p in 1:5) {
  reg.mcycle <- lm(accel ~ I(times^p), data=data)
  pred <- rbind(pred,fitted(reg.mcycle))
  #print(cooks.distance(reg.mcycle))
  plot(reg.mcycle)
  #m <- rbind(m,reg.mcycle)
}
pred[-1,]
moy <- colMeans(pred, na.rm = TRUE)
plot(moy)
#Satisfaisant visuellement : p=3, p=4

#Quesion 2
K <-5
P <- 10
folds <- sample(1:K, data, replace=TRUE)
result <- vector(length = p)
for(p in 1:P) {
  perf.mcycle <- 0
  for(i in 1:K) {
    reg.mcycle <- lm(accel ~ I(times^p), data=data[(folds!=i),])
    pred.mcycle <- predict(reg.mcycle, newdata = data[(folds==i),])
    perf.mcycle <- perf.mcycle + mean((data[(folds==i),]$accel-pred.mcycle)^2)
  }
  result[p] <- perf.mcycle/K
}

which.min(result)

#Quesion 3
K <-5
P <- 10
folds <- sample(1:K, data, replace=TRUE)
result <- vector(length = p)
for(p in 1:5) {
  for(i in 1:K) {
    test <- data[(folds==i),]
    train <- data[(folds!=i),]
  fit<-lm(accel ~ ns(times,df=p), data=train)
  ypred<-predict(fit,newdata=test,interval="c")
  plot(data$times,data$accel,xlim=range(test$times))
  lines(test$times,test$accel)
  lines(test$times,ypred[,"fit"],lty=1,col="red",lwd=2)
  }
}

