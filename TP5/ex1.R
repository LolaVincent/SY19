library(MASS)

#Question 1
data <- mcycle
times <- data[,-2]
summary(data)
pred <- matrix(ncol=133)
for(p in 1:5) {
  reg.mcycle <- lm(accel ~ I(times^p), data=data)
  #ou : 
  #reg.mcycle <- lm(accel ~ poly(times, p), data=data)
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
folds <- sample(1:K, nrow(data), replace=TRUE)
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
library(splines)
K <-5
P <- 10
folds <- sample(1:K, nrow(data), replace=TRUE)
result.spline <- vector(length = P)
for(p in 1:P) {
  yperf <- 0
  for(i in 1:K) {
    train <- data[(folds!=i),]
    test <- data[(folds==i),]
    fit<-lm(accel ~ ns(times,df=p), data=train)
    ypred<-predict(fit,newdata=test,interval="c")
    plot(data$times,data$accel,xlim=range(test$times))
    lines(test$times,test$accel)
    lines(test$times,ypred[,"fit"],lty=1,col="red",lwd=2)
    yperf <- yperf + mean((data[(folds==i),]$accel-ypred)^2)
  }
  result.spline[p] <- yperf/K
}
plot(result.spline)
which.min(result.spline) #9 degrés = best

#Question 4

#Cross-validation automatique, mais est-ce du leave-one-out ? 
fit.smooth <-smooth.spline(data$times, data$accel)
fit.smooth$df #12.20
plot(data$times,data$accel,xlim=range(test$times))
lines(fit.smooth, col="red", lwd=2)
fit.smooth <-smooth.spline(data$times, data$accel, cv=TRUE)
fit.smooth$lev
fit.smooth$cv.crit

K <-nrow(data) #Méthode du leave-one-out
dfmax <- 15
folds <- sample(1:K, nrow(data), replace=FALSE)
result.smooth <- vector(length = dfmax - 10)
for(DF in 10:dfmax) {
  perf.smooth <- 0
  for(i in 1:K) {
    train <- data[(folds!=i),]
    test <- data[(folds==i),]
    fit.smooth <-smooth.spline(train$times, train$accel, df=DF)
    pred.smooth<-predict(fit.smooth,newdata=test$accel,interval="c")
    perf.smooth <- perf.smooth + mean((data[(folds==i),]$accel-pred.smooth$y)^2)
  }
  result.smooth[DF-10] <- perf.smooth/K
} #Marche paaaas :(( Comment on calcule l'erreur ??

#On tire le même ensemble
folds <- sample(1:5, nrow(data), replace=TRUE)
app <- data[(folds!=1),]
test <- data[(folds==1),]
x <- test$times
#Régression polynomiale pour P=2
reg.mcycle <- lm(accel ~ poly(times, 2), data=app)
pred.mcycle <- predict(reg.mcycle, newdata = list(x))
#Splines pour DF=9
fit<-lm(accel ~ ns(times,df=9), data=app)
ypred<-predict(fit,newdata=test,interval="c")
#smooth
fit.smooth <-smooth.spline(data$times, data$accel)

par(mfrow=c(2,2))
plot(data$times,data$accel,xlim=range(test$times))
lines(test$times,test$accel)
lines(data$times,pred.mcycle,lty=1,col="red",lwd=2)
#splines
plot(data$times,data$accel,xlim=range(test$times))
lines(test$times,test$accel)
lines(test$times,ypred[,"fit"],lty=1,col="red",lwd=2)
#Smooth splines
plot(data$times,data$accel,xlim=range(test$times))
lines(test$times,test$accel)
lines(fit.smooth, col="red", lwd=2)
