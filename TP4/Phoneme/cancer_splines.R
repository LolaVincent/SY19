library(splines2)





fit.splines<-lm(wage ̃bs(age,df=5),data= )

fit.splines <- lm()
ypred<-predict(fit,newdata=data.frame(age=18:80),interval="c")
plot(Wage$age,Wage$wage,cex=0.5,xlab="age",ylab="wage")
lines(18:80,ypred[,"fit"],lty=1,col="blue",lwd=2)
lines(18:80,ypred[,"lwr"],lty=2,col="blue",lwd=2)
lines(18:80,ypred[,"upr"],lty=2,col="blue",lwd=2)