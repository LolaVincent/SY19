source(cancer_data.R)

library(glmnet)


cv.out.ridge <-cv.glmnet(as.matrix(traindata), as.matrix(traintime), alpha=0)
plot(cv.out.ridge)
fit.ridge<-glmnet(as.matrix(traindata), as.matrix(traintime), lambda=cv.out.ridge$lambda.min, alpha=0)
ridge.pred<-predict(fit.ridge, s=cv.out.ridge$lambda.min, newx=as.matrix(testdata))
mean((as.matrix(testtime)-ridge.pred)^2)  # 917 mean squared error 



# après PCR  (utile ?)
cv.out.ridge.cpr <- cv.glmnet(as.matrix(fit.pcr.train$scores), as.matrix(traintime), alpha=0)
plot(cv.out.ridge.cpr)
fit.ridge.cpr <- glmnet(as.matrix(fit.pcr.train$scores), as.matrix(traintime), lambda=cv.out.ridge.cpr$lambda.min, alpha=0)
ridge.pred.cpr <-predict(fit.ridge.cpr, s=cv.out.ridge.cpr$lambda.min, newx=as.matrix(fit.pcr.test$scores))
mean((as.matrix(testtime)-ridge.pred.cpr)^2) #1051