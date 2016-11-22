source("cancer_data.R")

#Ridge :

library(glmnet)

# Détermination du lambda_min
cv.out.ridge <-cv.glmnet(as.matrix(traindata), as.matrix(traintime), alpha=0)
plot(cv.out.ridge)
cv.out.ridge$lambda.min #24.0783

fit.ridge<-glmnet(as.matrix(traindata), as.matrix(traintime), lambda=cv.out.ridge$lambda.min, alpha=0)
ridge.pred<-predict(fit.ridge, s=cv.out.ridge$lambda.min, newx=as.matrix(testdata))

#Calcul du MSE (mean squared error)
mean((as.matrix(testtime)-ridge.pred)^2)  # 998.858 mean squared error 
