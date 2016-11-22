source("cancer_data.R")

#lasso

#Détermination du lambda_min
cv.out.lasso<-cv.glmnet(as.matrix(traindata), as.matrix(traintime), alpha=1)
plot(cv.out.lasso)
cv.out.lasso$lambda.min #0.75

fit.lasso<-glmnet(as.matrix(traindata), as.matrix(traintime), lambda=cv.out.lasso$lambda.min, alpha=0)
lasso.pred<-predict(fit.lasso,s=cv.out.lasso$lambda.min,newx=as.matrix(testdata))

#Calcul du MSE ( mean squared error )
mean((as.matrix(testtime)-lasso.pred)^2)  #1168.793  
