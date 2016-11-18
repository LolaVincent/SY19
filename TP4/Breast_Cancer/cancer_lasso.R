

cv.out.lasso<-cv.glmnet(as.matrix(traindata), as.matrix(traintime), alpha=1)
plot(cv.out.lasso)
fit.lasso<-glmnet(as.matrix(traindata), as.matrix(traintime), lambda=cv.out.lasso$lambda.min, alpha=0)
lasso.pred<-predict(fit.lasso,s=cv.out.lasso$lambda.min,newx=as.matrix(testdata))
mean((as.matrix(testtime)-lasso.pred)^2)  # 981.0911  
