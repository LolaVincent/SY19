library('splines')
fit<-lm(as.matrix(train[33])~bs(.),data=traindata)
ypred<-predict(fit,newdata=testdata,se=T)
mean((as.matrix(testtime)-ypred$fit)^2)  # 1311

#Avec subset selection : 
fit2<-lm(as.matrix(train[33])~texture_mean+area_mean+compactness_mean+concave_points_mean+symmetry_mean+ radius_se+ texture_se+ perimeter_se+smoothness_se+compactness_se+concavity_se+texture_worst+perimeter_worst+area_worst+smoothness_worst+fractal_dimension_worst,data=traindata)
ypred2<-predict(fit2,newdata=testdata,se=T)
mean((as.matrix(testtime)-ypred2$fit)^2)  # 1112

plot(as.data.frame(ypred)$fit, as.data.frame(test[33])$Time)

bc.fit<-smooth.spline(as.matrix(train[33]), train$texture_mean+train$area_mean+train$compactness_mean+train$concave_points_mean+train$symmetry_mean+ train$radius_se+ train$texture_se+ train$perimeter_se+train$smoothness_se+train$compactness_se+train$concavity_se+train$texture_worst+train$perimeter_worst+train$area_worst+train$smoothness_worst+train$fractal_dimension_worst, cv=TRUE)
lines(bc.fit, col="blue", lwd=2)