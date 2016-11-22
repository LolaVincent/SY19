source("cancer_data.R")
#Multiple Linear Regression
lm.bc = lm(as.matrix(train[33])~. , data=traindata)
summary(lm.bc) #Grands residuals et très grand RSE=32
confint(lm.bc) # Bornes de l'intervalle de confiance
plot(fitted(lm.bc), as.matrix(train[33]), xlab="Fitted values", ylab="Time")  #valeurs prédites yhat en fonction des yi 
abline(0,1,col="red")
#--> pas de relation visible sur ce graphique

plot(as.matrix(train[33]), rstandard(lm.bc)) #rstandart bien entre -2 et 2
qqnorm(lm.bc$residuals) #Suit bien une distribution normale

plot(residuals(lm.bc), fitted(lm.bc)) #Pas de relation entre l'erreur et les Xi : OK

lm.pred <- predict(lm.bc, testdata)
summary(lm.pred)
plot(lm.pred, as.matrix(test[33]), xlab="Fitted values", ylab="Time")  #valeurs prédites yhat en fonction des yi 
abline(0,1,col="red")
mean((as.matrix(testtime)-lm.pred)^2)  # 1311

#Trop de prédicteurs pour pouvoir faire un choix, rendre la relation non-linéaire, etc
#--> subset selection
library(leaps)
reg.fit <- regsubsets(as.matrix(train[33])~., data=traindata, method='exhaustive', nvmax=32)
plot(reg.fit, scale='r2')
reg.summary <- summary(reg.fit)
reg.summary$rsq
plot(reg.summary$adjr2, xlab="Number of Variables", ylab="R^2", type="l") #Rsq en fonction de R2 ajusté
which.max(reg.summary$adjr2) #16 --> model avec le plus grand R2
points(14, reg.summary$adjr2[14], col="red", cex=2, pch=20)
plot(reg.summary$rss, xlab="Number of Variables", ylab="Residual Sum of Squares", type="l") #RSS en fonction de R2 ajusté
which.max(reg.summary$rss) #16 --> model avec le plus grand R2
#On cherche le plus grand R2 ou le plus faible RSS
#method='exhaustive' --> best subset selection
#method='forward' --> stepwise selection
#method='backward' --> stepwise selection

#test avec 16 : 
mlr.bc = lm(as.matrix(train[33])~texture_mean+area_mean+compactness_mean+concave_points_mean+symmetry_mean+ radius_se+ texture_se+ perimeter_se+smoothness_se+compactness_se+concavity_se+texture_worst+perimeter_worst+area_worst+smoothness_worst+fractal_dimension_worst, data=traindata)
#mlr.bc = lm(as.matrix(train[33])~texture_mean+smoothness_mean+compactness_mean+symmetry_mean+symmetry_mean+ radius_se+ texture_se+ perimeter_se+smoothness_se+concavity_se+symmetry_se+texture_worst+smoothness_worst+fractal_dimension_worst+Tumor_size, data=traindata)

summary(mlr.bc) # RSE=32%
mlr.pred <- predict(mlr.bc, testdata)
plot(mlr.pred, as.matrix(test[33]))  #valeurs prédites yhat en fonction des yi 
abline(0,1,col="red")
mean((as.matrix(testtime)-mlr.pred)^2)  # 1112

reg.fwd <- regsubsets(as.matrix(train[33])~., data=traindata, method='forward', nvmax=32)
plot(reg.fwd, scale='r2')
fwd.summary <- summary(reg.fwd)
plot(fwd.summary$adjr2, xlab="Number of Variables", ylab="Adjusted RSq", type="l") #RSS en fonction de R2 ajusté
which.max(fwd.summary$adjr2) #18 --> model avec le plus grand R2
points(18, fwd.summary$adjr2[18], col="red", cex=2, pch=20)
