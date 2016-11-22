source("cancer_data.R")

#Multiple Linear Regression : 

#Sur l'ensemble d'apprentissage : 
lm.bc = lm(as.matrix(train[33])~. , data=traindata)
summary(lm.bc) 
#Grands residuals et RSE=32
confint(lm.bc) # Bornes de l'intervalle de confiance

#Plot des valeurs prédites yhat en fonction des yi :
plot(fitted(lm.bc), as.matrix(train[33]), xlab="Fitted values", ylab="Time")  
abline(0,1,col="red")
#Il n'y a pas de relation forte visible

#Vérifications : 
plot(as.matrix(train[33]), rstandard(lm.bc)) #rstandart bien entre -2 et 2
qqnorm(lm.bc$residuals) #Suit bien une distribution normale
plot(residuals(lm.bc), fitted(lm.bc)) #Pas de relation entre l'erreur et les Xi : OK

#Prédiction sur l'ensemble de test : 
lm.pred <- predict(lm.bc, testdata)
summary(lm.pred)
#valeurs prédites yhat en fonction des yi :
plot(lm.pred, as.matrix(test[33]), xlab="Fitted values", ylab="Time")
abline(0,1,col="red")
mean((as.matrix(testtime)-lm.pred)^2)  
#MSE = 1311


#Subset selection
library(leaps)
reg.fit <- regsubsets(as.matrix(train[33])~., data=traindata, method='exhaustive', nvmax=32)
plot(reg.fit, scale='r2')
reg.summary <- summary(reg.fit)
reg.summary$rsq
#On cherche le plus grand R2 ou le plus faible RSS
plot(reg.summary$adjr2, xlab="Number of Variables", ylab="R^2", type="l") #Rsq en fonction de R2 ajusté
which.max(reg.summary$adjr2) #16 --> model avec le plus grand R2
points(16, reg.summary$adjr2[16], col="red", cex=2, pch=20)

plot(reg.summary$rss, xlab="Number of Variables", ylab="Residual Sum of Squares", type="l") #RSS en fonction de R2 ajusté

#Régression linéaire multiple sur un subset de 16 variables :
mlr.bc = lm(as.matrix(train[33])~texture_mean+area_mean+compactness_mean+concave_points_mean+symmetry_mean+ radius_se+ texture_se+ perimeter_se+smoothness_se+compactness_se+concavity_se+texture_worst+perimeter_worst+area_worst+smoothness_worst+fractal_dimension_worst, data=traindata)

summary(mlr.bc) # RSE=30
mlr.pred <- predict(mlr.bc, testdata)
#valeurs prédites yhat en fonction des yi :
plot(mlr.pred, as.matrix(test[33]))  
abline(0,1,col="red")
mean((as.matrix(testtime)-mlr.pred)^2)  
#MSE = 1112

