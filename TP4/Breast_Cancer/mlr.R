source("cancer_data.R")
#Multiple Linear Regression
lm.bc = lm(as.matrix(train[33])~. , data=traindata)
summary(lm.bc) #Grands residuals et très grand RSE=32
confint(lm.bc) # Bornes de l'intervalle de confiance
plot(fitted(lm.bc), as.matrix(train[33]))  #valeurs prédites yhat en fonction des yi 
#--> pas de relation visible sur ce graphique

plot(as.matrix(train[33]), rstandard(lm.bc)) #rstandart bien entre -2 et 2
qqnorm(lm.bc$residuals) #Suit bien une distribution normale

plot(residuals(lm.bc), fitted(lm.bc)) #Pas de relation entre l'erreur et les Xi : OK

h <- predict(lm.bc, testdata, interval="confidence")
summary(h)

#Trop de prédicteurs pour pouvoir faire un choix, rendre la relation non-linéaire, etc
#--> subset selection
library(leaps)
reg.fit <- regsubsets(as.matrix(train[33])~., data=traindata, method='exhaustive', nvmax=32)
plot(reg.fit, scale='r2')
reg.summary <- summary(reg.fit)
reg.summary$rsq
plot(reg.summary$adjr2, xlab="Number of Variables", ylab="Adjusted RSq", type="l") #RSS en fonction de R2 ajusté
which.max(reg.summary$adjr2) #16 --> model avec le plus grand R2
points(16, reg.summary$adjr2[16], col="red", cex=2, pch=20)
plot(reg.summary$rss, xlab="Number of Variables", ylab="Adjusted RSq", type="l") #RSS en fonction de R2 ajusté
which.max(reg.summary$rss) #16 --> model avec le plus grand R2
points(16, reg.summary$rss[16], col="red", cex=2, pch=20)
#On cherche le plus grand R2 ou le plus faible RSS
#method='exhaustive' --> best subset selection
#method='forward' --> stepwise selection
#method='backward' --> stepwise selection

#test avec 16 : 
lm.bc = lm(as.matrix(train[33])~texture_mean+area_mean+compactness_mean+concave_points_mean+symmetry_mean+ radius_se+ texture_se+ perimeter_se+smoothness_se+compactness_se+concavity_se+texture_worst+perimeter_worst+area_worst+smoothness_worst+fractal_dimension_worst, data=traindata)
summary(lm.bc) # RSE=32%
plot(fitted(lm.bc), as.matrix(train[33]))  #valeurs prédites yhat en fonction des yi 
#--> pas de relation visible sur ce graphique
reg.pred <- predict(lm.bc, testdata, interval="confidence")

reg.fwd <- regsubsets(as.matrix(train[33])~., data=traindata, method='forward', nvmax=32)
plot(reg.fwd, scale='r2')
fwd.summary <- summary(reg.fwd)
plot(fwd.summary$adjr2, xlab="Number of Variables", ylab="Adjusted RSq", type="l") #RSS en fonction de R2 ajusté
which.max(fwd.summary$adjr2) #18 --> model avec le plus grand R2
points(18, fwd.summary$adjr2[18], col="red", cex=2, pch=20)
