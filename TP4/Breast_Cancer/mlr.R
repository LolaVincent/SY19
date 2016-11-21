source("cancer_data.R")
#Multiple Linear Regression
lm.bc = lm(as.matrix(train[33])~. , data=traindata)
summary(lm.bc) #Grands residuals et très grand RSE=32%
confint(lm.bc) # Bornes de l'intervalle de confiance
plot(fitted(lm.bc), as.matrix(train[33]))  #valeurs prédites yhat en fonction des yi 
#--> pas de relation visible sur ce graphique

plot(as.matrix(train[33]), rstandard(lm.bc)) #rstandart bien entre -2 et 2
qqnorm(lm.bc$residuals) #Suit bien une distribution normale

plot(residuals(lm.bc), fitted(lm.bc)) #Pas de relation entre l'erreur et les Xi : OK

h <- predict(lm.bc, testdata, interval="confidence")
summary(h)