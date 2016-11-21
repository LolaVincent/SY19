library(pls)

fit.pcr <- pcr(as.matrix(breastcan[,33]) ~ as.matrix(breastcan[,1:32]), scale=TRUE, validation="CV")

summary(fit.pcr) #minimum RMSEP (=root mean squared error) = 4comps avec 31.80
#73% de variance expliquée, pas mal

validationplot(fit.pcr, val.type = "MSEP", legendpos = "topright") 
#C'est bien 4 qui a la plus petite MSE

fit.pcr.train <- pcr(as.matrix(traintime) ~ as.matrix(traindata), scale=TRUE, validation="CV")
summary(fit.pcr.train)
validationplot(fit.pcr.train, val.type = "MSEP", legendpos = "topright")

#Sur les données de test : 
pcr.pred=predict(fit.pcr.train, as.matrix(testdata), ncomp=4)
mean((pcr.pred-testtime)^2)


