source("cancer_data.R")

library(pls)

#PCR :

# Construction du prédicteur avec les données d'apprentissage :
fit.pcr.train <- pcr(as.matrix(traintime) ~ as.matrix(traindata), scale=TRUE, validation="CV")

summary(fit.pcr.train)
#minimum RMSEP (=root mean squared error) = 4 comps avec 33.11, 
#74% de variance expliquée

validationplot(fit.pcr.train, val.type = "MSEP", legendpos = "topright")
# "CV" -> cross-validation estimate and "adjCV" (for RMSEP and MSEP) -> bias-corrected cross-validation estimate.
# C'est bien 4 qui a la plus petite MSE

#Prédiction :
pcr.pred=predict(fit.pcr.train, as.matrix(testdata), ncomp=4)
mean((pcr.pred-testtime)^2) #853
