#Données : prostate
#Var à expliquer : LPSA
#Sélection d'un modèle linéaire avec sous-ensemble de var explicatives
#--> Critère BIC
#Splines naturelles ou de lissage
#Sélection graphique des variables explicatives
#Meilleur modèle par validation croisée

#Chargement des données : 
prostate <- read.table("prostate.data")
prostate <- prostate[,-10]

#Subset selection : 
library(leaps)
reg.fit <- regsubsets(lpsa~., prostate)
plot(reg.fit, scale="bic") 
#BIC min : 3 var = lcavol + lweight + svi

#Autre méthode : 
reg.summary <- summary(reg.fit)
plot(reg.summary$bic, type="l")
#Le BIC est nettement inférieur pour 3 variables
reg.summary
#Le meilleur modèle avec 3 variables : lcavol + lweight + svi


folds <- sample(1:5, nrow(prostate), replace=TRUE)
perf <- 0
for(i in 1:5) {
  app <- prostate[(folds!=i),]
  test <- prostate[(folds==i),]
  testvar <- cbind(test[1:2], test[5])
  #Splines naturelles : 
  spline.ns = lm(lpsa~ns(lcavol) + ns(lweight) + ns(svi), data=app)
  pred.ns<-predict(spline.ns,newdata=testvar,interval="c")
  perf <- perf + mean((test$lpsa-pred.ns)^2)
}
perf/5

