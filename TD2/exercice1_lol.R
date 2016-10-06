data <- read.table("prostate.data")
reg.lpsa <- lm(data$lpsa~data$lcavol+data$lweight+data$age+data$lbph+data$svi+data$lcp+data$gleason+data$pgg45)
reg.lpsa <- lm(data$lpsa~ ., data=data)
plot(reg.lpsa)
#Significativement non nuls : lcavol, svi, weight(?)

inter <- confint(reg.lpsa) # Bornes de l'intervalle de confiance 

#3
plot( data$lpsa, reg.lpsa$fitted.values) # f(x0) correspond au fitted.values de reg.lpsa ? --> yhati=valeurs prédites=fitted values 
#data$lpsa --> f(x)

#4
#Résultats Lola
plot(data$lpsa, residuals(res.lpsa)) # résidus bruts en fonction des yi
plot(data$lpsa, rstudent(reg.lpsa)) # résidus standardisés en fonction des yi
#Résultats Ana : 
plot(data$lpsa, residuals(reg.lpsa))
plot(data$lpsa, rstandard(reg.lpsa)) 
plot(data$lpsa, rstudent(reg.lpsa))
plot(data$lweight, rstandard(reg.lpsa)) 
plot(data$age, rstudent(reg.lpsa))
plot(data$lcp, rstudent(reg.lpsa))

#If the errors are normal, the standardized residuals should be aproximately normally distributed, between -2 & 2

#5
qqnorm(reg.lpsa$residuals) #Diagramme quantile-quantile : on compare avec une loi normale. Normalement =
#Voir aussi dans plot(reg.lpsa) le graphique normal Q-Q
#Ici les points ne suivent pas tout à fait la distribution normale

#6 et 7: Stabilité de la régression
summary(reg.lpsa) #Pour obtenir l'erreur
reg.nolcavol <- lm(data$lpsa~data$lweight+data$age+data$lbph+data$svi+data$lcp+data$gleason+data$pgg45)
reg.nolweight <- lm(data$lpsa~data$lcavol+data$age+data$lbph+data$svi+data$lcp+data$gleason+data$pgg45)
reg.nosvi <- lm(data$lpsa~data$lcavol+data$lweight+data$age+data$lbph+data$lcp+data$gleason+data$pgg45)
plot(reg.nolcavol)
plot(reg.nolweight) #Best
plot(reg.nosvi) # Pas de grand changement
reg.new <- lm(data$lpsa~data$age+data$lbph+data$lcp+data$gleason+data$pgg45) # moins bien donc ce doit être ncavol + weight
qqnorm(reg.nolweight$residuals)

plot(data$lweight, fitted(reg.lpsa)) # pas linéaire
plot((1/data$lweight), fitted(reg.lpsa)) # un peu plus linéaire
plot(data$lcavol, fitted(reg.lpsa)) # linéaire

#8
reg.test <- lm(data$lpsa~data$lcavol+log(data$lweight)+data$age+data$lbph+data$svi+data$lcp+data$gleason+data$pgg45)
#pas mieux
reg.test <- lm(data$lpsa~data$lcavol+sqrt(data$lweight)+data$age+data$lbph+data$svi+data$lcp+data$gleason+data$pgg45)
reg.test <- lm(data$lpsa~data$lcavol+1/(data$lweight)+data$age+data$lbph+data$svi+data$lcp+data$gleason+data$pgg45)
#mieux !
reg.test <- lm(data$lpsa~1/(data$lcavol)+data$age+data$lbph+data$svi+data$lcp+data$gleason+data$pgg45)
#PAs mieux
reg.test <- lm(data$lpsa~1/(data$lweight)+data$age+data$lbph+data$svi+data$lcp+data$gleason+data$pgg45)

#AUTRE
plot(residuals(reg.lpsa), fitted(reg.lpsa)) # As the error should not depend on the Xj --> no relation