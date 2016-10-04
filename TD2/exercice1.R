data <- read.table("prostate.data")
reg.lpsa <- lm(data$lpsa~data$lcavol+data$lweight+data$age+data$lbph+data$svi+data$lcp+data$gleason+data$pgg45)
#Significativement non nuls : lcavol, svi, weight(?)

inter <- confint(reg.lpsa) # Bornes de l'intervalle de confiance 

plot(reg.lpsa$fitted.values, data$lpsa) # f(x0) correspond au fitted.values de reg.lpsa ? --> yhati=valeurs prédites=fitted values 
#data$lpsa --> f(x)

#4
plot(data$lpsa,reg.lpsa$residuals) # résidus bruts en fonction des yi
plot(data$lpsa, rstudent(reg.lpsa)) # résidus standardisés en fonction des yi
#Résultats Ana : 
plot(residuals(reg.lpsa), data$lpsa)
plot(rstandard(reg.lpsa), data$lpsa) 
plot(rstudent(reg.lpsa), data$lpsa)
#If the errors are normal, the standardized residuals should be aproximately normally distributed, between -2 & 2

#5
plot( reg.lpsa$fitted.values, reg.lpsa$residual) # vérification de la normalité des résidus, ok
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