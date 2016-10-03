data <- read.table("prostate.data")
reg.lpsa <- lm(data$lpsa~data$lcavol+data$lweight+data$age+data$lbph+data$svi+data$lcp+data$gleason+data$pgg45)
#Significativement non nuls : lcavol, svi, weight(?)

inter <- confint(reg.lpsa) # Bornes de l'intervalle de confiance 

plot(reg.lpsa$fitted.values, data$lpsa) # f(x0) correspond au fitted.values de reg.lpsa ?
plot(data$lpsa,reg.lpsa$residuals) # résidus bruts en fonction des yi
plot(data$lpsa, rstudent(reg.lpsa)) # résidus standardisés en fonction des yi
plot( reg.lpsa$fitted.values, reg.lpsa$residual) # vérification de la normalité des résidus, ok