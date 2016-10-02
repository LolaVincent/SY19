data <- read.table("prostate.data")
reg.lpsa <- lm(data$lpsa~data$lcavol+data$lweight+data$age+data$lbph+data$svi+data$lcp+data$gleason+data$pgg45)
#Significativement non nuls : lcavol, svi
confint(reg.lpsa) # Bornes de l'intervalle de confiance 