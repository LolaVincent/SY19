reg <- lm(data$lpsa~. - train, data=data) # toutes les variables moins train

#2
confint(reg)
#3
plot(data$lpsa, rstandard(reg))
#4
qqnorm(resid(reg)) # pour vérifier la normalité
qqline(resid(reg))


reg <- lm(data$lpsa~. - train-lweight, data=data) # toutes les variables moins train et lweight
hist(hatvalues(reg))
hist(cooks.distance(reg)) # problèème si beaucoup plus grands que 1

#7
reg1 <- lm(data$lpsa ~ lcavol+svi+lweight, data=data)
#etc 
#8
reg2 <- lm(data$lpsa ~ lcavol+svi+I(lweight^2), data=data)
