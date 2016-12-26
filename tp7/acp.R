source(exp_faciales_data.R)



data_acp <- prcomp(data_not_null) 
# scale =TRUE données normées, pas princomp trop de variables par rapport au nombre d'individus
plot(data_acp$x, col=palette()[y])
legend(-1000, 2500 , title= "Expressions faciales", inset=.001,  c("1", "2", "3","4","5", "6"), fill=palette(), horiz=TRUE, xpd=TRUE)
pairs(data_acp$x[,1:5], col=palette()[y])
# variance cumulées avec 2 composates : 21 %, 21 composantes : 70%
# avec le pot, on distingue les groupes mais difficile

# on peut les faire pour les ensembles de tests et d'apprentissage 

train.acp <- prcomp(data.train, cor=T)
plot(train.acp$x, col=palette()[y])
pairs(train.acp$x[,1:5], col=palette()[y])
# variance cumulées avec 2 composates : 21 %, 19 composantes : 70%

test.acp <- prcomp(data.test)
plot(test.acp$x, col=palette()[y])
pairs(test.acp$x[,1:5], col=palette()[y])
# variance cumulées avec 2 composates : 23 %, 15 composantes : 70%

# compliqué d'identifier des groupes dans les plot. 

#mais plus intéressant de faire l'acp et après de séparer 
acp.data_not_null <- prcomp(data_not_null)
n <- nrow(acp.data_not_null$x)
ntrain <- floor(2/3*n)
ntest <- n - ntrain
dtrain <- sample(1:n, ntrain)
data.acp.train <- acp.data_not_null$x[dtrain,]
data.acp.test <- acp.data_not_null$x[-dtrain,]
data.acp.trainclass <- y[dtrain,]
data.acp.testclass <- y[-dtrain,]

