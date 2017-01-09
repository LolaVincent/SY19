source("exp_faciales_data.R")

data_acp <- prcomp(data_not_null) 
# scale =TRUE données normées, pas princomp trop de variables par rapport au nombre d'individus
plot(data_acp$x, col=palette()[y])
legend(-1000, 2500 , title= "Expressions faciales", inset=.001,  c("1", "2", "3","4","5", "6"), fill=palette(), horiz=TRUE, xpd=TRUE)
pairs(data_acp$x[,1:5], col=palette()[y])
# variance cumulées avec 2 composates : 21 %, 21 composantes : 70%
# avec le pot, on distingue les groupes mais difficile

acp.data_not_null <- prcomp(data_not_null)
data.acp.train <- acp.data_not_null$x[dtrain,]
data.acp.test <- acp.data_not_null$x[-dtrain,]
data.acp.trainclass <- y[dtrain,]
data.acp.testclass <- y[-dtrain,]

