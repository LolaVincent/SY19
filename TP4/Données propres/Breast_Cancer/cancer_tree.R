library(MASS)

#Sur l'ensemble d'apprentissage : 
tree.cancer=tree(as.matrix(train[33])~., data=traindata)
summary(tree.cancer) 
#18 noeuds terminaux, deviance : 374.7 (somme des squared errors)
plot(tree.cancer)
text(tree.cancer, pretty=0)

#Sur les données de test : 
yhat=predict(tree.cancer, newdata = testdata)
plot(yhat, testtime)
mean((yhat-testtime)^2) 
#MSE=1338


#Élagage :
cv.tree.cancer=cv.tree(tree.cancer)
plot(cv.tree.cancer$size, cv.tree.cancer$dev, type='b')
# Plus faible taux d'erreur pour 4 noeuds

#On élague donc à 4 : 
prune.cancer=prune.tree(tree.cancer, best=4)
plot(prune.cancer)
text(prune.cancer, pretty=0)

#Données de test après élagage :
yhatprune=predict(prune.cancer, newdata = testdata)
plot(yhatprune, testtime)
mean((yhatprune-testtime)^2) 
#MSE = 931, meilleurs résultats

