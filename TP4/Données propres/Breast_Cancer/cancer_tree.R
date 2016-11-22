library(MASS)
tree.cancer=tree(as.matrix(train[33])~., data=traindata)
summary(tree.cancer) #18 noeuds terminaux, deviance : 374.7 (somme des squared errors)
plot(tree.cancer)
text(tree.cancer, pretty=0)

#Sur les données de test : 
yhat=predict(tree.cancer, newdata = testdata)
plot(yhat, testtime)
mean((yhat-testtime)^2) #1338


#Élagage :
cv.tree.cancer=cv.tree(tree.cancer)
plot(cv.tree.cancer$size, cv.tree.cancer$dev, type='b')
# Plus faible --> 4

prune.cancer=prune.tree(tree.cancer, best=4)
plot(prune.cancer)
text(prune.cancer, pretty=0)

yhatprune=predict(prune.cancer, newdata = testdata)
plot(yhatprune, testtime)
mean((yhatprune-testtime)^2) #931

