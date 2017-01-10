#Arbres de décision
source(exp_faciales_data.R)
source(acp+afd+adl.R)
library(tree)

traindata <- df.afd.train
testdata <- afd.test
testclass <- afd.testclass

#Arbre ACP+ AFD : 
tree.expr = tree(traindata$y ~ .,traindata)
summary(tree.expr)
plot(tree.expr)
text(tree.expr, pretty = 0)

#Prédiction sans élagage : 
tree.pred=predict(tree.expr, testdata, type="class")
tree.perf <- table(tree.pred, testclass)
(sum(tree.perf)-sum(diag(tree.perf)))/nrow(testdata) 
#12% d'erreur

#Elagage : 
cv.tree.expr = cv.tree(tree.expr, FUN = prune.misclass) 
#Cross-validation pour trouver le meilleur élagage
#Taux d'erreur : $dev
nb_nodes <- cv.tree.expr$size[which.min(cv.tree.expr$dev)] #Nb idéal noeuds terminaux

par(mfrow=c(1,2))
plot(cv.tree.expr$size, cv.tree.expr$dev, type="b")
plot(cv.tree.expr$k, cv.tree.expr$dev, type="b")

#On élague au nombre de noeuds conseillés
prune.tree.expr = prune.misclass(tree.expr, best=nb_nodes)
summary(prune.tree.expr)
par(mfrow=c(1,1))
plot(prune.tree.expr)
text(prune.tree.expr, pretty=0)

#Prédiction après élagage :
tree.pred=predict(prune.tree.expr, testdata, type="class")
tree.perf <- table(tree.pred, testclass)
(sum(tree.perf)-sum(diag(tree.perf)))/nrow(testdata) 
#5%

par(mfrow=c(1,2))
plot(tree.expr)
text(tree.expr, pretty = 0)
plot(prune.tree.expr)
text(prune.tree.expr, pretty=0)
