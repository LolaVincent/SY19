#Arbres de décision
source(exp_faciales_data.R)
source(acp.R)
library(tree)

#On obtient des résultats très variables, pas de nette amélioration après élagage

tree.expr = tree(as.factor(data.acp.trainclass) ~ .,as.data.frame(data.acp.train[,1:20]))
tree.expr = tree(as.factor(data.acp.trainclass) ~ .,expressions)
#On n'oublie pas le as.factor() pour avoir un arbre de classification
summary(tree.expr)
#13 variables utilisées, 15 feuilles et 14% de taux d'erreur
plot(tree.expr)
text(tree.expr, pretty = 0)

tree.pred=predict(tree.expr, as.data.frame(data.acp.test[,1:20]), type="class")
tree.perf <- table(tree.pred, data.acp.testclass)
(sum(tree.perf)-sum(diag(tree.perf)))/nrow(data.acp.test) 

cv.tree.expr = cv.tree(tree.expr, FUN = prune.misclass) 
#Cross-validation pour trouver le meilleur élagage
#Taux d'erreur : $dev
cv.tree.expr$size[which.min(cv.tree.expr$dev)] #Nb idéal noeuds terminaux

par(mfrow=c(1,2))
plot(cv.tree.expr$size, cv.tree.expr$dev, type="b")
plot(cv.tree.expr$k, cv.tree.expr$dev, type="b")

prune.tree.expr = prune.misclass(tree.expr, best=9)
par(mfrow=c(1,1))
plot(prune.tree.expr)
text(prune.tree.expr, pretty=0)

tree.pred=predict(prune.tree.expr, as.data.frame(data.acp.test[,1:20]), type="class")
tree.perf <- table(tree.pred, data.acp.testclass)
(sum(tree.perf)-sum(diag(tree.perf)))/nrow(data.acp.test) 


