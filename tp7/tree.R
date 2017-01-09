#Arbres de décision
source(exp_faciales_data.R)
source(acp.R)
library(tree)

nb_comp = 20

traindata <- data.frame(data.acp.train[,1:nb_comp], data.acp.trainclass)
traindata$data.acp.trainclass <- as.factor(traindata$data.acp.trainclass)
testdata <- data.frame(data.acp.test[,1:nb_comp])
testclass <- as.factor(data.acp.testclass)

tree.expr = tree(data.acp.trainclass ~ .,traindata)
summary(tree.expr)
plot(tree.expr)
text(tree.expr, pretty = 0)

#Prédiction sans élagage : 
tree.pred=predict(tree.expr, testdata, type="class")
tree.perf <- table(tree.pred, testclass)
(sum(tree.perf)-sum(diag(tree.perf)))/nrow(data.acp.test) 

cv.tree.expr = cv.tree(tree.expr, FUN = prune.misclass) 
#Cross-validation pour trouver le meilleur élagage
#Taux d'erreur : $dev
nb_nodes <- cv.tree.expr$size[which.min(cv.tree.expr$dev)] #Nb idéal noeuds terminaux

par(mfrow=c(1,2))
plot(cv.tree.expr$size, cv.tree.expr$dev, type="b")
plot(cv.tree.expr$k, cv.tree.expr$dev, type="b")

prune.tree.expr = prune.misclass(tree.expr, best=10)
par(mfrow=c(1,1))
plot(prune.tree.expr)
text(prune.tree.expr, pretty=0)

tree.pred=predict(prune.tree.expr, testdata, type="class")
tree.perf <- table(tree.pred, testclass)
(sum(tree.perf)-sum(diag(tree.perf)))/nrow(data.acp.test) 

########################################################################################################################

traindata <- data.frame(data.train, data.trainclass)
traindata$data.trainclass <- as.factor(traindata$data.trainclass)
testdata <- data.frame(data.test)
testclass <- as.factor(data.testclass)

tree.expr = tree(data.trainclass ~ .,traindata)
summary(tree.expr)
plot(tree.expr)
text(tree.expr, pretty = 0)

#Prédiction sans élagage : 
tree.pred = predict(tree.expr, testdata, type="class")
tree.perf <- table(tree.pred, testclass)
(sum(tree.perf)-sum(diag(tree.perf)))/nrow(data.acp.test) 

cv.tree.expr = cv.tree(tree.expr, FUN = prune.misclass) 
#Cross-validation pour trouver le meilleur élagage
#Taux d'erreur : $dev
nb_nodes <- cv.tree.expr$size[which.min(cv.tree.expr$dev)] #Nb idéal noeuds terminaux

par(mfrow=c(1,2))
plot(cv.tree.expr$size, cv.tree.expr$dev, type="b")
plot(cv.tree.expr$k, cv.tree.expr$dev, type="b")

prune.tree.expr = prune.misclass(tree.expr, best=12)
par(mfrow=c(1,1))
plot(prune.tree.expr)
text(prune.tree.expr, pretty=0)

tree.pred=predict(prune.tree.expr, testdata, type="class")
tree.perf <- table(tree.pred, testclass)
(sum(tree.perf)-sum(diag(tree.perf)))/nrow(data.acp.test) 
