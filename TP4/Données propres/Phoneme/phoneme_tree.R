source("phomene_AFD.R")
source("phomene_AFD.R")

#Classification tree : 
library(tree)

#On construit l'arbre depuis l'ensemble d'apprentissage : 
phoneme.tree=tree(app$g~ ., data=app) 
summary(phoneme.tree) #Missclassification error rate : 11%
#Plot :
plot(phoneme.tree)
text(phoneme.tree, pretty=0)

#Calcul de l'erreur depuis l'ensemble de test
tree.pred=predict(phoneme.tree, test, type="class")
tree.perf <- table(tree.pred, test$g)
(sum(tree.perf)-sum(diag(tree.perf)))/nrow(test) 
# 14% d'erreur

#On élague l'arbre pour déterminer le niveau de complexité optimal de l'arbre
phoneme.cv.tree= cv.tree(phoneme.tree, FUN=prune.misclass)
phoneme.cv.tree
#Size : number of terminal node, dev: cross validation error rate, k : cost-complexity parameter used
par(mfrow=c(1,2))
plot(phoneme.cv.tree$size, phoneme.cv.tree$dev, type="b")
plot(phoneme.cv.tree$k, phoneme.cv.tree$dev, type="b")
#Plus petit dev: size=11 ou 12, on a donc déjà l'arbre optimal. 


#Classification tree près AFD : 
phoneme.tree.afd=tree(app$g~ ., data=as.data.frame(Z)) 
summary(phoneme.tree.afd) #Missclassification error rate : 5%
#Plot : 
plot(phoneme.tree.afd)
text(phoneme.tree.afd, pretty=0)

#Calcul de l'erreur depuis l'ensemble de test
tree.pred.afd=predict(phoneme.tree.afd,as.data.frame(Ztest) , type="class")
tree.perf.afd <- table(tree.pred.afd, test$g)
(sum(tree.perf.afd)-sum(diag(tree.perf.afd)))/nrow(test) # 4,5% d'erreur

#pruning
phoneme.cv.tree.afd= cv.tree(phoneme.tree.afd, FUN=prune.misclass)
phoneme.cv.tree.afd
#Plus petit dev: size=5, donc on a déjà l'arbre optimal.

#Grâce à l'AFD on est passé de 14 à 4.5% d'erreur