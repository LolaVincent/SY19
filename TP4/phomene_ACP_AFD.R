#3 classifieurs : ACP, AFD, ACP+AFD --> régression logistique MULTINOMIALE (glmnet)
# Ne pas utiliser ADL sur AFD mais on peut utiliser AFD et ADQ 
#Calculer la performance du classifieur en faisant des ensembles de test/apprentissage
# regression logistique ok 

#Récupération des données :
phoneme <- read.table("phoneme.data", sep = ",", header = TRUE)

#Ensembles de test/apprentissage
n <- nrow(phoneme)
ntrain <- floor(2/3*n)
ntest <- n - ntrain
dtrain <- sample(1:n, ntrain)
phoneme.train <- phoneme[dtrain,]
phoneme.test <- phoneme[-dtrain,]
phoneme.trainquant <- phoneme.train[,2:257]
phoneme.testquant <- phoneme.test[,2:257]
phoneme.trainclass <- phoneme.train[,258]
phoneme.testclass <- phoneme.test[,258]

#ACP : 
acp.train <- princomp(phoneme.trainquant) #Prétraitement --> réduction variables
acp.test <- princomp(phoneme.testquant)

#ACP sur composantes 1 et 2 :
plot(acp.train$scores, pch=16, col=palette()[phoneme.train$g])
abline(h=0, v=0) #3 groupes qui se distinguent
legend("topright", inset=.05, title="Phoneme", c("aa", "ao", "dcl","iy","sh"), fill=palette(), horiz=FALSE)

#ACP sur composantes 2 et 3 :
plot(acp.train$scores[,c(2,3)], pch=16, col=palette()[phoneme.train$g])
abline(h=0, v=0) #3 groupes qui se distinguent
legend("topright", inset=.05, title="Phoneme", c("aa", "ao", "dcl","iy","sh"), fill=palette(), horiz=FALSE)


#knn avant ACP  
#m <- matrix(nrow=10, ncol=3)
#for(k in 1:10) {
  #for(i in 1:3) {
    #phoneme.knn <- knn(train = phoneme.trainquant, test = phoneme.testquant, cl = phoneme.trainclass, k=k)
    #perf.knn <- table(phoneme.testclass, phoneme.knn) 
    #Il est intéressant de voir en affichant perf.knn que tous les >0 hors diag concernent aa et ao
    #m[k,i]<-(sum(perf.knn)-sum(diag(perf.knn)))/ntest #9%
  #}
#}
#rowMeans(m) #Si on veut comparer en fonction de k. Je n'ai mis que jusqu'à 3 parce que ça prend bcp de tps
#boxplot(t(m)) #k=9 intéressant

#knn après ACP 
phoneme.knn.acp <- knn(train = acp.train$scores, test = acp.test$scores, cl = phoneme.trainclass, k=5)
phoneme.knn.acp 
perf.knn.acp <- table(phoneme.test[,258], phoneme.knn.acp)
(sum(perf.knn.acp)-sum(diag(perf.knn.acp)))/ntest #15%
#ACP permet de supprimer le bruit 
#puis AFD sélectionne les var les + discriminantes possibles parmi elles

#AFD seule : 
library(MASS)
phonfactor <- as.factor(phoneme$g)
lda.phoneme<-lda(phoneme$g~. ,data=phoneme_quant)
U <- lda.phoneme$scaling
X <- as.matrix(phoneme_quant)
Z <- X%*%U

#Composantes de l'AFD à utiliser : 
cp1 <- 1
cp2 <- 2

plot(Z[phoneme$g=="aa",cp1],Z[phoneme$g=="aa",cp2],xlim=range(Z[,1]),ylim=range(Z[,2]),xlab="Z1",ylab="Z2")
points(Z[phoneme$g=="ao",cp1],Z[phoneme$g=="ao",cp2],pch=2,col="blue")
points(Z[phoneme$g=="dcl",cp1],Z[phoneme$g=="dcl",cp2],pch=3,col="red")
points(Z[phoneme$g=="iy",cp1],Z[phoneme$g=="iy",cp2],pch=4,col="pink")
points(Z[phoneme$g=="sh",cp1],Z[phoneme$g=="sh",cp2],pch=5,col="yellow")
legend("topleft", inset=.05, title="Phoneme", c("aa", "ao", "dcl","iy","sh"), fill=c("black","blue","red","pink","yellow"), horiz=TRUE)

#AFD en plus de l'ACP
lda.phoneme2<-lda(phoneme$g~. ,data=as.data.frame(acp$scores[,1:5]))
U2 <- lda.phoneme2$scaling
X2 <- as.matrix(acp$scores[,1:5])
Z2 <- X2%*%U2

plot(Z2[phoneme$g=="aa",cp1],Z2[phoneme$g=="aa",cp2],xlim=range(Z2[,1]),ylim=range(Z2[,2]),xlab="Z1",ylab="Z2")
points(Z2[phoneme$g=="ao",cp1],Z2[phoneme$g=="ao",cp2],pch=2,col="blue")
points(Z2[phoneme$g=="dcl",cp1],Z2[phoneme$g=="dcl",cp2],pch=3,col="red")
points(Z2[phoneme$g=="iy",cp1],Z2[phoneme$g=="iy",cp2],pch=4,col="pink")
points(Z2[phoneme$g=="sh",cp1],Z2[phoneme$g=="sh",cp2],pch=5,col="yellow")
legend("topleft", inset=.05, title="Phoneme", c("aa", "ao", "dcl","iy","sh"), fill=c("black","blue","red","pink","yellow"), horiz=TRUE)       

#Avec ACP + AFD on a une meilleure séparation des phonemes dcl et iy
# Pour sélectionner la meilleure méthode de réducteur de dimensions, on peut regarder le taux d'erreur des Kppv

