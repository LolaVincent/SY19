#3 classifieurs : ACP, AFD, ACP+AFD --> régression logistique MULTINOMIALE (glmnet)
# Ne pas utiliser ADL sur AFD mais on peut utiliser AFD et ADQ 
#Calculer la performance du classifieur en faisant des ensembles de test/apprentissage
# regression logistique ok 

source("phoneme_data.R")

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

#knn après ACP 
phoneme.knn.acp <- knn(train = acp.train$scores[,1:4], test = acp.test$scores[,1:4], cl = phoneme.trainclass, k=5)
perf.knn.acp <- table(phoneme.test[,258], phoneme.knn.acp)
(sum(perf.knn.acp)-sum(diag(perf.knn.acp)))/ntest 
#16% avec 2 premières composantes, 12% avec 4  

#ACP permet de supprimer le bruit 
#puis AFD sélectionne les var les + discriminantes possibles parmi elles




#AFD seule : 
library(MASS)
lda.phoneme<-lda(phoneme.train[,258]~. ,data=phoneme.trainquant)
U <- lda.phoneme$scaling
X <- as.matrix(phoneme.trainquant)
Z <- X%*%U

lda.phonemetest<-lda(phoneme.test[,258]~. ,data=phoneme.testquant)
Utest <- lda.phonemetest$scaling
Xtest <- as.matrix(phoneme.testquant)
Ztest <- Xtest%*%Utest

#Composantes de l'AFD à utiliser : 
cp1 <- 1
cp2 <- 2

plot(Z[phoneme.train[,258]=="aa",cp1],Z[phoneme.train[,258]=="aa",cp2],xlim=range(Z[,1]),ylim=range(Z[,2]),xlab="Z1",ylab="Z2", pch=16)
points(Z[phoneme.train[,258]=="ao",cp1],Z[phoneme.train[,258]=="ao",cp2],pch=16,col="blue")
points(Z[phoneme.train[,258]=="dcl",cp1],Z[phoneme.train[,258]=="dcl",cp2],pch=16,col="red")
points(Z[phoneme.train[,258]=="iy",cp1],Z[phoneme.train[,258]=="iy",cp2],pch=16,col="pink")
points(Z[phoneme.train[,258]=="sh",cp1],Z[phoneme.train[,258]=="sh",cp2],pch=16,col="yellow")
legend("topleft", inset=.05, title="Phoneme", c("aa", "ao", "dcl","iy","sh"), fill=c("black","blue","red","pink","yellow"), horiz=TRUE)

#knn après AFD
phoneme.knn.afd <- knn(train = Z, test = Ztest, cl = phoneme.trainclass, k=9)
perf.knn.afd <- table(phoneme.test[,258], phoneme.knn.afd)
(sum(perf.knn.afd)-sum(diag(perf.knn.afd)))/ntest #4.5%  



#AFD en plus de l'ACP
lda.phoneme2<-lda(phoneme.train[,258]~. ,data=as.data.frame(acp.train$scores[,1:5]))
U2 <- lda.phoneme2$scaling
X2 <- as.matrix(acp.train$scores[,1:5])
Z2 <- X2%*%U2

lda.phonemetest2<-lda(phoneme.test[,258]~. ,data=as.data.frame(acp.test$scores[,1:5]))
Utest2 <- lda.phonemetest2$scaling
Xtest2 <- as.matrix(acp.test$scores[,1:5])
Ztest2 <- Xtest2%*%Utest2

plot(Z2[phoneme.train[,258]=="aa",cp1],Z2[phoneme.train[,258]=="aa",cp2],xlim=range(Z2[,1]),ylim=range(Z2[,2]),xlab="Z1",ylab="Z2")
points(Z2[phoneme.train[,258]=="ao",cp1],Z2[phoneme.train[,258]=="ao",cp2],pch=2,col="blue")
points(Z2[phoneme.train[,258]=="dcl",cp1],Z2[phoneme.train[,258]=="dcl",cp2],pch=3,col="red")
points(Z2[phoneme.train[,258]=="iy",cp1],Z2[phoneme.train[,258]=="iy",cp2],pch=4,col="pink")
points(Z2[phoneme.train[,258]=="sh",cp1],Z2[phoneme.train[,258]=="sh",cp2],pch=5,col="yellow")
legend("topleft", inset=.05, title="Phoneme", c("aa", "ao", "dcl","iy","sh"), fill=c("black","blue","red","pink","yellow"), horiz=TRUE) 

#knn après AFD/ACP
phoneme.knn.afd2 <- knn(train = Z2, test = Ztest2, cl = phoneme.trainclass, k=9)
perf.knn.afd2 <- table(phoneme.test[,258], phoneme.knn.afd2)
(sum(perf.knn.afd2)-sum(diag(perf.knn.afd2)))/ntest #70%....  

#La meilleure méthode a adopter au vu des résultats sur KNN est l'AFD seule 
#car c'est elle qui offre une meilleure séparation des clusters
