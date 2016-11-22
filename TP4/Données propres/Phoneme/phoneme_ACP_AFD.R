source("phoneme_data.R")
source("phoneme_ACP.R")
source("phoneme_AFD.R")

#AFD par dessus l'ACP

#Données d'apprentissage : 
lda.phoneme2<-lda(phoneme.train[,258]~. ,data=as.data.frame(acp.train$scores[,1:5]))
U2 <- lda.phoneme2$scaling
X2 <- as.matrix(acp.train$scores[,1:5])
Z2 <- X2%*%U2

#Données de test :
lda.phonemetest2<-lda(phoneme.test[,258]~. ,data=as.data.frame(acp.test$scores[,1:5]))
Utest2 <- lda.phonemetest2$scaling
Xtest2 <- as.matrix(acp.test$scores[,1:5])
Ztest2 <- Xtest2%*%Utest2

#Plot de l'AFD sur ACP : 
plot(Z2[phoneme.train[,258]=="aa",cp1],Z2[phoneme.train[,258]=="aa",cp2],xlim=range(Z2[,1]),ylim=range(Z2[,2]),xlab="Z1",ylab="Z2")
points(Z2[phoneme.train[,258]=="ao",cp1],Z2[phoneme.train[,258]=="ao",cp2],pch=2,col="blue")
points(Z2[phoneme.train[,258]=="dcl",cp1],Z2[phoneme.train[,258]=="dcl",cp2],pch=3,col="red")
points(Z2[phoneme.train[,258]=="iy",cp1],Z2[phoneme.train[,258]=="iy",cp2],pch=4,col="pink")
points(Z2[phoneme.train[,258]=="sh",cp1],Z2[phoneme.train[,258]=="sh",cp2],pch=5,col="yellow")
legend("topleft", inset=.05, title="Phoneme", c("aa", "ao", "dcl","iy","sh"), fill=c("black","blue","red","pink","yellow"), horiz=TRUE) 

#KNN après AFD/ACP
phoneme.knn.afd2 <- knn(train = Z2, test = Ztest2, cl = phoneme.trainclass, k=9)
perf.knn.afd2 <- table(phoneme.test[,258], phoneme.knn.afd2)
#Taux d'erreurs : 
(sum(perf.knn.afd2)-sum(diag(perf.knn.afd2)))/ntest 
#11%  

#La meilleure méthode a adopter au vu des résultats sur KNN est l'AFD seule 
#car c'est elle qui offre la meilleure inertie intra-classe