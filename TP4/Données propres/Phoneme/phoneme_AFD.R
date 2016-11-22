source("phoneme_data.R")

library(MASS)
#Données d'apprentissage : 
lda.phoneme<-lda(phoneme.train[,258]~. ,data=phoneme.trainquant)
U <- lda.phoneme$scaling
X <- as.matrix(phoneme.trainquant)
Z <- X%*%U

#Données de test : 
lda.phonemetest<-lda(phoneme.test[,258]~. ,data=phoneme.testquant)
Utest <- lda.phonemetest$scaling
Xtest <- as.matrix(phoneme.testquant)
Ztest <- Xtest%*%Utest

#Composantes de l'AFD à utiliser : 
cp1 <- 1
cp2 <- 2

#Plot de l'AFD : 
plot(Z[phoneme.train[,258]=="aa",cp1],Z[phoneme.train[,258]=="aa",cp2],xlim=range(Z[,1]),ylim=range(Z[,2]),xlab="Z1",ylab="Z2", pch=16)
points(Z[phoneme.train[,258]=="ao",cp1],Z[phoneme.train[,258]=="ao",cp2],pch=16,col="blue")
points(Z[phoneme.train[,258]=="dcl",cp1],Z[phoneme.train[,258]=="dcl",cp2],pch=16,col="red")
points(Z[phoneme.train[,258]=="iy",cp1],Z[phoneme.train[,258]=="iy",cp2],pch=16,col="pink")
points(Z[phoneme.train[,258]=="sh",cp1],Z[phoneme.train[,258]=="sh",cp2],pch=16,col="yellow")
legend("topleft", inset=.05, title="Phoneme", c("aa", "ao", "dcl","iy","sh"), fill=c("black","blue","red","pink","yellow"), horiz=TRUE)

#KNN sur AFD : 
phoneme.knn.afd <- knn(train = Z, test = Ztest, cl = phoneme.trainclass, k=9)
perf.knn.afd <- table(phoneme.test[,258], phoneme.knn.afd)
#Taux d'erreurs : 
(sum(perf.knn.afd)-sum(diag(perf.knn.afd)))/ntest 
#4.5%  

