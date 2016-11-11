#3 classifieurs : ACP, AFD, ACP+AFD --> régression logistique MULTINOMIALE (glmnet)
# Ne pas utiliser ADL sur AFD mais on peut utiliser ADL et ADQ
#Calculer la performance du classifieur en faisant des ensembles de test/apprentissage

#Récupération des données :
phoneme <- read.table("phoneme.data", sep = ",", header = TRUE)
#phoneme_quant <- phoneme[,2:257]
n <- nrow(phoneme)
ntrain <- floor(2/3*n)
ntest <- n - ntrain
dtrain <- sample(1:n, ntrain)
phoneme.train <- phoneme[dtrain,]
phoneme.test <- phoneme[-dtrain,]
phoneme.trainquant <- phoneme.train[,2:257]
phoneme.testquant <- phoneme.test[,2:257]
acp <- princomp(phoneme.trainquant) #Prétraitement --> réduction variables

#ACP sur composantes 1 et 2 :
plot(acp$scores, pch=16, col=palette()[phoneme.train$g])
abline(h=0, v=0) #3 groupes qui se distinguent
legend("topright", inset=.05, title="Phoneme", c("aa", "ao", "dcl","iy","sh"), fill=palette(), horiz=FALSE)

#ACP sur composantes 2 et 3 :
plot(acp$scores[,c(2,3)], pch=16, col=palette()[phoneme.train$g])
abline(h=0, v=0) #3 groupes qui se distinguent
legend("topright", inset=.05, title="Phoneme", c("aa", "ao", "dcl","iy","sh"), fill=palette(), horiz=FALSE)

phoneme.knn <- knn(train = acp$scores, test = phoneme.testquant, cl = phoneme.train[,258], k=5)
phoneme.knn # Marche pas :(
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

#Régression logistique :
