source("adl.R")

library(MASS)

# on récupère les données après ACP et séparation en deux ensembles
# on prend 20 composantes principales car on a vu avec la lda que c'etait la qu'on avait le pourcentage d'erreur le plus faible

#Données d'apprentissage : 
lda.afd<-lda(data.acp.trainclass~. , data=data.frame(data.acp.train[,1:20], data.acp.trainclass))
U <- lda.afd$scaling
X <- as.matrix(data.acp.train[,1:20])
Z <- X%*%U

#Données de test : 
lda.test<-lda(data.acp.testclass~. ,data=data.frame(data.acp.test[,1:20], data.acp.testclass))
Utest <- lda.test$scaling
Xtest <- as.matrix(data.acp.test[,1:20])
Ztest <- Xtest%*%Utest


#Composantes de l'AFD à utiliser : 
cp1 <- 1
cp2 <- 2

#Plot de l'AFD : 
plot(Z[data.acp.trainclass==1,cp1],Z[data.acp.trainclass==1,cp2],xlim=range(Z[,1]),ylim=range(Z[,2]),xlab="Z1",ylab="Z2", pch=16)
points(Z[data.acp.trainclass==2,cp1],Z[data.acp.trainclass==2,cp2],pch=16,col="blue")
points(Z[data.acp.trainclass==3,cp1],Z[data.acp.trainclass==3,cp2],pch=16,col="red")
points(Z[data.acp.trainclass==4,cp1],Z[data.acp.trainclass==4,cp2],pch=16,col="pink")
points(Z[data.acp.trainclass==5,cp1],Z[data.acp.trainclass==5,cp2],pch=16,col="yellow")
points(Z[data.acp.trainclass==6,cp1],Z[data.acp.trainclass==6,cp2],pch=16,col="green")
legend(-5, 9, xpd=TRUE, inset=.05, title="Expressions", c("1", "2", "3","4","5", "6"), fill=c("black","blue","red","pink","yellow", "green"), horiz=TRUE)

#plot intéressant pour les 2 premières composantes on voit bien les 6 groupes
