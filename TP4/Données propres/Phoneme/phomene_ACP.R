source("phoneme_data.R")

acp.train <- princomp(phoneme.trainquant, cor=TRUE) 
acp.test <- princomp(phoneme.testquant, cor=TRUE)


#Plot de l'ACP sur les composantes 1 et 2 :
plot(acp.train$scores, pch=16, col=palette()[phoneme.train$g])
abline(h=0, v=0) #3 groupes qui se distinguent
legend("topright", inset=.05, title="Phoneme", c("aa", "ao", "dcl","iy","sh"), fill=palette(), horiz=FALSE)

#Plot de l'ACP sur les composantes 2 et 3 :
plot(acp.train$scores[,c(2,3)], pch=16, col=palette()[phoneme.train$g])
abline(h=0, v=0) #3 groupes qui se distinguent
legend("topright", inset=.05, title="Phoneme", c("aa", "ao", "dcl","iy","sh"), fill=palette(), horiz=FALSE)

#KNN sur ACP 
phoneme.knn.acp <- knn(train = acp.train$scores[,1:4], test = acp.test$scores[,1:6], cl = phoneme.trainclass, k=8)
perf.knn.acp <- table(phoneme.test[,258], phoneme.knn.acp)
#Calcul du taux d'erreur : 
(sum(perf.knn.acp)-sum(diag(perf.knn.acp)))/ntest 
#16% avec 2 premières composantes, 12% avec 4  


