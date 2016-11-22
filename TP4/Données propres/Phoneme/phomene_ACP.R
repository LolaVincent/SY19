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

#sur les deux premières composantes principales : 
phoneme.knn.acp <- knn(train = acp.train$scores[,1:2], test = acp.test$scores[,1:2], cl = phoneme.trainclass, k=9)
perf.knn.acp <- table(phoneme.testclass, phoneme.knn.acp)
#Calcul du taux d'erreur : 
(sum(perf.knn.acp)-sum(diag(perf.knn.acp)))/ntest #16%

# sur les quatre premières composantes principales :
phoneme.knn.acp <- knn(train = acp.train$scores[,1:4], test = acp.test$scores[,1:4], cl = phoneme.trainclass, k=9)
perf.knn.acp <- table(phoneme.testclass, phoneme.knn.acp)
#Calcul du taux d'erreur : 
(sum(perf.knn.acp)-sum(diag(perf.knn.acp)))/ntest #12,6%


