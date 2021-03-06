#####Reconnaissance d'expressions faciales######

#Chargement des données : 
data <- load("data_expressions.RData")
X # données
y # classification
X[i] #Image de taille 60*70=4200 pixels

#Visualisation des images :
i=2 # exemple
I<-matrix(X[i,],60,70)
I1 <- apply(I, 1, rev)
image(t(I1),col=gray(0:255 / 255))

#Suppression des données nulles :
data_not_null <- matrix(nrow=216)
for (i in 1:4200){ if (X[,i]!=0){ data_not_null <- cbind(data_not_null, X[,i]) }}
data_not_null <- data_not_null[,2:ncol(data_not_null)]

#Ensembles d'apprentissage et de test
n <- nrow(data_not_null)
ntrain <- floor(2/3*n)
ntest <- n - ntrain
dtrain <- sample(1:n, ntrain)

#--------------------------------------- ACP ---------------------------------------#

data_acp <- prcomp(data_not_null) 
#Ppas princomp car trop de variables par rapport au nombre d'individus
# scale =TRUE données normées

#Affichage : 
plot(data_acp$x, col=palette()[y])
legend(-1000, 2500 , title= "Expressions faciales", inset=.001,  c("1", "2", "3","4","5", "6"), fill=palette(), horiz=TRUE, xpd=TRUE)
pairs(data_acp$x[,1:5], col=palette()[y])
# Variance cumulée avec 2 composantes : 21 %, 21 composantes : 70%
# avec le plot, on distingue les groupes mais difficile

#Découpage en ensemble de test/apprentissage : 
acp.data_not_null <- prcomp(data_not_null)
data.acp.train <- acp.data_not_null$x[dtrain,]
data.acp.test <- acp.data_not_null$x[-dtrain,]
data.acp.trainclass <- y[dtrain,]
data.acp.testclass <- y[-dtrain,]

#---------------------------------- ADL et Classifieur Bayésien Naïf avec Cross Validation ----------------------------------#
library(MASS)
library(lda)
library(e1071) #Naive


#ADL sur ACP
#ACP sur l'ensemble des données sans les 0 et après séparation en un ensemble d'apprentissage et de test
#Valeurs différentes du nombre de composantes (nbCompo) et plusieurs fois (ici 10) pour avoir la moyenne de l'erreur


K = 5
nbCompo <- c(2, 5, 10, 20, 30, 40, 50, 100)
n.adl <- nrow(data_acp$x)
folds <- sample(1:K, n.adl, replace=TRUE)
adlErrorRate <- matrix(-1, nrow=10, ncol=length(nbCompo))
nbcErrorRate <- matrix(-1, nrow=10, ncol=length(nbCompo))
for (j in 1:length(nbCompo) ){
  for (t in 1:10){
    res.lda.acp  <- 0
    res.nbc.acp <- 0
    for (i in 1:K) {
      #donnees
      ytrain <- y[(folds!=i)]
      ytest <-  y[(folds==i)]
      datatrain <- data.frame(data_acp$x[(folds!=i),1:nbCompo[j]], ytrain)
      datatrain$ytrain <- as.factor(datatrain$ytrain)
      datatest <- data.frame(data_acp$x[(folds==i),1:nbCompo[j]])
      
      
      #adl 
      lda.exp.acp <- lda(ytrain~ ., data = datatrain )
      pred.lda.acp <- predict(lda.exp.acp, newdata=datatest)
      perf.lda.acp <- table(ytest, pred.lda.acp$class)
      #Taux d'erreur :
      res.lda.acp <- (sum(perf.lda.acp)-sum(diag(perf.lda.acp)))/length(ytest) + res.lda.acp
      
      #nbc
      naive.acp <- naiveBayes(ytrain~., data=datatrain)
      pred.naive.acp <-predict(naive.acp, newdata=datatest)
      perf.naive.acp <-table(ytest, pred.naive.acp)
      res.nbc.acp <- res.nbc.acp + (sum(perf.naive.acp)-sum(diag(perf.naive.acp)))/length(ytest)
    }
    nbcErrorRate[t,j] <- res.nbc.acp/K
    adlErrorRate[t,j] <- res.lda.acp/K
  }
}
colMeans(adlErrorRate)
boxplot(adlErrorRate)

colMeans(nbcErrorRate)
boxplot(nbcErrorRate)


#--------------------------------------- AFD ---------------------------------------#

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
legend(-7, 12, xpd=TRUE, inset=.05, title="Expressions", c("1", "2", "3","4","5", "6"), fill=c("black","blue","red","pink","yellow", "green"), horiz=TRUE)

#plot intéressant pour les 2 premières composantes on voit bien les 6 groupes


#Données de l'ACP : 
nb_comp <- 20
acp.data <- data.frame(acp.data_not_null$x[,1:nb_comp], y)
acp.data$y <- as.factor(y)

#Application de l'AFD
lda.afd<-lda(acp.data$y~. , data=acp.data)
U <- lda.afd$scaling
X <- as.matrix(acp.data_not_null$x[,1:nb_comp])
Z <- X%*%U

#Découpage en apprentissage/test
df.afd.train <- data.frame(Z[dtrain,], y= y[dtrain,])
df.afd.train$y = as.factor(y[dtrain,])
afd.test <- Z[-dtrain,]
afd.testclass <- y[-dtrain,]


#Test sur l'ADL pour obtenir le taux d'erreur de l'AFD comme classifieur
lda.afd.exp <- lda(df.afd.train$y ~ ., data = df.afd.train)
pred.lda <- predict(lda.afd.exp, newdata=as.data.frame(afd.test))
perf.lda <- table(afd.testclass, pred.lda$class)
(sum(perf.lda)-sum(diag(perf.lda)))/nrow(afd.test)
# 8% d'erreur

#Cross-validation avec K=5 pour le test sur l'ADL (grande variance)
folds <- sample(1:K, nrow(Z), replace=TRUE)
error_rate <- vector(length = K)
for(i in 1:K) {
  df.afd.train <- data.frame(Z[(folds!=i),], y= y[(folds!=i),])
  df.afd.train$y = as.factor(y[(folds!=i),])
  afd.test <- data.frame(Z[(folds==i),])
  afd.testclass <- y[(folds==i),]
  
  lda.afd.exp <- lda(df.afd.train$y ~ ., data = df.afd.train)
  pred.lda <- predict(lda.afd.exp, newdata=afd.test)
  perf.lda <- table(afd.testclass, pred.lda$class)
  error_rate[i] <- (sum(perf.lda)-sum(diag(perf.lda)))/nrow(afd.test)
}
error_rate
mean(error_rate)
median(error_rate)
boxplot(error_rate) 
#médiane 9.4%, moyenne 10.4%


########### 1: Classifieur bayésien naïf après AFD ########### 
K <- 5
folds <- sample(1:K, nrow(Z), replace=TRUE)
error_rate <- vector(length = K)
for(i in 1:K) {
  df.afd.train <- data.frame(Z[(folds!=i),], y= y[(folds!=i),])
  df.afd.train$y = as.factor(y[(folds!=i),])
  afd.test <- data.frame(Z[(folds==i),])
  afd.testclass <- y[(folds==i),]
  
  naive.afd <- naiveBayes(df.afd.train$y~., data=df.afd.train)
  pred.naive.afd <-predict(naive.afd, newdata=afd.test)
  perf.naive.afd <-table(afd.testclass, pred.naive.afd)
  error_rate[i] <- (sum(perf.naive.afd)-sum(diag(perf.naive.afd)))/nrow(afd.test) 
}
error_rate
mean(error_rate)
median(error_rate)
boxplot(error_rate)
#11%

#------------------------------- Réseaux de neurones -------------------------------#
# Réseaux de neurones avec les données après ACP avec le package nnet
library(nnet)

data_nnet <- data.frame(data.acp.train[,1:20], data.acp.trainclass)
data_nnet$data.acp.trainclass = as.factor(data_nnet$data.acp.trainclass)
name_nnet <- names(data_nnet)
formula_nnet <- as.formula(paste("data.acp.trainclass ~", paste(name_nnet[!name_nnet %in% "data.acp.trainclass"], collapse = " + ")))

size <- c(30,40, 50, 70, 80, 85, 90, 95, 100) #Test du nombre de noeuds sur la couche. 
decay <- seq(5*10^-4, 1, 0.1) # paramétrage du poids
taux_erreur_nnet <- matrix(-1, nrow=length(decay), ncol = length(size))

for (j in 1:length(decay)){
  for (i in 1:length(size)){
    model_nnet <-nnet(formula_nnet, data_nnet, MaxNWts = 3000, size = size[i], decay = decay[j])
    result = max.col(predict(model_nnet, data.acp.test[,1:20]))
    erreur_nnet <- table(result, data.acp.testclass)
    taux_erreur_nnet[j,i] <- (sum(erreur_nnet) - sum(diag(erreur_nnet)))/nrow(data.acp.test)
  }
}
colMeans(taux_erreur_nnet)
rowMeans(taux_erreur_nnet)
taux_total_acp <- 0
for (i in 1:length(size)){
  taux_total_acp <- taux_total_acp + colMeans(taux_erreur_nnet)[i]
}
taux_total_acp <- taux_total_acp /length(size)
boxplot(taux_erreur_nnet)

#  Réseaux de neurones avec les données après AFD avec le package nnet

name_nnet_afd <- names(df.afd.train)
formula_nnet_afd <- as.formula(paste("y ~", paste(name_nnet_afd[!name_nnet_afd %in% "y"], collapse = " + ")))

size_afd <- c(10,20,30,40, 50, 70, 80, 90, 100)
decay_afd <- seq(5*10^-4, 1, 0.1)
taux_erreur_nnet_afd <- matrix(-1, nrow=length(decay_afd), ncol = length(size_afd))


for (j in 1:length(decay_afd)){
  for (i in 1:length(size_afd)){
    model_nnet_afd <-nnet(formula_nnet_afd, df.afd.train, MaxNWts = 3000, size = size_afd[i], decay = decay_afd[j])
    result_afd = max.col(predict(model_nnet_afd, afd.test))
    erreur_nnet_afd <- table(result_afd, afd.testclass)
    taux_erreur_nnet_afd[j,i] <- (sum(erreur_nnet_afd) - sum(diag(erreur_nnet_afd)))/nrow(data.acp.test)
  }
}
colMeans(taux_erreur_nnet_afd)
rowMeans(taux_erreur_nnet_afd)
taux_total_afd <- 0
for (i in 1:length(size_afd)){
  taux_total_afd <- taux_total_afd + colMeans(taux_erreur_nnet_afd)[i]
}
taux_total_afd <- taux_total_afd /length(size_afd)
boxplot(taux_erreur_nnet_afd)

#--------------------------------------- SVM ---------------------------------------#

acp_traindata <- data.frame(data.acp.train[,1:nb_comp], y=as.factor(data.acp.trainclass))
acp_testdata <- data.frame(data.acp.test[,1:nb_comp])
acp_testclass <- as.factor(data.acp.testclass)

traindata <- df.afd.train
testdata <- afd.test
testclass <- afd.testclass

####GENERAL TUNING : COMPARISON ACP VS ACP+AFD###
afd.tune = tune(svm, train.y=traindata$y , train.x=traindata[1:5], ranges=list(
  kernel=c("linear", "polynomial", "radial", "sigmoid"),
  cost=c(1:10),
  gamma=c(0.00001,0.0001,0.005, 0.001,0.01,.5),
  degree=c(1:5)
)
)
afd.tune
#Best perf : 9% avec sigmoid kernel, cost 8 

acp.tune = tune(svm, train.y=acp_traindata$y , train.x=acp_traindata[1:nb_comp], ranges=list(
  kernel=c("linear", "polynomial", "radial", "sigmoid"),
  cost=c(1:10),
  gamma=c(0.00001,0.0001,0.005, 0.001,0.01,.5),
  degree=c(1:5)
)
)
acp.tune
#Best performance : 20% avec linear kernel cost=2 

######Radial : 
library(e1071)
traindata <- df.afd.train
testdata <- afd.test
testclass <- afd.testclass

K <- 5
folds <- sample(1:K, nrow(Z), replace=TRUE)
error_rate <- vector(length = K)
for(i in 1:K) {
  traindata <- data.frame(Z[(folds!=i),], y= as.factor(y[(folds!=i),]))
  testdata <- data.frame(Z[(folds==i),])
  testclass <- y[(folds==i),]
  
  default_svmfit = svm(traindata$y~., data=traindata)  
  default_svm.pred <- predict(default_svmfit, testdata)
  default_svm.perf <- table(default_svm.pred, testclass)
  error_rate[i] <- (sum(default_svm.perf)-sum(diag(default_svm.perf)))/nrow(testdata) 
}
error_rate
mean(error_rate)
median(error_rate)
boxplot(error_rate)

default_svmfit = svm(traindata$y~., data=traindata)
summary(default_svmfit) 
#Kernel : radial, cost = 1, gamma = 0.05
default_svm.pred <- predict(default_svmfit, testdata)
default_svm.perf <- table(default_svm.pred, testclass)
(sum(default_svm.perf)-sum(diag(default_svm.perf)))/nrow(testdata) 
#7.69% d'erreur

####LINEAR###
linear_svmfit = svm(traindata$y~., data=traindata, kernel="linear")
summary(linear_svmfit) 
#Kernel : linear, cost = 1, gamma = 0.05
linear_svm.pred <- predict(linear_svmfit, testdata)
linear_svm.perf <- table(linear_svm.pred, testclass)
(sum(linear_svm.perf)-sum(diag(linear_svm.perf)))/nrow(testdata) 

####POLYNOMIAL###
svmfit = svm(traindata$y~., data=traindata, kernel="polynomial")
summary(svmfit) 
#Kernel : linear, cost = 1, degree=3 gamma = 0.05, coef=0
svm.pred <- predict(svmfit, testdata)
svm.perf <- table(svm.pred,testclass)
(sum(svm.perf)-sum(diag(svm.perf)))/nrow(testdata) 

svm_tune <- tune(svm, train.y=traindata$y , train.x=traindata[1:5], 
                 kernel="polynomial", ranges=list(degre=c(1,2,3,4,5),cost=10^(-1:2), gamma=c(0.00001, 0.00005,0.0001, 0.001)))
svm_tune
#Cost : 0.1, gamma : 1e-05

svm_after_tune <- svm(traindata$y~., data=traindata, kernel="polynomial", cost=100, gamma=0.001, degree=1)
summary(svm_after_tune)
svm.pred <- predict(svm_after_tune, testdata)
svm.perf <- table(svm.pred, testclass)
(sum(svm.perf)-sum(diag(svm.perf)))/nrow(testdata) 
#25% d'erreur

#####SIGMOID
svmfit = svm(traindata$y~., data=traindata, kernel="sigmoid")
summary(svmfit) 
#Kernel : sigmoid, cost = 1, gamma = 0.05, coef=0
svm.pred <- predict(svmfit,testdata)
svm.perf <- table(svm.pred, testclass)
(sum(svm.perf)-sum(diag(svm.perf)))/nrow(testdata) 
#9.7% d'erreur

library(pROC)
roc_curve<-roc(testclass, as.numeric(linear_svm.pred))
plot(roc_curve)

#--------------------------------------- Trees ---------------------------------------#
library(tree)
#Arbre ACP+ AFD : 
tree.expr = tree(traindata$y ~ .,traindata)
summary(tree.expr)
plot(tree.expr)
text(tree.expr, pretty = 0)

#Prédiction sans élagage : 
tree.pred=predict(tree.expr, testdata, type="class")
tree.perf <- table(tree.pred, testclass)
(sum(tree.perf)-sum(diag(tree.perf)))/nrow(testdata) 
#12% d'erreur

#Elagage : 
cv.tree.expr = cv.tree(tree.expr, FUN = prune.misclass) 
#Cross-validation pour trouver le meilleur élagage
#Taux d'erreur : $dev
nb_nodes <- cv.tree.expr$size[which.min(cv.tree.expr$dev)] #Nb idéal noeuds terminaux

par(mfrow=c(1,2))
plot(cv.tree.expr$size, cv.tree.expr$dev, type="b")
plot(cv.tree.expr$k, cv.tree.expr$dev, type="b")

#On élague au nombre de noeuds conseillés
prune.tree.expr = prune.misclass(tree.expr, best=nb_nodes)
summary(prune.tree.expr)
par(mfrow=c(1,1))
plot(prune.tree.expr)
text(prune.tree.expr, pretty=0)

#Prédiction après élagage :
tree.pred=predict(prune.tree.expr, testdata, type="class")
tree.perf <- table(tree.pred, testclass)
(sum(tree.perf)-sum(diag(tree.perf)))/nrow(testdata) 
#5%

par(mfrow=c(1,2))
plot(tree.expr)
text(tree.expr, pretty = 0)
plot(prune.tree.expr)
text(prune.tree.expr, pretty=0)

#--------------------------------------- Random Forest ---------------------------------------#
library(randomForest)

rand.forest = randomForest(traindata$y ~ .,data=traindata, ranges=list(
  ntree=c(25, 50, 100, 250, 500, 750),
  mtry=c(1:15))
)

rand.forest
pred.forest  = predict(rand.forest, newdata = testdata)
forest.perf <- table(pred.forest, testclass)
(sum(forest.perf)-sum(diag(forest.perf)))/nrow(testdata) 
#7.69%
importance(rand.forest)
varImpPlot(rand.forest)
