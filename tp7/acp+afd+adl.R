source("adl.R")

#AFD

library(MASS)

#Données de l'ACP : 
nb_comp <- 20
acp.data <- data.frame(acp.data_not_null$x[,1:nb_comp], y)
acp.data$y <- as.factor(y)

#Application de l'AFD
lda.afd<-lda(acp.data$y~. , data=acp.data)
U <- lda.afd$scaling
X <- as.matrix(acp.data_not_null$x[,1:nb_comp])
Z <- X%*%U

#Composantes de l'AFD à utiliser : 
cp1 <- 1
cp2 <- 2

#Plot de l'AFD : 
plot(Z[y==1,cp1],Z[y==1,cp2],xlim=range(Z[,1]),ylim=range(Z[,2]),xlab="Z1",ylab="Z2", pch=16)
points(Z[y==2,cp1],Z[y==2,cp2],pch=16,col="blue")
points(Z[y==3,cp1],Z[y==3,cp2],pch=16,col="red")
points(Z[y==4,cp1],Z[y==4,cp2],pch=16,col="pink")
points(Z[y==5,cp1],Z[y==5,cp2],pch=16,col="yellow")
points(Z[y==6,cp1],Z[y==6,cp2],pch=16,col="green")
legend(-3, 7, xpd=TRUE, inset=.05, title="Expressions", c("1", "2", "3","4","5", "6"), fill=c("black","blue","red","pink","yellow", "green"), horiz=TRUE)


#Découpage en apprentissage/test
df.afd.train <- data.frame(Z[dtrain,], y= y[dtrain,])
df.afd.train$y = as.factor(y[dtrain,])
afd.test <- Z[-dtrain,]
afd.testclass <- y[-dtrain,]

#Test sur l'ADL
lda.afd.exp <- lda(df.afd.train$afd.trainclass ~ ., data = df.afd.train)
pred.lda <- predict(lda.afd.exp, newdata=as.data.frame(afd.test))
perf.lda <- table(afd.testclass, pred.lda$class)
(sum(perf.lda)-sum(diag(perf.lda)))/nrow(afd.test)
#8% d'erreur


################################################################################################
#Cross-validation avec K=5(grande variance)
K <- 5
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
boxplot(error_rate) #moyenne 11.5%