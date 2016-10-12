spam <- read.table("spambase.dat")

n <- 4601
napp <- 3067
train<-sample(1:n,napp)
app <- spam[train,]
app <- as.data.frame(app)
test <- spam[-train,]
ntst <- nrow(test)

library(MASS)
lda.spam<- lda(V58~., data=app)
pred.spam<-predict(lda.spam,newdata=test)
perf <-table(test$V58,pred.spam$class)
1-sum(diag(perf))/ntst


#regression logistique
glm.fit<- glm(V58~.,data=app,family=binomial)
summary(glm.fit)

pred.reg <-predict(glm.fit,newdata=test,type="response")
perf_reg <- table(test$V58,pred.reg>0.5)
1-sum(diag(perf_reg))/ntst


# Au vue des pourcentages d'erreur reg logistique meilleure que régression linéaire


library(pROC)
roc_curve_lda<-roc(test$V58, as.vector(pred.spam$x))
roc_curve_reg<-roc(test$V58, as.vector(pred.reg))
plot(roc_curve_lda)
plot(roc_curve_reg, col='red', add=TRUE)


# Données importantes (coeff significativement non nuls) : V5, V7, V9, V16, V23, V25, V27, V46, V52, V53, V56
n <- 4601
napp <- 3067
train<-sample(1:n,napp)
app <- spam[train,]
app <- as.data.frame(app)
test <- spam[-train,]
ntst <- nrow(test)

library(MASS)
lda.spam<- lda(V58~., data=app)
pred.spam<-predict(lda.spam,newdata=test)
perf <-table(test$V58,pred.spam$class)
1-sum(diag(perf))/ntst

