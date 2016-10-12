library(MASS)

pi1 <- 0.5
pi2 <- 0.5

mu1 <- matrix(0, nrow=3) 
mu2 <- matrix(1, nrow=3)

sig1 <- matrix(0, nrow=3, ncol=3)
diag(sig1) <-  c(1, 1,1)
sig2 <- 0.8*sig1

nvalues <- c(30, 100, 1000, 10000)
erreur <- matrix(0, nrow=4, ncol=2)
for (j in 1:length(nvalues)) {
  n <- nvalues[j] + 10000
  print(n)
  data <- matrix(0, nrow=n, ncol=4)
  class <- rbinom(n, 1, 0.5)
  for (i in 1:length(class)){
    if (class[i] ==0){
      l <- mvrnorm(1, mu2, sig2)
        
    }
    else {
      l<-mvrnorm(1, mu1, sig1)
    }
    data[i,] <- c(l, class[i])
    
  }
  test <- data[1:10000,]
  app <- data[10001:n,]
  app <- as.data.frame(app)
  test <- as.data.frame(test)
  lda.app <- lda(V4~. , data=app)
  pred.lda <- predict(lda.app, newdata=test)
  perf.lda <-table(test$V4, pred.lda$class)
  qda.app <- qda(V4~. , data=app)
  pred.qda <- predict(qda.app, newdata=test)
  perf.qda <-table(test$V4, pred.qda$class)
  erreur[j,1] <- (1-sum(diag(perf.lda))/10000)*100
  erreur[j,2] <- (1-sum(diag(perf.qda))/10000)*100
}
erreur


# grosse différence pour les petits échantillons d'apprentissage (mieux adq) mais après plus vraiment de différence
