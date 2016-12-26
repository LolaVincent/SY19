source(exp_faciales_data.R)
source(acp.R)
library(MASS)
library(lda)

lda.exp <- lda(as.matrix(data.trainclass) ~ data.train, data = data.frame(data.train, data.trainclass))
# problèmes car les variables sont colinéaires, vaut mieux faire après ACP
pred.lda <- predict(lda.exp, newdata=as.data.frame(data.test))
perf.lda <- table(data.testclass, pred.lda$class)
(sum(perf.lda)-sum(diag(perf.lda)))/nrow(test)


#ADL sur ACP
#ACP sur l'ensemble des données sans les 0 et après séparation en un ensemble d'apprentissage et de test
#valeurs différentes du nombre de composantes (nbCompo) et plusieurs fois (ici 10) pour avoir la moyenne de l'erreur

###### ATTENTION ça peut être long, exemple de résultats en dessous 
adlErrorRate <- vector(length = 10)
nbCompo <- c(2, 5, 10, 20, 30, 40, 50, 100)
nbCompoErrorRate <-  vector(length = 8)
j = 0
for (nb in nbCompo) {
  print(nb)
  print(j)
  j = j + 1
  for (i in 1:10) {
    #ACP + séparation en ensemble de test et d'apprentissage
    acp.adl.data_not_null <- prcomp(data_not_null)
    n.adl <- nrow(acp.adl.data_not_null$x)
    ntrain.adl <- floor(2/3*n)
    ntest.adl <- n - ntrain
    dtrain.adl <- sample(1:n, ntrain.adl)
    data.adl.train <- acp.data_not_null$x[dtrain.adl,]
    data.adl.test <- acp.data_not_null$x[-dtrain.adl,]
    data.adl.trainclass <- y[dtrain.adl,]
    data.adl.testclass <- y[-dtrain.adl,]
    
    # analyse discriminante
    lda.exp.acp <- lda(data.adl.trainclass~ ., data = data.frame(data.adl.train[,1:nb], data.adl.trainclass))
    pred.lda.acp <- predict(lda.exp.acp, newdata=as.data.frame(data.adl.test[,1:nb]))
    perf.lda.acp <- table(data.adl.testclass, pred.lda.acp$class)
    #Taux d'erreur :
    adlErrorRate[i] <- (sum(perf.lda.acp)-sum(diag(perf.lda.acp)))/ntest.adl
  }
  # moyenne de l'erreur pour un certain nombre de composante
 nbCompoErrorRate[j] <- mean(adlErrorRate)
}
# 0.4111111 0.3361111 0.2958333 0.1375000 0.1611111 0.1708333 0.1708333 0.3041667 0.6958333 (dernière valeur -> 150)
# 0.4347222 0.3555556 0.3000000 0.1638889 0.1638889 0.1638889 0.1819444 0.3180556
