#nnet
source("acp.R")
source("afd.R")
library(nnet)

data_nnet <- data.frame(data.acp.train[,1:20], data.acp.trainclass)
data_nnet$data.acp.trainclass = as.factor(data_nnet$data.acp.trainclass)
name_nnet <- names(data_nnet)
formula_nnet <- as.formula(paste("data.acp.trainclass ~", paste(name_nnet[!name_nnet %in% "data.acp.trainclass"], collapse = " + ")))

size <- c(30,40, 50, 70, 80, 85, 90, 95, 100)
decay <- seq(5*10^-4, 1, 0.1)
taux_erreur_nnet <- matrix(-1, nrow=length(decay), ncol = length(size))

for (j in 1:length(decay)){
  for (i in 1:length(size)){
    model_nnet <-nnet(formula_nnet, data_nnet, MaxNWts = 3000, size = size[i], decay = decay[j])
    result = max.col(predict(model_nnet, data.acp.test[,1:20]))
    erreur_nnet <- table(result, data.acp.testclass)
    taux_erreur_nnet[j,i] <- (sum(erreur_nnet) - sum(diag(erreur_nnet)))/nrow(data.acp.test)
  }
}


#test1
#[,1]      [,2]      [,3]      [,4]      [,5]      [,6]      [,7]      [,8]      [,9]
#[1,] 0.2638889 0.2777778 0.3194444 0.2222222 0.2500000 0.2083333 0.2638889 0.1666667 0.2361111
#[2,] 0.2222222 0.2500000 0.2083333 0.2361111 0.1666667 0.1805556 0.2083333 0.2083333 0.2500000
#[3,] 0.2777778 0.2361111 0.1805556 0.2083333 0.2222222 0.2500000 0.1666667 0.2638889 0.1944444
#[4,] 0.2638889 0.2361111 0.2222222 0.1805556 0.2361111 0.1805556 0.2500000 0.2361111 0.2777778
#[5,] 0.2361111 0.1666667 0.2361111 0.3333333 0.1805556 0.2083333 0.2361111 0.2361111 0.2083333
#[6,] 0.2222222 0.2500000 0.2916667 0.1666667 0.1666667 0.1944444 0.1666667 0.1944444 0.1388889
#[7,] 0.2638889 0.2361111 0.2361111 0.2083333 0.1666667 0.1805556 0.1250000 0.1944444 0.1666667
#[8,] 0.2361111 0.2500000 0.1527778 0.1944444 0.1666667 0.2916667 0.2083333 0.1527778 0.2638889
#[9,] 0.1527778 0.2083333 0.2638889 0.1805556 0.1805556 0.2083333 0.1944444 0.2083333 0.2500000
#[10,] 0.3055556 0.3611111 0.2916667 0.2500000 0.1944444 0.1944444 0.2083333 0.1805556 0.1666667
#> test2
#[,1]      [,2]      [,3]      [,4]      [,5]      [,6]      [,7]      [,8]      [,9]
#[1,] 0.3611111 0.3611111 0.2361111 0.1805556 0.3194444 0.3055556 0.2638889 0.3055556 0.2083333
#[2,] 0.4305556 0.3472222 0.3472222 0.2916667 0.2638889 0.2500000 0.2361111 0.3194444 0.2500000
#[3,] 0.5000000 0.2916667 0.3611111 0.2638889 0.2777778 0.2361111 0.2777778 0.2222222 0.1944444
#[4,] 0.3472222 0.4027778 0.2916667 0.3055556 0.2638889 0.2638889 0.3611111 0.2777778 0.2777778
#[5,] 0.3055556 0.2916667 0.2361111 0.2916667 0.2500000 0.3055556 0.1944444 0.2638889 0.2500000
#[6,] 0.2916667 0.4027778 0.2083333 0.2638889 0.3194444 0.2777778 0.1666667 0.2222222 0.2500000
#[7,] 0.3194444 0.3472222 0.2500000 0.3472222 0.2500000 0.2777778 0.2083333 0.2222222 0.2222222
#[8,] 0.3472222 0.3055556 0.2777778 0.2916667 0.3194444 0.2500000 0.3750000 0.2777778 0.2222222
#[9,] 0.3472222 0.2222222 0.3055556 0.3194444 0.2638889 0.2222222 0.2638889 0.2916667 0.2500000
#[10,] 0.3194444 0.2777778 0.2638889 0.1944444 0.2638889 0.2361111 0.2361111 0.2916667 0.2500000

#AFD -> taux d'erreurs beaucoup trop elevé 

data_nnet_afd <- data.frame(Z, data.acp.trainclass)
data_nnet_afd$data.acp.trainclass = as.factor(data_nnet_afd$data.acp.trainclass)
name_nnet_afd <- names(data_nnet_afd)
formula_nnet_afd <- as.formula(paste("data.acp.trainclass ~", paste(name_nnet_afd[!name_nnet_afd %in% "data.acp.trainclass"], collapse = " + ")))

size_afd <- c(30,40, 50, 70, 80,90,100, 110)
decay_afd <- seq(5*10^-4, 1, 0.1)
taux_erreur_nnet_afd <- matrix(-1, nrow=length(decay_afd), ncol = length(size_afd))

for (j in 1:length(decay_afd)){
  for (i in 1:length(size_afd)){
    model_nnet_afd <-nnet(formula_nnet_afd, data_nnet_afd, MaxNWts = 3000, size = size_afd[i], decay = decay_afd[j])
    result_afd = max.col(predict(model_nnet_afd, Ztest))
    erreur_nnet_afd <- table(result_afd, data.acp.testclass)
    taux_erreur_nnet_afd[j,i] <- (sum(erreur_nnet_afd) - sum(diag(erreur_nnet_afd)))/nrow(data.acp.test)
  }
}
taux_erreur_nnet_afd
# rien en dessous de 70% ... 
