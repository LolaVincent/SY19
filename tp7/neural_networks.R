source("afd.R")
library(neuralnet)
library(nnet)
library(caret)


# pour les données avant acp ou afd mais trop long ne fonctionne pas 
# normalisation des données (min-max method) et spération en ensemble de test et d'apprentissage

maxs <- apply(data_not_null, 2, max) 
mins <- apply(data_not_null, 2, min)

scaled <- as.data.frame(scale(data_not_null, center = mins, scale = maxs - mins))

scaled_y <- data.frame(scaled, y)
neural.train <- scaled_y[dtrain,]
neural.test <- scaled_y[-dtrain,]

name <- names(neural.train)
f <- as.formula(paste("y ~", paste(name[!name %in% "y"], collapse = " + ")))
nn <- neuralnet(f, data=neural.train, hidden=10, threshold=0,01) # ca fait tout planter.....  :( 

#The hidden argument accepts a vector with the number of neurons for each hidden layer
#regression -> linear.output=TRUE 
#classification -> linear.output=FALSE



# après ACP
dim <- c(10, 20, 30, 40)  # nombres de composantes différents
hid <- c(6,10, 50, 60) # nombres de noeuds
taux_erreur_acp <- matrix(-1, nrow= length(dim), ncol = length(hid))
for (j in 1:length(dim)){
  # méhode pour la classification on binarise les données
  nnet_train_acp <- data.frame(data.acp.train[,1:dim[j]], data.acp.trainclass, data.acp.trainclass == '1', data.acp.trainclass == '2', data.acp.trainclass == '3', data.acp.trainclass == '4', data.acp.trainclass == '5', data.acp.trainclass == '6')
  names(nnet_train_acp)[dim[j]+1] <- 'trainclass'
  names(nnet_train_acp)[dim[j]+2] <- 'a'
  names(nnet_train_acp)[dim[j]+3] <- 'b'
  names(nnet_train_acp)[dim[j]+4] <- 'c'
  names(nnet_train_acp)[dim[j]+5] <- 'd'
  names(nnet_train_acp)[dim[j]+6] <- 'e'
  names(nnet_train_acp)[dim[j]+7] <- 'f'
  
  name_acp <- names(nnet_train_acp)
  
  f_acp <- as.formula(paste("a + b + c + d + e + f ~", paste(name_acp[!name_acp %in% c("a", "b", "c", "d", "e", "f", "trainclass")],
                                                             collapse = " + ")))
  
  for (i in 1:length(hid)){
    nn_acp <- neuralnet(f_acp, data=nnet_train_acp, hidden=hid[i], rep=5) # rep -> nombre de répétition de l'apprentissage
    pr.nn.acp <- compute(nn_acp, data.acp.test[,1:dim[j]])
    
    mypredict <- pr.nn.acp$net.result
    # Put multiple binary output to categorical output
    maxidx <- function(arr) {
      return(which(arr == max(arr)))
    }
    idx <- apply(mypredict, c(1), maxidx)
    prediction_acp <- c('1', '2', '3', '4', '5', '6')[idx]
    erreur_acp <- table(prediction_acp, data.acp.testclass)
    taux_erreur_acp[j,i] <- (sum(erreur_acp) - sum(diag(erreur_acp)))/nrow(data.acp.test)
  }
}


# après AFD
hid <- c(1,2,3,4,5)
taux_erreur_afd <- matrix(-1, nrow=1, ncol = length(hid))
dim = ncol(Z)
nnet_train_afd <- data.frame(Z, data.acp.trainclass, data.acp.trainclass == '1', data.acp.trainclass == '2', data.acp.trainclass == '3', data.acp.trainclass == '4', data.acp.trainclass == '5', data.acp.trainclass == '6')
names(nnet_train_afd )[dim+1] <- 'trainclass'
names(nnet_train_afd )[dim+2] <- 'a'
names(nnet_train_afd )[dim+3] <- 'b'
names(nnet_train_afd )[dim+4] <- 'c'
names(nnet_train_afd )[dim+5] <- 'd'
names(nnet_train_afd )[dim+6] <- 'e'
names(nnet_train_afd )[dim+7] <- 'f'
name_afd <- names(nnet_train_afd)
  
f_afd <- as.formula(paste("a + b + c + d + e + f ~", paste(name_afd[!name_afd %in% c("a", "b", "c", "d", "e", "f", "trainclass")],
                                                             collapse = " + ")))


for (i in 1:length(hid)){
  nn_afd <- neuralnet(f_afd, data=nnet_train_afd, hidden=hid[i])
  pr.nn.afd <- compute(nn_afd, Ztest)
  
  mypredict_afd <- pr.nn.afd$net.result
  # Put multiple binary output to categorical output
  maxidx <- function(arr) {
    return(which(arr == max(arr)))
  }
  idx <- apply(mypredict_afd, 1, maxidx)
  prediction_afd <- c('1', '2', '3', '4', '5', '6')[idx]
  erreur_afd <- table(prediction_afd, data.acp.testclass)
  taux_erreur_afd[1,i] <- (sum(erreur_afd) - sum(diag(erreur_afd)))/nrow(data.acp.test)
}
  
