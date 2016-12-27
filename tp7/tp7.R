data <- load("data_expressions.RData")
X # données
y # classification
X[i] # 


I<-matrix(X[i,],60,70)
I1 <- apply(I, 1, rev)
image(t(I1),col=gray(0:255 / 255))


data_not_null <- matrix(nrow=216)
for (i in 1:4200){ if (X[,i]!=0){ data_not_null <- cbind(data_not_null, X[,i]) }}
data_not_null <- data_not_null[,2:ncol(data_not_null)]


data_acp <- prcomp(data_not_null) # scale =TRUE données normées, pas princomp trop de variables par rapport au nombre d'individus
#package factoextra pour la foncion get_pca_var



