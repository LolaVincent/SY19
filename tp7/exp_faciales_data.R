data <- load("data_expressions.RData")
X # données
y # classification


#Les données consistent en une matrice X de taille 216 × 4200 et un vecteur y de longueur 216. 
#Chaque ligne X[i,] contient les niveaux de gris d’une image de taille 60 × 70.
# X -> 4200 colonnes et 216 lignes, 216 individus et 4200 variables
# y -> 1 colonnes  et 216 lignes,  valeurs possibles -> 1 à 6 

#Visualisation des images
i=2 # exemple
I<-matrix(X[i,],60,70)
I1 <- apply(I, 1, rev)
image(t(I1),col=gray(0:255 / 255))

data_not_null <- matrix(nrow=216)
for (i in 1:4200){ if (X[,i]!=0){ data_not_null <- cbind(data_not_null, X[,i]) }}
data_not_null <- data_not_null[,2:ncol(data_not_null)]

non_cor <- sweep(data_not_null, 2, colSums(data_not_null)/216) # variables centrées/réduites

#ensemble d'apprentissage et de test
n <- nrow(data_not_null)
ntrain <- floor(2/3*n)
ntest <- n - ntrain
dtrain <- sample(1:n, ntrain)
data.train <- data_not_null[dtrain,]
data.test <- data_not_null[-dtrain,]
data.trainclass <- y[dtrain,]
data.testclass <- y[-dtrain,]
