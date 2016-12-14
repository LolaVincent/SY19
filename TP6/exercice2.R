ecoli <- read.table("data/ecoli.txt")
wine <- read.table("data/wine.txt")
seeds <- read.table("data/seeds.txt")

mod <- NULL
mod$ecoli <- Mclust(ecoli[,1:8])
summary(mod$ecoli) #EEI
plot(mod$ecoli, what = c("BIC", "classification"))

mod$wine <- Mclust(wine[,2:14])
summary(mod$wine) #EVE
plot(mod$wine, what = c("BIC", "classification"))

#Beaucoup de données pour visualiser la classification : 
#Possibilité de faire une ACP au préalable et comparer

mod$seeds <- Mclust(seeds[,1:7])
summary(mod$seeds) #VEV
plot(mod$seeds, what = c("BIC", "classification")) 

