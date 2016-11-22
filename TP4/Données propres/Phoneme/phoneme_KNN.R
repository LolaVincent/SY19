source("phomene_data.R")

m <- matrix(nrow=10, ncol=3)
for(k in 1:10) {
  for(i in 1:3) {
    phoneme.knn <- knn(train = phoneme.trainquant, test = phoneme.testquant, cl = phoneme.trainclass, k=k)
    perf.knn <- table(phoneme.testclass, phoneme.knn) 
    #Il est intéressant de voir en affichant perf.knn que tous les >0 hors diag concernent aa et ao
    m[k,i]<-(sum(perf.knn)-sum(diag(perf.knn)))/ntest 
    #9%
  }
}
rowMeans(m) #La plus petite moyenne détermine la meilleure valeur de K
boxplot(t(m)) #k=9 intéressant