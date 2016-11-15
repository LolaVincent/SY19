#Récupération des données :
phoneme <- read.table("phoneme.data", sep = ",", header = TRUE)

#Ensembles de test/apprentissage
n <- nrow(phoneme)
ntrain <- floor(2/3*n)
ntest <- n - ntrain
dtrain <- sample(1:n, ntrain)
phoneme.train <- phoneme[dtrain,]
phoneme.test <- phoneme[-dtrain,]
phoneme.trainquant <- phoneme.train[,2:257]
phoneme.testquant <- phoneme.test[,2:257]
phoneme.trainclass <- phoneme.train[,258]
phoneme.testclass <- phoneme.test[,258]

app = as.data.frame(phoneme.train[,2:258])
test = phoneme.test[,2:258]