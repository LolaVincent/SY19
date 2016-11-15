#Breast cancer data
#Task : predicting time to recur

breastcan <- read.table("r_breast_cancer.data", sep = ",", header = TRUE)
summary(breastcan)

n <- nrow(breastcan)
ntrain <- floor(2/3*n)
ntest <- n - ntrain
train <- sample(1:n, ntrain)
train <- breastcan[dtrain,]
test <- breastcan[-dtrain,]
traindata <- train[,1:32]
testdata <- test[,1:32]
traintime <- train[,33]
testtime <- test[,33]