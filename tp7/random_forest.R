library(randomForest)

rand.forest = randomForest(traindata$y ~ .,data=traindata, ranges=list(
  ntree=c(25, 50, 100, 250, 500, 750),
  mtry=c(1:15))
)

rand.forest
pred.forest  = predict(rand.forest, newdata = testdata)
forest.perf <- table(pred.forest, testclass)
(sum(forest.perf)-sum(diag(forest.perf)))/nrow(testdata) 
#7.69%
importance(rand.forest)
varImpPlot(rand.forest)
