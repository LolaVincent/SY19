source(cancer_data.R)

knn.breastcan <- knn(train = traindata, test = testdata, cl = traintime, k=5)
perf.knn <- table(testtime, knn.breastcan)
(sum(perf.knn)-sum(diag(perf.knn)))/ntest