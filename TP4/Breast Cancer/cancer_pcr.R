library(pls)


fit.pcr <- pcr(as.matrix(breastcan[,33]) ~ as.matrix(breastcan[,1:32]), scale=TRUE, validation="CV")

summary(fit.pcr)

validationplot(fit.pcr, val.type = "MSEP", legendpos = "topright") 


fit.pcr.train <- pcr(as.matrix(traintime) ~ as.matrix(traindata), scale=TRUE, validation="CV")
summary(fit.pcr.train)
validationplot(fit.pcr.train, val.type = "MSEP", legendpos = "topright")


fit.pcr.test <- pcr(as.matrix(testtime) ~ as.matrix(testdata), scale=TRUE, validation="CV")
summary(fit.pcr.test)
validationplot(fit.pcr.test, val.type = "MSEP", legendpos = "topright") 

