source("phoneme_data.R")
source("phomene_ACP_AFD.R")



regl <- lm(as.numeric(phoneme.train[,258]) ~ as.matrix(phoneme.trainquant))
pred.regl <- predict(regl, int="p", newdata=as.data.frame(phoneme.testquant))


#perf.regl <- table(as.numeric(phoneme.test[,258]), pred.regl[,1]> 2,5)
#(sum(perf.regl[1:2,2]) + sum(perf.regl[3:5,1]) )/nrow(test)


plot(phoneme.testclass, regl) # fonctionne pas, problème de dimension

qqnorm(resid(regl)) # pour vérifier la normalité
qqline(resid(regl))
