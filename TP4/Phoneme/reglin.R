source("phoneme_data.R")
source("phomene_ACP_AFD.R")

# pas terminé problème au momet de la prédiction


regl <- lm(as.numeric(phoneme.train[,258]) ~ as.matrix(phoneme.trainquant))
pred.regl <- predict(regl, int="p", newdata=as.data.frame(phoneme.testquant))
perf.regl <- table(as.numeric(phoneme.test[,258]), pred.regl$class)
(sum(perf.regl)-sum(diag(perf.regl)))/nrow(test)