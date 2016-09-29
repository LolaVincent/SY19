data <- read.table("prostate.data")

apptot <- data[which(data[,10]==T),]
testtot <- data[which(data[,10]==F),]
app <- apptot[,1:4]
test <- testtot[,1:4]
testlpsa <- testtot[,9]
applpsa <- apptot[,9]
#package FNN
rep <- knn.reg(app, test, applpsa, k=3)
# package zoo et hydroGOF
mse(rep$pred, testlpsa)

res <- vector(length=10)
for ( i in 1:10) {
	r <- knn.reg(app, test, applpsa, k=i)
	res[i] <- mse(r$pred, testlpsa)
	res
}


res2 <- vector(length=10)
for ( i in 1:10) {
	r <- knn.reg(app, NULL, applpsa, k=i)
	res2[i] <- r$PRESS
	res2
}

plot(res)

