# Y correspond au dataset

x = 0.75
for( i in 
	n <- sample(1, 100, 1)
	epsilon <- rnorm(n, mean = 0, sd=0.5, log=FALSE)
	X <- runif(n, 0, 1) # ensemble d'apprentissage
	Y <- 1 + 5*sqrt(X) + epsilon
	for (j in 1:40) {
		knn.reg(X, x, y,