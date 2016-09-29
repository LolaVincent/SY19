x0 <- 0.5
n <- 50
sig <- 0.5
Ey0 <- 1+5*x0^2
Kmax <- 40
N <- 10000
y0 <- rep(0,N)
yhat <- matrix(0,N, Kmax) #N lignes et Kmax colonnes
for (i in 1:N){ # On tire un ensemble d'apprentissage
	x <- runif(n)
	y <- 1+5*x^2+sig * rnorm(n)
	d <- abs(x-x0)
	tri <- sort(d, index.return=TRUE)
	#Tirage du y0
	y0[i] <- Ey0 + sig*rnorm(1)
	for(K in 1:Kmax) {
		yhat[i,K] <- mean(y[tri$ix[1:K]])
	}
}
error <- rep(0,Kmax)
biais2 <- rep(0, Kmax)
variance <- rep(0, Kmax)
for(K in 1:Kmax) {
	error[K] <- mean((y0-yhat[,K])^2)
	#Biais = (E(fhatx(x0))-f(x0))^2
	biais2[K] <- (mean(yhat[,K])-Ey0)^2
	variance[K] <- var(yhat[,K])
}
plot(1:Kmax,error, type="l", ylim=range(error, biais2, variance))
lines(1:Kmax, biais2, lty=2)
#lines(1:Kmax, variance, lty=2)
#lines(1:Kmax, variance+biais2+sig^2, lty=2)