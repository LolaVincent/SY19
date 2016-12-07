# correction exercice 1 tp6

N=100
for (i in 1:N){
  y[i] <- sample(c(1,0), size=1, prob=c(pi, 1-pi))
  if (y[i] ==1) 
    x[i] <- rnorm(1, mu, sigma) 
  else 
    x[i] <- runif (1, min=-a, max=a)
}

N1 <- rbinom(1, size=N, proba=pi)
x1 <- rnorm(N1, mu, sigma)
x2 <- runif(N-N1, min=-a, max=a)
x <- c(x1, x2)