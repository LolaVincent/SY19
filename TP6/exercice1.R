#Génaration de l'échantillon
n <- 100
mu <- runif(1, min=1.5, max=3)
sigma <- runif(1)
pi <- runif(1)
theta <- c(mu, sigma, pi)

a <- sample(c(1:100), 1)
c <- 1/(2*a) #densité loi uniforme

phi <- rnorm(n, mu, sigma) #densité loi normale

p <- pi * phi + (1-pi) * c #densité mélange

boxplot(p)


#Algorithme EM
EM <- function() {
  gamma <- NULL
  i <- 0
  #Initial guesses : 
  mu.hat <- runif(1, min=1.5, max=3)
  sigma.hat <- runif(1)
  pi.hat <- runif(1)
  
  epsilon <- 0.05 #convergence estimée
  
  repeat {
    i <- i + 1
    #1 Expectation Step : compute the responsabilities
    gamma <- cbind(gamma,(pi.hat[i]*rnorm(n, mu.hat[i], sigma.hat[i]))/(pi.hat[i]*rnorm(n, mu.hat[i], sigma.hat[i]) + (1-pi.hat[i]) * c))
    
    #2 Maximization Step : compute the weighted means and variances
    mu.num <- 0
    gamma.sum <- 0
    for (j in 1:n) {
      mu.num <- mu.num + gamma[j, i] * p[j]
      gamma.sum <- gamma.sum + gamma[j, i]
    }
    mu.hat[i+1] <- mu.num/gamma.sum
    
    sigma.num <- 0
    for (j in 1:n) {
      sigma.num <- sigma.num + gamma[j,i] * (p[j] - mu.hat[i+1])^2
    }
    sigma.hat[i+1] <- sqrt(sigma.num/gamma.sum) #prend des valeurs négatives
    
    pi.hat[i+1] <- gamma.sum/n
    
    #print(pi.hat[i+1])
    
    #Condition d'arrêt
    if(abs(pi.hat[i] - pi.hat[i+1])<=epsilon || i==50){
      theta.hat$mu <- mu.hat 
      theta.hat$sigma <- sigma.hat
      theta.hat$pi <- pi.hat
      break
    }
  }
theta.hat
}

theta.hat <- EM()

#Différentes initialisations :
it <- 10
theta.hat <- vector(length = it)
for(i in 1:it) {
  theta.hat[i] <- EM()
}
