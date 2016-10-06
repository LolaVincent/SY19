# On fixe X1 et X2 (loi uniforme),  les beta j, le sigma



n <- 100
N <- 100
l <- 60
sig <- 1
epsilon <- rnorm(n, 0, sig)
X1 <- runif(n, 0, 1)
X2 <- runif(n, 0, 1)
beta0 <- 2
beta1 <- 0.7
beta2 <- 2
Y <- rnorm(beta0 + beta1*x1 + beta2*x2, sig^2)
X1 <- X1[1:l]
X2 <- X2[1:l]
for (i in 1:N){
  Yapp <- sample(Y, 60)
  reg <- lm(Yapp ~ X1 + X2)
  predict(reg, int="c")
  }
