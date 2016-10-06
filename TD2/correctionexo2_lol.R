n <- 20 
x1 <- rnorm(n,0,1)
x2 <- rnorm(n,0,1)
X <- cbind(rep(1,n),x1, x2)

  
N <- 5000
I <- matrix(0,N,3)
for(i in 1:N){
  y<-Ey+rnorm(n,0,sig)
  reg <- lm(y~x1+x2)
  CI <- confint(reg)
  
}
colMeans(I)
mean(apply(I,1,min))

# intervalles de confiance et de prédiction
x0 <- c(0.9,0.9)
Ey0=beta[1]+0.9*beta[2]+0.9*beta[3]
N <- 5000
IC <- rep(0,N)
IP <- IC

for ( i in 1:N){
  y<-Ey+rnorm(n,0,sig)
  y0<-EyO+rnorm(n,0,sig)
  reg <- lm(y~x1+x2)
  int <- predict(reg, int="c", newdata=data.frame(x1=0.9,x2=0.9))
  IC[i] <- (int[,2] <= y0) & (int[,3] <= y0)
  int <- predict(reg, int="p", newdata=data.frame(x1=0.9,x2=0.9))
  IP[i] <- (int[,2] <= y0) & (int[,3] <= y0)
}
mean(IP)
mean(IC)