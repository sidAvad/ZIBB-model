generator <- function(B,r){
  
x1 <- sample(0:10000,10000)
x2 <- sample(0:10000,10000)
ones <- rep(1,10000)
x <- cbind(ones,x1/10000,x2/10000)
y <- vector()
muvec <- vector()
for (i in 1:10000){
  A <- x[i,]
  mu <- sigmoid(A%*%B)
  muvec[i] <- mu
  y[i] <- rbetabinom(1,32,mu,rho = r)
  }
y <- as.vector(y)

}
