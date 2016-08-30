zerogenv <- function(B1,B2,sig){
  
  
  x1 <- rnorm(10000, mean=0, sd=1)
  x2 <- rnorm(10000, mean=0, sd=1)
  x <- cbind(x1,x2)
  y <- vector()
  muvec <- vector()
  
  ones <- rep(1,10000)
  X1 <- cbind(ones,x1)
  X2 <- cbind(ones,x2)
  pi <- sigmoid(X2%*%B2)
  mupi <- sigmoid(X1%*%B1)  
  muin <- mupi/pi
  muin <- 0.5
  ##index <- muin < 1
  ##muin <- muin[index]
  y<- rZIBB(10000, mu = muin, sigma=sig, nu=0.1, bd=32)
  ##y <- y[index]
  ##x <- x[index,]
  ##Y1 <- cbind(x,y)
 
}