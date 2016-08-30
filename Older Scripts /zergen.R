zerogen <- function(B1,B2,sig){

    
    x1 <- rnorm(10000, mean=0, sd=1)
    x2 <- rnorm(10000, mean=0, sd=1)
    x <- cbind(x1,x2)
    y <- vector()
    muvec <- vector()
    
    for (i in 1:10000){
      X1 <- as.vector(cbind(1,x1[i]/10))
      X2 <- as.vector(cbind(1,x2[i]/10))
      pi <- sigmoid(X2%*%B2)
      mupi <- sigmoid(X1%*%B1)  
      muin <- mupi/pi
      if (muin < 1){
        muvec[i] <- muin
        y[i] <- rZIBB(1, mu = muin, sigma=sig, nu=0.1, bd=32)
      }else {
        y[i] <- NA}
    
      }
    y <- as.vector(y)
    Y1 <- cbind(x,y)
    Y1 <- Y1[complete.cases(Y1),]
}