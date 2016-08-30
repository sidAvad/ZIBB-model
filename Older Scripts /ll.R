ll <- function(theta,y){
  
  lastcol <- ncol(y)
  y1 <- y[,lastcol]
  X <- y[,1:ncol(Y)-1]
  n <- 32 
  phi <- theta[1];
  B <- theta[c(2:length(theta))]
  
  ##ones <- rep(1,nrow(X))
  ##X <- cbind(ones,X)
  mu <- sigmoid(X%*%B)
 
  
  loglik <- lgamma(phi) - lgamma(phi*mu) - 
    lgamma(phi*(1-mu)) + lgamma(y1+phi*mu) +
    lgamma(n - y1 + phi*(1-mu)) - lgamma(n+phi) - 
    lbeta(y1+1,n-y1+1)
  
  
  final <- as.numeric(sum(loglik))
  final2 <- -final
  
  print(final2)
}
