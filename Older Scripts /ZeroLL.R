ZeroLL <- function(theta,y){
  
  lastcol <- ncol(y)
  y1 <- y[,lastcol]
  X <- y[,1:ncol(y)-1]
  
  ones <- rep(1,nrow(y))
  X1 <- cbind(ones,X[,1])
  X2 <- cbind(ones,X[,2])
  n <- 32 
  
  phi <- theta[length(theta)]
  
  B <- theta[c(1:length(theta)-1)]
  B1 <- B[c(1,2)] 
  B2 <- B[c(3,4)] 
  
  pi <- sigmoid(X2%*%B2) 
  mupi <- sigmoid(X1%*%B1)  
  mu <- mupi/pi 
  
  loglik <- vector()

for (i in 1:length(y1)){
  
  if (mu[i] < 1){
    
    if(y1[i] != 0){
        
        loglik[i] <- log(pi[i]) + lgamma(phi) - lgamma(phi*mu[i]) - 
          lgamma(phi*(1-mu[i])) + lgamma(y1[i]+phi*mu[i]) +
          lgamma(n - y1[i] + phi*(1-mu[i])) - lgamma(n+phi) - 
          lbeta(y1[i]+1,n-y1[i]+1)
      
      }else{
        loglik[i] <- log(1-pi[i] + (pi[i]*beta(y1[i]+phi*mu[i],n -y1[i] + phi*(1-mu[i])))/(beta((phi*mu[i]),phi*(1-mu[i]))*beta(y1[i]+1,n-y1[i]+1)))
       
       
         }
      
    }else{loglik[i] <- 0}
     
}
  
  final <- sum(loglik)
  final1 <- -final
  print(final1)
}
  


