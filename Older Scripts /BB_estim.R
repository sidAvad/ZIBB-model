 # this script will estimate the parameters from data fit to 
 # a beta-binomial logistic regression model. 

BB_estim <- function(X,y,n) {
  
  m <- nrow(X)           ## No. of training examples
  p <- ncol(X) + 1       ## No. of parameters (p = ncol(X) + 1)
  
  phi <- runif(1,0,10)    ## randomly initialized phi between 0 and 10
  
  ones <- rep(1, each = m )
  X1 <- cbind(ones,X)       ## add a column of ones to X. 
  
  B <- vector("numeric",p)   ## vector of Betas  
  
  ####### FUNCTION BODY MAIN ############################
  
  ## 1)  Initialize and calculate the value of log-likelihood
  loglik <- vector("numeric", m) 
  loglik <- ll(X1,y,phi,B,n=32)
    
  ## 2) Initialize and calculate the derivative
  gradll[i] <- digamma()
  
  
  
}
