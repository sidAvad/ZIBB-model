ll <- function(theta,y){
  
        ## evaluates the likelihood function of a 
        ## beta-binomial distribution given theta and y
        ## note : n is specified inside the function 
  
  lastcol <- ncol(y)
  y1 <- y[,lastcol]
  X <- y[,1:ncol(y)-1]
  
  
  ones <- rep(1,nrow(y))
  X <- cbind(ones,X)
  n <- 10
  
  rho <- 1/(1+exp(-theta[length(theta)]))
  print(rho)
  phi <- (1-rho)/rho
  print(phi)
  
  B <- theta[c(1:length(theta)-1)]
  
  Gamma <- B[c(1,2,3)] 
  Alpha <- B[c(4,5,6)]
  
  Beta <- Gamma + exp(Alpha)
  
  loglik <- vector(mode="numeric", length(y1))
  
  ## YOU HAVE TO LOOP OVER THE i VALUES HERE
 
  
  for (i in 1:length(y1)){
          
          pi <- 1/(1+exp(-X[i,]%*%Beta)) 
          mupi <- 1/(1+exp(-X[i,]%*%Gamma)) 
          mu <- mupi/pi

      if(y1[i] != 0){
        

        loglik[i] <- log(pi) + lgamma(phi) - lgamma(phi*mu) - 
          lgamma(phi*(1-mu)) + lgamma(y1[i]+phi*mu) +
          lgamma(n - y1[i] + phi*(1-mu)) - lgamma(n+phi) - 
          lgamma(y1[i]+1)-lgamma(n-y1[i]+1) + lgamma(n+1)
        
      }else{
        loglik[i] <- log(1-pi + choose(n,y1[i])*pi*exp(lbeta(y1[i]+phi*mu,n -y1[i] + phi*(1-mu))-lbeta((phi*mu),phi*(1-mu))))
        }

  } 
  
  
  final <- sum(loglik)

  final1 <- -final
  print(final1)
  

 }





