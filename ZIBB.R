ZIBB <- function(a,b,n){
        
        ## Generates a random sample of 10,000 zero-inflated BetaBinomial
        ## numbers given the parameters alpha,beta and n
        ## Note : Takes pi to be uniformly sampled from 0 to 1
       y<- vector()
       
         for (i in 1:10000){
               
                random <- runif(1)
                pi <- runif(1)
                z <- betabin(a,b,n,0)
                f0 <- 1-pi + pi*z
                cdf <- f0    
     
                for (k in 0:n){
                        if(cdf <= random){
                                f <- pi*betabin(a,b,n,k+1)         
                                cdf <- cdf + f
                         
                                         }else{break}
                
                               }
                y[i] <- k 
                
                          }

return(y)

}