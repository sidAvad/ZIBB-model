zibbone <- function(a,b,n,pi){
        
##Calls betabin and randomly samples one value from ZIBB distribution. 
        
        z <- betabin(a,b,n,0)
        f0 <- 1-pi + pi*z
        cdf <- f0    
        
        
        random = runif(1)
        
        
        for (k in 0:n){
                if(cdf <= random){
                        f <- pi*betabin(a,b,n,k+1)         
                        cdf <- cdf + f
                        
                }else{break}
                
        }
        
        y <- k 
        
}       
        
        
