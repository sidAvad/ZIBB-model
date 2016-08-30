cbetabin <- function(a,b,n,k){
        
        ## Returns a list containing the cdf (at x = k) of a beta binomial 
        ##distribution and a list of the pmf values for x = 0 to k
        
        sum <- 0
        y <- vector()
        
        for (i in 0:k){
                summand <- betabin(a,b,n,i)
                sum <- sum + summand
                y[i+1] <- summand
                }
        
       
  return(list(sum,y))
       
}