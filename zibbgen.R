zibbgen <- function(theta, rho,n){
        
        ## Same as ZIBB but with linear structure:
        ## mupi and pi depend on covariates 
        ##  a and b are calculated from pho and mu
        ## please note that rho is between 0 and 1.
        
        x1 <- runif(10000)
        x2 <- runif(10000)
        ones <- rep(1,10000)
        X <- cbind(ones,x1/10,x2/10)
        
        Gamma <- theta[c(1,2,3)]
        Alpha<- theta[c(4,5,6)]
        Beta <- Gamma + exp(Alpha)
        
        print(Beta)
        print(Gamma)
        
        y <- vector()
        
     
        
        for (i in 1:10000){
                
                pi <- 1/(1+exp(-X[i,]%*%Beta)) 
                pi_p<- 1/(1+exp(-X[i,]%*%Gamma))  
                
                p <- pi_p/pi
               

                phi <- (1-rho)/rho
                a <- phi*p
                b <- phi*(1-p)
                
              y[i] <- zibbone(a,b,n,pi)
       
               }

        Y <- cbind(x1/10,x2/10,y)
        return(Y)
        
}