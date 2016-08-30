muvec <- matrix(nrow=10000, ncol=500)
pivec <- matrix(nrow=10000, ncol=500)
yvec <- matrix(nrow=10000, ncol=500)
phivec <- vector()
pivec2 <- matrix(nrow=10000, ncol=500)

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
        
        rho <- theta[length(theta)]
        phi <<- (1-rho)/rho
        
        B <- theta[c(1:length(theta)-1)]
        
        Gamma <- B[c(1,2,3)] 
        Alpha <- B[c(4,5,6)]
        
        Beta <- Gamma + exp(Alpha)
        
        
        pi <- 1/(1+exp(-X%*%Beta)) 
        mupi <- 1/(1+exp(-X%*%Gamma))  
        mu <- mupi/pi
        
        
        loglik <- vector()
        ## YOU HAVE TO LOOP OVER THE i VALUES HERE
        
        j <- 0 
        
        for (i in 1:length(y1)){
                
                
                
                if(y1[i] != 0){
                        
                        
                        loglik[i] <- log(pi[i]) + lgamma(phi) - lgamma(phi*mu[i]) - 
                                lgamma(phi*(1-mu[i])) + lgamma(y1[i]+phi*mu[i]) +
                                lgamma(n - y1[i] + phi*(1-mu[i])) - lgamma(n+phi) - 
                                lgamma(y1[i]+1)-lgamma(n-y1[i]+1) + lgamma(n+2)-log(n+1)
                        
                }else{
                        loglik[i] <- log(1-pi[i] + choose(n,y1[i])*pi[i]*exp(lbeta(y1[i]+phi*mu[i],n -y1[i] + phi*(1-mu[i]))-lbeta((phi*mu[i]),phi*(1-mu[i]))))
                }
                
                if(is.na(loglik[i])){
                        muvec[j,k] <<- mu[i]
                        pivec[j,k] <<- pi[i]
                        yvec[j,k] <<- y1[i]
                        phivec[k] <<- phi
                        j <- j + 1
                }
                
                pivec2[i,l] <<- pi[i]  
        }
        
        
        final <- sum(loglik)
        if(is.na(final)){
                k <<- k + 1
        }
        final1 <- -final
        print(final1)
        
        l <<- l + 1 
}



