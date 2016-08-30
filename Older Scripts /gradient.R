ds$grad.log.density <- function(theta,y){
        
        ## Evaluates the gradient of the likelihood function of ZIBB 
        ## given theta and the data (y)
        y <-r 
        lastcol <- ncol(y)
        y1 <- y[,lastcol]
        X <- y[,1:ncol(y)-1]
        
        ones <- rep(1,nrow(y))
        X <- cbind(ones,X)
        n <- 10
        
        rho <- theta[length(theta)]
        phi <- (1-rho)/rho
        
        B <- theta[c(1:length(theta)-1)]
        Alpha <- B[c(1,2,3)] 
        Gamma <- B[c(4,5,6)] 
        Beta <- Gamma + exp(Alpha)
        
        
        pi <- sigmoid(X%*%Beta) 
        mupi <- sigmoid(X%*%Gamma)  
        mu <- mupi/pi
        
        gradb <- vector()
        gradt <- vector()
        
        
        P2 <- gamma(n+phi)
        P1 <- gamma(phi)
        
        der <- matrix(nrow=length(y1),ncol=7)
        
        for (i in 1:length(y1)){
                
                cb <- (mupi[i]/(pi[i])^2)*(1-pi[i])*(pi[i])
                ct <- (1/pi[i])*(mupi[i])*(1-mupi[i])
                gradb <- cb*X[i,]          ## Derivatives of mu w.r.t beta(gradb) and theta(gradt)
                gradt <- ct*X[i,]
                grad <- append(gradb,gradt)
                
                
                
                if(y1[i] != 0){
                       
                        a <- phi*mu[i]
                        b <- phi*(1-mu[i])
                        
                        ## derivative w.r.t thetas ( all params except phi)
                        
                        T2mu <-  digamma(y1[i]+a)*(phi*grad) + digamma(n-y1[i]+b)*(-phi*grad) - 
                                    digamma(a)*(phi*grad) - digamma(b)*(-phi*grad)
                        
                        ## derivative w.r.t phi
                        
                        T2phi <- digamma(y1[i]+a)*mu[i] + digamma(n-y1[i]+b)*(1-mu[i]) - digamma(n+phi) + 
                                 digamma(phi) - digamma(a)*mu[i] - digamma(b)*(1-mu[i])
                        
                        app <- append(T2mu,T2phi)
                        der[i,] <- app
                        
                }else{
                        
                        R <- beta(y[i]+mu[i]*phi,n-y[i]+phi*(1-mu[i]))/beta(phi*mu[i],phi*(1-mu[i]))
                        A1 <- pi[i]*choose(n,y1[i])/(1-pi[i] + pi[i]*choose(n,y1[i])*R)
                        a <- phi*mu[i]
                        b <- phi*(1-mu[i])
                        D1 <- gamma(a)
                        D2 <- gamma(b)
                        D3 <- gamma(y1[i]+ a)
                        D4 <- gamma(n-y1[i]+ b)
                        
                        C <- A1*gamma(phi)/gamma(n+phi)
                        
                        ## derivative w.r.t thetas ( all params except phi)

                        T1mu <- (C/(gamma(a)*gamma(b))^2)*( D1*D2*(D3*D4*digamma(n-y1[i]+b)*(-phi*grad) + 
                                           D4*D3*digamma(y1[i]+a)*(phi*grad)) - 
                                   D3*D4*(D1*D2*digamma(b)*(-phi*grad) + 
                                           D2*D1*digamma(a)*(phi*grad))
                                 )
                        
                        ## derivative w.r.t phi
                        
                        T1phi <- (A1/(gamma(n+phi)*gamma(a)*gamma(b))^2)*(P2*D1*D2*(P1*D3*D4*digamma(n-y1[i]+b)*(1-mu[i]) + 
                                                P1*D4*D3*digamma(y1[i]+a)*(mu[i]) + D3*D4*P1*digamma(phi)) - 
                                                 P1*D3*D4*(P2*D1*D2*digamma(b)*(1-mu[i]) + 
                                                 P2*D2*D1*digamma(a)*(mu[i]) + D1*D2*P2*digamma(n+phi))
                                                 )
                                                
                        der[i,] <-   append(T1mu,T1phi)                  
                                
                                         
                       }
                
                  }
        
derfinal <- colSums(der)
derfinal1 <- -derfinal

                
}
