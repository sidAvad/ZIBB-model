initialize <- function(X){
  ones <- rep(1,nrow(X))
  X <- cbind(ones,X)
  p <- ncol(X)
  B <- runif(p,0,10)
  phi <- runif(1,1,10)
  theta <- append(B,phi,0)
}
