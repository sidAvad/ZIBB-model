inequality <- function(theta,y){
 
  B <- theta[c(1:length(theta)-1)]
  Beta <- B[c(1,2,3)]
  Theta <- B[c(4,5,6)]
  
  constr <- vector()
  constr <- (Beta - Theta)
     
}