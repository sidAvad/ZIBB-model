density <- function(theta,y){
        
        list(density=log.density(theta,y),gradient=grad.log.density(theta,y))
      
}