# Inactivation 
inactivation <- function( trich, time, temp ){
  y <- trich * exp( - time * temp/ 1000 ) 
  
  # Afronden is niet niet goed. Gebruik binomiaal
  fract <- y - floor( y )
  y <- floor( y) + rbinom( n=length(fract), size=1, prob=fract )
  return( y )
    
} 