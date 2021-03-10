dose.response <- function( ab, d )
{
  if( length(d)==0){
    return( 0 )
  }else{
    r <- 0.7
    # Prob survival
    index <- sample.int(1000, size=1 )
    a <- ab[ index, 1 ]
    b <- ab[ index, 2 ]
    p <- rbeta( n=1, shape1=a, shape2=b )
  
    return( 1 + exp( d *log(1-p) ) - exp(d*log(1-r*p))- exp(d*log(1-(1-r)*p) )  )
  }
}