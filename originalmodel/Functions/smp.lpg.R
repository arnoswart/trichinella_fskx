smp.lpg <- function( lpg.part, freq, n ){
  lpg.smp <- integer( n )
  cs <- cumsum(freq)
  smp <- sample.int(max(cs), n, replace=TRUE )
  
  m <- ncol( lpg.part )
  
  for( i in 1:n ){
    index <- which( cs >= smp[i] )[1]
    col <- sample.int( 1, n=m ) + 1
    lpg.smp[i] <-  lpg.part[ index, col ]
  }
  
  return( lpg.smp )
}