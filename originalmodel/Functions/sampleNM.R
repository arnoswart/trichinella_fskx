####
# Sampling from the NM, with m observations in category 1,
# p is the probability vector. Generate n variates
# Procedure found at: https://books.google.nl/books?id=BaKVBbB8O7oC&pg=PA148&lpg=PA148&dq=sampling+%22negative+multinomial%22+distribution&source=bl&ots=B6dD5DYKBs&sig=_JSRk-NsXZ2RNE1RxJGf-GxBf_s&hl=nl&sa=X&ei=X_c1Vc69EYvaauj6gLAB&ved=0CCAQ6AEwADgU#v=onepage&q=sampling%20%22negative%20multinomial%22%20distribution&f=false
####
sampleNM <- function( m, p, n ){
  w <- rgamma( n, shape=m, rate=p[1] )
  result <- matrix( 0, nrow = n, ncol=length( p )-1 )
  for( i in 1:n )
    result[i,] <- rpois( (length(p)-1) , lambda=p[-1]*w[i] )
  return( result )
}