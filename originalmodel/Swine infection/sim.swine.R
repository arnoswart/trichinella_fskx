sim.swine <- function( m, k, n.swine, swine.per.pool, propDiaphragm, sens=1 ){
  # a and b for beta distribution prob of detection 1 larva
  a <- 1.53575 
  b <- 0.289405

  # Generate n.swine samples of larva in 100g (m and k times 2)
  larvaInHunderdGrams <- rnbinom( n= n.swine, size= 2*k, prob= k/(k + m))
  
  # Sample five grams, uniformly distributed over muscle group
  larvaInFiveGrams <- rbinom( n=length( larvaInHunderdGrams), size=larvaInHunderdGrams, prob=propDiaphragm )
  
  # Make pools of swine.per.pool diaphragm samples
  df.swine <- data.frame( pool= ceiling((1:n.swine)/swine.per.pool ), larva=larvaInFiveGrams )
  df.pool <- df.swine %>% group_by( pool ) %>% summarise( larvaInPool = sum( larva ))
  nPool <- nrow( df.pool )
  
  # For each pool, recovery beta-binomial, then a bernoulli trial
  recoveryProb <- with( df.pool, 1- (gamma(a)/beta(a,b)) * exp(lgamma( b + larvaInPool )-lgamma( a+b+larvaInPool )))
  detected  <- rbinom( n=nPool, size=1, prob=recoveryProb*sens )
  
  # Find those pools that were not detected, but positive
  escaped <- which( detected==0 & df.pool$larvaInPool > 0 )
  
  # Corresponding swine
  escapedSwine <- data.frame( larva = numeric(0) )
  
  
  if( length( escaped>0) )
    for( i in 1:length( escaped) ){
      ind <- ((escaped[i]-1)*swine.per.pool+1) : (escaped[i]*swine.per.pool)
      escapedSwine <- rbind( escapedSwine, data.frame( larva=larvaInHunderdGrams[ ind ]) )
    }
  # Tally, make frequency table
  escapedSwine <- escapedSwine %>% group_by( larva ) %>% tally %>% select( larva, Freq=n )
    
  return( escapedSwine )
}