#----------------------------------------------
# functions
#----------------------------------------------

add.to.swine.table <- function( swine.table, i, nSwine, n.zeros, n.nzeros ){
  rbind( swine.table, 
         data.frame( simulation = i,
                     nSwine=nSwine,
                     n.falseneg=n.zeros + n.nzeros,
                     p.falseneg=(n.zeros + n.nzeros)/nSwine,
                     n.zeros=n.zeros, 
                     n.nzeros=n.nzeros,
                     # Prevalence of positive carcasses, 
                     # over all carcasses from false negative batche
                     p.carc=ifelse( n.nzeros!=0, n.nzeros/(n.zeros+n.nzeros), NA))) }

sim.swine <- function( m, k, nSwine, swine_per_pool, propDiaphragm, a, b ){
  
  # Generate nSwine samples of larva in 100g (m and k times 2)
  larvaInHunderdGrams <- rnbinom( n=nSwine, size=2*k, prob= k/(k + m))
  
  # Sample five grams, uniformly distributed over muscle group
  larvaInFiveGrams <- rbinom( n=length( larvaInHunderdGrams), size=larvaInHunderdGrams, prob=propDiaphragm )
  
  # Make pools of swine_per_pool diaphragm samples
  df.swine <- data.frame( pool= ceiling((1:nSwine)/swine_per_pool ), larva=larvaInFiveGrams )
  df.pool <- df.swine %>% group_by( pool ) %>% summarise( larvaInPool = sum( larva ))
  nPool <- nrow( df.pool )
  
  # For each pool, recovery beta-binomial, then a bernoulli trial
  recoveryProb <- with( df.pool, 1- (gamma(a)/beta(a,b)) * exp(lgamma( b + larvaInPool )-lgamma( a+b+larvaInPool )))
  detected  <- rbinom( n=nPool, size=1, prob=recoveryProb )
  
  # Find those pools that were not detected, but positive
  escaped <- which( detected==0 & df.pool$larvaInPool > 0 )
  
  # Corresponding swine
  escapedSwine <- tibble( larva = numeric(0) )
  
  
  if( length( escaped>0) ){
    for( i in 1:length( escaped) ){
      ind <- ((escaped[i]-1)*swine_per_pool+1) : (escaped[i]*swine_per_pool)
      escapedSwine <- rbind( escapedSwine, tibble( larva=larvaInHunderdGrams[ ind ]) )
    }
  } else {
    escapedSwine <- rbind( escapedSwine, tibble( larva=NA) )
  }
  
  # Tally, make frequency table
  escapedSwine <- escapedSwine %>% 
    group_by( larva ) %>% 
    tally %>% 
    select( larva, Freq=n ) 
  return( escapedSwine )
}

####
# Sampling from the NM, with m observations in category 1,
# p is the probability vector. Generate n variates
# Procedure found at: https://books.google.nl/books?id=BaKVBbB8O7oC&pg=PA148&lpg=PA148&dq=sampling+%22negative+multinomial%22+distribution&source=bl&ots=B6dD5DYKBs&sig=_JSRk-NsXZ2RNE1RxJGf-GxBf_s&hl=nl&sa=X&ei=X_c1Vc69EYvaauj6gLAB&ved=0CCAQ6AEwADgU#v=onepage&q=sampling%20%22negative%20multinomial%22%20distribution&f=false
####
sampleNM <- function( m, p, n ){
  w <- rgamma( n, shape=m, rate=p[1] )
  result <- tibble( shoulder=numeric(n), 
                    belly=numeric(n),
                    loin=numeric(n),
                    ham=numeric(n),
                    other=numeric(n))
  for( i in 1:n )
    result[i,] <- t( rpois( (length(p)-1) , lambda=p[-1]*w[i] ) )
  return( result )
}

########################################
# Infection status list of swine  #
########################################
infec.stat.swine <- function(m, k, nSwine, swine_per_pool, propDiaphragm, sim_max, alpha, beta){
  # number of larvae of false negative carcasses
  # larva=larvaInHunderdGrams of diaphragma 
  # freq in how many swine the number of larvae occur
  
  larvae.dia <- map_dfr( 1:sim_max, 
                         function(x){
                           sim.swine( m, k, nSwine, swine_per_pool, propDiaphragm, alpha, beta) %>% 
                             mutate( iteration=x) })
}

my_rmultinom <- function( larvae_per_part, portions_per_part,...){
  return( cbind(...,
                larvae_per_part,
                portions_per_part,
                tibble( portion=1:portions_per_part,
                        larvae_per_portion=rmultinom(1, larvae_per_part, 
                                                     rep(1/portions_per_part, portions_per_part))[,1])))
}
