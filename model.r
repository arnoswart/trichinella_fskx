#----------------------------------------------
# Reference and info
#----------------------------------------------
# The mathematical model is published in Frits Franssen, Arno Swart, Joke van der Giessen, Arie Havelaar, Katsuhisa Takumi, Parasite to patient: A quantitative risk model for Trichinella spp. in pork and wild boar meat, International Journal of Food Microbiology, Volume 241, 2017, Pages 262-275, ISSN 0168-1605.

# Trichinella muscle larvae (ML)

#----------------------------------------------
# library
#----------------------------------------------
library( dplyr )

#----------------------------------------------
# name/load input
#----------------------------------------------
names(w)<-c('diaphragm', 'shoulder', 'belly','loin','ham', 'other')
names(p)<-c('diaphragm', 'shoulder', 'belly','loin','ham', 'other')

#----------------------------------------------
# load functions
#----------------------------------------------
add.table.row <- function( table.input, n.portions, n.zeros, n.cooked.zeros, p.cooked, p.ill, p.swine, type )
{
  return( rbind( table.input, data.frame( n.portions = n.portions,
                                          n.zeros = n.zeros,
                                          p.portion = 1 - n.zeros/n.portions,
                                          n.cooked.zeros = n.cooked.zeros,
                                          p.cooked = p.cooked,
                                          p.ill = p.cooked * p.ill,
                                          n.ill = p.cooked * p.ill * n.portions,
                                          n.ill.overall = p.cooked * p.ill * n.portions * p.swine,
                                          per.million = p.cooked * p.ill * p.swine * 10^6,
                                          type = type
  )
  )
  )
}

add.to.table <- function( table.input, nCarc, w, p.swine, shoulder, belly, loin ){
  
  # Shoulder 
  n.portions     <- nCarc * w[2]               # nr. of carcassess * nr of portions per carcass
  n.zeros        <- shoulder$zeros
  n.cooked.zeros <- shoulder$zeros.after.cooking + shoulder$zeros # Need to add them up!
  p.cooked       <- 1 - n.cooked.zeros/n.portions
  p.ill          <- mean( shoulder$ill )
  
  table.input <- add.table.row( table.input, n.portions, n.zeros, n.cooked.zeros, p.cooked, p.ill, p.swine, "shoulder" )
  
  # Belly
  n.portions     <- nCarc * w[3]               # nr. of carcassess * nr of portions per carcass
  n.zeros        <- belly$zeros
  n.cooked.zeros <- belly$zeros.after.cooking + belly$zeros # Need to add them up!
  p.cooked       <- 1 - n.cooked.zeros/n.portions
  p.ill          <- mean( belly$ill )
  
  table.input <- add.table.row( table.input, n.portions, n.zeros, n.cooked.zeros, p.cooked, p.ill, p.swine, "belly" )
  
  # Loin
  n.portions     <- nCarc * w[4]               # nr. of carcassess * nr of portions per carcass
  n.zeros        <- loin$zeros
  n.cooked.zeros <- loin$zeros.after.cooking + loin$zeros # Need to add them up!
  p.cooked       <- 1 - n.cooked.zeros/n.portions
  p.ill          <- mean( loin$ill )
  
  table.input <- add.table.row( table.input, n.portions, n.zeros, n.cooked.zeros, p.cooked, p.ill, p.swine, "loin" )
  
  return( table.input)
}

add.to.swine.table <- function( swine.table, nSwine, n.zeros, n.nzeros ){
  return( rbind( swine.table, data.frame( nSwine=nSwine,
                                          n.falseneg=n.zeros + n.nzeros,
                                          p.falseneg=(n.zeros + n.nzeros)/nSwine,
                                          n.zeros=n.zeros, 
                                          n.nzeros=n.nzeros, 
                                          p.carc=ifelse( n.nzeros!=0, n.nzeros/(n.zeros+n.nzeros), NA) # Prevalence of positive carcasses, over all carcasses from false negative batches
  )
  )
  )
}

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
  escapedSwine <- data.frame( larva = numeric(0) )
  
  
  if( length( escaped>0) )
    for( i in 1:length( escaped) ){
      ind <- ((escaped[i]-1)*swine_per_pool+1) : (escaped[i]*swine_per_pool)
      escapedSwine <- rbind( escapedSwine, data.frame( larva=larvaInHunderdGrams[ ind ]) )
    }
  # Tally, make frequency table
  escapedSwine <- escapedSwine %>% group_by( larva ) %>% tally %>% select( larva, Freq=n )
  
  return( escapedSwine )
}

inactivation <- function( k, alpha.plus, T.star, I0, T0,T1, t1 ){
  I0*((exp(k*T.star) + exp( k * T1 ))/(exp(k*T.star) +exp( k * T0 )))^(alpha.plus * t1 /( k*(T0-T1)))                                                                                          
}

#
# New inactivation function
#
inactivation_new <- function(I0, T0,T1, t1){
  # just using the final temperature for now.
  # later better do a piecewise linear approximation between T0 and T1
  temp = T1
  p_surv = -1.38e3 + 8.56*t1 + 1.09e1*temp - 4.06e-2*t1*temp +8.31e2*log(t1) +
            2.94e2*log(temp) -2.53e2*log(t1)*log(temp)
  return( I0*(1-p_surv) )
}

cook <- function( part ){
  is.welldone <- rbinom( length( part$x ), 1, 0.9 )
  
  cooked.rare   <- round( inactivation( k=0.17, alpha.plus=0.63, T.star=59.3, I0=part$x, T0=20,T1=54, t1=2.5 ) )
  cooked.welldone <- round( inactivation( k=0.17, alpha.plus=0.63, T.star=59.3, I0=cooked.rare, T0=54,T1=76.7, t1=7.5 ) )
  cooked.medium   <- round( inactivation( k=0.17, alpha.plus=0.63, T.star=59.3, I0=cooked.rare, T0=54,T1=63, t1=1.5 ) )
  
  ### Medium, USDA, 4 minutes cooking + 3 minutes rest ###
  cooked.medium <- round( inactivation( k=0.17, alpha.plus=0.63, T.star=59.3,I0=cooked.medium, T0=63,T1=62.9, t1=3 ) )
  
  ### Well done USDA ###
  # cooked.welldone <- round( inactivation( k=0.17, alpha.plus=0.63, T.star=59.3,I0=cooked.rare, T0=54,T1=71, t1=6.5 ) )
  
  ### Well done Chef ###
  # cooked.welldone <- round( inactivation( k=0.17, alpha.plus=0.63, T.star=59.3,I0=cooked.rare, T0=54,T1=76.7, t1=7.5 ) )
  
  ### Well done Traditional, lower limit ###
  # cooked.welldone <- round( inactivation( k=0.17, alpha.plus=0.63, T.star=59.3,I0=cooked.welldone, T0=76.7,T1=76.9, t1=11 ) )
  
  ### Well done Traditional, upper limit ###
  # cooked.welldone <- round( inactivation( k=0.17, alpha.plus=0.63, T.star=59.3,I0=cooked.welldone, T0=76.7,T1=76.9, t1=21 ) )
  
  ### cooked welldone +2 Swart ####
  # cooked.welldone <- round( inactivation( k=0.17, alpha.plus=0.63, T.star=59.3,I0=cooked.welldone, T0=54,T1=75, t1=2 ) )
  
  # Proportion welldone, proportion medium
  part$x <- is.welldone * cooked.welldone + (1-is.welldone) * cooked.medium
  
  # All medium
  # part$x <- cooked.medium
  # Cooked rare
  # part$x <- cooked.rare
  # With resting
  # part$x <- cooked.medium.rest
  
  part <- remove.zeros( part, cooked=T)
  
  return( part  )
}

remove.zeros <- function( part, cooked ){
  
  index <- part$x==0|part$x==1
  
  if( cooked ){
    part$zeros.after.cooking <- part$zeros.after.cooking + sum( index )
  }else{
    part$zeros <- part$zeros + sum( index )
  }
  
  part$x     <- part$x[!index]
  
  return( part )
}

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

dose.response <- function( ab, d, r )
{
  if( length(d)==0){
    return( 0 )
  }else{
    # Prob survival
    index <- sample.int(1000, size=1 )
    a <- ab[ index, 1 ]
    b <- ab[ index, 2 ]
    p <- rbeta( n=1, shape1=a, shape2=b )
    
    return( 1 + exp( d *log(1-p) ) - exp(d*log(1-r*p))- exp(d*log(1-(1-r)*p) )  )
  }
}

########################################
# Infection status list of swine  #
########################################
infec.stat.swine <- function(m, k, nSwine, swine_per_pool, propDiaphragm, sim_max, alpha, beta){
  larvae.dia <- list()
  
  for( i in 1:sim_max ){  
    escapedSwine <- sim.swine( m, k, nSwine, swine_per_pool, propDiaphragm, alpha, beta)
    
    if( nrow( escapedSwine )==0 ){
      larvae.dia[[i]] <- data.frame(larva=NA, Freq=NA)
    }else{
      larvae.dia[[i]] <- escapedSwine
    }
  }
  # number of larvae of false negative carcasses
  # larva=larvaInHunderdGrams of diaphragma 
  # freq in how many swine the number of larvae occur
  return(larvae.dia)
}

########################################
# Meat preparation and consumption
########################################
prep.consumption <- function(larvae.sim, w, p, ab, r, shoulder, belly, lion, ham, n.nzeros){
  
  ############################################
  # Make portions          #
  ############################################
  
  # next two lines proportionally pick a row
  # sets thresholed for how often learvea are found (independent how many larvae were found)
  smp <- sample.int(n.nzeros, 1 ) # n.nzeros= max(larvae$cs) right?
  row <- which( larvae.sim$cs >= smp )[1]
  
  # Make a realisation of division of larva over muscle groups,
  # to be interpreteted as total, i.e. per w[i] portions.
  
  sampleNM.res <- sampleNM( n=1, p=p, m=larvae.sim$larva[row]*w[1] ) # w[1] =w$diaphragma
  colnames(sampleNM.res) <- c('shoulder', 'belly','loin','ham', 'other') # =names(p)[-1]
  
  ############################################
  # Make portions (per muscle group)         #
  ############################################
  #in order this is 1 diaphragm, 2 shoulder, 3 belly,4 loin,5 ham, 6 other
  
  # shoulder$x <- rmultinom(1, sampleNM.res[2], rep(1/w[2], w[2]))
  # belly$x    <- rmultinom(1, sampleNM.res[3], rep(1/w[3], w[3]))
  # loin$x     <- rmultinom(1, sampleNM.res[4], rep(1/w[4], w[4]))
  # ham$x      <- rmultinom(1, sampleNM.res[5], rep(1/w[5], w[5]))
  shoulder$x <- rmultinom(1, sampleNM.res[,'shoulder'], rep(1/w[['shoulder']], w[['shoulder']]))
  belly$x    <- rmultinom(1, sampleNM.res[,'belly'], rep(1/w[['belly']], w[['belly']]))
  loin$x     <- rmultinom(1, sampleNM.res[,'loin'], rep(1/w[['loin']], w[['loin']]))
  ham$x      <- rmultinom(1, sampleNM.res[,'ham'], rep(1/w[['ham']], w[['ham']]))
  
  shoulder   <- remove.zeros( shoulder, cooked=F )
  loin       <- remove.zeros( loin, cooked=F )
  belly      <- remove.zeros( belly, cooked=F )
  ham        <- remove.zeros( ham, cooked=F )
  
  ############################################
  # Cook the portions                        #
  ############################################
  
  shoulder <- cook( shoulder ) # For convenience, the cooking includes removal of zeros
  loin     <- cook( loin )
  belly    <- cook( belly )
  ham      <- cook( ham ) 
  
  ############################################
  # dose-resp                                #
  ############################################
  shoulder$ill <- c( shoulder$ill, dose.response(ab=ab, d=shoulder$x, r=r) )
  loin$ill     <- c( loin$ill, dose.response(ab=ab, d=loin$x, r=r) )
  belly$ill    <- c( belly$ill, dose.response(ab=ab, d=belly$x, r=r) )
  ham$ill      <- c( ham$ill, dose.response(ab=ab, d=ham$x, r=r) )
  
  #output
  output <- list( shoulder=shoulder,
                  loin=loin,
                  belly=belly,
                  ham=ham
  )
  return(output)
}

#----------------------------------------------
# define parameters 
#----------------------------------------------

swine.table <- data.frame( 
  nSwine=numeric(0),
  n.falseneg=numeric(0), 
  p.falseneg=numeric(0), 
  n.zeros=numeric(0), 
  n.nzeros=numeric(0), 
  p.carc=numeric(0)
)

table.res <- data.frame(n.portions = numeric(0), 
                        n.zeros = numeric(0), 
                        p.portion = numeric(0), 
                        n.cooked.zeros = numeric(0), 
                        p.cooked = numeric(0), 
                        p.ill = numeric(0), 
                        n.ill = numeric(0),
                        n.ill.overall = numeric(0),
                        per.million = numeric(0),
                        type = character( 0)
)

#----------------------------------------------
# Run simulations 
#----------------------------------------------

for( larvae.sim in larvae.dia )
{ 
  if( nrow( larvae.sim) > 1 )
  {
    
    n.zeros   <- larvae.sim$Freq[1]            # Number of negative carcasses, from false negative carcasses
    larvae.sim    <- larvae.sim[-1, ]              # Row with zeros not needed any more.
    larvae.sim$cs <- cumsum(larvae.sim$Freq)       # Precalculate for later
    n.nzeros  <- larvae.sim$cs[length(larvae.sim$cs)] # How often were larvae found? => n.nzeros-times
    
    ############################################
    # Calculate total larvae in muscle groups (over all carcasses. #
    ############################################
    # 
    shoulder <- list( x=numeric(0), ill=numeric(0), zeros=0, zeros.after.cooking=0 )
    loin     <- list( x=numeric(0), ill=numeric(0), zeros=0, zeros.after.cooking=0 )
    belly    <- list( x=numeric(0), ill=numeric(0), zeros=0, zeros.after.cooking=0 )
    ham      <- list( x=numeric(0), ill=numeric(0), zeros=0, zeros.after.cooking=0 )
    
    for( i in 1:nCarc )
    {
      consumer.res <- prep.consumption(larvae.sim = larvae.sim, n.nzeros=n.nzeros, w = w, p = p, ab = ab, r=r, shoulder=shoulder, belly=belly, lion=lion, ham=ham ) 
      shoulder <- consumer.res$shoulder
      loin     <- consumer.res$loin
      belly    <- consumer.res$belly
      ham      <- consumer.res$ham
    }
    
    swine.table <- add.to.swine.table( swine.table, nSwine=nSwine, n.zeros=n.zeros, n.nzeros=n.nzeros )
    
    table.res <- add.to.table( table.res, nCarc, w, n.nzeros/nSwine, shoulder, belly, loin )
  } 
  else 
  { #no false negative batches this year
    swine.table <- add.to.swine.table( swine.table, nSwine, n.zeros=0, n.nzeros=0 )
    
    shoulder <- list( zeros = nCarc * w[2], zeros.after.cooking=0, ill=0 )
    belly <- list( zeros = nCarc * w[3], zeros.after.cooking=0, ill=0 )
    loin <- list( zeros = nCarc * w[4], zeros.after.cooking=0, ill=0 )
    table.res <- add.to.table( table.res, nCarc, w, p.swine=0, shoulder, belly, loin )
  }
  
}

#----------------------------------------------
# Process results
#----------------------------------------------
table.res.summary <- table.res %>% group_by( type) %>% summarize( p.portion.mean = mean( p.portion ),
                                                                  p.portion.p2.5 = quantile( p.portion, 0.025, na.rm=T ),
                                                                  p.portion.p97.5 = quantile( p.portion, 0.975 , na.rm=T),
                                                                  p.cooked.mean = mean( p.cooked),
                                                                  p.cooked.p2.5 = quantile( p.cooked, 0.025, na.rm=T),
                                                                  p.cooked.p97.5 = quantile( p.cooked, 0.975, na.rm=T),
                                                                  p.ill.mean = mean( p.ill),
                                                                  p.ill.p2.5 = quantile( p.ill, 0.025, na.rm=T),
                                                                  p.ill.p97.5 = quantile( p.ill, 0.975, na.rm=T),
                                                                  per.million.mean = mean( per.million ),
                                                                  per.million.p2.5 = quantile( per.million, 0.025, na.rm=T),
                                                                  per.million.p97.5 = quantile( per.million, 0.975, na.rm=T )
)

swine.table.summary <- swine.table %>% summarize( p.falseneg.mean = mean( p.falseneg, na.rm=T),
                                                  p.falseneg.p2.5 = quantile( p.falseneg, 0.025, na.rm=T),
                                                  p.falseneg.p97.5 = quantile( p.falseneg, 0.975, na.rm=T ),
                                                  p.carc.mean = mean( p.carc, na.rm=T),
                                                  p.carc.p2.5 = quantile( p.carc, 0.025, na.rm=T),
                                                  p.carc.p97.5 = quantile( p.carc, 0.975, na.rm=T )
)

# name results
names(table.res.summary$p.ill.mean)<-c('shoulder', 'belly', 'loin')
names(table.res.summary$p.portion.mean)<-c('shoulder', 'belly', 'loin')
names(table.res.summary$p.cooked.mean)<-c('shoulder', 'belly', 'loin')
names(table.res.summary$p.ill.mean)<-c('shoulder', 'belly', 'loin')
names(table.res.summary$per.million.mean)<-c('shoulder', 'belly', 'loin')


output.mean <- list(A.FlaseNegPool = swine.table.summary$p.falseneg.mean,
           B.PosCarcInPool = swine.table.summary$p.carc.mean,
           C.TrichInfecPortFromPosCarc = table.res.summary$p.portion.mean,
     D.TrichInfecCookPortFromPosCarc = table.res.summary$p.cooked.mean,
     E.PIllFromCookPortFormPosCarc = table.res.summary$p.ill.mean,
       F.NIllPerMilPort = table.res.summary$per.million.mean,
     G.AvgNIllPerMilPort= mean(table.res.summary$per.million.mean),
     K.TotPredHumCasePerYear = mean(table.res.summary$per.million.mean)*n_portions_per_person*pop*1e-6,
     L.PredHumCasePerMilPerYear = mean(table.res.summary$per.million.mean)*n_portions_per_person #mean(table.res.summary$per.million.mean)*1e-6*n_portions_per_person*pop/pop*1e6
)

#make a table out of the lsit
output.mean.df <-as.data.frame(matrix(NA, ncol = 4, nrow = length(output.mean), 
                                      dimnames = list(names(output.mean),paste(c('all.parts', 'shoulder', 'belly', 'loin'),  '- mean'))))
for(i.iter in 1:length(output.mean)){
  temp <- output.mean[[i.iter]]
  #temp <- sapply(temp, function(j.iter) round(j.iter, digits = 2))
  if(length(temp)>1){
    output.mean.df[i.iter,2:4] <-sapply(temp, function(j.iter) round(j.iter, digit =3))
  }else{
    output.mean.df[i.iter,1] <- temp
  }
}

output.CI <- list(A.FlaseNegPool = paste(swine.table.summary$p.falseneg.p2.5, '-', swine.table.summary$p.falseneg.p97.5),
                  B.PosCarcInPool = paste(swine.table.summary$p.carc.p2.5,'-', swine.table.summary$p.carc.p97.5), 
                  C.TrichInfecPortFromPosCarc = paste(table.res.summary$p.portion.p2.5,'-', table.res.summary$p.portion.p97.5),
                  D.TrichInfecCookPortFromPosCarc = paste(table.res.summary$p.cooked.p2.5,'-',table.res.summary$p.cooked.p97.5),
                  E.PIllFromCookPortFormPosCarc = paste(table.res.summary$p.ill.p2.5,'-', table.res.summary$p.ill.p97.5),
                  F.NIllPerMilPort = paste(table.res.summary$per.million.p2.5,'-',table.res.summary$per.million.p97.5 ),
                  G.AvgNIllPerMilPort= paste(mean(table.res.summary$per.million.p2.5),'-', mean(table.res.summary$per.million.p97.5)),
                  K.TotPredHumCasePerYear = paste(mean(table.res.summary$per.million.p2.5)*n_portions_per_person*pop*1e-6,'-', mean(table.res.summary$per.million.p97.5)*n_portions_per_person*pop*1e-6),
                  L.PredHumCasePerMilPerYear = paste(mean(table.res.summary$per.million.p2.5)*n_portions_per_person,'-', mean(table.res.summary$per.million.p97.5)*n_portions_per_person)
)

#make a data.frame out of the lsit
output.CI.df <-as.data.frame(matrix(NA, ncol = 4, nrow = length(output.CI), 
                                    dimnames = list(names(output.CI),paste(c('all.parts', 'shoulder', 'belly', 'loin'), '- CI'))))
for(i.iter in 1:length(output.CI)){
  temp <- output.CI[[i.iter]]
  #temp <- sapply(temp, function(j.iter) round(j.iter, digits = 2))
  if(length(temp)>1){
    output.CI.df[i.iter,2:4] <-temp
  }else{
    output.CI.df[i.iter,1] <- temp
  }
}

output <- cbind(output.mean.df, output.CI.df)
