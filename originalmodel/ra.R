#setwd("R:/Projecten/V092112 Trichinella/AAA - model/Model")
#setwd( "/s-schijf/swarta/Trichinella RA" )
setwd( "/s-schijf/franssef/AAA - model/Model" )

library( dplyr )
library( ggplot2 )
library( xtable )

source( "./Functions/read.b.R" )
source( "./Functions/sampleNM.R" )
source( "./Functions/load.larvae.numbers.R" )
source( "./Functions/addtotable.R" )
source( "./Inactivation model/inactivation.R" )
source( "./Dose Response/dr.r")

#type.animal <- "boar"
type.animal <- "pig"
#type.animal <- "custom"


# a and b parameters for dose-response
ab      <- read.csv( file = "./Dose Response/trich-ab.csv", header=F,colClasses=c("numeric","numeric") )
n.carc  <- 5000             # Number of carcasses to cook
pop     <- 230020458 / 6    # Population of Poland


#iter <- 5000  #number of iterations => TO BE ADAPTED!!



if( type.animal=="boar" ){
  # Wild Boar
  n.swine <- 114266               # Number of swine, per simulation, all of them, both pos and neg
  base.filename <- "swine"
  
  # calculate # trichinella in total weight of parts (Table 5 of manuscript)
  #in order this is diaphragm, shoulder, belly,loin,ham, other
  w <- c( 2, 50, 76, 103, 120, 26 )
  # calculated average p over eight years from data Anne Mayer-Scholl (Table 6 of manuscri)
  p <- c( 0.01344, 0.102788, 0.2776, 0.10103, 0.164062,0.3410 )
  n.portions.per.person <- 0.68
  
}else if(type.animal=="pig"){
  # Pig
  n.swine <- 22879163
  base.filename <- "pig"
  w <- c(2, 35, 54, 144, 121, 48 )
  p <- c(0.01103, 0.05903, 0.16183, 0.11588, 0.13571, 0.51652 )
  
  #n.portions.per.person <- 403
  n.portions.per.person <- 246  #corrected for proportion of portions shoulder, loin & belly = 58.99% of portions
} else if(type.animal=="custom"){
  
  base.filename <- "scenario.custom"
 
  m <- 0.00159125
  k <- 1.3523e-7
 
  ##################
  # Adjust m and k #
  ##################
  
  #m <- m/10
  #m <- m/100
  #m <- m/1000
  #m <- m/10000
  
   
  #k <- k/10
  #k <- k/100
  #k <- k/1000
  #k <- k/10000

  ###########################################################
  #  parameters for simulation pigs from controlled housing #
  ###########################################################
  
  n.swine <- 120000000  # annual no. of swine whole EU
  pop <- 508000000 # population of EU
  #m <- m/152
  #m <- m/2500
  
  #k <- k/152
  #k <- k/2500
  
  
  
  #n.swine <- 22879163   # annual no. of swine Poland
  
  
  swine.per.pool <- 100 #CHOOSE 100 for pigs, 20 for wild boar
  propDiaphragm <- 0.01 # 1g of diaphragm tested
  #propDiaphragm <- 0.05 # 5g of diaphragm tested
  
  w <- c(2, 35, 54, 144, 121, 48 )
  p <- c(0.01103, 0.05903, 0.16183, 0.11588, 0.13571, 0.51652 )
  
  n.portions.per.person <- 403
}else stop( "Error. Unknown option for animal type")



swine.table <- data.frame( 
                           n.swine=numeric(0),
                           n.falseneg=numeric(0), 
                           p.falseneg=numeric(0), 
                           n.zeros=numeric(0), 
                           n.nzeros=numeric(0), 
                           p.carc=numeric(0)
                         )

table <- data.frame(n.portions = numeric(0), 
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

########################################
# Load infection status list of swine  #
########################################
if( type.animal=="custom" ){
  source("./Swine infection/sim.swine.R" )
  larvae.dia <- list()
  
  for( i in 1:iter ){  ### <<<------ DIT GETAL AANPASSEN
    escapedSwine <- sim.swine( m, k, n.swine, swine.per.pool, propDiaphragm )

    if( nrow( escapedSwine )==0 ){
      larvae.dia[[i]] <- data.frame(larva=NA, Freq=NA)
    }else{
      larvae.dia[[i]] <- escapedSwine
    }
  }
}else{
  larvae.dia <- load.larvae.numbers( type.animal, plot.dist=F ) # Larvae numbers per g grams, different for different animal
}

n.sim   <- length( larvae.dia ) # Number of simulations


pb <- txtProgressBar(min = 0, max = n.sim, style=3 )

loop <- 0
for( larvae in larvae.dia )
{ 
  setTxtProgressBar(pb, loop <- loop + 1 )
  if( nrow( larvae) > 1 )
  {

    ############################################
    # Calculate total larvae in muscle groups. #
    ############################################
    
    n.zeros   <- larvae$Freq[1]            # Number of negative carcasses, from false negative batches
    larvae    <- larvae[-1, ]              # Row with zeros not needed any more.
    larvae$cs <- cumsum(larvae$Freq)       # Precalculate for later
    n.nzeros  <- larvae$cs[length(larvae$cs)]
  
    ############################################
    # For a large number of carcasses:
    #  Make portions
    #  Cook
    #  Dose response
    ############################################
    
    
    shoulder <- list( x=numeric(0), ill=numeric(0), zeros=0, zeros.after.cooking=0 )
    loin     <- list( x=numeric(0), ill=numeric(0), zeros=0, zeros.after.cooking=0 )
    belly    <- list( x=numeric(0), ill=numeric(0), zeros=0, zeros.after.cooking=0 )
    ham      <- list( x=numeric(0), ill=numeric(0), zeros=0, zeros.after.cooking=0 )
    
    for( i in 1:n.carc )
    {
      # next two lines proportionally pick a row
      smp <- sample.int(max(larvae$cs), 1 )
      row <- which( larvae$cs >= smp )[1]
      
      # Make a realisation of division of larva over muscle groups,
      # to be interpreteted as total, i.e. per w[i] portions.
      
      sample <- sampleNM( n=1, p=p, m=larvae$larva[row]*w[1] )
   
      ############################################
      # Make portions                            #
      ############################################
      #in order this is 1 diaphragm, 2 shoulder, 3 belly,4 loin,5 ham, 6 other
      
      shoulder$x <- rmultinom(1, sample[2], rep(1/w[2], w[2]))
      belly$x    <- rmultinom(1, sample[3], rep(1/w[3], w[3]))
      loin$x     <- rmultinom(1, sample[4], rep(1/w[4], w[4]))
      ham$x      <- rmultinom(1, sample[5], rep(1/w[5], w[5]))
      
      shoulder   <- remove.zeros( shoulder, cooked=F )
      loin       <- remove.zeros( loin, cooked=F )
      belly      <- remove.zeros( belly, cooked=F )
      ham        <- remove.zeros( ham, cooked=F )
      
      ############################################
      # Cook the portions                        #
      ############################################
      
      scenario <- 1 # CHOOSE scenario here: 0 for everyone uncooked, 1 for Baseline 90% welldone Chef & 10% Medium Swart
      
      shoulder <- cook( shoulder, scenario ) # For convenience, the cooking includes removal of zeros
      loin     <- cook( loin, scenario )
      belly    <- cook( belly, scenario )
      #ham      <- cook( ham, 1 ) # TODO this is probably not what you want
      
      shoulder$ill <- c( shoulder$ill, dose.response(ab, shoulder$x) )
      loin$ill     <- c( loin$ill, dose.response(ab, loin$x) )
      belly$ill    <- c( belly$ill, dose.response(ab, belly$x) )
      #ham$ill      <- c( ham$ill, dose.response(ab, ham$x) )
    }
    
    swine.table <- add.to.swine.table( swine.table, n.swine, n.zeros, n.nzeros )
    
    table <- add.to.table( table, n.carc, w, n.nzeros/n.swine, shoulder, belly, loin )
  } 
  else 
  { #no false negative batches this year
    swine.table <- add.to.swine.table( swine.table, n.swine, n.zeros=0, n.nzeros=0 )
    
    shoulder <- list( zeros = n.carc * w[2], zeros.after.cooking=0, ill=0 )
    belly <- list( zeros = n.carc * w[3], zeros.after.cooking=0, ill=0 )
    loin <- list( zeros = n.carc * w[4], zeros.after.cooking=0, ill=0 )
    table <- add.to.table( table, n.carc, w, p.swine=0, shoulder, belly, loin )
  }
  
}

# Make now also an per.million for the sum of the muscle parts

t <- table %>% mutate( iteration = rep( 1:length( larvae.dia), each=3)) %>%
  group_by( iteration ) %>% 
  summarize( per.million.portions = mean( per.million ) ) %>% 
  summarize( per.million.portions.mean = mean( per.million.portions), 
             per.million.portions.p2.5 = quantile( per.million.portions, 0.025, na.rm=T ),
             per.million.portions.p97.5 = quantile( per.million.portions, 0.975, na.rm=T)
             ) %>%
  mutate( type="all muscle", n.portions.per.person=n.portions.per.person, pop=pop) %>% 
  mutate( tot.consumed.portions = n.portions.per.person *pop,
          cases.mean = tot.consumed.portions * per.million.portions.mean / 10^6,
          cases.p2.5 = tot.consumed.portions * per.million.portions.p2.5 / 10^6,
          cases.p97.5 = tot.consumed.portions * per.million.portions.p97.5 / 10^6,
          cases.per.milion.persons.mean =  n.portions.per.person * per.million.portions.mean,
          cases.per.milion.persons.p2.5 =  n.portions.per.person * per.million.portions.p2.5,
          cases.per.milion.persons.p97.5 =  n.portions.per.person * per.million.portions.p97.5
          )

table <- table %>% group_by( type) %>% summarize( p.portion.mean = mean( p.portion ), 
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

swine.table <- swine.table %>% summarize( p.falseneg.mean = mean( p.falseneg, na.rm=T),
                                          p.falseneg.p2.5 = quantile( p.falseneg, 0.025, na.rm=T),
                                          p.falseneg.p97.5 = quantile( p.falseneg, 0.975, na.rm=T ),
                                          p.carc.mean = mean( p.carc, na.rm=T),
                                          p.carc.p2.5 = quantile( p.carc, 0.025, na.rm=T),
                                          p.carc.p97.5 = quantile( p.carc, 0.975, na.rm=T )
                                        )

print( xtable(swine.table, digits = -2),  include.rownames=F, file=paste0("./Outcomes/", base.filename, ".table.html"), type='html')
print( xtable(table, digits=-2 ),  include.rownames=F, file=paste0("./Outcomes/", base.filename, ".parts.table.html"), type='html')
print( xtable(t, digits=-2 ),  include.rownames=F, file=paste0("./Outcomes/", base.filename, ".riskcharacterisation.table.html"), type='html')
