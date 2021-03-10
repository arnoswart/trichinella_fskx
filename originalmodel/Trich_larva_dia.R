#setwd("R:/Projecten/V092112 Trichinella/AAA/Model")
#setwd( "/s-schijf/swarta/Trichinella RA" )
#setwd( "/s-schijf/franssef/AAA - model/Model" )
setwd("//client/C$/Users/franssef/model")


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
#type.animal <- "pig"
type.animal <- "pig.controlled"
#type.animal <- "custom"


iter <- 10000  #number of iterations for type.animal = "custom" => TO BE ADAPTED!!
test.sens <- 0 #test sensitivity of artificial digestion at meat inspection -> values between 0 and 1
max.larvae <- 3400 #maxium LPG for larvae.dia
# adjust m&k




  base.filename <- "scenario.custom"
  
  ######################################
  # Adjust m and k CHOOSE HOST & LEVEL #
  ######################################
  
  m <- 0.00159125 # domestic pigs Poland
  k <- 1.3523e-7  # domestic pigs Poland
  
  #m <- 0.00687212 # domestic pigs non-controlled housing EU-27
  #k <- 5.82858E-7 # domestic pigs non-controlled housing EU-27
  
  #m <-  # domestic pigs controlled housing EU-27
  #k <-  # domestic pigs controlled housing EU-27
  
  #m <- 5.24    # wild boar
  #k <- 4.47e-4 # wild boar
  
  
  #m <- m/10
  #m <- m/100
  #m <- m/1000
  m <- m/3000
  #m <- m/10000
  
  #k <- k/10
  #k <- k/100
  #k <- k/1000
  k <- k/3000
  #k <- k/10000
  
  ##################
  
  
  #n.swine <- 22879163   # annual no. of swine Poland
  #n.swine <- 114266      # annual no of wild boar Poland
  #n.swine <- 120000000  # annual no. of swine controlled housing whole EU
  #n.swine <- 119002199  # actual annual no. of swine controlled housing whole EU
  #n.swine <- 78935978   # actual annual no. of swine non-controlled housing whole EU
  
  n.swine <- 120000000
 
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
    
  }  else if(type.animal=="pig"){
    # Pig
    n.swine <- 22879163
    base.filename <- "pig"
    w <- c(2, 35, 54, 144, 121, 48 )
    p <- c(0.01103, 0.05903, 0.16183, 0.11588, 0.13571, 0.51652 )
    
    n.portions.per.person <- 403 # all cunsumed portions of pork in Poland
    
  }  else if(type.animal=="pig.controlled"){
    # Pig
    n.swine <- 120000000
    base.filename <- "pig.controlled"
    w <- c(2, 35, 54, 144, 121, 48 )
    p <- c(0.01103, 0.05903, 0.16183, 0.11588, 0.13571, 0.51652 )
    
    n.portions.per.person <- 403 # all cunsumed portions of pork in Poland
    
  } else if(type.animal=="custom"){  

########################################
# Load infection status list of swine  #
########################################

  source("./Swine infection/sim.swine.R" )
  larvae.dia <- list()
  
  for( i in 1:iter ){  ### <<<------ DIT GETAL AANPASSEN
    escapedSwine <- sim.swine( m, k, n.swine, swine.per.pool, propDiaphragm, sens=test.sens ) ### sens = test sensitivity at slaughterhouse
    
    if( nrow( escapedSwine )==0 ){
      larvae.dia[[i]] <- data.frame(larva=NA, Freq=NA)
    }else{
      larvae.dia[[i]] <- escapedSwine %>% filter( larva < max.larvae )  ### <<<------ DIT GETAL AANPASSEN
    }
  }
}

n.sim   <- length( larvae.dia ) # Number of simulations


pb <- txtProgressBar(min = 0, max = n.sim, style=3 )

loop <- 0
for( larvae in larvae.dia )
  
 setTxtProgressBar(pb, loop <- loop + 1 )


 