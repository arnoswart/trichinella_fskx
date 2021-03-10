#setwd( "R:/Projecten/V092112 Trichinella/AAA - model/Model/Swine infection" )
setwd( "/s-schijf/swarta/Trichinella RA/Swine infection")

library( dplyr )

source( "Truncated.R" )
source( "sim.swine.R" )

#type.animal <- "boar"
type.animal <- "pig"

# Negative binomial parameters for 50g, as estimated by Katsuhisa
# This is scaled up later to 100g
if( type.animal=="boar" ){
  # Wild Boar
  m <- 5.244308845834319
  k <- 0.00044705143472671417
  propDiaphragm <- 0.05 # 5% of diaphragm tested
  nSwine <- 114266
  swine.per.pool <- 20
  base.filename <- "sim.boar"
}else if(type.animal=="pig"){
  # Pig
  m <- 0.00159125
  k <- 1.3523e-7
  propDiaphragm <- 0.01 # 1% of diaphragm tested
  nSwine <- 22879163
  swine.per.pool <- 100
  base.filename <- "sim.pig"
} else stop( "Error. Unknown option for animal type")

for( i in 1:50 ){
  escapedSwine <- sim.swine( m, k, nSwine, swine.per.pool, propDiaphragm )

  files <- dir( pattern = paste0( base.filename, "[0-9]+.csv") )
  if( length(files)==0 ){
    newfile <- paste0( base.filename, "001.csv")
  }else{
    newfile = sprintf(  paste0( base.filename, "%03d.csv"),
                                as.numeric( substr( files[length( files)], nchar( base.filename)+1,nchar( base.filename)+3 ) )+1 )
  }
  
  write.csv( escapedSwine, newfile, row.names=F )
}