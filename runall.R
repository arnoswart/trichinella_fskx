library( here )
library( tidyverse )

rm( list=ls() )

setwd( here("1. meatproduction/") )
source( "./simulations/QuickPig.r" )
source( "model.r")

setwd( here("2. consumption") )
source( "./simulations/defaultSimulation.r")
source( "model.r")

setwd( here("3. doseresponse") )
source( "./simulations/defaultSimulation.r")
source( "model.r")

setwd( here("4. risk characterisation") )
source( "./simulations/defaultSimulation.r")
