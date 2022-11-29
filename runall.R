rm( list=ls() )

library( here )
library( tidyverse )

setwd( here("1. meatproduction/") )
source( "./simulations/defaultSimulation.r" )
source( "model.r")
#source( "visualization.r")

setwd( here("2. consumption") )
source( "./simulations/defaultSimulation.r")
source( "model.r")
#source( "visualization.r")

setwd( here("3A inactivation old") )
#setwd( here("3B inactivation new") )
source( "./simulations/defaultSimulation.r")
source( "model.r")

setwd( here("4. cooking model") )
source( "./simulations/wellDoneUSDA.r")
source( "model.r")
source( "visualization.r")

setwd( here("5. doseresponse") )
source( "./simulations/defaultSimulation.r")
source( "model.r")
source( "visualization.r")

setwd( here("6. risk characterisation") )
source( "./simulations/defaultSimulation.r")
source( "model.r")
source( "visualization.r")
