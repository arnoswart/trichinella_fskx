rm( list=ls() )
 
library( here )
library( tidyverse )

setwd( here("1. meatproduction/") )
source( "./simulations/QuickPig.r" )
source( "model.r")
source( "visualization.r")

setwd( here("2. consumption") )
source( "./simulations/defaultSimulation.r")
source( "model.r")

#setwd( here("3A. cooking old model") )
setwd( here("3B. cooking new model") )

source( "./simulations/defaultSimulation.r")
source( "model.r")
source( "visualisation.r")

setwd( here("4. doseresponse") )
source( "./simulations/defaultSimulation.r")
source( "model.r")

setwd( here("5. risk characterisation") )
source( "./simulations/defaultSimulation.r")
source( "model.r")
source( "visualization.r")
