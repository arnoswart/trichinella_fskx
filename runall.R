rm( list=ls() )

library( here )
library( tidyverse )

start_time <- Sys.time()

setwd( here("1. meatproduction/") )
source( "./simulations/Pig.R" )
source( "model.R")
source( "visualization.R")

setwd( here("2. consumption") )
source( "./simulations/defaultSimulation.r")
source( "model.r")
#source( "visualization.r")

setwd( here("3A inactivation old") )
#setwd( here("3B inactivation new") )
source( "./simulations/defaultSimulation.r")
source( "model.R")

setwd( here("4. cooking model") )
source( "./simulations/wellDoneUSDA.R")
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

end_time <- Sys.time()
end_time - start_time
