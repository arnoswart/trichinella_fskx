#
# Simulate an fskx lab workflow
#

rm(list=ls())

# Assuming that a project file was used,
# 'here' always brings you to the top folder
library( here )

setwd( here() )

setwd( "./1. meatproduction/")
source( "./simulations/QuickPig.r")
source( "model.r" )

setwd("../2. consumption/")
source( "./simulations/defaultSimulation.r")
source( "model.r" )

setwd("../3. doseresponse/")
source( "./simulations/defaultSimulation.r")
source( "model.r" )
