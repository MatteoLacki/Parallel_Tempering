directory <- "/home/matteo/Documents/Scienza/Laurea_di_Matematica/Implementation"
setwd(directory)
source("./referenceObjects/simulations.R")
source("./referenceObjects/parallelTempering.R")


########################################################### TESTS

Aha <- function(x){ return(dnorm(x)) }

z <- parallelTemperingSimulation$new(
	noOfIterations	=10,	
	temperatures	= c(1.4, 5.4, 6.9, 10),
	strategyNumber	=2,
	problemDimension=2,
	targetDensity	=Aha
)
z

z$stateSpaceStructure$quasiMetric
z$stateSpaceStructure$targetDensity
z$stateSpaceStructure
