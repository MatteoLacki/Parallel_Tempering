directory <- "/home/matteo/Documents/Scienza/Laurea_di_Matematica/Implementation"
setwd(directory)
source("./referenceObjects/simulations.R")
source("./referenceObjects/parallelTempering.R")


########################################################### TESTS

Aha <- function(x){ return(dnorm(x)) }

z <- parallelTemperingSimulation$new(
	noOfIterations	=2,	
	temperatures	= c(1.4, 5.4, 6.9, 10),
	strategyNumber	=2,
	problemDimension=2,
	targetDensity	=Aha
)
z
z$stateSpaceStructure

z$stateSpaceStructure$quasiMetric
z$stateSpaceStructure$targetDensity
z$stateSpaceStructure

z$translatorFromLexicOrderToTranspositions

system.time(
	parallelTemperingSimulation$new(
		noOfIterations	=100000,	
		temperatures	= c(1.4, 5.4, 6.9, 10),
		strategyNumber	=2,
		problemDimension=2,
		targetDensity	=Aha,
		proposalsCovariance	= matrix(nrow=0, ncol=0)
	)
)





