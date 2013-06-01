directory <- "/home/matteo/Documents/Scienza/Laurea_di_Matematica/Implementation"
setwd(directory)
source("./referenceObjects/simulations.R")
source("./referenceObjects/parallelTempering.R")


########################################################### TESTS
ls()
source("./Distributions_to_check/Liang_Example.R")

LiangWangExample <- parallelTemperingSimulation$new(
	noOfIterations	= 2,
	temperatures 	= c(2.8, 7.7, 21.6, 60),	
	strategyNumber  = 2,
	problemDimension= 2,
	targetDensity	= LIANG_TARGET_DENSITY
)






z <- parallelTemperingSimulation$new(
	noOfIterations	=2,	
	temperatures	= c(1.4, 5.4, 6.9, 10),
	strategyNumber	=2,
	problemDimension=2,
	targetDensity	=Aha
)
z
z$stateSpaceStructure
z$currentStatesLogUnnormalisedDensities
z$indicesOfStatesUpdatedInRandomWalk

ls()

realFiniteDimensionalStateSpaceStructure$updateLogsOfUnnormalisedDensities()


source("./referenceObjects/simulations.R")
y <- parallelTemperingSimulation$new(
	noOfIterations	=2,	
	temperatures	= c(1.4, 5.4, 6.9, 10),
	strategyNumber	=2,
	problemDimension=2,
	targetDensity	=Aha,
	proposalCovariances = matrix(c(4,0,0,2),2,2)
)
y
y$stateSpaceStructure

z <- parallelTemperingSimulation$new(
	noOfIterations	=2,	
	temperatures	= c(1.4, 5.4, 6.9, 10),
	strategyNumber	=2,
	problemDimension=2,
	targetDensity	=Aha,
	proposalCovariances = list(matrix(c(5,0,0,2),2,2),matrix(c(6,0,0,2),2,2),matrix(c(4,0,0,2),2,2),matrix(c(4,0,0,2),2,2),matrix(c(4,0,0,2),2,2))
)
z
z$stateSpaceStructure



system.time(
	parallelTemperingSimulation$new(
		noOfIterations	=100000,	
		temperatures	= c(1.4, 5.4, 6.9, 10),
		strategyNumber	=2,
		problemDimension=2,
		targetDensity	=Aha,
		proposalsCovariance = matrix(nrow=0, ncol=0)
	)
)


g <- function(x)
{
	class(x)
}

g(matrix(1:4,2,2))

a <- matrix(1:3, nrow=3, ncol=1)

apply(a,2, function(x) crossprod(x))
a <- cbind(a,a)
apply(a,2, function(x) crossprod(x))

rep(TRUE,times=3)

(1:5)[c(rep(TRUE,times=4),FALSE)]

integer(133L)
