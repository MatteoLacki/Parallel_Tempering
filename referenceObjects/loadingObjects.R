directory <- "/home/matteo/Documents/Scienza/Laurea_di_Matematica/Implementation"
setwd(directory)
source("./referenceObjects/simulations.R")
source("./referenceObjects/parallelTempering.R")


########################################################### TESTS

source("./Distributions_to_check/Liang_Example.R")

LiangWangExample <- ParallelTempering$new(
	noOfIterations	= 2,
	temperatures 	= c(2.8, 7.7, 21.6, 60),	
	strategyNumber  = 2,
	problemDimension= 2,
	targetDensity	= LIANG_TARGET_DENSITY,
	detailedOutput	= TRUE
)



LiangWangExample
LiangWangExample$stateSpace
LiangWangExample$simulate()

ParallelTempering$methods()
